/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang3.text;

import static org.junit.jupiter.api.Assertions.assertFalse;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Reader;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;

import org.apache.commons.lang3.SerializationUtils;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link StrBuilder#clear()} zero-copy APIs expose backing char buffer.
 * <p>
 * readFrom(Readable) Reader branch: reads directly into the internal char[] buffer, so a Reader that is also an attacker can observe stale chars in that buffer
 * beyond the logical content.
 * </p>
 *
 * <p>
 * Pre-patch: A Reader can inspect chars beyond the current write position.
 * </p>
 * <p>
 * Post-patch: readFrom uses a temporary buffer so stale internal chars are not exposed.
 * </p>
 */
@SuppressWarnings("deprecation")
public class StrBuilderClearTest {

    /**
     * A Reader that, upon reading, inspects the char array it has been given access to (positions beyond offset+len that may contain stale data), records them,
     * then supplies its normal data.
     */
    static class SpyReader extends Reader {

        private boolean done;
        private char[] observedExtra;
        private final char[] supply;

        SpyReader(final String supply) {
            this.supply = supply.toCharArray();
        }

        @Override
        public void close() {
            // empty
        }

        boolean observedStaleChars(final String marker) {
            if (observedExtra == null) {
                return false;
            }
            return new String(observedExtra).contains(marker);
        }

        @Override
        public int read(final char[] cbuf, final int off, final int len) {
            if (done) {
                return -1;
            }
            done = true;
            // Record chars in the buffer beyond where we will write
            final int toWrite = Math.min(supply.length, len);
            final int staleStart = off + toWrite;
            final int staleLen = cbuf.length - staleStart;
            if (staleLen > 0) {
                observedExtra = new char[staleLen];
                System.arraycopy(cbuf, staleStart, observedExtra, 0, staleLen);
            }
            System.arraycopy(supply, 0, cbuf, off, toWrite);
            return toWrite;
        }
    }

    /** Search for a string encoded as UTF-16BE (2 bytes per char) in a byte array. */
    private static boolean containsUtf16Be(final byte[] haystack, final String needle) throws IOException {
        final byte[] needleBytes = needle.getBytes(StandardCharsets.UTF_16BE);
        outer: for (int i = 0; i <= haystack.length - needleBytes.length; i++) {
            for (int j = 0; j < needleBytes.length; j++) {
                if (haystack[i + j] != needleBytes[j]) {
                    continue outer;
                }
            }
            return true;
        }
        return false;
    }

    @Test
    public void testDeserializedStrBuilderHasNoStaleBufferContent() throws Exception {
        final StrBuilder sb = new StrBuilder("secret_password_xyzzy");
        sb.clear();
        sb.append("safe");
        final byte[] serialized = SerializationUtils.serialize(sb);
        final StrBuilder sb2;
        // Deserialize and inspect the buffer
        try (ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(serialized))) {
            sb2 = (StrBuilder) ois.readObject();
        }
        final Field bufField = StrBuilder.class.getDeclaredField("buffer");
        bufField.setAccessible(true);
        final Field sizeField = StrBuilder.class.getDeclaredField("size");
        sizeField.setAccessible(true);
        final char[] buf2 = (char[]) bufField.get(sb2);
        final String bufContent = new String(buf2);
        assertFalse(bufContent.contains("secret_password"), "Deserialized StrBuilder buffer must not contain stale chars: " + bufContent);
    }

    @Test
    public void testReadFromReaderDoesNotExposeStaleInternalBuffer() throws IOException {
        final StrBuilder sb = new StrBuilder();
        // Write a long "secret" string to fill the internal buffer
        sb.append("SECRET_DATA_SHOULD_NOT_LEAK_ABCDEFGHIJ");
        // Now clear it (logical size goes to 0, but internal buffer still holds old chars)
        sb.clear();
        // sb now has logical size 0 but internal buffer still contains "SECRET_DATA..."
        // Now readFrom a SpyReader that only supplies short data but inspects the buffer.
        // The spy observes what's in the buffer BEYOND the bytes it writes.
        // We write "hi" (2 chars), so stale content starts at position 2:
        // "CRET_DATA_SHOULD_NOT_LEAK_ABCDEFGHIJ..." is at positions 2+.
        try (SpyReader spy = new SpyReader("hi")) {
            sb.readFrom(spy);
            // Post-patch: stale chars must NOT be visible to the Reader
            assertFalse(spy.observedStaleChars("_DATA_SHOULD_NOT_LEAK"));
        }
    }

    @Test
    public void testStaleCharsNotLeakedAfterClear() throws Exception {
        final StrBuilder sb = new StrBuilder("secret_password_xyzzy_leak");
        // clear() resets logical size to 0 but leaves chars in buffer
        sb.clear();
        // append something shorter than the original
        sb.append("ok");
        // Stale content is serialized as UTF-16BE char[] data.
        // "xyzzy_leak" was at positions 15+, well beyond "ok" (len=2), so must not appear.
        assertFalse(containsUtf16Be(SerializationUtils.serialize(sb), "xyzzy_leak"));
    }

    @Test
    public void testStaleCharsNotLeakedAfterTruncate() throws Exception {
        final StrBuilder sb = new StrBuilder("top_secret_key_material");
        // truncate to a short length – tail remains in buffer
        sb.delete(6, sb.length());
        // sb now logically contains "top_se"
        assertFalse(containsUtf16Be(SerializationUtils.serialize(sb), "secret_key_material"));
    }
}

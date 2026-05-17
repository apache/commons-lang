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

import java.io.IOException;
import java.io.Reader;

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
}

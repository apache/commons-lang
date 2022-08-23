/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang3;

import java.nio.charset.Charset;
import java.nio.charset.IllegalCharsetNameException;

/**
 * Character encoding names required of every implementation of the Java platform.
 *
 * <p>According to <a href="https://docs.oracle.com/javase/8/docs/api/java/nio/charset/Charset.html">JRE character
 * encoding names</a>:</p>
 *
 * <p><cite>Every implementation of the Java platform is required to support the following character encodings.
 * Consult the release documentation for your implementation to see if any other encodings are supported.
 * </cite></p>
 *
 * @see <a href="https://docs.oracle.com/javase/8/docs/technotes/guides/intl/encoding.doc.html">JRE character encoding names</a>
 * @since 2.1
 * @deprecated Java 7 introduced {@link java.nio.charset.StandardCharsets}, which defines these constants as
 * {@link Charset} objects. Use {@link Charset#name()} to get the string values provided in this class.
 * This class will be removed in a future release.
 */
@Deprecated
public class CharEncoding {

    /**
     * ISO Latin Alphabet #1, also known as ISO-LATIN-1.
     *
     * <p>Every implementation of the Java platform is required to support this character encoding.</p>
     */
    public static final String ISO_8859_1 = "ISO-8859-1";

    /**
     * Seven-bit ASCII, also known as ISO646-US, also known as the Basic Latin block
     * of the Unicode character set.
     *
     * <p>Every implementation of the Java platform is required to support this character encoding.</p>
     */
    public static final String US_ASCII = "US-ASCII";

    /**
     * Sixteen-bit Unicode Transformation Format, byte order specified by a mandatory initial
     * byte-order mark (either order accepted on input, big-endian used on output).
     *
     * <p>Every implementation of the Java platform is required to support this character encoding.</p>
     */
    public static final String UTF_16 = "UTF-16";

    /**
     * Sixteen-bit Unicode Transformation Format, big-endian byte order.
     *
     * <p>Every implementation of the Java platform is required to support this character encoding.</p>
     */
    public static final String UTF_16BE = "UTF-16BE";

    /**
     * Sixteen-bit Unicode Transformation Format, little-endian byte order.
     *
     * <p>Every implementation of the Java platform is required to support this character encoding.</p>
     */
    public static final String UTF_16LE = "UTF-16LE";

    /**
     * Eight-bit Unicode Transformation Format.
     *
     * <p>Every implementation of the Java platform is required to support this character encoding.</p>
     */
    public static final String UTF_8 = "UTF-8";

    /**
     * Returns whether the named charset is supported.
     *
     * <p>This is similar to <a
     * href="https://docs.oracle.com/javase/8/docs/api/java/nio/charset/Charset.html#isSupported%28java.lang.String%29">
     * java.nio.charset.Charset.isSupported(String)</a> but handles more formats</p>
     *
     * @param name  the name of the requested charset; may be either a canonical name or an alias, null returns false
     * @return {@code true} if the charset is available in the current Java virtual machine
     * @deprecated Please use {@link Charset#isSupported(String)} instead, although be aware that {@code null}
     * values are not accepted by that method and an {@link IllegalCharsetNameException} may be thrown.
     */
    @Deprecated
    public static boolean isSupported(final String name) {
        if (name == null) {
            return false;
        }
        try {
            return Charset.isSupported(name);
        } catch (final IllegalCharsetNameException ex) {
            return false;
        }
    }

}

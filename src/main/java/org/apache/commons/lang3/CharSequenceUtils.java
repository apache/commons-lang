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

/**
 * Null-safe CharSequence utility methods.
 *
 * @author Gary Gregory
 * @version $Id$
 */
public class CharSequenceUtils {

    /**
     * Returns a new <code>CharSequence</code> that is a subsequence of this
     * sequence starting with the <code>char</code> value at the specified
     * index. The length (in <code>char</code>s) of the returned sequence is
     * <code>length() - start</code>, so if <code>start == end</code> then an
     * empty sequence is returned. </p>
     *
     * @param cs
     *            the specified subsequence, may be null
     * @param start
     *            the start index, inclusive
     * @return a new subsequence or null
     *
     * @throws IndexOutOfBoundsException
     *             if <code>start</code> is negative or if <code>start</code> is
     *             greater than <code>length()</code>
     * @since 3.0
     */
    public static CharSequence subSequence(CharSequence cs, int start) {
        return cs == null ? null : cs.subSequence(start, cs.length());
    }
}

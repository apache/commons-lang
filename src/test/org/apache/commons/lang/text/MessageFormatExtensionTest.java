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
package org.apache.commons.lang.text;

import java.text.FieldPosition;
import java.text.Format;
import java.text.MessageFormat;
import java.text.ParsePosition;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Locale;

/**
 * Extension tests for {@link ExtendedMessageFormat}
 * 
 * @author Matt Benson
 * @since 2.4
 * @version $Id$
 */
public class MessageFormatExtensionTest extends AbstractMessageFormatTest {

    static class ProperNameCapitalizationFormat extends Format {
        private static final long serialVersionUID = -6081911520622186866L;
        private static final StrMatcher MATCH = StrMatcher
                .charSetMatcher(" ,.");

        /*
         * (non-Javadoc)
         * 
         * @see java.text.Format#format(java.lang.Object,
         *      java.lang.StringBuffer, java.text.FieldPosition)
         */
        public StringBuffer format(Object obj, StringBuffer toAppendTo,
                FieldPosition fpos) {
            char[] buffer = String.valueOf(obj).toCharArray();
            ParsePosition pos = new ParsePosition(0);
            while (pos.getIndex() < buffer.length) {
                char c = buffer[pos.getIndex()];
                if (Character.isLowerCase(c)) {
                    c = Character.toUpperCase(c);
                }
                if (Character.isUpperCase(c)) {
                    toAppendTo.append(c);
                    next(pos);
                }
                int start = pos.getIndex();
                seekDelimiter(buffer, pos);
                toAppendTo.append(new String(buffer, start, pos.getIndex()
                        - start).toLowerCase());
            }
            return toAppendTo;
        }

        /**
         * Unable to do much; return the String.
         */
        public Object parseObject(String source, ParsePosition pos) {
            return source.substring(pos.getIndex());
        }

        private static void seekDelimiter(char[] buffer, ParsePosition pos) {
            for (; pos.getIndex() < buffer.length
                    && MATCH.isMatch(buffer, pos.getIndex()) == 0; next(pos))
                ;
            if (pos.getIndex() >= buffer.length) {
                return;
            }
            int len = 0;
            do {
                len = MATCH.isMatch(buffer, pos.getIndex());
                pos.setIndex(pos.getIndex() + len);
            } while (len > 0 && pos.getIndex() < buffer.length);
        }

        private static void next(ParsePosition pos) {
            pos.setIndex(pos.getIndex() + 1);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.commons.lang.text.AbstractMessageFormatTest#createMessageFormat(java.lang.String)
     */
    protected MessageFormat createMessageFormat(String pattern) {
        return new ExtendedMessageFormat(pattern, new MultiFormat.Builder()
                .add(ExtendedMessageFormat.createDefaultMetaFormat(Locale.US)).add(
                        new NameKeyedMetaFormat.Builder().put("properName",
                                new ProperNameCapitalizationFormat())
                                .toNameKeyedMetaFormat()).toMultiFormat());
    }

    public void testProperName() {
        doAssertions("John Q. Public; John Q. Public",
                "{0,properName}; {1,properName}", new String[] {
                        "JOHN Q. PUBLIC", "john q. public" });
    }

    public void testMixed() {
        doAssertions("John Q. Public was born on Thursday, January 1, 1970.",
                "{0,properName} was born on {1,date,full}.", new Object[] {
                        "john q. public",
                        new GregorianCalendar(1970, Calendar.JANUARY, 01, 0,
                                15, 20).getTime() });
    }
}

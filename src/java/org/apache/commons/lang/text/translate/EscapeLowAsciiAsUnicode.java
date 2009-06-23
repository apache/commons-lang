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
package org.apache.commons.lang.text.translate;

import java.io.IOException;
import java.io.Writer;

/**
 * Escapes ASCII under 32 to Unicode, except for the following 
 * special values, '\b \n \t \f \r', which are escaped to their 
 * Java types.
 * @since 3.0
 */
// TODO: Is this not the combination of a LookupTranslator for the 5 
// TODO: special values, followed by a UnicodeEscaper?
// TODO: It means passing a numerical range to the UnicodeEscaper 
// TOOD: to make it only hit < 32.
public class EscapeLowAsciiAsUnicode extends UnicodeEscaper {

    /**
     * {@inheritDoc}
     */
    public boolean translate(int ch, Writer out) throws IOException {
        if (ch < 32) {
            switch (ch) {
                case '\b' :
                    out.write('\\');
                    out.write('b');
                    break;
                case '\n' :
                    out.write('\\');
                    out.write('n');
                    break;
                case '\t' :
                    out.write('\\');
                    out.write('t');
                    break;
                case '\f' :
                    out.write('\\');
                    out.write('f');
                    break;
                case '\r' :
                    out.write('\\');
                    out.write('r');
                    break;
                default :
                    super.translate(ch, out);
                    break;
            }
            return true;
        } else {
            return false;
        }
    }
}

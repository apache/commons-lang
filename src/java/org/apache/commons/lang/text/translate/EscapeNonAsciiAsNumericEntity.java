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
 * Translates codepoints greater than ASCII 127 to their numerical 
 * XML entity.
 * @since 3.0
 */
public class EscapeNonAsciiAsNumericEntity extends CodePointTranslator {

    /**
     * {@inheritDoc}
     */
    public boolean translate(int codepoint, Writer out) throws IOException {
        // TODO: if (codepoint > 0xffff) {
        if (codepoint > 0x7f) {
            out.write("&#");
            out.write(Integer.toString(codepoint, 10));
            out.write(';');
            return true;
        } else {
            return false;
        }
    }

}

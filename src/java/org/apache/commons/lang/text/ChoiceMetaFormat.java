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

import java.text.ChoiceFormat;
import java.text.FieldPosition;
import java.text.ParsePosition;

/**
 * Stock "choice" MetaFormat.
 * 
 * @see ExtendedMessageFormat
 * @author Matt Benson
 * @since 2.4
 * @version $Id$
 */
public class ChoiceMetaFormat extends MetaFormatSupport {
    private static final long serialVersionUID = 3802197832963795129L;

    /**
     * Singleton-usable instance.
     */
    public static final ChoiceMetaFormat INSTANCE = new ChoiceMetaFormat();

    /**
     * Create a new ChoiceMetaFormat.
     */
    public ChoiceMetaFormat() {
        super();
    }

    /**
     * {@inheritDoc}
     */
    public StringBuffer format(Object obj, StringBuffer toAppendTo,
            FieldPosition pos) {
        if (obj instanceof ChoiceFormat) {
            return toAppendTo.append(((ChoiceFormat) obj).toPattern());
        }
        throw new IllegalArgumentException(String.valueOf(obj));
    }

    /**
     * {@inheritDoc}
     */
    public Object parseObject(String source, ParsePosition pos) {
        int start = pos.getIndex();
        seekFormatElementEnd(source, pos);
        return new ChoiceFormat(source.substring(start, pos.getIndex()));
    }

}

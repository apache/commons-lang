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
package org.apache.commons.lang3.text;

import java.text.Format;
import java.util.Locale;

/**
 * Format factory.
 *
 * @since 2.4
 * @deprecated As of 3.6, use Apache Commons Text
 * <a href="https://commons.apache.org/proper/commons-text/javadocs/api-release/org/apache/commons/text/FormatFactory.html">
 * FormatFactory</a> instead
 */
@Deprecated
public interface FormatFactory {

    /**
     * Create or retrieve a format instance.
     *
     * @param name The format type name
     * @param arguments Arguments used to create the format instance. This allows the
     *                  {@link FormatFactory} to implement the "format style"
     *                  concept from {@code java.text.MessageFormat}.
     * @param locale The locale, may be null
     * @return The format instance
     */
    Format getFormat(String name, String arguments, Locale locale);

}

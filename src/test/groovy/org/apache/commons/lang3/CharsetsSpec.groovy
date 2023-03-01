/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.commons.lang3

import spock.lang.Title

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets

@Title("org.apache.commons.lang3.CharsetsTest")
class CharsetsSpec extends AbstractLangSpec {


    def "testToCharset_Charset"() {
        expect:
        Charset.defaultCharset() == Charsets.toCharset((Charset) null)
        Charset.defaultCharset() == Charsets.toCharset(Charset.defaultCharset())
        StandardCharsets.UTF_8 == Charsets.toCharset(StandardCharsets.UTF_8)
    }

    def "testToCharset_String"() {
        expect:
        Charset.defaultCharset() == Charsets.toCharset((String) null)
        Charset.defaultCharset() == Charsets.toCharset(Charset.defaultCharset().name())
        StandardCharsets.UTF_8 == Charsets.toCharset("UTF-8")
    }

    def "testToCharsetName"() {
        expect:
        Charset.defaultCharset().name() == Charsets.toCharsetName((String) null)
        "UTF-8" == Charsets.toCharsetName("UTF-8")
    }
}

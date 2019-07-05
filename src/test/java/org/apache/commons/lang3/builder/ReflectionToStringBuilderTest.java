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
package org.apache.commons.lang3.builder;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class ReflectionToStringBuilderTest {
    private final Integer base = Integer.valueOf(5);

    @Test
    public void testConstructorWithNullObject() {
        assertThrows(IllegalArgumentException.class,
            () -> new ReflectionToStringBuilder(null, ToStringStyle.DEFAULT_STYLE, new StringBuffer()));
    }


    public static class TestJsonStyle{
        private String a;
        public void setA(String a){
            this.a = a;
        }
        @Override
        public String toString() {
            return ReflectionToStringBuilder.toString(this, ToStringStyle.JSON_STYLE);
        }
    }

    @Test
    public void testJsonStyle() {
        TestJsonStyle jsonStyle = new TestJsonStyle();
        jsonStyle.setA("测试");
        Assertions.assertEquals("{\"a\":\"测试\"}",jsonStyle.toString());
    }
}

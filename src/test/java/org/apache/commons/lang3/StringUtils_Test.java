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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Test;

public class StringUtils_TestPK {

    @Test
    void testReverseStringIsNull(){
      assertNull(StringUtils.reverse(null));
    }
    @Test 
    void testReverseStringWithSpaces(){
        assertEquals("dlrow olleh", StringUtils.reverse("hello world"), 
        "Failed test when string is : hello world");
    }
    @Test 
    void testReverseSingleCharacter(){
        assertEquals("p", StringUtils.reverse("p"), 
        "Failed test when string is : p");
    }
    @Test 
    void testReverseEmptyString(){
        assertEquals(" ", StringUtils.reverse(" "), 
        " Failed test when string is Emplty");
    }
    @Test 
    void testReverseString(){
        assertEquals("dlrow", StringUtils.reverse("world"), 
        "Failed test when string is : world");
    }
    @Test
    void testReverseWhenUpperCaseAndLowerCase(){
        assertEquals("dLroW", StringUtils.reverse("WorLd"), 
        "Failed test when inout is : WorLd");
    }
}

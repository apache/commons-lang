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
package org.apache.commons.lang3.math;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

public class NumberUtils_TestPK {
     // parametrized test on max()
    @ParameterizedTest
    @CsvSource({
        "-128 , -128, -128 , -128" , //Test all at min and equal 
        "127 , 127 , 127 , 127"  ,  //Test all at max and equal value
        "0 , 0 , 0 ,0 ", //Test all at typical value
        "-128 , -127 , -126, -126", // Test for all min boundary edge cases
        "127, 126, 125 , 127", // Test for all max boudary edge cases
        "-128, 0, 127, 127", //Test across the entire range
        "-128, 126, 127, 127", // Test with edge value with upper limit
        "-128, -128, 127, 127" //Test all extreme mix values   

    })
    void testMaxByte(byte a, byte b, byte c, byte expected) {

       assertEquals(expected, NumberUtils.max(a,b,c),"Failed for inputs: " + a +"," + b + ", " + c);  
    }

    // white box test on min()
      
    @Test
    void testMinValueNormal(){
        assertEquals(1, NumberUtils.min(5,3,8,1,7));
    }
    @Test
    void testMinValueAtStart(){
        assertEquals(1, NumberUtils.min(1,5,3,8,7));
    }
    @Test
    void testMinValueAtEnd(){
        assertEquals(1, NumberUtils.min(5,3,8,7,1));
    }
    @Test
    void testMinValueWithinNegativeNum(){
        assertEquals(-8, NumberUtils.min(-5,-3,-8,-1,-7));
    }
    @Test
    void testMinValueWithMixNUmbers(){
        assertEquals(-8, NumberUtils.min(-5,3,-8,1,-7));
    }
    @Test
    void testMinValueWithSingleElement(){
        assertEquals(7, NumberUtils.min(7));
    }
    //checking test for indirectly calling ValidateArray
    @Test
    void testMinNullArray(){
        assertThrows(NullPointerException.class, () -> NumberUtils.min((int[]) null));
    }
    @Test
    void testMinEmptyArray(){
        assertThrows(IllegalArgumentException.class, () -> NumberUtils.min ());
    }
       
}

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

import java.io.UnsupportedEncodingException;
import java.util.HashSet;
import java.util.Set;

import org.junit.Assert;
import org.junit.Test;

/**
 * Unit tests for {@link org.apache.commons.lang3.text.AlphabetConverter}.
 */
public class AlphabetConverterTest {

    private static char[] lower_case_english = {' ','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'};
    private static char[] english_and_numbers = {'0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',' ' };
    private static char[] lower_case_english_and_numbers = {'0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',' ' };
    private static char[] numbers = {'0','1','2','3','4','5','6','7','8','9'};
    private static char[] binary = {'0','1'};
    private static char[] hebrew = {'_', ' ', '\u05e7','\u05e8','\u05d0','\u05d8','\u05d5','\u05df','\u05dd','\u05e4','\u05e9','\u05d3','\u05d2','\u05db','\u05e2','\u05d9','\u05d7','\u05dc','\u05da','\u05e3','\u05d6','\u05e1','\u05d1','\u05d4','\u05e0','\u05de','\u05e6','\u05ea','\u05e5'};
    private static char[] empty = {};

    private static int[] unicode = {32,35395,35397,36302,36291,35203,35201,35215,35219,35268,97,98,99,100,101,102,103,104,105,106,107,108,109,110,1001,1002,1003,1004,1005};
    private static int[] lower_case_english_codepoints = {32,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122};
    private static int[] doNotEncodePoints = {32,97,98,99}; // space, a, b, c
    
    private static Set<Character> makeSet(char[] chars) {
        Set<Character> set = new HashSet<Character>();
        
        for (char c : chars) {
            set.add(c);
        }
        
        return set;
    }

    private static Set<Integer> makeCodePointSet(int[] ints) {
        Set<Integer> set = new HashSet<Integer>();
        
        for (int i: ints) {
            set.add(i);
        }
        
        return set;
    }

    @Test
    public void binaryTest() throws UnsupportedEncodingException {
        test(numbers, binary, empty, "12345", "0");
        test(lower_case_english, binary, empty, "abc", "a");
    }

    @Test
    public void hebrewTest() throws UnsupportedEncodingException {
        test(hebrew, binary, empty, "\u05d0", "\u05e2", "\u05d0\u05dc\u05e3_\u05d0\u05d5\u05d4\u05d1\u05dc_\u05d1\u05d9\u05ea_\u05d6\u05d4_\u05d1\u05d9\u05ea_\u05d2\u05d9\u05de\u05dc_\u05d6\u05d4_\u05db\u05de\u05dc_\u05d2\u05d3\u05d5\u05dc");
        test(hebrew, numbers, empty, "\u05d0", "\u05e2", "\u05d0\u05dc\u05e3_\u05d0\u05d5\u05d4\u05d1\u05dc_\u05d1\u05d9\u05ea_\u05d6\u05d4_\u05d1\u05d9\u05ea_\u05d2\u05d9\u05de\u05dc_\u05d6\u05d4_\u05db\u05de\u05dc_\u05d2\u05d3\u05d5\u05dc");
        test(numbers, hebrew, empty, "123456789", "1", "5");
        test(lower_case_english, hebrew, empty, "this is a test");
    }

    @Test
    public void doNotEncodeTest() throws UnsupportedEncodingException {
        test(english_and_numbers, lower_case_english_and_numbers, lower_case_english, "1", "456", "abc", "ABC", "this will not be converted but THIS WILL");
        test(english_and_numbers, lower_case_english_and_numbers, numbers, "1", "456", "abc", "ABC", "this will be converted but 12345 and this will be");
    }
    
    /*
     * Test constructor from code points
     */
    @Test
    public void unicodeTest() throws UnsupportedEncodingException {
        AlphabetConverter ac = AlphabetConverter.createConverter(makeCodePointSet(unicode), makeCodePointSet(lower_case_english_codepoints), makeCodePointSet(doNotEncodePoints));
        
        String original = "\u8a43\u8a45 \u8dce ab \u8dc3 c \u8983";
        String encoded = ac.encode(original);
        String decoded = ac.decode(encoded);
        
        Assert.assertEquals("Encoded '" + original + "' into '" + encoded + "', but decoded into '" + decoded + "'", original, decoded);
    }
    @Test(expected=IllegalArgumentException.class)
    public void noEncodingLettersTest() {
        AlphabetConverter.createConverterFromChars(makeSet(english_and_numbers), makeSet(numbers), makeSet(numbers));
    }

    @Test(expected=IllegalArgumentException.class)
    public void onlyOneEncodingLettersTest() {
        Set<Character> numbersPlusUnderscore = makeSet(numbers);
        
        numbersPlusUnderscore.add('_');
        
        AlphabetConverter.createConverterFromChars(makeSet(english_and_numbers), numbersPlusUnderscore, makeSet(numbers));
    }

    private void test(char[] originalChars, char[] encodingChars, char[] doNotEncodeChars, String... strings) throws UnsupportedEncodingException {
        
        // test AlphabetConverter creation
        Set<Character> originals = makeSet(originalChars);
        Set<Character> encodings = makeSet(encodingChars);
        Set<Character> doNotEncode = makeSet(doNotEncodeChars);
        
        AlphabetConverter ac = AlphabetConverter.createConverterFromChars(originals, encodings, doNotEncode);
        
        AlphabetConverter reconstructedAlphabetConverter = AlphabetConverter.createConverterFromMap(ac.getOriginalToEncoded());
        
        Assert.assertEquals(ac, reconstructedAlphabetConverter);
        Assert.assertEquals(ac.toString(), reconstructedAlphabetConverter.toString());
        Assert.assertEquals(null, ac.encode(null)); // test null conversions
        Assert.assertEquals("", ac.encode("")); // test empty conversion
        
        // test all the trial strings
        for (String s : strings) {
            String encoded = ac.encode(s);

            // test that only encoding chars are used
            for (int i = 0; i < encoded.length(); i++) {
                Assert.assertTrue(encodings.contains(encoded.charAt(i)));
            }
            
            String decoded = ac.decode(encoded);
    
            // test that only the original alphabet is used after decoding
            for (int i = 0; i < decoded.length(); i++) {
                Assert.assertTrue(originals.contains(decoded.charAt(i)));
            }
            
            Assert.assertEquals("Encoded '" + s + "' into '" + encoded + "', but decoded into '" + decoded + "'", s, decoded);
        }
    }
}

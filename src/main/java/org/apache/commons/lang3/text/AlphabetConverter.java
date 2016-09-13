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
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

/**
 * Converts from one alphabet to another, with the possibility of leaving certain characters unencoded. 
 *
 * The target and do not encode languages must be in the Unicode BMP, but the source language does not.
 * 
 * The encoding will all be of a fixed length, except for the 'do not encode' chars, which will be of length 1
 */
public class AlphabetConverter {

    private final Map<Integer, String> originalToEncoded;
    private final Map<String, String> encodedToOriginal;
    private final Map<Integer, String> doNotEncodeMap;
    
    private final int encodedLetterLength;

    private AlphabetConverter(  Map<Integer, String> originalToEncoded,
                                Map<String, String> encodedToOriginal,
                                Map<Integer, String> doNotEncodeMap,
                                int encodedLetterLength) {

        this.originalToEncoded = originalToEncoded;
        this.encodedToOriginal = encodedToOriginal;
        this.doNotEncodeMap = doNotEncodeMap;
        this.encodedLetterLength = encodedLetterLength;
    }

    /**
     * Use this to recreate a new AlphabetConverter from the map received after calling getOriginalToEncoded()
     * 
     *  @param originalToEncoded a map returned from getOriginalToEncoded()
     *  @return the reconstructed AlphabetConverter
     *  @see AlphabetConverter#getOriginalToEncoded()
     */
    public static AlphabetConverter createConverterFromMap (Map<Integer, String> originalToEncoded) {
        Map<String, String> encodedToOriginal = new LinkedHashMap<String,String>();
        Map<Integer, String> doNotEncodeMap = new HashMap<Integer, String>();
        
        int encodedLetterLength = 1;
        
        for (Entry<Integer, String> e : originalToEncoded.entrySet()) {
            String originalAsString = codePointToString(e.getKey());
            encodedToOriginal.put(e.getValue(), originalAsString );
            
            if (e.getValue().equals(originalAsString)) {
                doNotEncodeMap.put(e.getKey(), e.getValue());
            }
            
            if (e.getValue().length() > encodedLetterLength) {
                encodedLetterLength = e.getValue().length(); 
            }
        }

        return new AlphabetConverter(originalToEncoded, encodedToOriginal, doNotEncodeMap, encodedLetterLength);
    }

    /**
     * Creates an alphabet converter, for converting from the original alphabet, to the encoded alphabet, while leaving the characters in
     * doNotEncode as they are (if possible) 
     * 
     *  @param original a Set of chars representing the original alphabet
     *  @param encoding a Set of chars representing the alphabet to be used for encoding
     *  @param doNotEncode a Set of chars to be encoded using the original alphabet - every char here must appear in both the previous params
     *  @return the AlphabetConverter
     *  @throws IllegalArgumentException if an AlphabetConverter cannot be constructed
     */
    public static AlphabetConverter createConverterFromChars (Set<Character> original, Set<Character> encoding, Set<Character> doNotEncode) {
        return AlphabetConverter.createConverter(convertCharsToIntegers(original), convertCharsToIntegers(encoding), convertCharsToIntegers(doNotEncode));
    }
    
    private static Set<Integer> convertCharsToIntegers(Set<Character> chars) {
        Set<Integer> integers = new HashSet<Integer>();
        
        for (Character c : chars) {
            integers.add((int)c);
        }
        
        return integers;
    }
    
    /**
     * Creates an alphabet converter, for converting from the original alphabet, to the encoded alphabet, while leaving the characters in
     * doNotEncode as they are (if possible) 
     * 
     *  @param original a Set of ints representing the original alphabet in codepoints
     *  @param encoding a Set of ints representing the alphabet to be used for encoding, in codepoints
     *  @param doNotEncode a Set of ints representing the chars to be encoded using the original alphabet - every char here must appear in both the previous params
     *  @return the AlphabetConverter
     *  @throws IllegalArgumentException if an AlphabetConverter cannot be constructed
     */  
    public static AlphabetConverter createConverter (Set<Integer> original, Set<Integer> encoding, Set<Integer> doNotEncode) {
        
        final Map<Integer, String> originalToEncoded = new LinkedHashMap<Integer, String>();
        final Map<String, String> encodedToOriginal = new LinkedHashMap<String, String>();
        final Map<Integer, String> doNotEncodeMap = new HashMap<Integer, String>();
        
        int encodedLetterLength;
        
        for (int i : doNotEncode) {
            if (! original.contains(i)) {
                throw new IllegalArgumentException("Can not use 'do not encode' list because original alphabet does not contain '" + codePointToString(i) + "'");
            }
            
            if (! encoding.contains(i)) {
                throw new IllegalArgumentException("Can not use 'do not encode' list because encoding alphabet does not contain '" + codePointToString(i) + "'");
            }

            doNotEncodeMap.put(i, codePointToString(i));
        }
        
        if (encoding.size() >= original.size()) {
            encodedLetterLength = 1;
            
            Iterator<Integer> it = encoding.iterator();
            
            for (int originalLetter : original) {
                String originalLetterAsString = codePointToString(originalLetter);
                
                if (doNotEncodeMap.containsKey(originalLetter)) {
                    originalToEncoded.put(originalLetter, originalLetterAsString);
                    encodedToOriginal.put(originalLetterAsString, originalLetterAsString);
                } else {
                    Integer next = it.next();
                    
                    while (doNotEncode.contains(next)) {
                        next = it.next();
                    }
                    
                    String encodedLetter = codePointToString(next);
                    
                    originalToEncoded.put(originalLetter, encodedLetter);
                    encodedToOriginal.put(encodedLetter, originalLetterAsString);
                }
            }
            
            return new AlphabetConverter(originalToEncoded, encodedToOriginal, doNotEncodeMap, encodedLetterLength);
        
        } else if (encoding.size() - doNotEncode.size() < 2) {
            throw new IllegalArgumentException("Must have at least two encoding characters (not counting those in the 'do not encode' list), but has  " + (encoding.size() - doNotEncode.size()));
        } else {
            // we start with one which is our minimum, and because we do the first division outside the loop
            int lettersSoFar = 1;
            
            // the first division takes into account that the doNotEncode letters can't be in the leftmost place
            int lettersLeft = ( original.size() - doNotEncode.size() ) / (encoding.size() - doNotEncode.size());

            while (lettersLeft / encoding.size() >= 1) {
                lettersLeft = lettersLeft / encoding.size();
                lettersSoFar++;
            }

            encodedLetterLength = lettersSoFar + 1;

            AlphabetConverter ac = new AlphabetConverter(originalToEncoded, encodedToOriginal, doNotEncodeMap, encodedLetterLength);
            
            ac.addSingleEncoding(encodedLetterLength, "", encoding, original.iterator());
            
            return ac;
        }
    }

    /**
     * Decodes a given string
     * 
     * @param encoded a string that has been encoded using this AlphabetConverter
     * @return the decoded string such that AlphabetConverter.encode() will return encoded 
     * @throws UnsupportedEncodingException if unexpected characters that cannot be handled are encountered
     */
    public String decode (String encoded) throws UnsupportedEncodingException {
        int j = 0;
        
        StringBuilder result = new StringBuilder();
        
        while (j < encoded.length()) {
            Integer i = encoded.codePointAt(j);
            
            if (doNotEncodeMap.containsKey(i)) {
                result.append(doNotEncodeMap.get(i));
                j++; // because we do not encode in Unicode extended the length of each encoded char is 1
            } else {
                if (j + encodedLetterLength > encoded.length()) {
                    throw new UnsupportedEncodingException("Unexpected end of string while decoding " + encoded);
                } else {
                    String nextGroup = encoded.substring(j, j + encodedLetterLength);
                    String next = encodedToOriginal.get(nextGroup);
                    
                    if (next == null) {
                        throw new UnsupportedEncodingException("Unexpected string without decoding (" + nextGroup + ") in " + encoded);
                    } else {
                        result.append(next);
                        j += encodedLetterLength;
                    }
                }
            }
        }
        
        return result.toString();
    }

    /**
     * Encodes a given string
     * 
     * @param original the string to be encoded
     * @return the encoded string
     * @throws UnsupportedEncodingException if chars that are not supported by this AlphabetConverter are encountered
     */
    public String encode (String original) throws UnsupportedEncodingException {
        
        if (original == null) {
            return null;
        }
    
        StringBuilder sb = new StringBuilder();

        for (int i=0; i < original.length(); ) {
            int codepoint = original.codePointAt(i);

            String nextLetter = originalToEncoded.get(codepoint);
            
            if (nextLetter == null) {
                throw new UnsupportedEncodingException("Couldn't find encoding for '" + codePointToString(codepoint) + "' in " + original);
            }
            
            sb.append(nextLetter);
            
            i += Character.charCount(codepoint);
        }
        
        return sb.toString();
    }
    
    /**
     * Recursive method used when creating encoder/decoder
     */
    private void addSingleEncoding(int level, String currentEncoding, Collection<Integer> encoding, Iterator<Integer> originals) {
        
        if (level > 0) {
            for (int encodingLetter : encoding) {
                if (originals.hasNext()) {
                    
                    // this skips the doNotEncode chars if they are in the leftmost place
                    if (level != encodedLetterLength || ! doNotEncodeMap.containsKey(encodingLetter)) {
                        addSingleEncoding(level - 1, currentEncoding + codePointToString(encodingLetter), encoding, originals);
                    }
                } else {
                    return; // done encoding all the original alphabet
                }
            }
        } else {
            Integer next = originals.next();
            
            while (doNotEncodeMap.containsKey(next)) {
                String originalLetterAsString = codePointToString(next);

                originalToEncoded.put(next, originalLetterAsString);
                encodedToOriginal.put(originalLetterAsString, originalLetterAsString);
                
                if (! originals.hasNext()) {
                    return;
                }
                
                next = originals.next();
            }

            String originalLetterAsString = codePointToString(next);

            originalToEncoded.put(next, currentEncoding);
            encodedToOriginal.put(currentEncoding, originalLetterAsString);
        }
    }

    /**
     * from http://www.oracle.com/us/technologies/java/supplementary-142654.html
     */
    private static String codePointToString(int i) {
        if (Character.charCount(i) == 1) {
            return String.valueOf((char) i);
        } else {
            return new String(Character.toChars(i));
        }
    }
    
    /**
     * How many characters in the encoded alphabet are necessary for each character in the original alphabet
     */
    public int getEncodedCharLength() {
        return encodedLetterLength;
    }
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        
        for (Entry<Integer, String> i : originalToEncoded.entrySet()) {
            sb.append(codePointToString(i.getKey())).append(" -> ").append(i.getValue()).append("\n");
        }
        
        return sb.toString();
    }

    /**
     * Get the mapping from integer code point of source language to encoded string. Use to reconstruct AlphabetConverter from serialized map
     */
    public Map<Integer, String> getOriginalToEncoded() {
        return originalToEncoded;
    }

    /**
     * Get the 'do not encode' chars
     */
    public Collection<String> getDoNotEncode() {
        return doNotEncodeMap.values();
    }
    
    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        if (obj instanceof AlphabetConverter == false) {
            return false;
        }
        final AlphabetConverter other = (AlphabetConverter) obj;
        return  doNotEncodeMap.equals(other.doNotEncodeMap) &&
                originalToEncoded.equals(other.originalToEncoded) &&
                encodedToOriginal.equals(other.encodedToOriginal) &&
                encodedLetterLength == other.encodedLetterLength;
    }
}

/*
 * Copyright 2002-2004 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
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

import java.util.Map;
import java.util.ArrayList;

/**
 * Substitutes values from one or more maps into designated places within a string.
 * <p>
 * For example:
 * </p>
 * <pre>
 *   "User ${id} has uid ${uid} and path of env{path}"
 * </pre>
 * can be expanded using values from a map associated with "$" and a different
 * map associated with "env".
 * <p> 
 * This class is similar to the {@link java.text.MessageFormat} class, but allows
 * data to be retrieved from one or maps instead of from a single list.
 * Unlike the MessageFormat class, however, it simply uses the result of 
 * applying toString() to the referenced values in the map(s) and does not 
 * support any "format" operations applied to the objects before substitution 
 * into the target string.
 * </p>
 * <p>
 * Originally org.apache.commons.digester.substitution.MultiVariableExpander.
 * </p>
 *
 * @since 2.1
 */
public class MappedMessageFormat {

    private int nEntries = 0;
    private ArrayList markers = new ArrayList(2);
    private ArrayList sources = new ArrayList(2);
    
    public MappedMessageFormat() {
    }
    
    /**
     * Defines a source of data that can later be substituted into
     * strings passed to the "format" methods.
     */
    public void addSource(String marker, Map source) {
        nEntries++;
        markers.add(marker);
        sources.add(source);
    }

    /**
     * Expands any variable declarations using any of the known
     * variable marker strings.
     * 
     * @throws IllegalArgumentException if the input param references
     * a variable which is not known to the specified source.
     */
    public String format(String param) {
        for(int i=0; i<nEntries; i++) {
            param = format( param, (String) markers.get(i), (Map) sources.get(i));
        }
        return param;
    }
    
    /**
     * Replace any occurrences within the string of the form
     * "marker{key}" with the value from source[key].
     * <p>
     * Commonly, the variable marker is "$", in which case variables
     * are indicated by ${key} in the string.
     * <p>
     * Returns the string after performing all substitutions.
     * <p>
     * If no substitutions were made, the input string object is
     * returned (not a copy).
     *
     * @throws IllegalArgumentException if the input param references
     * a variable which is not known to the specified source.
     */
    public static String format(String str, String marker, Map source) {
        String startMark = marker + "{";
        int markLen = startMark.length();
        
        int index = 0;
        while(true) {
            index = str.indexOf(startMark, index);
            if (index == -1) {
                return str;
            }
            
            int startIndex = index + markLen;
            if (startIndex > str.length()) {
                throw new IllegalArgumentException("var expression starts at end of string");
            }
            
            int endIndex = str.indexOf("}", index + markLen);
            if (endIndex == -1) {
                throw new IllegalArgumentException("var expression starts but does not end");
            }
            
            String key = str.substring(index+markLen, endIndex);
            Object value =  source.get(key);
            if (value == null) {
                throw new IllegalArgumentException("parameter [" + key + "] is not defined.");
            }
            String varValue = value.toString();
            
            str = str.substring(0, index) + varValue + str.substring(endIndex+1);
            index += varValue.length();
        }
    }
        
}


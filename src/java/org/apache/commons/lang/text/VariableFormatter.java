/*
 * Copyright 2005 The Apache Software Foundation.
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

import java.text.FieldPosition;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * <p>
 * Replaces variables in text with other text.
 * </p>
 * <p>
 * This class can be given a text which can contain an arbitrary number of variables. The default notation for a
 * variable in text is <code>${variableName}</code>. However by calling the <code>setVariablePrefix()</code> and
 * <code>setVariableSuffix()</code> methods it is possible to use a different prefix or suffix. Variable values are
 * resolved from a map.
 * </p>
 * <p>
 * The simplest example is to use this class to replace Java System properties. For example:
 * 
 * <pre>
 * VariableFormatter.replaceSystemProperties(
 *      "You are running with java.version = ${java.version} and os.name = ${os.name}.");
 * </pre>
 * 
 * </p>
 * <p>
 * Typical usage of this class follows the following pattern: First an instance is created and initialized with the map
 * that contains the values for the available variables. If a prefix and/or suffix for variables should be used other
 * than the default ones, the appropriate settings can be performed. After that the <code>replace()</code> method can
 * be called passing in the source text for interpolation. In the returned text all variable references (as long as
 * their values are known) will be resolved. The following example demonstrates this:
 * </p>
 * <p>
 * 
 * <pre>
 * Map valuesMap = HashMap();
 * valuesMap.put(&quot;animal&quot;, &quot;quick brown fox&quot;);
 * valuesMap.put(&quot;target&quot;, &quot;lazy dog&quot;);
 * String templateString = &quot;The ${animal} jumped over the ${target}.&quot;;
 * VariableFormatter vf = new VariableFormatter(valuesMap);
 * String resolvedString = cf.replace(templateString);
 * </pre>
 * 
 * yielding:
 * 
 * <pre>
 *      The quick brown fox jumped over the lazy dog.
 * </pre>
 * 
 * </p>
 * <p>
 * In addition to this usage pattern there are some static convenience methods that cover the most common use cases.
 * These methods can be used without the need of creating an instance. However if multiple replace operations are to be
 * performed, creating and reusing an instance of this class will be more efficient.
 * </p>
 * <p>
 * Variable replacement works in a recursive way, i.e. it is possible that a variable's value is a text which again
 * contains variable references. These new variables will be replaced, too. Cyclic replacements are detected and will
 * cause an exception to be thrown.
 * </p>
 * <p>
 * Sometimes the interpolation's result must contain a variable prefix. As an example take the following source text:
 * </p>
 * <p>
 * <code>The variable ${${name}} must be used.</code>
 * </p>
 * <p>
 * Here only the variable's name refered to in the text should be replaced resulting in the text (assuming that the
 * value of the <code>name</code> variable is <code>x</code>:
 * </p>
 * <p>
 * <code>The variable ${x} must be used.</code>
 * </p>
 * <p>
 * To achieve this effect there are two possibilities: Either set a different prefix and suffix for variables which do
 * not conflict with the result text you want to produce. The other possibility is to use the escape character that can
 * be set through the <code>setEscapeCharacter()</code> method. If this character is placed before a variable
 * reference, this reference is ignored and won't be replaced. Per default the escape character is set to the
 * <code>$</code> character, so that in the example above the text could have run:
 * </p>
 * <p>
 * <code>The variable $${${name}} must be used.</code>
 * </p>
 * 
 * 
 * @author Oliver Heger
 * @version $Id$
 * @since 2.2
 */
public class VariableFormatter {

    /**
     * A VariableResolver backed by a {@link Map}.
     * 
     * @author <a href="mailto:ggregory@seagullsw.com">Gary Gregory</a>
     * @version $Id$
     */
    public static class MapVariableResolver implements VariableResolver {
        /**
         * Map keys are variable names and value
         */
        private Map map;

        /**
         * Creates a new VariableResolver backed by a Map.
         * 
         * @param map
         *            The variable names and values.
         */
        public MapVariableResolver(Map map) {
            this.setMap(map);
        }

        /**
         * Gets the variable names and values.
         * 
         * @return the variable names and values.
         */
        public Map getMap() {
            return this.map;
        }

        /**
         * Resolves the given variable name with the backing Map.
         * 
         * @param varName
         *            a variable name
         * @return a value or <code>null</code> if the variable name is not in Map
         */
        public Object resolveVariable(String varName) {
            if (this.getMap() == null) {
                return null;
            }
            return this.getMap().get(varName);
        }

        /**
         * Gets the variable names and values.
         * 
         * @param map
         *            the variable names and values.
         */
        public void setMap(Map map) {
            this.map = map;
        }
    }

    /**
     * A helper class for detecting variables in the source text.
     * This class provides simple tokenizer functionality. It splits input
     * text into tokens for text, variables, and escaped variable start tokens.
     */
    protected static class VariableParser {
        /** Constant for the token type ESCAPED_VAR. */
        static final short ESCAPED_VAR_TOKEN = 3;

        /** Constant for the token type TEXT. */
        static final short TEXT_TOKEN = 1;

        /** Constant for the token type VARIABLE. */
        static final short VARIABLE_TOKEN = 2;

        /**
         * Creates a new variable token.
         * 
         * @param aStartIndex
         *            The token starting index
         * @param aLength
         *            The token length
         * @return a new token
         */
        protected static FieldPosition newEscapedVariableToken(int aStartIndex, int aLength) {
            return newToken(VariableParser.ESCAPED_VAR_TOKEN, aStartIndex, aLength);
        }

        /**
         * Creates a new variable token.
         * 
         * @param aStartIndex
         *            The token starting index
         * @param aLength
         *            The token length
         * @return a new token
         */
        protected static FieldPosition newTextToken(int aStartIndex, int aLength) {
            return newToken(VariableParser.TEXT_TOKEN, aStartIndex, aLength);
        }

        /**
         * Creates a new token of the specified type.
         * 
         * @param type
         *            The token type
         * @param beginIndex
         *            The token starting index
         * @param length
         *            The token length
         * @return a new token
         */
        private static FieldPosition newToken(int type, int beginIndex, int length) {
            FieldPosition fp = new FieldPosition(type);
            fp.setBeginIndex(beginIndex);
            fp.setEndIndex(beginIndex + length);
            return fp;
        }

        /**
         * Creates a new variable token.
         * 
         * @param aStartIndex
         *            The token starting index
         * @param aLength
         *            The token length
         * @return a new token
         */
        protected static FieldPosition newVariableToken(int aStartIndex, int aLength) {
            return newToken(VariableParser.VARIABLE_TOKEN, aStartIndex, aLength);
        }

        /** Stores the end index. */
        private int endIndex;

        /** Stores the matcher for escaped variable start tokens. */
        private StrMatcher escVarMatcher;

        /** Stores the length of the data. */
        private int length;

        /** Stores the current position. */
        private int pos;

        /** Stores a list with the pending tokens. */
        private LinkedList tokenList;

        /** Stores the matcher for variable end tokens. */
        private StrMatcher varEndMatcher;

        /** Stores the matcher for variable start tokens. */
        private StrMatcher varStartMatcher;

        /**
         * Creates a new instance of <code>VariableParser</code> and initializes it.
         * 
         * @param startMatcher
         *            the variable start matcher
         * @param endMatcher
         *            the variable end matcher
         * @param escMatcher
         *            the escaped variable start matcher
         * @param startPos
         *            the start index in the source data
         * @param length
         *            the length of the source data
         */
        protected VariableParser(StrMatcher startMatcher, StrMatcher endMatcher,
                StrMatcher escMatcher, int startPos, int length) {
            this.setVarStartMatcher(startMatcher);
            this.setVarEndMatcher(endMatcher);
            this.setEscVarMatcher(escMatcher);
            this.setPos(startPos);
            this.setLength(length);
            this.setEndIndex(startPos + length);
            this.setTokenList(new LinkedList());
        }

        /**
         * Checks if there is a text token before the current position.
         * 
         * @param startPos
         *            the start pos for the current <code>nextToken()</code> invocation
         */
        private void checkTextToken(int startPos) {
            if (startPos < getPos()) {
                getTokenList().addLast(VariableParser.newTextToken(startPos, getPos() - startPos));
            }
        }

        /**
         * @return Returns the endIndex.
         */
        private int getEndIndex() {
            return this.endIndex;
        }

        /**
         * @return Returns the escVarMatcher.
         */
        private StrMatcher getEscVarMatcher() {
            return this.escVarMatcher;
        }

        /**
         * @return Returns the length.
         */
        private int getLength() {
            return this.length;
        }

        /**
         * @return Returns the pos.
         */
        private int getPos() {
            return this.pos;
        }

        /**
         * @return Returns the tokenList.
         */
        private LinkedList getTokenList() {
            return this.tokenList;
        }

        /**
         * @return Returns the varEndMatcher.
         */
        private StrMatcher getVarEndMatcher() {
            return this.varEndMatcher;
        }

        /**
         * @return Returns the varStartMatcher.
         */
        private StrMatcher getVarStartMatcher() {
            return this.varStartMatcher;
        }

        /**
         * Returns whether there is more to parse.
         * 
         * @return a flag whether there is more to parse.
         */
        // Named method like java.util.Iterator#hasNext()
        private boolean hasNext() {
            return getPos() < getEndIndex();
        }

        /**
         * Returns the next token in the given data.
         * 
         * @param data
         *            the array with the source data
         * @return the next token or <b>null</b> if the end is reached
         */
        protected FieldPosition nextToken(char[] data) {
            if (getTokenList().isEmpty()) {
                if (!hasNext()) {
                    // end of data is reached
                    return null;
                }
                int startPos = getPos();
                int tokenLen;
                while (hasNext() && getTokenList().isEmpty()) {
                    if ((tokenLen = getEscVarMatcher().isMatch(data, getPos(), 0, getLength())) > 0) {
                        checkTextToken(startPos);
                        getTokenList().addLast(VariableParser.newEscapedVariableToken(getPos(), tokenLen));
                        setPos(getPos() + tokenLen);
                    } else if ((tokenLen = getVarStartMatcher().isMatch(data, getPos(), 0, getLength())) > 0) {
                        checkTextToken(startPos);
                        setPos(getPos() + tokenLen);
                        int varStart = getPos(), endLen = 0;
                        while ( hasNext() && 
                                (endLen = getVarEndMatcher().isMatch(data, getPos(), 0, getLength())) <= 0
                              )
                        {
                            setPos(getPos() + 1);
                        }
                        if (endLen <= 0) {
                            checkTextToken(varStart - tokenLen);
                        } else {
                            getTokenList().addLast(VariableParser.newVariableToken(varStart, getPos() - varStart));
                            setPos(getPos() + endLen);
                        }
                    } else {
                        setPos(getPos() + 1);
                    }
                }
                if (getTokenList().isEmpty()) {
                    checkTextToken(startPos);
                }
            }
            return (FieldPosition) getTokenList().removeFirst();
        }

        /**
         * @param endIndex
         *            The endIndex to set.
         */
        private void setEndIndex(int endIndex) {
            this.endIndex = endIndex;
        }

        /**
         * @param escVarMatcher
         *            The escVarMatcher to set.
         */
        private void setEscVarMatcher(StrMatcher escVarMatcher) {
            this.escVarMatcher = escVarMatcher;
        }

        /**
         * @param length
         *            The length to set.
         */
        private void setLength(int length) {
            this.length = length;
        }

        /**
         * @param pos
         *            The pos to set.
         */
        private void setPos(int pos) {
            this.pos = pos;
        }

        /**
         * @param tokenList
         *            The tokenList to set.
         */
        private void setTokenList(LinkedList tokenList) {
            this.tokenList = tokenList;
        }

        /**
         * @param varEndMatcher
         *            The varEndMatcher to set.
         */
        private void setVarEndMatcher(StrMatcher varEndMatcher) {
            this.varEndMatcher = varEndMatcher;
        }

        /**
         * @param varStartMatcher
         *            The varStartMatcher to set.
         */
        private void setVarStartMatcher(StrMatcher varStartMatcher) {
            this.varStartMatcher = varStartMatcher;
        }
    }

    /**
     * <p>
     * Definition of an interface for obtaining values for variables.
     * </p>
     * <p>
     * Objects implementing this interface can be passed to <code>VariableFormatter</code>
     * as source for the values of the variables. The interface is quite simple and defines
     * only a single method for retrieving the value of a specified value.
     * </p>
     */
    public static interface VariableResolver {
        /**
         * Returns the value of the specified variable. The variable's value
         * can be an arbitrary object. If no variable with the given name is known,
         * an implementation should return <code>null</code>.
         * 
         * @param varName
         *            the name of the searched variable
         * @return the variable's value
         */
        Object resolveVariable(String varName);
    }

    /** Constant for the default escape character. */
    public static final char DEFAULT_ESCAPE = '$';

    /** Constant for the default variable prefix. */
    public static final String DEFAULT_PREFIX = "${";

    /** Constant for the default variable suffix. */
    public static final String DEFAULT_SUFFIX = "}";

    /**
     * Replaces the occurrences of all variables in the given source data by
     * their current values obtained from the passed in map.
     * 
     * @param valueMap
     *            the map with the values
     * @param source
     *            the source text
     * @return the result of the replace operation
     */
    public static String replace(Map valueMap, Object source) {
        return new VariableFormatter(valueMap).replace(source);
    }

    /**
     * Replaces the occurrences of all variables in the given source data by
     * their current values obtained from the passed in map. This method allows
     * to specifiy a custom variable prefix and suffix
     * 
     * @param valueMap
     *            the map with the values
     * @param prefix
     *            the prefix of variables
     * @param suffix
     *            the suffix of variables
     * @param source
     *            the source text
     * @return the result of the replace operation
     */
    public static String replace(Map valueMap, String prefix, String suffix, Object source) {
        return new VariableFormatter(valueMap, prefix, suffix).replace(source);
    }

    /**
     * Replaces all variables in the given source data with values obtained
     * from system properties.
     * 
     * @param source
     *            the source text
     * @return the result of the replace operation
     */
    public static String replaceSystemProperties(Object source) {
        return new VariableFormatter(System.getProperties()).replace(source);
    }

    /** Stores the escape character. */
    private char escapeCharacter;

    /** Stores the variable prefix. */
    private String variablePrefix;

    /**
     * Variable resolution is delegated to an implementor of VariableResolver.
     */
    private VariableResolver variableResolver;

    /** Stores the variable suffix. */
    private String variableSuffix;

    /**
     * Creates a new instance with defaults for variable prefix and suffix
     * and the escaping character.
     */
    public VariableFormatter() {
        this((VariableResolver) null, DEFAULT_PREFIX, DEFAULT_SUFFIX, DEFAULT_ESCAPE);
    }

    /**
     * Creates a new instance and initializes it. Uses defaults for variable
     * prefix and suffix and the escaping character.
     * 
     * @param valueMap
     *            the map with the variables' values
     */
    public VariableFormatter(Map valueMap) {
        this(valueMap, DEFAULT_PREFIX, DEFAULT_SUFFIX, DEFAULT_ESCAPE);
    }

    /**
     * Creates a new instance and initializes it. Uses a default escaping character.
     * 
     * @param valueMap
     *            the map with the variables' values
     * @param prefix
     *            the prefix for variables
     * @param suffix
     *            the suffix for variables
     */
    public VariableFormatter(Map valueMap, String prefix, String suffix) {
        this(valueMap, prefix, suffix, DEFAULT_ESCAPE);
    }

    /**
     * Creates a new instance and initializes it.
     * 
     * @param valueMap
     *            the map with the variables' values
     * @param prefix
     *            the prefix for variables
     * @param suffix
     *            the suffix for variables
     * @param escape
     *            the escape character
     */
    public VariableFormatter(Map valueMap, String prefix, String suffix, char escape) {
        this(new MapVariableResolver(valueMap), prefix, suffix, escape);
    }

    /**
     * Creates a new instance and initializes it.
     * 
     * @param variableResolver
     *            the variable resolver
     * @param prefix
     *            the prefix for variables
     * @param suffix
     *            the suffix for variables
     * @param escape
     *            the escape character
     */
    public VariableFormatter(VariableResolver variableResolver, String prefix, String suffix, char escape) {
        this.setVariableResolver(variableResolver);
        this.setVariablePrefix(prefix);
        this.setVariableSuffix(suffix);
        this.setEscapeCharacter(escape);
    }

    //-----------------------------------------------------------------------
    /**
     * Replaces the occurrences of all variables in the given source array by
     * their current values.
     * 
     * @param data
     *            a character array with the source data
     * @return the result of the replace operation
     */
    public String replace(char[] data) {
        return replace(data, 0, data == null ? 0 : data.length);
    }

    /**
     * Replaces the occurrences of all variables in the given source array by their
     * current values. Only the specified portion of the array will be processed.
     * 
     * @param data
     *            a character array with the source data
     * @param offset
     *            the start offset; processing will start at this position
     * @param length
     *            the length of the portion to be processed
     * @return the result of the replace operation
     */
    public String replace(char[] data, int offset, int length) {
        Object result = doReplace(data, offset, length, null, null);
        return result == null ? null : result.toString();
    }

    /**
     * Replaces the occurrences of all variables in the given source data by
     * their current values.
     * 
     * @param source
     *            the text to be interpolated; this can be an arbitrary object
     *            whose <code>toString()</code> method will be called
     * @return the result of the replace operation
     */
    public String replace(Object source) {
        Object result = replaceObject(source);
        return result == null ? null : result.toString();
    }

    /**
     * Replaces the occurrences of all variables in the given source data by
     * their current values. If the source consists only of a single variable
     * reference, this method directly returns the value of this variable
     * (which can be an arbitrary object). If the source contains multiple
     * variable references or static text, the return value will always be a
     * String with the concatenation of all these elements.
     * 
     * @param source
     *            the text to be interpolated; this can be an arbitrary object
     *            whose <code>toString()</code> method will be called
     * @return the result of the replace operation
     */
    public Object replaceObject(Object source) {
        return doReplace(source, null);
    }

    //-----------------------------------------------------------------------
    /**
     * Recursive handler for multiple levels of interpolation. This is the main
     * interpolation method for interpolating objects. It is called for recursively
     * processing the values of resolved variables.
     * 
     * @param obj
     *            the data to be interpolated (as object)
     * @param priorVariables
     *            keeps track of the replaced variables
     * @return the result of the interpolation process
     */
    private Object doReplace(Object obj, List priorVariables) {
        if (obj == null) {
            return null;
        }
        char[] data = obj.toString().toCharArray();
        return doReplace(data, 0, data.length, obj, priorVariables);
    }

    /**
     * Recursive handler for multiple levels of interpolation. This is the main
     * interpolation method, which resolves the values of all variable references
     * contained in the passed in text.
     * 
     * @param data
     *            the text to be interpolated (as character array)
     * @param offset
     *            the start offset in the text array
     * @param length
     *            the length of the data to be processed
     * @param ref
     *            a reference object which will be returned if no interpolation was performed
     * @param priorVariables
     *            keeps track of the replaced variables
     * @return the result of the interpolation process
     */
    private Object doReplace(char[] data, int offset, int length, Object ref, List priorVariables) {
        if (data == null) {
            return null;
        }

        Object resultObj = ref;
        int tokenCnt = 0;
        StrBuilder buf = new StrBuilder(length);

        // on the first call initialize priorVariables
        if (priorVariables == null) {
            priorVariables = new ArrayList();
            priorVariables.add(new String(data, offset, length));
        }

        VariableParser parser = createParser(data, offset, length);
        FieldPosition tok;
        while ((tok = parser.nextToken(data)) != null) {
            switch (tok.getField()) {
                case VariableParser.TEXT_TOKEN :
                    buf.append(data, tok.getBeginIndex(), getLength(tok));
                    break;

                case VariableParser.ESCAPED_VAR_TOKEN :
                    buf.append(getVariablePrefix());
                    tokenCnt++;
                    break;

                case VariableParser.VARIABLE_TOKEN :
                    String variable = new String(data, tok.getBeginIndex(), getLength(tok));

                    // if we've got a loop, create a useful exception message and
                    // throw
                    if (priorVariables.contains(variable)) {
                        String initialBase = priorVariables.remove(0).toString();
                        priorVariables.add(variable);
                        StrBuilder priorVariableSb = new StrBuilder();

                        // create a nice trace of interpolated variables like so:
                        // var1->var2->var3
                        for (Iterator it = priorVariables.iterator(); it.hasNext();) {
                            priorVariableSb.append(it.next());
                            if (it.hasNext()) {
                                priorVariableSb.append("->");
                            }
                        }
                        throw new IllegalStateException("Infinite loop in property interpolation of "
                            + initialBase
                            + ": "
                            + priorVariableSb.toString());
                    }
                    // otherwise, add this variable to the interpolation list.
                    priorVariables.add(variable);

                    resultObj = resolveVariable(variable);
                    if (resultObj != null) {
                        resultObj = doReplace(resultObj, priorVariables);
                        buf.append(resultObj);
                    } else {
                        // variable not defined - so put it back in the value
                        buf.append(getVariablePrefix()).append(variable).append(getVariableSuffix());
                    }

                    // pop the interpolated variable off the stack
                    // this maintains priorVariables correctness for
                    // properties with multiple interpolations, e.g.
                    // prop.name=${some.other.prop1}/blahblah/${some.other.prop2}
                    priorVariables.remove(priorVariables.size() - 1);
                    break;
            }
            tokenCnt++;
        }

        if (resultObj != null && tokenCnt == 1) {
            // if there was only one token, return the reference object
            return resultObj;
        }
        return buf.toString();
    }

    /**
     * Gets the length from the parsed token.
     * 
     * @param tok  the token
     * @return the length
     */
    private int getLength(FieldPosition tok) {
        return tok.getEndIndex() - tok.getBeginIndex();
    }

    /**
     * Creates a parser object for tokenizing the input data.
     * 
     * @param data
     *            the input data
     * @param offset
     *            the offset in the source array
     * @param length
     *            the length of the data to be processed
     * @return the parser
     */
    protected VariableParser createParser(char[] data, int offset, int length) {
        return new VariableParser(
                StrMatcher.stringMatcher(getVariablePrefix()),
                StrMatcher.stringMatcher(getVariableSuffix()),
                StrMatcher.stringMatcher(String.valueOf(getEscapeCharacter()) + getVariablePrefix()), offset, length);
    }

    /**
     * Resolves the specified variable. This method is called whenever a variable
     * reference is detected in the source text. It is passed the variable's name
     * and must return the corresponding value. This implementation accesses the
     * value map using the variable's name as key. Derived classes may override
     * this method to implement a different strategy for resolving variables.
     * 
     * @param name
     *            the name of the variable
     * @return the variable's value or <b>null</b> if the variable is unknown
     */
    protected Object resolveVariable(String name) {
        if (this.getVariableResolver() == null) {
            return null;
        }
        return this.getVariableResolver().resolveVariable(name);
    }

    //-----------------------------------------------------------------------
    /**
     * Returns the escape character.
     * 
     * @return the character used for escaping variable references
     */
    public char getEscapeCharacter() {
        return this.escapeCharacter;
    }

    /**
     * Sets the escape character.
     * If this character is placed before a variable reference in the source
     * text, this variable will be ignored.
     * 
     * @param escapeCharacter
     *            the escape character (0 for disabling escaping)
     */
    public void setEscapeCharacter(char escapeCharacter) {
        this.escapeCharacter = escapeCharacter;
    }

    /**
     * Returns the prefix for variables.
     * 
     * @return the prefix for variables
     */
    public String getVariablePrefix() {
        return this.variablePrefix;
    }

    /**
     * Sets the prefix for variables.
     * 
     * @param variablePrefix
     *            the prefix for variables, not null
     * @throws IllegalArgumentException
     *             if the prefix is <b>null</b>
     */
    public void setVariablePrefix(String variablePrefix) {
        if (variablePrefix == null) {
            throw new IllegalArgumentException("Variable prefix must not be null!");
        }
        this.variablePrefix = variablePrefix;
    }

    /**
     * Returns the suffix for variables.
     * 
     * @return the suffix for variables
     */
    public String getVariableSuffix() {
        return this.variableSuffix;
    }

    /**
     * Sets the suffix for variables
     * 
     * @param variableSuffix
     *            the suffix for variables, not null
     * @throws IllegalArgumentException
     *             if the prefix is <b>null</b>
     */
    public void setVariableSuffix(String variableSuffix) {
        if (variableSuffix == null) {
            throw new IllegalArgumentException("Variable suffix must not be null!");
        }
        this.variableSuffix = variableSuffix;
    }

    /**
     * Gets the VariableResolver
     * 
     * @return the VariableResolver
     */
    public VariableResolver getVariableResolver() {
        return this.variableResolver;
    }

    /**
     * Sets the VariableResolver
     * 
     * @param variableResolver
     *            the VariableResolver
     */
    public void setVariableResolver(VariableResolver variableResolver) {
        this.variableResolver = variableResolver;
    }

}

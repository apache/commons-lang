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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;

/**
 * Candidate class to replace Interpolation and MappedMessageFormat?
 * 
 * <p>
 * A class for variable interpolation (substitution).
 * </p>
 * <p>
 * This class can be given a text which can contain an arbitrary number of variables. It will then try to replace all
 * variables by their current values, which are obtained from a map. A variable per default is specified using the
 * typical notation &quot; <code>${&lt;varname&gt;}</code> &quot;. However by calling the
 * <code>setVariablePrefix()</code> and <code>setVariableSuffix()</code> methods it is possible to use a different
 * prefix or suffix.
 * </p>
 * <p>
 * Typical usage of this class follows the following pattern: First an instance is created and initialized with the map
 * that contains the values for the available variables. If a prefix and/or suffix for variables should be used other
 * than the default ones, the appropriate settings can be performed. After that the <code>replace()</code> method can
 * be called passing in the source text for interpolation. In the returned text all variable references (as long as
 * their values are known) will be resolved. The following example demonstrates this:
 * </p>
 * <p>
 * <code><pre>
 * Map valuesMap = HashMap();
 * valuesMap.put(&quot;animal&quot;, &quot;quick brown fox&quot;);
 * valuesMap.put(&quot;target&quot;, &quot;lazy dog&quot;);
 * String templateString = &quot;The ${animal} jumped over the ${target}.&quot;;
 * VariableFormat vf = new VariableVormat(valuesMap);
 * String resolvedString = cf.replace(templateString);
 * </pre></code> yielding: <code><pre>
 *    The quick brown fox jumped over the lazy dog.
 * </pre></code>
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
 * reference, this reference is ignored and won't be replaced. It can also be placed before a variable suffix, then this
 * suffix will be ignored, too. Per default the escape character is set to the <code>$</code> character, so that in
 * the example above the text could have run:
 * </p>
 * <p>
 * <code>The variable $${${name$}} must be used.</code>
 * </p>
 * 
 * 
 * @author Oliver Heger
 * @version $Id$
 * @since 2.2
 */
public class VariableFormatter {
    /** Constant for the default variable prefix. */
    static final String DEFAULT_PREFIX = "${";

    /** Constant for the default variable suffix. */
    static final String DEFAULT_SUFFIX = "}";

    /** Constant for the default escape character. */
    static final char DEFAULT_ESCAPE = '$';

    /** Stores the map with the variables' values. */
    private Map valueMap;

    /** Stores the variable prefix. */
    private String variablePrefix;

    /** Stores the variable suffix. */
    private String variableSuffix;

    /** Stores the escape character. */
    private char escapeCharacter;

    /**
     * Creates a new instance of <code>VariableFormat</code> and initializes it.
     * 
     * @param valueMap
     *            the map with the variables' values
     * @param prefix
     *            the prefix for variables
     * @param suffix
     *            the suffix for variables
     * @param escape
     *            the escape character
     * @throws IllegalArgumentException
     *             if the map is undefined
     */
    public VariableFormatter(Map valueMap, String prefix, String suffix, char escape) {
        setValueMap(valueMap);
        setVariablePrefix(prefix);
        setVariableSuffix(suffix);
        setEscapeCharacter(escape);
    }

    /**
     * Creates a new instance of <code>VariableFormat</code> and initializes it.
     * Uses a default escaping character.
     * 
     * @param valueMap
     *            the map with the variables' values
     * @param prefix
     *            the prefix for variables
     * @param suffix
     *            the suffix for variables
     * @throws IllegalArgumentException
     *             if the map is undefined
     */
    public VariableFormatter(Map valueMap, String prefix, String suffix) {
        this(valueMap, prefix, suffix, DEFAULT_ESCAPE);
    }

    /**
     * Creates a new instance of <code>VariableFormat</code> and initializes it.
     * Uses defaults for variable prefix and suffix and the escaping character.
     * 
     * @param valueMap
     *            the map with the variables' values
     * @throws IllegalArgumentException
     *             if the map is undefined
     */
    public VariableFormatter(Map valueMap) {
        this(valueMap, DEFAULT_PREFIX, DEFAULT_SUFFIX, DEFAULT_ESCAPE);
    }

    /**
     * Returns the escape character.
     * 
     * @return the character used for escaping variable references
     */
    public char getEscapeCharacter() {
        return this.escapeCharacter;
    }

    /**
     * Sets the escape character. If this character is placed before a
     * variable reference in the source text, this variable will be ignored.
     * 
     * @param escapeCharacter
     *            the escape character (0 for disabling escaping)
     */
    public void setEscapeCharacter(char escapeCharacter) {
        this.escapeCharacter = escapeCharacter;
    }

    /**
     * Returns the map with the variables' values.
     * 
     * @return the values of the variables
     */
    public Map getValueMap() {
        return this.valueMap;
    }

    /**
     * Sets the map with the variables' values.
     * 
     * @param valueMap
     *            the values of the variables
     * @throws IllegalArgumentException
     *             if <code>valueMap</code> is <b>null</b>
     */
    public void setValueMap(Map valueMap) throws IllegalArgumentException {
        if (valueMap == null) {
            throw new IllegalArgumentException("Value map must not be null");
        }
        this.valueMap = valueMap;
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
     *            the prefix for variables
     * @throws IllegalArgumentException
     *             if the prefix is <b>null</b>
     */
    public void setVariablePrefix(String variablePrefix) throws IllegalArgumentException {
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
     *            the suffix for variables
     * @throws IllegalArgumentException
     *             if the prefix is <b>null</b>
     */
    public void setVariableSuffix(String variableSuffix) throws IllegalArgumentException {
        if (variableSuffix == null) {
            throw new IllegalArgumentException("Variable suffix must not be null!");
        }
        this.variableSuffix = variableSuffix;
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
     *            the text to be interpolated; this can be an arbitrary object whose <code>toString()</code> method
     *            will be called
     * @return the result of the replace operation
     */
    public Object replaceObject(Object source) {
        return doReplace(source, null);
    }

    /**
     * Replaces the occurrences of all variables in the given source data by
     * their current values.
     * 
     * @param source
     *            the text to be interpolated; this can be an arbitrary object whose <code>toString()</code> method
     *            will be called
     * @return the result of the replace operation
     */
    public String replace(Object source) {
        Object result = replaceObject(source);
        return (result == null) ? null : result.toString();
    }

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
     * their current values obtained from the passed in map. This method
     * allows to specifiy a custom variable prefix and suffix
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

    /**
     * Checks if the variable reference found at the specified position is
     * escaped and if this is the case, where the escaped text starts.
     * 
     * @param text
     *            the text to be processed
     * @param beginIndex
     *            the start index of the variable reference to check
     * @return the starting index of the escaped text or -1 if this reference is not escaped
     */
    protected int escaped(String text, int beginIndex) {
        if (beginIndex < 1 || text.charAt(beginIndex - 1) != getEscapeCharacter()) {
            return -1;
        }
        int idx = beginIndex - 2;
        while (idx >= 0 && text.charAt(idx) == getEscapeCharacter()) {
            idx--;
        }
        return idx + 1;
    }

    /**
     * Unescapes an escaped variable reference. This method is called if
     * <code>escaped()</code> has determined an escaped variable reference.
     * Its purpose is to remove any escaping characters and to add the
     * resulting text into the target buffer. This implementation will remove
     * the first escape character. So if the default values are used,
     * a text portion of <code>$${myvar}</code> will become <code>${myvar}</code>,
     * <code>$$$${var with dollars}</code> will result in <code>$$${var with
     * dollars}</code>. Text between the first variable start token and the last
     * unescaped variable end token can contain variable references and will be
     * recursively replaced. So constructs of the following form can be built:
     * <code>Variable $${${varName$}} is incorrect!</code> (note how the first
     * &quot;}&quot; character is escaped, so that the second &quot;}&quot;
     * marks the end of this construct.
     * 
     * @param buf
     *            the target buffer
     * @param text
     *            the text to be processed
     * @param beginIndex
     *            the begin index of the escaped variable reference
     * @param endIndex
     *            the end index of the escaped variable reference
     * @param priorVariables
     *            keeps track of the replaced variables
     */
    protected void unescape(StringBuffer buf, String text, int beginIndex, int endIndex, List priorVariables) {
        int startToken = text.indexOf(getVariablePrefix(), beginIndex);
        buf.append(text.substring(beginIndex + 1, startToken));
        buf.append(getVariablePrefix());
        String escapedContent = text.substring(startToken + getVariablePrefix().length(), endIndex);
        buf.append(doReplace(StringUtils.replace(escapedContent, String.valueOf(getEscapeCharacter())
            + getVariableSuffix(), getVariableSuffix()), priorVariables));
    }

    /**
     * Searches for a variable end token in the given string from the
     * specified start position.
     * 
     * @param text
     *            the text to search
     * @param beginIndex
     *            the start index
     * @return the index of the end token or -1 if none was found
     */
    protected int findEndToken(String text, int beginIndex) {
        int pos = beginIndex - getVariableSuffix().length();

        do {
            pos = text.indexOf(getVariableSuffix(), pos + getVariableSuffix().length());
        } while (pos > 0 && getEscapeCharacter() == text.charAt(pos - 1));

        return pos;
    }

    /**
     * Resolves the specified variable. This method is called whenever a
     * variable reference is detected in the source text. It is passed the
     * variable's name and must return the corresponding value.
     * This implementation accesses the value map using the variable's name
     * as key. Derived classes may override this method to implement a different
     * strategy for resolving variables.
     * 
     * @param name
     *            the name of the variable
     * @return the variable's value or <b>null</b> if the variable is unknown
     */
    protected Object resolveVariable(String name) {
        return getValueMap().get(name);
    }

    /**
     * Recursive handler for multple levels of interpolation. This is the main
     * interpolation method, which resolves the values of all variable
     * references contained in the passed in text.
     * 
     * @param base
     *            string with the ${key} variables
     * @param priorVariables
     *            serves two purposes: to allow checking for loops, and creating a meaningful exception message should a
     *            loop occur. It's 0'th element will be set to the value of base from the first call. All subsequent
     *            interpolated variables are added afterward. When called for the first time, this argument should be
     *            <b>null </b>.
     * @param obj
     *            the text to be interpolated (as object)
     * @param priorVariables
     *            keeps track of the replaced variables
     * @return the result of the interpolation process
     */
    private Object doReplace(Object obj, List priorVariables) {
        if (obj == null) {
            return null;
        }

        String base = obj.toString();
        if (base.indexOf(getVariablePrefix()) < 0) {
            return obj;
        }

        // on the first call initialize priorVariables
        // and add base as the first element
        if (priorVariables == null) {
            priorVariables = new ArrayList();
            priorVariables.add(base);
        }

        int begin = -1;
        int end = -1;
        int prec = 0 - getVariableSuffix().length();
        String variable = null;
        StringBuffer result = new StringBuffer();
        Object objResult = null;
        int objLen = 0;

        while (((begin = base.indexOf(getVariablePrefix(), prec + getVariableSuffix().length())) > -1)
            && ((end = findEndToken(base, begin)) > -1)) {
            int escBegin = escaped(base, begin);
            if (escBegin >= 0) {
                result.append(base.substring(prec + getVariableSuffix().length(), escBegin));
                unescape(result, base, escBegin, end + getVariableSuffix().length(), priorVariables);
            }

            else {
                result.append(base.substring(prec + getVariableSuffix().length(), begin));
                variable = base.substring(begin + getVariablePrefix().length(), end);

                // if we've got a loop, create a useful exception message and
                // throw
                if (priorVariables.contains(variable)) {
                    String initialBase = priorVariables.remove(0).toString();
                    priorVariables.add(variable);
                    StringBuffer priorVariableSb = new StringBuffer();

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

                objResult = resolveVariable(variable);
                if (objResult != null) {
                    objResult = doReplace(objResult, priorVariables);
                    result.append(objResult);
                    objLen = objResult.toString().length();

                    // pop the interpolated variable off the stack
                    // this maintains priorVariables correctness for
                    // properties with multiple interpolations, e.g.
                    // prop.name=${some.other.prop1}/blahblah/${some.other.prop2}
                    priorVariables.remove(priorVariables.size() - 1);
                } else {
                    // variable not defined - so put it back in the value
                    result.append(getVariablePrefix()).append(variable).append(getVariableSuffix());
                }
            }

            prec = end;
        }

        result.append(base.substring(prec + getVariableSuffix().length(), base.length()));
        return (objResult != null && objLen > 0 && objLen == result.length()) ? objResult : result.toString();
    }

}

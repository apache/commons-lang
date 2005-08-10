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

import java.util.HashMap;
import java.util.Map;

import junit.framework.TestCase;

import org.apache.commons.lang.text.VariableFormatter.MapVariableResolver;

/**
 * Test class for VariableFormatter.
 * 
 * @author Oliver Heger
 * @version $Id$
 */
public class VariableFormatterTest extends TestCase {

    private static final String KEY_ANIMAL = "animal";

    private static final String KEY_TARGET = "target";

    static final String REPLACE_TEMPLATE = "The ${animal} jumps over the ${target}.";

    static final String REPLACE_TEMPLATE_EMPTY_KEYS = "The ${} jumps over the ${}.";

    static final String REPLACE_TEMPLATE_NO_ESCAPE = "The {animal} jumps over the {target}.";

    static final String REPLACE_TEMPLATE_NO_MARKERS = "The animal jumps over the target.";

    static final String REPLACE_TEMPLATE_NO_PREFIX = "The $animal} jumps over the $target}.";

    static final String REPLACE_TEMPLATE_NO_SUFFIX = "The ${animal jumps over the ${target.";

    private VariableFormatter format;

    private Map values;

    VariableFormatter getFormat() {
        return this.format;
    }

    MapVariableResolver getMapVariableResolver() {
        return (MapVariableResolver) this.getFormat().getVariableResolver();
    }

    private Map getValueMap() {
        return this.getMapVariableResolver().getMap();
    }

    Map getValues() {
        return this.values;
    }

    void setFormat(VariableFormatter format) {
        this.format = format;
    }

    protected void setUp() throws Exception {
        super.setUp();
        Map map = new HashMap();
        map.put(KEY_ANIMAL, "quick brown fox");
        map.put(KEY_TARGET, "lazy dog");
        setValues(map);
        setFormat(new VariableFormatter(map));
    }

    private void setValueMap(Map valuesMap) {
        this.getMapVariableResolver().setMap(valuesMap);
    }

    void setValues(Map values) {
        this.values = values;
    }

    /**
     * Tests a cyclic replace operation. The cycle should be detected and cause an exception to be thrown.
     */
    public void testCyclicReplacement() {
        Map valuesMap = new HashMap();
        valuesMap.put(KEY_ANIMAL, "${critter}");
        valuesMap.put(KEY_TARGET, "${pet}");
        valuesMap.put("pet", "${petCharacteristic} dog");
        valuesMap.put("petCharacteristic", "lazy");
        valuesMap.put("critter", "${critterSpeed} ${critterColor} ${critterType}");
        valuesMap.put("critterSpeed", "quick");
        valuesMap.put("critterColor", "brown");
        valuesMap.put("critterType", "${animal}");
        this.setValueMap(valuesMap);
        try {
            this.getFormat().replace(REPLACE_TEMPLATE);
            fail("Cyclic replacement was not detected!");
        } catch (IllegalStateException isx) {
            // ok
        }
    }

    /**
     * Tests escaping variable references.
     */
    public void testEscape() {
        assertEquals("${", this.getFormat().replace("$${"));
        assertEquals("${animal}", this.getFormat().replace("$${animal}"));
        this.getValueMap().put("var_name", "x");
        assertEquals("Many $$$$${target} $s", this.getFormat().replace("Many $$$$$${target} $s"));
        assertEquals("Variable ${x} must be used!", this.getFormat().replace("Variable $${${var_name}} must be used!"));
    }

    /**
     * Tests creating new <code>VariableFormat</code> objects.
     */
    public void testInitialize() {
        assertNotNull(this.getFormat().getVariableResolver());
        assertEquals(VariableFormatter.DEFAULT_PREFIX, this.getFormat().getVariablePrefix());
        assertEquals(VariableFormatter.DEFAULT_SUFFIX, this.getFormat().getVariableSuffix());
        assertEquals(VariableFormatter.DEFAULT_ESCAPE, this.getFormat().getEscapeCharacter());

        format = new VariableFormatter(values, "<<", ">>", '\\');
        assertEquals("<<", this.getFormat().getVariablePrefix());
        assertEquals(">>", this.getFormat().getVariableSuffix());
        assertEquals('\\', this.getFormat().getEscapeCharacter());

        // new VariableFormatter(null) should be OK IMO
        // Gary Gregory - July 14 2005
        // try {
        // format = new VariableFormatter(null);
        // fail("Could create format object with null map!");
        // } catch (IllegalArgumentException iex) {
        // // ok
        // }

        try {
            format = new VariableFormatter(values, "${", null);
            fail("Could create format object with undefined suffix!");
        } catch (IllegalArgumentException iex) {
            // ok
        }

        try {
            format = new VariableFormatter(values, null, "]");
            fail("Could create format object with undefined prefix!");
        } catch (IllegalArgumentException iex) {
            // ok
        }
    }

    /**
     * Tests chaning variable prefix and suffix and the escaping character.
     */
    public void testNonDefaultTokens() {
        format = new VariableFormatter(values, "<<", ">>", '\\');
        assertEquals("The quick brown fox jumps over the lazy dog.", format
                .replace("The <<animal>> jumps over the <<target>>."));
        assertEquals("The quick brown fox jumps over the <<target>>.", format
                .replace("The <<animal>> jumps over the \\<<target>>."));
    }

    /**
     * Tests invoking the static convenience methods.
     */
    public void testNonInstanceMethods() {
        assertEquals("The quick brown fox jumps over the lazy dog.", VariableFormatter
                .replace(values, REPLACE_TEMPLATE));
        values.put(KEY_ANIMAL, "cow");
        values.put(KEY_TARGET, "moon");
        assertEquals("The cow jumps over the moon.", VariableFormatter.replace(values, "&", ";",
                "The &animal; jumps over the &target;."));
    }

    public void testNoResolver() throws Exception {
        this.testNoResolver(new VariableFormatter());
        this.testNoResolver(new VariableFormatter(null));
    }

    void testNoResolver(VariableFormatter formatter) throws Exception {
        formatter.setVariableResolver(null);
        this.validateNoReplace(formatter);
    }

    public void testNullMap() throws Exception {
        VariableFormatter formatter = new VariableFormatter(null);
        validateNoReplace(formatter);
    }

    /**
     * Tests recursive replacements.
     */
    public void testRecursiveReplacement() {
        Map valuesMap = new HashMap();
        valuesMap.put(KEY_ANIMAL, "${critter}");
        valuesMap.put(KEY_TARGET, "${pet}");
        valuesMap.put("pet", "${petCharacteristic} dog");
        valuesMap.put("petCharacteristic", "lazy");
        valuesMap.put("critter", "${critterSpeed} ${critterColor} ${critterType}");
        valuesMap.put("critterSpeed", "quick");
        valuesMap.put("critterColor", "brown");
        valuesMap.put("critterType", "fox");
        this.setValueMap(valuesMap);
        assertEquals("The quick brown fox jumps over the lazy dog.", this.getFormat().replace(REPLACE_TEMPLATE));
    }

    /**
     * Tests typical replace operations.
     */
    public void testReplace() {
        assertEquals("The quick brown fox jumps over the lazy dog.", this.getFormat().replaceObject(REPLACE_TEMPLATE));
        Map map = this.getValueMap();
        map.put(KEY_ANIMAL, "cow");
        map.put(KEY_TARGET, "moon");
        assertEquals("The cow jumps over the moon.", this.getFormat().replace(REPLACE_TEMPLATE));

        assertEquals("Variable ${var} is unknown!", this.getFormat().replace("Variable ${var} is unknown!"));
    }

    /**
     * Tests a replace template with missing empty marker strings.
     */
    public void testReplaceEmptyKeys() {
        testReplaceNoElement(REPLACE_TEMPLATE_EMPTY_KEYS);
    }

    void testReplaceNoElement(String badReplaceTemplate) {
        assertEquals(badReplaceTemplate, this.getFormat().replaceObject(badReplaceTemplate));
        Map map = this.getValueMap();
        map.put(KEY_ANIMAL, "cow");
        map.put(KEY_TARGET, "moon");
        assertEquals("The cow jumps over the moon.", this.getFormat().replace(REPLACE_TEMPLATE));
        assertEquals(badReplaceTemplate, this.getFormat().replaceObject(badReplaceTemplate));
    }

    /**
     * Tests a replace template with missing escape strings.
     */
    public void testReplaceNoEscape() {
        testReplaceNoElement(REPLACE_TEMPLATE_NO_ESCAPE);
    }

    /**
     * Tests a replace template with missing marker strings.
     */
    public void testReplaceNoMarkers() {
        testReplaceNoElement(REPLACE_TEMPLATE_NO_MARKERS);
    }

    /**
     * Tests a replace template with missing prefix strings.
     */
    public void testReplaceNoPrefix() {
        testReplaceNoElement(REPLACE_TEMPLATE_NO_PREFIX);
    }

    /**
     * Tests a replace template with missing postfix strings.
     */
    public void testReplaceNoSuffix() {
        testReplaceNoElement(REPLACE_TEMPLATE_NO_SUFFIX);
    }

    /**
     * Tests source texts with nothing to replace.
     */
    public void testReplaceNothing() {
        assertNull(this.getFormat().replace(null));
        assertNull(this.getFormat().replace((Object)null));
        assertEquals("Nothing to replace.", this.getFormat().replace("Nothing to replace."));
        assertEquals("42", this.getFormat().replace(new Integer(42)));
    }

    /**
     * Tests operating on objects.
     */
    public void testReplaceObject() {
        this.getValueMap().put("value", new Integer(42));
        assertEquals(new Integer(42), this.getFormat().replaceObject("${value}"));
        assertEquals("The answer is 42.", this.getFormat().replaceObject("The answer is ${value}."));
    }

    /**
     * Tests interpolation with system properties.
     */
    public void testReplaceSystemProperties() {
        StringBuffer buf = new StringBuffer();
        buf.append("Hi ").append(System.getProperty("user.name"));
        buf.append(", you are working with ");
        buf.append(System.getProperty("os.name"));
        buf.append(", your home directory is ");
        buf.append(System.getProperty("user.home")).append('.');
        assertEquals(buf.toString(), VariableFormatter.replaceSystemProperties("Hi ${user.name}, you are "
            + "working with ${os.name}, your home "
            + "directory is ${user.home}."));
    }

    /**
     * Tests interpolation with weird boundary patterns.
     */
    public void testReplaceWeirdPattens() {
        testReplaceNoElement("");
        testReplaceNoElement("${}");
        testReplaceNoElement("${ }");
        testReplaceNoElement("${\t}");
        testReplaceNoElement("${\n}");
        testReplaceNoElement("${\b}");
        testReplaceNoElement("${");
        testReplaceNoElement("$}");
        testReplaceNoElement("}");
        testReplaceNoElement("${}$");
        testReplaceNoElement("${${");
        testReplaceNoElement("${${}}");
        testReplaceNoElement("${$${}}");
        testReplaceNoElement("${$$${}}");
        testReplaceNoElement("${$$${$}}");
        testReplaceNoElement("${${}}");
        testReplaceNoElement("${${ }}");
    }
    
    /**
     * Tests replace operations on char arrays.
     */
    public void testReplaceCharArray() {
        assertEquals(null, this.getFormat().replace((char[]) null));
        assertEquals("", this.getFormat().replace(new char[0]));
        assertEquals(new String(new char[1]), this.getFormat().replace(new char[1]));
        char[] data = REPLACE_TEMPLATE.toCharArray();
        assertEquals("The quick brown fox jumps over the lazy dog.", this.getFormat().replace(data));
        assertEquals("The quick brown fox", this.getFormat().replace(data, 0, 13));
        assertEquals("", this.getFormat().replace(data, 0, 0));
        char[] empty = new char[0];
        assertEquals("", this.getFormat().replace(empty));
    }

    void validateNoReplace(VariableFormatter formatter) {
        String srcString = "Hello ${user.name}";
        String destString = formatter.replace(srcString);
        assertEquals(srcString, destString);
    }
}

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

/**
 * Test class for VariableResolver.
 * 
 * @author Oliver Heger
 * @version $Id$
 */
public class VariableFormatTest extends TestCase {
    static final String REPLACE_TEMPLATE = "The ${animal} jumps over the ${target}.";

    private VariableFormat format;

    private Map values;

    protected void setUp() throws Exception {
        super.setUp();
        Map map = new HashMap();
        map.put("animal", "quick brown fox");
        map.put("target", "lazy dog");
        setValues(map);
        setFormat(new VariableFormat(map));
    }

    /**
     * Tests creating new <code>VariableFormat</code> objects.
     */
    public void testInitialize() {
        assertNotNull(format.getValueMap());
        assertEquals(VariableFormat.DEFAULT_PREFIX, format.getVariablePrefix());
        assertEquals(VariableFormat.DEFAULT_SUFFIX, format.getVariableSuffix());
        assertEquals(VariableFormat.DEFAULT_ESCAPE, format.getEscapeCharacter());

        format = new VariableFormat(values, "<<", ">>", '\\');
        assertEquals("<<", format.getVariablePrefix());
        assertEquals(">>", format.getVariableSuffix());
        assertEquals('\\', format.getEscapeCharacter());

        try {
            format = new VariableFormat(null);
            fail("Could create format object with null map!");
        } catch (IllegalArgumentException iex) {
            // ok
        }

        try {
            format = new VariableFormat(values, "${", null);
            fail("Could create format object with undefined suffix!");
        } catch (IllegalArgumentException iex) {
            // ok
        }

        try {
            format = new VariableFormat(values, null, "]");
            fail("Could create format object with undefined prefix!");
        } catch (IllegalArgumentException iex) {
            // ok
        }
    }

    /**
     * Tests typical replace operations.
     */
    public void testReplace() {
        assertEquals("The quick brown fox jumps over the lazy dog.", format.replaceObject(REPLACE_TEMPLATE));

        format.getValueMap().put("animal", "cow");
        format.getValueMap().put("target", "moon");
        assertEquals("The cow jumps over the moon.", format.replace(REPLACE_TEMPLATE));

        assertEquals("Variable ${var} is unknown!", format.replace("Variable ${var} is unknown!"));
    }

    /**
     * Tests source texts with nothing to replace.
     */
    public void testReplaceNothing() {
        assertNull(format.replace(null));
        assertEquals("Nothing to replace.", format.replace("Nothing to replace."));
        assertEquals("42", format.replace(new Integer(42)));
    }

    /**
     * Tests escaping variable references.
     */
    public void testEscape() {
        assertEquals("${animal}", format.replace("$${animal}"));
        format.getValueMap().put("var_name", "x");
        assertEquals("Many $$$$${target} $s", format.replace("Many $$$$$${target} $s"));
        assertEquals("Variable ${x} must be used!", format.replace("Variable $${${var_name$}} must be used!"));
    }

    /**
     * Tests recursive replacements.
     */
    public void testRecursiveReplacement() {
        Map valuesMap = new HashMap();
        valuesMap.put("animal", "${critter}");
        valuesMap.put("target", "${pet}");
        valuesMap.put("pet", "${petCharacteristic} dog");
        valuesMap.put("petCharacteristic", "lazy");
        valuesMap.put("critter", "${critterSpeed} ${critterColor} ${critterType}");
        valuesMap.put("critterSpeed", "quick");
        valuesMap.put("critterColor", "brown");
        valuesMap.put("critterType", "fox");
        format.setValueMap(valuesMap);
        assertEquals("The quick brown fox jumps over the lazy dog.", format.replace(REPLACE_TEMPLATE));
    }

    /**
     * Tests a cyclic replace operation. The cycle should be detected and cause an exception to be thrown.
     */
    public void testCyclicReplacement() {
        Map valuesMap = new HashMap();
        valuesMap.put("animal", "${critter}");
        valuesMap.put("target", "${pet}");
        valuesMap.put("pet", "${petCharacteristic} dog");
        valuesMap.put("petCharacteristic", "lazy");
        valuesMap.put("critter", "${critterSpeed} ${critterColor} ${critterType}");
        valuesMap.put("critterSpeed", "quick");
        valuesMap.put("critterColor", "brown");
        valuesMap.put("critterType", "${animal}");
        format.setValueMap(valuesMap);
        try {
            format.replace(REPLACE_TEMPLATE);
            fail("Cyclic replacement was not detected!");
        } catch (IllegalStateException isx) {
            // ok
        }
    }

    /**
     * Tests operating on objects.
     */
    public void testReplaceObject() {
        format.getValueMap().put("value", new Integer(42));
        assertEquals(new Integer(42), format.replaceObject("${value}"));
        assertEquals("The answer is 42.", format.replaceObject("The answer is ${value}."));
    }

    /**
     * Tests chaning variable prefix and suffix and the escaping character.
     */
    public void testNonDefaultTokens() {
        format = new VariableFormat(values, "<<", ">>", '\\');
        assertEquals("The quick brown fox jumps over the lazy dog.", format
                .replace("The <<animal>> jumps over the <<target>>."));
        assertEquals("The quick brown fox jumps over the <<target>>.", format
                .replace("The <<animal>> jumps over the \\<<target>>."));
    }

    /**
     * Tests invoking the static convenience methods.
     */
    public void testNonInstanceMethods() {
        assertEquals("The quick brown fox jumps over the lazy dog.", VariableFormat.replace(values, REPLACE_TEMPLATE));
        values.put("animal", "cow");
        values.put("target", "moon");
        assertEquals("The cow jumps over the moon.", VariableFormat.replace(values, "&", ";",
                "The &animal; jumps over the &target;."));
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
        assertEquals(buf.toString(), VariableFormat.replaceSystemProperties("Hi ${user.name}, you are "
            + "working with ${os.name}, your home "
            + "directory is ${user.home}."));
    }

    Map getValues() {
        return this.values;
    }

    void setValues(Map values) {
        this.values = values;
    }

    VariableFormat getFormat() {
        return this.format;
    }

    void setFormat(VariableFormat format) {
        this.format = format;
    }
}

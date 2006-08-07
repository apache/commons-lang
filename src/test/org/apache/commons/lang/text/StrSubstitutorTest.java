/*
 * Copyright 2005-2006 The Apache Software Foundation.
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

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Test class for StrSubstitutor.
 * 
 * @author Oliver Heger
 * @version $Id: StrSubstitutorTest.java 231316 2005-08-10 20:36:26Z ggregory $
 */
public class StrSubstitutorTest extends TestCase {

    private Map values;

    /**
     * Main method.
     * 
     * @param args  command line arguments, ignored
     */
    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    /**
     * Return a new test suite containing this test case.
     * 
     * @return a new test suite containing this test case
     */
    public static Test suite() {
        TestSuite suite = new TestSuite(StrSubstitutorTest.class);
        suite.setName("StrSubstitutor Tests");
        return suite;
    }

    protected void setUp() throws Exception {
        super.setUp();
        values = new HashMap();
        values.put("animal", "quick brown fox");
        values.put("target", "lazy dog");
    }

    protected void tearDown() throws Exception {
        super.tearDown();
        values = null;
    }

    //-----------------------------------------------------------------------
//    /**
//     * Tests escaping variable references.
//     */
//    public void testEscape() {
//        assertEquals("${", this.getFormat().replace("$${"));
//        assertEquals("${animal}", this.getFormat().replace("$${animal}"));
//        this.getValueMap().put("var_name", "x");
//        assertEquals("Many $$$$${target} $s", this.getFormat().replace("Many $$$$$${target} $s"));
//        assertEquals("Variable ${x} must be used!", this.getFormat().replace("Variable $${${var_name}} must be used!"));
//    }
//
//    /**
//     * Tests creating new <code>VariableFormat</code> objects.
//     */
//    public void testInitialize() {
//        assertNotNull(this.getFormat().getVariableResolver());
//        assertEquals(StrSubstitutor.DEFAULT_PREFIX, this.getFormat().getVariablePrefixMatcher());
//        assertEquals(StrSubstitutor.DEFAULT_SUFFIX, this.getFormat().getVariableSuffixMatcher());
//        assertEquals(StrSubstitutor.DEFAULT_ESCAPE, this.getFormat().getEscapeChar());
//
//        format = new StrSubstitutor(values, "<<", ">>", '\\');
//        assertEquals("<<", this.getFormat().getVariablePrefixMatcher());
//        assertEquals(">>", this.getFormat().getVariableSuffixMatcher());
//        assertEquals('\\', this.getFormat().getEscapeChar());
//
//        // new StrSubstitutor(null) should be OK IMO
//        // Gary Gregory - July 14 2005
//        // try {
//        // format = new StrSubstitutor(null);
//        // fail("Could create format object with null map!");
//        // } catch (IllegalArgumentException iex) {
//        // // ok
//        // }
//
//        try {
//            format = new StrSubstitutor(values, "${", null);
//            fail("Could create format object with undefined suffix!");
//        } catch (IllegalArgumentException iex) {
//            // ok
//        }
//
//        try {
//            format = new StrSubstitutor(values, null, "]");
//            fail("Could create format object with undefined prefix!");
//        } catch (IllegalArgumentException iex) {
//            // ok
//        }
//    }
//
//    /**
//     * Tests chaning variable prefix and suffix and the escaping character.
//     */
//    public void testNonDefaultTokens() {
//        format = new StrSubstitutor(values, "<<", ">>", '\\');
//        assertEquals("The quick brown fox jumps over the lazy dog.", format
//                .replace("The <<animal>> jumps over the <<target>>."));
//        assertEquals("The quick brown fox jumps over the <<target>>.", format
//                .replace("The <<animal>> jumps over the \\<<target>>."));
//    }
//
//    /**
//     * Tests invoking the static convenience methods.
//     */
//    public void testNonInstanceMethods() {
//        assertEquals("The quick brown fox jumps over the lazy dog.",
//                StrSubstitutor.replace(REPLACE_TEMPLATE, values));
//        values.put(KEY_ANIMAL, "cow");
//        values.put(KEY_TARGET, "moon");
//        assertEquals("The cow jumps over the moon.",
//                StrSubstitutor.replace("The &animal; jumps over the &target;.", values, "&", ";"));
//    }
//
//    public void testNoResolver() throws Exception {
//        this.testNoResolver(new StrSubstitutor());
//        this.testNoResolver(new StrSubstitutor(null));
//    }
//
//    void testNoResolver(StrSubstitutor formatter) throws Exception {
//        formatter.setVariableResolver(null);
//        this.validateNoReplace(formatter);
//    }
//
//    public void testNullMap() throws Exception {
//        StrSubstitutor formatter = new StrSubstitutor(null);
//        validateNoReplace(formatter);
//    }
//

    //-----------------------------------------------------------------------
    /**
     * Tests simple key replace.
     */
    public void testReplaceSimple() {
        doTestReplace("The quick brown fox jumps over the lazy dog.", "The ${animal} jumps over the ${target}.", true);
    }

    /**
     * Tests simple key replace.
     */
    public void testReplaceSolo() {
        doTestReplace("quick brown fox", "${animal}", false);
    }

    /**
     * Tests replace with no variables.
     */
    public void testReplaceNoVariables() {
        doTestNoReplace("The balloon arrived.");
    }

    /**
     * Tests replace with null.
     */
    public void testReplaceNull() {
        doTestNoReplace(null);
    }

    /**
     * Tests replace with null.
     */
    public void testReplaceEmpty() {
        doTestNoReplace("");
    }

    /**
     * Tests key replace changing map after initialization (not recommended).
     */
    public void testReplaceChangedMap() {
        StrSubstitutor sub = new StrSubstitutor(values);
        values.put("target", "moon");
        assertEquals("The quick brown fox jumps over the moon.", sub.replace("The ${animal} jumps over the ${target}."));
    }

    /**
     * Tests unknown key replace.
     */
    public void testReplaceUnknownKey() {
        doTestReplace("The ${person} jumps over the lazy dog.", "The ${person} jumps over the ${target}.", true);
    }

    /**
     * Tests adjacent keys.
     */
    public void testReplaceAdjacentAtStart() {
        values.put("code", "GBP");
        values.put("amount", "12.50");
        StrSubstitutor sub = new StrSubstitutor(values);
        assertEquals("GBP12.50 charged", sub.replace("${code}${amount} charged"));
    }

    /**
     * Tests adjacent keys.
     */
    public void testReplaceAdjacentAtEnd() {
        values.put("code", "GBP");
        values.put("amount", "12.50");
        StrSubstitutor sub = new StrSubstitutor(values);
        assertEquals("Amount is GBP12.50", sub.replace("Amount is ${code}${amount}"));
    }

    /**
     * Tests simple recursive replace.
     */
    public void testReplaceRecursive() {
        values.put("animal", "${critter}");
        values.put("target", "${pet}");
        values.put("pet", "${petCharacteristic} dog");
        values.put("petCharacteristic", "lazy");
        values.put("critter", "${critterSpeed} ${critterColor} ${critterType}");
        values.put("critterSpeed", "quick");
        values.put("critterColor", "brown");
        values.put("critterType", "fox");
        doTestReplace("The quick brown fox jumps over the lazy dog.", "The ${animal} jumps over the ${target}.", true);
    }

    /**
     * Tests escaping.
     */
    public void testReplaceEscaping() {
        doTestReplace("The ${animal} jumps over the lazy dog.", "The $${animal} jumps over the ${target}.", true);
    }

    /**
     * Tests escaping.
     */
    public void testReplaceSoloEscaping() {
        doTestReplace("${animal}", "$${animal}", false);
    }

    /**
     * Tests complex escaping.
     */
    public void testReplaceComplexEscaping() {
        doTestReplace("The ${quick brown fox} jumps over the lazy dog.", "The $${${animal}} jumps over the ${target}.", true);
    }

    /**
     * Tests when no prefix or suffix.
     */
    public void testReplaceNoPefixNoSuffix() {
        doTestReplace("The animal jumps over the lazy dog.", "The animal jumps over the ${target}.", true);
    }

    /**
     * Tests when no incomplete prefix.
     */
    public void testReplaceIncompletePefix() {
        doTestReplace("The {animal} jumps over the lazy dog.", "The {animal} jumps over the ${target}.", true);
    }

    /**
     * Tests when prefix but no suffix.
     */
    public void testReplacePrefixNoSuffix() {
        doTestReplace("The ${animal jumps over the ${target} lazy dog.", "The ${animal jumps over the ${target} ${target}.", true);
    }

    /**
     * Tests when suffix but no prefix.
     */
    public void testReplaceNoPrefixSuffix() {
        doTestReplace("The animal} jumps over the lazy dog.", "The animal} jumps over the ${target}.", true);
    }

    /**
     * Tests when no variable name.
     */
    public void testReplaceEmptyKeys() {
        doTestReplace("The ${} jumps over the lazy dog.", "The ${} jumps over the ${target}.", true);
    }

    /**
     * Tests replace creates output same as input.
     */
    public void testReplaceToIdentical() {
        values.put("animal", "$${${thing}}");
        values.put("thing", "animal");
        doTestReplace("The ${animal} jumps.", "The ${animal} jumps.", true);
    }

    /**
     * Tests a cyclic replace operation.
     * The cycle should be detected and cause an exception to be thrown.
     */
    public void testCyclicReplacement() {
        Map map = new HashMap();
        map.put("animal", "${critter}");
        map.put("target", "${pet}");
        map.put("pet", "${petCharacteristic} dog");
        map.put("petCharacteristic", "lazy");
        map.put("critter", "${critterSpeed} ${critterColor} ${critterType}");
        map.put("critterSpeed", "quick");
        map.put("critterColor", "brown");
        map.put("critterType", "${animal}");
        StrSubstitutor sub = new StrSubstitutor(map);
        try {
            sub.replace("The ${animal} jumps over the ${target}.");
            fail("Cyclic replacement was not detected!");
        } catch (IllegalStateException ex) {
            // expected
        }
    }

    /**
     * Tests interpolation with weird boundary patterns.
     */
    public void testReplaceWeirdPattens() {
        doTestNoReplace("");
        doTestNoReplace("${}");
        doTestNoReplace("${ }");
        doTestNoReplace("${\t}");
        doTestNoReplace("${\n}");
        doTestNoReplace("${\b}");
        doTestNoReplace("${");
        doTestNoReplace("$}");
        doTestNoReplace("}");
        doTestNoReplace("${}$");
        doTestNoReplace("${${");
        doTestNoReplace("${${}}");
        doTestNoReplace("${$${}}");
        doTestNoReplace("${$$${}}");
        doTestNoReplace("${$$${$}}");
        doTestNoReplace("${${}}");
        doTestNoReplace("${${ }}");
    }

    //-----------------------------------------------------------------------
    /**
     * Tests protected.
     */
    public void testResolveVariable() {
        final StrBuilder builder = new StrBuilder("Hi ${name}!");
        Map map = new HashMap();
        map.put("name", "commons");
        StrSubstitutor sub = new StrSubstitutor(map) {
            protected String resolveVariable(String variableName, StrBuilder buf, int startPos, int endPos) {
                assertEquals("name", variableName);
                assertSame(builder, buf);
                assertEquals(3, startPos);
                assertEquals(10, endPos);
                return "jakarta";
            }
        };
        sub.replace(builder);
        assertEquals("Hi jakarta!", builder.toString());
    }

    //-----------------------------------------------------------------------
    /**
     * Tests static.
     */
    public void testStaticReplace() {
        Map map = new HashMap();
        map.put("name", "commons");
        assertEquals("Hi commons!", StrSubstitutor.replace("Hi ${name}!", map));
    }

    /**
     * Tests static.
     */
    public void testStaticReplacePrefixSuffix() {
        Map map = new HashMap();
        map.put("name", "commons");
        assertEquals("Hi commons!", StrSubstitutor.replace("Hi <name>!", map, "<", ">"));
    }

    /**
     * Tests interpolation with system properties.
     */
    public void testStaticReplaceSystemProperties() {
        StrBuilder buf = new StrBuilder();
        buf.append("Hi ").append(System.getProperty("user.name"));
        buf.append(", you are working with ");
        buf.append(System.getProperty("os.name"));
        buf.append(", your home directory is ");
        buf.append(System.getProperty("user.home")).append('.');
        assertEquals(buf.toString(), StrSubstitutor.replaceSystemProperties("Hi ${user.name}, you are "
            + "working with ${os.name}, your home "
            + "directory is ${user.home}."));
    }

    //-----------------------------------------------------------------------
    private void doTestReplace(String expectedResult, String replaceTemplate, boolean substring) {
        String expectedShortResult = expectedResult.substring(1, expectedResult.length() - 1);
        
        StrSubstitutor sub = new StrSubstitutor(values);
        assertEquals(expectedResult, sub.replace(replaceTemplate));
        if (substring) {
            assertEquals(expectedShortResult, sub.replace(replaceTemplate, 1, replaceTemplate.length() - 2));
        }
        
        char[] chars = replaceTemplate.toCharArray();
        assertEquals(expectedResult, sub.replace(chars));
        if (substring) {
            assertEquals(expectedShortResult, sub.replace(chars, 1, chars.length - 2));
        }
        
        StringBuffer buf = new StringBuffer(replaceTemplate);
        assertEquals(expectedResult, sub.replace(buf));
        
        StrBuilder bld = new StrBuilder(replaceTemplate);
        assertEquals(true, sub.replace(bld));
        assertEquals(expectedResult, bld.toString());
        
        if (substring) {
            bld = new StrBuilder(replaceTemplate);
            assertEquals(true, sub.replace(bld, 1, bld.length() - 2));
            assertEquals(expectedResult, bld.toString());  // expect full result as remainder is untouched
        }
    }

    private void doTestNoReplace(String replaceTemplate) {
        StrSubstitutor sub = new StrSubstitutor(values);
        assertEquals(replaceTemplate, sub.replace(replaceTemplate));
        
        if (replaceTemplate == null) {
            assertEquals(null, sub.replace((char[]) null));
            assertEquals(null, sub.replace((Object) null));
            assertEquals(false, sub.replace((StrBuilder) null));
        } else {
            StrBuilder bld = new StrBuilder(replaceTemplate);
            assertEquals(false, sub.replace(bld));
            assertEquals(replaceTemplate, bld.toString());
        }
    }

}

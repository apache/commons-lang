/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.commons.lang3;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import junit.framework.TestCase;

/**
 * Unit tests {@link org.apache.commons.lang3.Validate}.
 *
 * @author Apache Software Foundation
 * @author Norm Deane
 * @version $Id$
 */
public class ValidateTest extends TestCase {

    public ValidateTest(String name) {
        super(name);
    }

    //-----------------------------------------------------------------------
    public void testIsTrue1() {
        Validate.isTrue(true);
        try {
            Validate.isTrue(false);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("The validated expression is false", ex.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testIsTrue2() {
        Validate.isTrue(true, "MSG");
        try {
            Validate.isTrue(false, "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testIsTrue3() {
        Validate.isTrue(true, "MSG", new Integer(6));
        try {
            Validate.isTrue(false, "MSG", new Integer(6));
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testIsTrue4() {
        Validate.isTrue(true, "MSG", 7);
        try {
            Validate.isTrue(false, "MSG", 7);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testIsTrue5() {
        Validate.isTrue(true, "MSG", 7.4d);
        try {
            Validate.isTrue(false, "MSG", 7.4d);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    //-----------------------------------------------------------------------
    public void testNotNull1() {
        Validate.notNull(new Object());
        try {
            Validate.notNull(null);
            fail("Expecting NullPointerException");
        } catch (NullPointerException ex) {
            assertEquals("The validated object is null", ex.getMessage());
        }
        
        String str = "Hi";
        String testStr = Validate.notNull(str);
        assertSame(str, testStr);
    }

    //-----------------------------------------------------------------------
    public void testNotNull2() {
        Validate.notNull(new Object(), "MSG");
        try {
            Validate.notNull(null, "MSG");
            fail("Expecting NullPointerException");
        } catch (NullPointerException ex) {
            assertEquals("MSG", ex.getMessage());
        }
        
        String str = "Hi";
        String testStr = Validate.notNull(str, "Message");
        assertSame(str, testStr);
    }

    //-----------------------------------------------------------------------
    //-----------------------------------------------------------------------
    public void testNotEmptyArray1() {
        Validate.notEmpty(new Object[] {null});
        try {
            Validate.notEmpty((Object[]) null);
            fail("Expecting NullPointerException");
        } catch (NullPointerException ex) {
            assertEquals("The validated array is empty", ex.getMessage());
        }
        try {
            Validate.notEmpty(new Object[0]);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("The validated array is empty", ex.getMessage());
        }
        
        String[] array = new String[] {"hi"};
        String[] test = Validate.notEmpty(array);
        assertSame(array, test);
    }

    //-----------------------------------------------------------------------
    public void testNotEmptyArray2() {
        Validate.notEmpty(new Object[] {null}, "MSG");
        try {
            Validate.notEmpty((Object[]) null, "MSG");
            fail("Expecting NullPointerException");
        } catch (NullPointerException ex) {
            assertEquals("MSG", ex.getMessage());
        }
        try {
            Validate.notEmpty(new Object[0], "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
        
        String[] array = new String[] {"hi"};
        String[] test = Validate.notEmpty(array, "Message");
        assertSame(array, test);
    }

    //-----------------------------------------------------------------------
    //-----------------------------------------------------------------------
    public void testNotEmptyCollection1() {
        Collection<Integer> coll = new ArrayList<Integer>();
        try {
            Validate.notEmpty((Collection<?>) null);
            fail("Expecting NullPointerException");
        } catch (NullPointerException ex) {
            assertEquals("The validated collection is empty", ex.getMessage());
        }
        try {
            Validate.notEmpty(coll);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("The validated collection is empty", ex.getMessage());
        }
        coll.add(new Integer(8));
        Validate.notEmpty(coll);
        
        Collection<Integer> test = Validate.notEmpty(coll);
        assertSame(coll, test);
    }

    //-----------------------------------------------------------------------
    public void testNotEmptyCollection2() {
        Collection<Integer> coll = new ArrayList<Integer>();
        try {
            Validate.notEmpty((Collection<?>) null, "MSG");
            fail("Expecting NullPointerException");
        } catch (NullPointerException ex) {
            assertEquals("MSG", ex.getMessage());
        }
        try {
            Validate.notEmpty(coll, "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
        coll.add(new Integer(8));
        Validate.notEmpty(coll, "MSG");
        
        Collection<Integer> test = Validate.notEmpty(coll, "Message");
        assertSame(coll, test);
    }

    //-----------------------------------------------------------------------
    //-----------------------------------------------------------------------
    public void testNotEmptyMap1() {
        Map<String, Integer> map = new HashMap<String, Integer>();
        try {
            Validate.notEmpty((Map<?, ?>) null);
            fail("Expecting NullPointerException");
        } catch (NullPointerException ex) {
            assertEquals("The validated map is empty", ex.getMessage());
        }
        try {
            Validate.notEmpty(map);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("The validated map is empty", ex.getMessage());
        }
        map.put("ll", new Integer(8));
        Validate.notEmpty(map);
        
        Map<String, Integer> test = Validate.notEmpty(map);
        assertSame(map, test);
    }

    //-----------------------------------------------------------------------
    public void testNotEmptyMap2() {
        Map<String, Integer> map = new HashMap<String, Integer>();
        try {
            Validate.notEmpty((Map<?, ?>) null, "MSG");
            fail("Expecting NullPointerException");
        } catch (NullPointerException ex) {
            assertEquals("MSG", ex.getMessage());
        }
        try {
            Validate.notEmpty(map, "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
        map.put("ll", new Integer(8));
        Validate.notEmpty(map, "MSG");
        
        Map<String, Integer> test = Validate.notEmpty(map, "Message");
        assertSame(map, test);
    }

    //-----------------------------------------------------------------------
    //-----------------------------------------------------------------------
    public void testNotEmptyString1() {
        Validate.notEmpty("hjl");
        try {
            Validate.notEmpty((String) null);
            fail("Expecting NullPointerException");
        } catch (NullPointerException ex) {
            assertEquals("The validated character sequence is empty", ex.getMessage());
        }
        try {
            Validate.notEmpty("");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("The validated character sequence is empty", ex.getMessage());
        }
        
        String str = "Hi";
        String testStr = Validate.notEmpty(str);
        assertSame(str, testStr);
    }

    //-----------------------------------------------------------------------
    public void testNotEmptyString2() {
        Validate.notEmpty("a", "MSG");
        try {
            Validate.notEmpty((String) null, "MSG");
            fail("Expecting NullPointerException");
        } catch (NullPointerException ex) {
            assertEquals("MSG", ex.getMessage());
        }
        try {
            Validate.notEmpty("", "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
        
        String str = "Hi";
        String testStr = Validate.notEmpty(str, "Message");
        assertSame(str, testStr);
    }

    //-----------------------------------------------------------------------
    //-----------------------------------------------------------------------
    public void testNotBlankNullStringShouldThrow() {
        //given
        String string = null;

        try {
            //when
            Validate.notBlank(string);
            fail("Expecting NullPointerException");
        } catch (NullPointerException e) {
            //then
            assertEquals("The validated character sequence is blank", e.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testNotBlankMsgNullStringShouldThrow() {
        //given
        String string = null;

        try {
            //when
            Validate.notBlank(string, "Message");
            fail("Expecting NullPointerException");
        } catch (NullPointerException e) {
            //then
            assertEquals("Message", e.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testNotBlankEmptyStringShouldThrow() {
        //given
        String string = "";

        try {
            //when
            Validate.notBlank(string);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            //then
            assertEquals("The validated character sequence is blank", e.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testNotBlankBlankStringWithWhitespacesShouldThrow() {
        //given
        String string = "   ";

        try {
            //when
            Validate.notBlank(string);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            //then
            assertEquals("The validated character sequence is blank", e.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testNotBlankBlankStringWithNewlinesShouldThrow() {
        //given
        String string = " \n \t \r \n ";

        try {
            //when
            Validate.notBlank(string);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            //then
            assertEquals("The validated character sequence is blank", e.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testNotBlankMsgBlankStringShouldThrow() {
        //given
        String string = " \n \t \r \n ";

        try {
            //when
            Validate.notBlank(string, "Message");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            //then
            assertEquals("Message", e.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testNotBlankMsgBlankStringWithWhitespacesShouldThrow() {
        //given
        String string = "   ";

        try {
            //when
            Validate.notBlank(string, "Message");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            //then
            assertEquals("Message", e.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testNotBlankMsgEmptyStringShouldThrow() {
        //given
        String string = "";

        try {
            //when
            Validate.notBlank(string, "Message");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            //then
            assertEquals("Message", e.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    public void testNotBlankNotBlankStringShouldNotThrow() {
        //given
        String string = "abc";

        //when
        Validate.notBlank(string);

        //then should not throw
    }

    //-----------------------------------------------------------------------
    public void testNotBlankNotBlankStringWithWhitespacesShouldNotThrow() {
        //given
        String string = "  abc   ";

        //when
        Validate.notBlank(string);

        //then should not throw
    }

    //-----------------------------------------------------------------------
    public void testNotBlankNotBlankStringWithNewlinesShouldNotThrow() {
        //given
        String string = " \n \t abc \r \n ";

        //when
        Validate.notBlank(string);

        //then should not throw
    }

    //-----------------------------------------------------------------------
    public void testNotBlankMsgNotBlankStringShouldNotThrow() {
        //given
        String string = "abc";

        //when
        Validate.notBlank(string, "Message");

        //then should not throw
    }

    //-----------------------------------------------------------------------
    public void testNotBlankMsgNotBlankStringWithWhitespacesShouldNotThrow() {
        //given
        String string = "  abc   ";

        //when
        Validate.notBlank(string, "Message");

        //then should not throw
    }

    //-----------------------------------------------------------------------
    public void testNotBlankMsgNotBlankStringWithNewlinesShouldNotThrow() {
        //given
        String string = " \n \t abc \r \n ";

        //when
        Validate.notBlank(string, "Message");

        //then should not throw
    }

    //-----------------------------------------------------------------------
    public void testNotBlankReturnValues1() {
        String str = "Hi";
        String test = Validate.notBlank(str);
        assertSame(str, test);
    }

    public void testNotBlankReturnValues2() {
        String str = "Hi";
        String test = Validate.notBlank(str, "Message");
        assertSame(str, test);
    }

    //-----------------------------------------------------------------------
    //-----------------------------------------------------------------------
    public void testNoNullElementsArray1() {
        String[] array = new String[] {"a", "b"};
        Validate.noNullElements(array);
        try {
            Validate.noNullElements((Object[]) null);
            fail("Expecting NullPointerException");
        } catch (NullPointerException ex) {
            assertEquals("The validated object is null", ex.getMessage());
        }
        array[1] = null;
        try {
            Validate.noNullElements(array);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("The validated array contains null element at index: 1", ex.getMessage());
        }
        
        array = new String[] {"a", "b"};
        String[] test = Validate.noNullElements(array);
        assertSame(array, test);
    }

    //-----------------------------------------------------------------------
    public void testNoNullElementsArray2() {
        String[] array = new String[] {"a", "b"};
        Validate.noNullElements(array, "MSG");
        try {
            Validate.noNullElements((Object[]) null, "MSG");
            fail("Expecting NullPointerException");
        } catch (NullPointerException ex) {
            assertEquals("The validated object is null", ex.getMessage());
        }
        array[1] = null;
        try {
            Validate.noNullElements(array, "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
        
        array = new String[] {"a", "b"};
        String[] test = Validate.noNullElements(array, "Message");
        assertSame(array, test);
    }

    //-----------------------------------------------------------------------
    //-----------------------------------------------------------------------
    public void testNoNullElementsCollection1() {
        List<String> coll = new ArrayList<String>();
        coll.add("a");
        coll.add("b");
        Validate.noNullElements(coll);
        try {
            Validate.noNullElements((Collection<?>) null);
            fail("Expecting NullPointerException");
        } catch (NullPointerException ex) {
            assertEquals("The validated object is null", ex.getMessage());
        }
        coll.set(1, null);
        try {
            Validate.noNullElements(coll);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("The validated collection contains null element at index: 1", ex.getMessage());
        }
        
        coll.set(1, "b");
        List<String> test = Validate.noNullElements(coll);
        assertSame(coll, test);
    }

    //-----------------------------------------------------------------------
    public void testNoNullElementsCollection2() {
        List<String> coll = new ArrayList<String>();
        coll.add("a");
        coll.add("b");
        Validate.noNullElements(coll, "MSG");
        try {
            Validate.noNullElements((Collection<?>) null, "MSG");
            fail("Expecting NullPointerException");
        } catch (NullPointerException ex) {
            assertEquals("The validated object is null", ex.getMessage());
        }
        coll.set(1, null);
        try {
            Validate.noNullElements(coll, "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
        
        coll.set(1, "b");
        List<String> test = Validate.noNullElements(coll, "Message");
        assertSame(coll, test);
    }

    //-----------------------------------------------------------------------
    //-----------------------------------------------------------------------
    public void testConstructor() {
        assertNotNull(new Validate());
        Constructor<?>[] cons = Validate.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertEquals(true, Modifier.isPublic(cons[0].getModifiers()));
        assertEquals(true, Modifier.isPublic(Validate.class.getModifiers()));
        assertEquals(false, Modifier.isFinal(Validate.class.getModifiers()));
    }

    //-----------------------------------------------------------------------
    //-----------------------------------------------------------------------
    public void testValidIndex_withMessage_array() {
        Object[] array = new Object[2];
        Validate.validIndex(array, 0, "Broken: ");
        Validate.validIndex(array, 1, "Broken: ");
        try {
            Validate.validIndex(array, -1, "Broken: ");
            fail("Expecting IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException ex) {
            assertEquals("Broken: ", ex.getMessage());
        }
        try {
            Validate.validIndex(array, 2, "Broken: ");
            fail("Expecting IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException ex) {
            assertEquals("Broken: ", ex.getMessage());
        }
        
        String[] strArray = new String[] {"Hi"};
        String[] test = Validate.noNullElements(strArray, "Message");
        assertSame(strArray, test);
    }

    public void testValidIndex_array() {
        Object[] array = new Object[2];
        Validate.validIndex(array, 0);
        Validate.validIndex(array, 1);
        try {
            Validate.validIndex(array, -1);
            fail("Expecting IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException ex) {
            assertEquals("The validated array index is invalid: -1", ex.getMessage());
        }
        try {
            Validate.validIndex(array, 2);
            fail("Expecting IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException ex) {
            assertEquals("The validated array index is invalid: 2", ex.getMessage());
        }
        
        String[] strArray = new String[] {"Hi"};
        String[] test = Validate.noNullElements(strArray);
        assertSame(strArray, test);
    }

    //-----------------------------------------------------------------------
    //-----------------------------------------------------------------------
    public void testValidIndex_withMessage_collection() {
        Collection<String> coll = new ArrayList<String>();
        coll.add(null);
        coll.add(null);
        Validate.validIndex(coll, 0, "Broken: ");
        Validate.validIndex(coll, 1, "Broken: ");
        try {
            Validate.validIndex(coll, -1, "Broken: ");
            fail("Expecting IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException ex) {
            assertEquals("Broken: ", ex.getMessage());
        }
        try {
            Validate.validIndex(coll, 2, "Broken: ");
            fail("Expecting IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException ex) {
            assertEquals("Broken: ", ex.getMessage());
        }
        
        List<String> strColl = Arrays.asList(new String[] {"Hi"});
        List<String> test = Validate.validIndex(strColl, 0, "Message");
        assertSame(strColl, test);
    }

    public void testValidIndex_collection() {
        Collection<String> coll = new ArrayList<String>();
        coll.add(null);
        coll.add(null);
        Validate.validIndex(coll, 0);
        Validate.validIndex(coll, 1);
        try {
            Validate.validIndex(coll, -1);
            fail("Expecting IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException ex) {
            assertEquals("The validated collection index is invalid: -1", ex.getMessage());
        }
        try {
            Validate.validIndex(coll, 2);
            fail("Expecting IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException ex) {
            assertEquals("The validated collection index is invalid: 2", ex.getMessage());
        }
        
        List<String> strColl = Arrays.asList(new String[] {"Hi"});
        List<String> test = Validate.validIndex(strColl, 0);
        assertSame(strColl, test);
    }

    //-----------------------------------------------------------------------
    //-----------------------------------------------------------------------
    public void testValidIndex_withMessage_charSequence() {
        CharSequence str = "Hi";
        Validate.validIndex(str, 0, "Broken: ");
        Validate.validIndex(str, 1, "Broken: ");
        try {
            Validate.validIndex(str, -1, "Broken: ");
            fail("Expecting IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException ex) {
            assertEquals("Broken: ", ex.getMessage());
        }
        try {
            Validate.validIndex(str, 2, "Broken: ");
            fail("Expecting IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException ex) {
            assertEquals("Broken: ", ex.getMessage());
        }
        
        String input = "Hi";
        String test = Validate.validIndex(input, 0, "Message");
        assertSame(input, test);
    }

    public void testValidIndex_charSequence() {
        CharSequence str = "Hi";
        Validate.validIndex(str, 0);
        Validate.validIndex(str, 1);
        try {
            Validate.validIndex(str, -1);
            fail("Expecting IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException ex) {
            assertEquals("The validated character sequence index is invalid: -1", ex.getMessage());
        }
        try {
            Validate.validIndex(str, 2);
            fail("Expecting IndexOutOfBoundsException");
        } catch (IndexOutOfBoundsException ex) {
            assertEquals("The validated character sequence index is invalid: 2", ex.getMessage());
        }
        
        String input = "Hi";
        String test = Validate.validIndex(input, 0);
        assertSame(input, test);
    }
    
    public void testMatchesPattern()
    {
        CharSequence str = "hi";
        Validate.matchesPattern(str, "[a-z]*");
        try
        {
            Validate.matchesPattern(str, "[0-9]*");
            fail("Expecting IllegalArgumentException");
        }
        catch (IllegalArgumentException e)
        {
            assertEquals("The string hi does not match the pattern [0-9]*", e.getMessage());
        }
    }
    
    public void testMatchesPattern_withMessage()
    {
        CharSequence str = "hi";
        Validate.matchesPattern(str, "[a-z]*", "Does not match");
        try
        {
            Validate.matchesPattern(str, "[0-9]*", "Does not match");
            fail("Expecting IllegalArgumentException");
        }
        catch (IllegalArgumentException e)
        {
            assertEquals("Does not match", e.getMessage());
        }
    }
    
    public void testInclusiveBetween()
    {
        Validate.inclusiveBetween("a", "c", "b");
        Validate.inclusiveBetween(0, 2, 1);
        Validate.inclusiveBetween(0, 2, 2);
        try {
            Validate.inclusiveBetween(0, 5, 6);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            assertEquals("The value 6 is not in the specified inclusive range of 0 to 5", e.getMessage());
        }
    }
    
    public void testInclusiveBetween_withMessage()
    {
        Validate.inclusiveBetween("a", "c", "b", "Error");
        Validate.inclusiveBetween(0, 2, 1, "Error");
        Validate.inclusiveBetween(0, 2, 2, "Error");
        try {
            Validate.inclusiveBetween(0, 5, 6, "Error");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            assertEquals("Error", e.getMessage());
        }
    }
    
    public void testExclusiveBetween()
    {
        Validate.exclusiveBetween("a", "c", "b");
        Validate.exclusiveBetween(0, 2, 1);
        try {
            Validate.exclusiveBetween(0, 5, 6);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            assertEquals("The value 6 is not in the specified exclusive range of 0 to 5", e.getMessage());
        }
        try {
            Validate.exclusiveBetween(0, 5, 5);
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            assertEquals("The value 5 is not in the specified exclusive range of 0 to 5", e.getMessage());
        }
    }
    
    public void testExclusiveBetween_withMessage()
    {
        Validate.exclusiveBetween("a", "c", "b", "Error");
        Validate.exclusiveBetween(0, 2, 1, "Error");
        try {
            Validate.exclusiveBetween(0, 5, 6, "Error");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            assertEquals("Error", e.getMessage());
        }
        try {
            Validate.exclusiveBetween(0, 5, 5, "Error");
            fail("Expecting IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            assertEquals("Error", e.getMessage());
        }
    }

    public void testIsInstanceOf() {
        Validate.isInstanceOf(String.class, "hi");
        Validate.isInstanceOf(Integer.class, 1);
        try {
            Validate.isInstanceOf(List.class, "hi");
            fail("Expecting IllegalArgumentException");
        } catch(IllegalArgumentException e) {
            assertEquals("The validated object is not an instance of java.util.List", e.getMessage());
        }
    }
    
    public void testIsInstanceOf_withMessage() {
        Validate.isInstanceOf(String.class, "hi", "Error");
        Validate.isInstanceOf(Integer.class, 1, "Error");
        try {
            Validate.isInstanceOf(List.class, "hi", "Error");
            fail("Expecting IllegalArgumentException");
        } catch(IllegalArgumentException e) {
            assertEquals("Error", e.getMessage());
        }
    }
    
    public void testIsAssignable() {
        Validate.isAssignableFrom(CharSequence.class, String.class);
        Validate.isAssignableFrom(AbstractList.class, ArrayList.class);
        try {
            Validate.isAssignableFrom(List.class, String.class);
            fail("Expecting IllegalArgumentException");
        } catch(IllegalArgumentException e) {
            assertEquals("The validated class can not be converted to the java.util.List class", e.getMessage());
        }
    }
    
    public void testIsAssignable_withMessage() {
        Validate.isAssignableFrom(CharSequence.class, String.class, "Error");
        Validate.isAssignableFrom(AbstractList.class, ArrayList.class, "Error");
        try {
            Validate.isAssignableFrom(List.class, String.class, "Error");
            fail("Expecting IllegalArgumentException");
        } catch(IllegalArgumentException e) {
            assertEquals("Error", e.getMessage());
        }
    }

}

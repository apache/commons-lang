/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002-2003 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */
package org.apache.commons.lang;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Unit tests {@link org.apache.commons.lang.ClassUtils}.
 *
 * @author Stephen Colebourne
 * @version $Id: ClassUtilsTest.java,v 1.3 2003/03/23 21:47:30 scolebourne Exp $
 */
public class ClassUtilsTest extends TestCase {

    public ClassUtilsTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
    	TestSuite suite = new TestSuite(ClassUtilsTest.class);
    	suite.setName("ClassUtils Tests");
        return suite;
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    private static class Inner {
    }
    
    // -------------------------------------------------------------------------
    public void test_getShortClassName_Object() {
        assertEquals("ClassUtils", ClassUtils.getShortClassName(new ClassUtils(), "<null>"));
        assertEquals("ClassUtilsTest.Inner", ClassUtils.getShortClassName(new Inner(), "<null>"));
        assertEquals("String", ClassUtils.getShortClassName("hello", "<null>"));
        assertEquals("<null>", ClassUtils.getShortClassName(null, "<null>"));
    }
    
    public void test_getShortClassName_Class() {
        assertEquals("ClassUtils", ClassUtils.getShortClassName(ClassUtils.class));
        assertEquals("Map.Entry", ClassUtils.getShortClassName(Map.Entry.class));
        try {
            ClassUtils.getShortClassName((Class) null);
            fail();
        } catch (IllegalArgumentException ex) {}
    }
    
    public void test_getShortClassName_String() {
        assertEquals("ClassUtils", ClassUtils.getShortClassName(ClassUtils.class.getName()));
        assertEquals("Map.Entry", ClassUtils.getShortClassName(Map.Entry.class.getName()));
        try {
            ClassUtils.getShortClassName((String) null);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            ClassUtils.getShortClassName("");
            fail();
        } catch (IllegalArgumentException ex) {}
    }
    
    // -------------------------------------------------------------------------
    public void test_getPackageName_Object() {
        assertEquals("org.apache.commons.lang", ClassUtils.getPackageName(new ClassUtils(), "<null>"));
        assertEquals("org.apache.commons.lang", ClassUtils.getPackageName(new Inner(), "<null>"));
        assertEquals("<null>", ClassUtils.getPackageName(null, "<null>"));
    }
    
    public void test_getPackageName_Class() {
        assertEquals("java.lang", ClassUtils.getPackageName(String.class));
        assertEquals("java.util", ClassUtils.getPackageName(Map.Entry.class));
        try {
            ClassUtils.getPackageName((Class) null);
            fail();
        } catch (IllegalArgumentException ex) {}
    }
    
    public void test_getPackageName_String() {
        assertEquals("org.apache.commons.lang", ClassUtils.getPackageName(ClassUtils.class.getName()));
        assertEquals("java.util", ClassUtils.getPackageName(Map.Entry.class.getName()));
        try {
            ClassUtils.getPackageName((String) null);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            ClassUtils.getPackageName("");
            fail();
        } catch (IllegalArgumentException ex) {}
    }
    
    // -------------------------------------------------------------------------
    public void test_getAllSuperclasses_Class() {
        List list = ClassUtils.getAllSuperclasses(CY.class);
        assertEquals(2, list.size());
        assertEquals(CX.class, list.get(0));
        assertEquals(Object.class, list.get(1));
        try {
            ClassUtils.getAllSuperclasses(null);
            fail();
        } catch (IllegalArgumentException ex) {}
    }
    
    public void test_getAllInterfaces_Class() {
        List list = ClassUtils.getAllInterfaces(CY.class);
        assertEquals(6, list.size());
        assertEquals(IB.class, list.get(0));
        assertEquals(IC.class, list.get(1));
        assertEquals(ID.class, list.get(2));
        assertEquals(IE.class, list.get(3));
        assertEquals(IF.class, list.get(4));
        assertEquals(IA.class, list.get(5));
        try {
            ClassUtils.getAllInterfaces(null);
            fail();
        } catch (IllegalArgumentException ex) {}
    }
    
    private static interface IA {
    }
    private static interface IB {
    }
    private static interface IC extends ID, IE {
    }
    private static interface ID {
    }
    private static interface IE extends IF {
    }
    private static interface IF {
    }
    private static class CX implements IB, IA, IE {
    }
    private static class CY extends CX implements IB, IC {
    }
    
    // -------------------------------------------------------------------------
    public void test_convertClassNamesToClasses_List() {
        List list = new ArrayList();
        List result = ClassUtils.convertClassNamesToClasses(list);
        assertEquals(0, result.size());
        
        list.add("java.lang.String");
        list.add("java.lang.xxx");
        list.add("java.lang.Object");
        result = ClassUtils.convertClassNamesToClasses(list);
        assertEquals(3, result.size());
        assertEquals(String.class, result.get(0));
        assertEquals(null, result.get(1));
        assertEquals(Object.class, result.get(2));

        list.add(new Object());
        try {
            ClassUtils.convertClassNamesToClasses(list);
            fail();
        } catch (ClassCastException ex) {}
        
        try {
            ClassUtils.convertClassNamesToClasses(null);
            fail();
        } catch (IllegalArgumentException ex) {}
    }
    
    public void test_convertClassesToClassNames_List() {
        List list = new ArrayList();
        List result = ClassUtils.convertClassesToClassNames(list);
        assertEquals(0, result.size());
        
        list.add(String.class);
        list.add(Object.class);
        result = ClassUtils.convertClassesToClassNames(list);
        assertEquals(2, result.size());
        assertEquals("java.lang.String", result.get(0));
        assertEquals("java.lang.Object", result.get(1));

        list.add(new Object());
        try {
            ClassUtils.convertClassesToClassNames(list);
            fail();
        } catch (ClassCastException ex) {}
        
        try {
            ClassUtils.convertClassesToClassNames(null);
            fail();
        } catch (IllegalArgumentException ex) {}
    }
    
    // -------------------------------------------------------------------------
    public void test_isInnerClass_Class() {
        assertEquals(true, ClassUtils.isInnerClass(Inner.class));
        assertEquals(true, ClassUtils.isInnerClass(Map.Entry.class));
        assertEquals(true, ClassUtils.isInnerClass(new Cloneable() {
        }.getClass()));
        assertEquals(false, ClassUtils.isInnerClass(this.getClass()));
        assertEquals(false, ClassUtils.isInnerClass(String.class));
        try {
            ClassUtils.isInnerClass(null);
            fail();
        } catch (IllegalArgumentException ex) {}
    }
    
    // -------------------------------------------------------------------------
    public void test_isAssignable_ClassArray_ClassArray() throws Exception {
        Class[] array2 = new Class[] {Object.class, Object.class};
        Class[] array1 = new Class[] {Object.class};
        Class[] array1s = new Class[] {String.class};
        Class[] array0 = new Class[] {};

        assertEquals(false, ClassUtils.isAssignable(array1, array2));
        assertEquals(false, ClassUtils.isAssignable(null, array2));
        assertEquals(true, ClassUtils.isAssignable(null, array0));
        assertEquals(true, ClassUtils.isAssignable(array0, array0));
        assertEquals(true, ClassUtils.isAssignable(array0, null));
        assertEquals(true, ClassUtils.isAssignable((Class[]) null, (Class[]) null));
        
        assertEquals(false, ClassUtils.isAssignable(array1, array1s));
        assertEquals(true, ClassUtils.isAssignable(array1s, array1s));
        assertEquals(true, ClassUtils.isAssignable(array1s, array1));
    }
    
    public void test_isAssignable() throws Exception {
        try {
            ClassUtils.isAssignable(String.class, null);
            fail();
        } catch (IllegalArgumentException ex) {}
        try {
            ClassUtils.isAssignable((Class) null, (Class) null);
            fail();
        } catch (IllegalArgumentException ex) {}
        assertEquals(true, ClassUtils.isAssignable(null, Object.class));
        assertEquals(true, ClassUtils.isAssignable(null, Integer.class));
        assertEquals(false, ClassUtils.isAssignable(null, Integer.TYPE));
        assertEquals(true, ClassUtils.isAssignable(String.class, Object.class));
        assertEquals(true, ClassUtils.isAssignable(String.class, String.class));
        assertEquals(false, ClassUtils.isAssignable(Object.class, String.class));
        assertEquals(false, ClassUtils.isAssignable(Integer.TYPE, Integer.class));
        assertEquals(false, ClassUtils.isAssignable(Integer.class, Integer.TYPE));
        assertEquals(true, ClassUtils.isAssignable(Integer.TYPE, Integer.TYPE));
        assertEquals(true, ClassUtils.isAssignable(Integer.class, Integer.class));
    }
    
    public void test_isAssignable_Widening() throws Exception {
        // test byte conversions
        assertEquals("byte -> char", false, ClassUtils.isAssignable(Byte.TYPE, Character.TYPE));
        assertEquals("byte -> byte", true, ClassUtils.isAssignable(Byte.TYPE, Byte.TYPE));
        assertEquals("byte -> short", true, ClassUtils.isAssignable(Byte.TYPE, Short.TYPE));
        assertEquals("byte -> int", true, ClassUtils.isAssignable(Byte.TYPE, Integer.TYPE));
        assertEquals("byte -> long", true, ClassUtils.isAssignable(Byte.TYPE, Long.TYPE));
        assertEquals("byte -> float", true, ClassUtils.isAssignable(Byte.TYPE, Float.TYPE));
        assertEquals("byte -> double", true, ClassUtils.isAssignable(Byte.TYPE, Double.TYPE));
        assertEquals("byte -> boolean", false, ClassUtils.isAssignable(Byte.TYPE, Boolean.TYPE));
        
        // test short conversions
        assertEquals("short -> char", false, ClassUtils.isAssignable(Short.TYPE, Character.TYPE));
        assertEquals("short -> byte", false, ClassUtils.isAssignable(Short.TYPE, Byte.TYPE));
        assertEquals("short -> short", true, ClassUtils.isAssignable(Short.TYPE, Short.TYPE));
        assertEquals("short -> int", true, ClassUtils.isAssignable(Short.TYPE, Integer.TYPE));
        assertEquals("short -> long", true, ClassUtils.isAssignable(Short.TYPE, Long.TYPE));
        assertEquals("short -> float", true, ClassUtils.isAssignable(Short.TYPE, Float.TYPE));
        assertEquals("short -> double", true, ClassUtils.isAssignable(Short.TYPE, Double.TYPE));
        assertEquals("short -> boolean", false, ClassUtils.isAssignable(Short.TYPE, Boolean.TYPE));
        
        // test char conversions
        assertEquals("char -> char", true, ClassUtils.isAssignable(Character.TYPE, Character.TYPE));
        assertEquals("char -> byte", false, ClassUtils.isAssignable(Character.TYPE, Byte.TYPE));
        assertEquals("char -> short", false, ClassUtils.isAssignable(Character.TYPE, Short.TYPE));
        assertEquals("char -> int", true, ClassUtils.isAssignable(Character.TYPE, Integer.TYPE));
        assertEquals("char -> long", true, ClassUtils.isAssignable(Character.TYPE, Long.TYPE));
        assertEquals("char -> float", true, ClassUtils.isAssignable(Character.TYPE, Float.TYPE));
        assertEquals("char -> double", true, ClassUtils.isAssignable(Character.TYPE, Double.TYPE));
        assertEquals("char -> boolean", false, ClassUtils.isAssignable(Character.TYPE, Boolean.TYPE));
        
        // test int conversions
        assertEquals("int -> char", false, ClassUtils.isAssignable(Integer.TYPE, Character.TYPE));
        assertEquals("int -> byte", false, ClassUtils.isAssignable(Integer.TYPE, Byte.TYPE));
        assertEquals("int -> short", false, ClassUtils.isAssignable(Integer.TYPE, Short.TYPE));
        assertEquals("int -> int", true, ClassUtils.isAssignable(Integer.TYPE, Integer.TYPE));
        assertEquals("int -> long", true, ClassUtils.isAssignable(Integer.TYPE, Long.TYPE));
        assertEquals("int -> float", true, ClassUtils.isAssignable(Integer.TYPE, Float.TYPE));
        assertEquals("int -> double", true, ClassUtils.isAssignable(Integer.TYPE, Double.TYPE));
        assertEquals("int -> boolean", false, ClassUtils.isAssignable(Integer.TYPE, Boolean.TYPE));
 
        // test long conversions
        assertEquals("long -> char", false, ClassUtils.isAssignable(Long.TYPE, Character.TYPE));
        assertEquals("long -> byte", false, ClassUtils.isAssignable(Long.TYPE, Byte.TYPE));
        assertEquals("long -> short", false, ClassUtils.isAssignable(Long.TYPE, Short.TYPE));
        assertEquals("long -> int", false, ClassUtils.isAssignable(Long.TYPE, Integer.TYPE));
        assertEquals("long -> long", true, ClassUtils.isAssignable(Long.TYPE, Long.TYPE));
        assertEquals("long -> float", true, ClassUtils.isAssignable(Long.TYPE, Float.TYPE));
        assertEquals("long -> double", true, ClassUtils.isAssignable(Long.TYPE, Double.TYPE));
        assertEquals("long -> boolean", false, ClassUtils.isAssignable(Long.TYPE, Boolean.TYPE));
 
        // test float conversions
        assertEquals("float -> char", false, ClassUtils.isAssignable(Float.TYPE, Character.TYPE));
        assertEquals("float -> byte", false, ClassUtils.isAssignable(Float.TYPE, Byte.TYPE));
        assertEquals("float -> short", false, ClassUtils.isAssignable(Float.TYPE, Short.TYPE));
        assertEquals("float -> int", false, ClassUtils.isAssignable(Float.TYPE, Integer.TYPE));
        assertEquals("float -> long", false, ClassUtils.isAssignable(Float.TYPE, Long.TYPE));
        assertEquals("float -> float", true, ClassUtils.isAssignable(Float.TYPE, Float.TYPE));
        assertEquals("float -> double", true, ClassUtils.isAssignable(Float.TYPE, Double.TYPE));
        assertEquals("float -> boolean", false, ClassUtils.isAssignable(Float.TYPE, Boolean.TYPE));
        
        // test float conversions
        assertEquals("double -> char", false, ClassUtils.isAssignable(Double.TYPE, Character.TYPE));
        assertEquals("double -> byte", false, ClassUtils.isAssignable(Double.TYPE, Byte.TYPE));
        assertEquals("double -> short", false, ClassUtils.isAssignable(Double.TYPE, Short.TYPE));
        assertEquals("double -> int", false, ClassUtils.isAssignable(Double.TYPE, Integer.TYPE));
        assertEquals("double -> long", false, ClassUtils.isAssignable(Double.TYPE, Long.TYPE));
        assertEquals("double -> float", false, ClassUtils.isAssignable(Double.TYPE, Float.TYPE));
        assertEquals("double -> double", true, ClassUtils.isAssignable(Double.TYPE, Double.TYPE));
        assertEquals("double -> boolean", false, ClassUtils.isAssignable(Double.TYPE, Boolean.TYPE));
        
        // test float conversions
        assertEquals("boolean -> char", false, ClassUtils.isAssignable(Boolean.TYPE, Character.TYPE));
        assertEquals("boolean -> byte", false, ClassUtils.isAssignable(Boolean.TYPE, Byte.TYPE));
        assertEquals("boolean -> short", false, ClassUtils.isAssignable(Boolean.TYPE, Short.TYPE));
        assertEquals("boolean -> int", false, ClassUtils.isAssignable(Boolean.TYPE, Integer.TYPE));
        assertEquals("boolean -> long", false, ClassUtils.isAssignable(Boolean.TYPE, Long.TYPE));
        assertEquals("boolean -> float", false, ClassUtils.isAssignable(Boolean.TYPE, Float.TYPE));
        assertEquals("boolean -> double", false, ClassUtils.isAssignable(Boolean.TYPE, Double.TYPE));
        assertEquals("boolean -> boolean", true, ClassUtils.isAssignable(Boolean.TYPE, Boolean.TYPE));
    }
    
//    public static List getAssignableFrom(List classes, Class superclass) {
//    public static boolean isAssignable(Class[] classArray, Class[] toClassArray) {
//    public static boolean isAssignable(Class cls, Class toClass) {
}

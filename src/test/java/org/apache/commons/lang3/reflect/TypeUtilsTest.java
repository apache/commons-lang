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
package org.apache.commons.lang3.reflect;

import java.io.Serializable;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.net.URI;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.reflect.testbed.Foo;
import org.apache.commons.lang3.reflect.testbed.GenericParent;
import org.apache.commons.lang3.reflect.testbed.GenericTypeHolder;
import org.apache.commons.lang3.reflect.testbed.StringParameterizedChild;
import org.junit.Assert;
import org.junit.Test;

/**
 * Test TypeUtils
 * @version $Id$
 */
@SuppressWarnings({ "unchecked", "unused" , "rawtypes", "null"})
//raw types, where used, are used purposely
public class TypeUtilsTest<B> {

    public interface This<K, V> {
    }

    public class That<K, V> implements This<K, V> {
    }

    public interface And<K, V> extends This<Number, Number> {
    }

    public class The<K, V> extends That<Number, Number> implements And<String, String> {
    }

    public class Other<T> implements This<String, T> {
    }

    public class Thing<Q> extends Other<B> {
    }

    public class Tester implements This<String, B> {
    }

    public This<String, String> dis;

    public That<String, String> dat;

    public The<String, String> da;

    public Other<String> uhder;

    public Thing ding;

    public TypeUtilsTest<String>.Tester tester;

    public Tester tester2;

    public TypeUtilsTest<String>.That<String, String> dat2;

    public TypeUtilsTest<Number>.That<String, String> dat3;

    public Comparable<? extends Integer>[] intWildcardComparable;

    public static Comparable<String> stringComparable;

    public static Comparable<URI> uriComparable;

    public static Comparable<Integer> intComparable;

    public static Comparable<Long> longComparable;

    public static URI uri;

    public void dummyMethod(List list0, List<Object> list1, List<?> list2,
            List<? super Object> list3, List<String> list4, List<? extends String> list5,
            List<? super String> list6, List[] list7, List<Object>[] list8, List<?>[] list9,
            List<? super Object>[] list10, List<String>[] list11, List<? extends String>[] list12,
            List<? super String>[] list13) {
    }

    @SuppressWarnings("boxing") // deliberately used here
    @Test
    public void testIsAssignable() throws SecurityException, NoSuchMethodException,
            NoSuchFieldException {
        List list0 = null;
        List<Object> list1 = null;
        List<?> list2 = null;
        List<? super Object> list3 = null;
        List<String> list4 = null;
        List<? extends String> list5 = null;
        List<? super String> list6 = null;
        List[] list7 = null;
        List<Object>[] list8 = null;
        List<?>[] list9 = null;
        List<? super Object>[] list10 = null;
        List<String>[] list11 = null;
        List<? extends String>[] list12 = null;
        List<? super String>[] list13;
        Class<?> clazz = getClass();
        Method method = clazz.getMethod("dummyMethod", List.class, List.class, List.class,
                List.class, List.class, List.class, List.class, List[].class, List[].class,
                List[].class, List[].class, List[].class, List[].class, List[].class);
        Type[] types = method.getGenericParameterTypes();
//        list0 = list0;
        delegateBooleanAssertion(types, 0, 0, true);
        list1 = list0;
        delegateBooleanAssertion(types, 0, 1, true);
        list0 = list1;
        delegateBooleanAssertion(types, 1, 0, true);
        list2 = list0;
        delegateBooleanAssertion(types, 0, 2, true);
        list0 = list2;
        delegateBooleanAssertion(types, 2, 0, true);
        list3 = list0;
        delegateBooleanAssertion(types, 0, 3, true);
        list0 = list3;
        delegateBooleanAssertion(types, 3, 0, true);
        list4 = list0;
        delegateBooleanAssertion(types, 0, 4, true);
        list0 = list4;
        delegateBooleanAssertion(types, 4, 0, true);
        list5 = list0;
        delegateBooleanAssertion(types, 0, 5, true);
        list0 = list5;
        delegateBooleanAssertion(types, 5, 0, true);
        list6 = list0;
        delegateBooleanAssertion(types, 0, 6, true);
        list0 = list6;
        delegateBooleanAssertion(types, 6, 0, true);
//        list1 = list1;
        delegateBooleanAssertion(types, 1, 1, true);
        list2 = list1;
        delegateBooleanAssertion(types, 1, 2, true);
        list1 = (List<Object>) list2;
        delegateBooleanAssertion(types, 2, 1, false);
        list3 = list1;
        delegateBooleanAssertion(types, 1, 3, true);
        list1 = (List<Object>) list3;
        delegateBooleanAssertion(types, 3, 1, false);
        // list4 = list1;
        delegateBooleanAssertion(types, 1, 4, false);
        // list1 = list4;
        delegateBooleanAssertion(types, 4, 1, false);
        // list5 = list1;
        delegateBooleanAssertion(types, 1, 5, false);
        // list1 = list5;
        delegateBooleanAssertion(types, 5, 1, false);
        list6 = list1;
        delegateBooleanAssertion(types, 1, 6, true);
        list1 = (List<Object>) list6;
        delegateBooleanAssertion(types, 6, 1, false);
//        list2 = list2;
        delegateBooleanAssertion(types, 2, 2, true);
        list2 = list3;
        delegateBooleanAssertion(types, 2, 3, false);
        list2 = list4;
        delegateBooleanAssertion(types, 3, 2, true);
        list3 = (List<? super Object>) list2;
        delegateBooleanAssertion(types, 2, 4, false);
        list2 = list5;
        delegateBooleanAssertion(types, 4, 2, true);
        list4 = (List<String>) list2;
        delegateBooleanAssertion(types, 2, 5, false);
        list2 = list6;
        delegateBooleanAssertion(types, 5, 2, true);
        list5 = (List<? extends String>) list2;
        delegateBooleanAssertion(types, 2, 6, false);
//        list3 = list3;
        delegateBooleanAssertion(types, 6, 2, true);
        list6 = (List<? super String>) list2;
        delegateBooleanAssertion(types, 3, 3, true);
        // list4 = list3;
        delegateBooleanAssertion(types, 3, 4, false);
        // list3 = list4;
        delegateBooleanAssertion(types, 4, 3, false);
        // list5 = list3;
        delegateBooleanAssertion(types, 3, 5, false);
        // list3 = list5;
        delegateBooleanAssertion(types, 5, 3, false);
        list6 = list3;
        delegateBooleanAssertion(types, 3, 6, true);
        list3 = (List<? super Object>) list6;
        delegateBooleanAssertion(types, 6, 3, false);
//        list4 = list4;
        delegateBooleanAssertion(types, 4, 4, true);
        list5 = list4;
        delegateBooleanAssertion(types, 4, 5, true);
        list4 = (List<String>) list5;
        delegateBooleanAssertion(types, 5, 4, false);
        list6 = list4;
        delegateBooleanAssertion(types, 4, 6, true);
        list4 = (List<String>) list6;
        delegateBooleanAssertion(types, 6, 4, false);
//        list5 = list5;
        delegateBooleanAssertion(types, 5, 5, true);
        list6 = (List<? super String>) list5;
        delegateBooleanAssertion(types, 5, 6, false);
        list5 = (List<? extends String>) list6;
        delegateBooleanAssertion(types, 6, 5, false);
//        list6 = list6;
        delegateBooleanAssertion(types, 6, 6, true);

//        list7 = list7;
        delegateBooleanAssertion(types, 7, 7, true);
        list8 = list7;
        delegateBooleanAssertion(types, 7, 8, true);
        list7 = list8;
        delegateBooleanAssertion(types, 8, 7, true);
        list9 = list7;
        delegateBooleanAssertion(types, 7, 9, true);
        list7 = list9;
        delegateBooleanAssertion(types, 9, 7, true);
        list10 = list7;
        delegateBooleanAssertion(types, 7, 10, true);
        list7 = list10;
        delegateBooleanAssertion(types, 10, 7, true);
        list11 = list7;
        delegateBooleanAssertion(types, 7, 11, true);
        list7 = list11;
        delegateBooleanAssertion(types, 11, 7, true);
        list12 = list7;
        delegateBooleanAssertion(types, 7, 12, true);
        list7 = list12;
        delegateBooleanAssertion(types, 12, 7, true);
        list13 = list7;
        delegateBooleanAssertion(types, 7, 13, true);
        list7 = list13;
        delegateBooleanAssertion(types, 13, 7, true);
//        list8 = list8;
        delegateBooleanAssertion(types, 8, 8, true);
        list9 = list8;
        delegateBooleanAssertion(types, 8, 9, true);
        list8 = (List<Object>[]) list9;
        delegateBooleanAssertion(types, 9, 8, false);
        list10 = list8;
        delegateBooleanAssertion(types, 8, 10, true);
        list8 = (List<Object>[]) list10; // NOTE cast is required by Sun Java, but not by Eclipse
        delegateBooleanAssertion(types, 10, 8, false);
        // list11 = list8;
        delegateBooleanAssertion(types, 8, 11, false);
        // list8 = list11;
        delegateBooleanAssertion(types, 11, 8, false);
        // list12 = list8;
        delegateBooleanAssertion(types, 8, 12, false);
        // list8 = list12;
        delegateBooleanAssertion(types, 12, 8, false);
        list13 = list8;
        delegateBooleanAssertion(types, 8, 13, true);
        list8 = (List<Object>[]) list13;
        delegateBooleanAssertion(types, 13, 8, false);
//        list9 = list9;
        delegateBooleanAssertion(types, 9, 9, true);
        list10 = (List<? super Object>[]) list9;
        delegateBooleanAssertion(types, 9, 10, false);
        list9 = list10;
        delegateBooleanAssertion(types, 10, 9, true);
        list11 = (List<String>[]) list9;
        delegateBooleanAssertion(types, 9, 11, false);
        list9 = list11;
        delegateBooleanAssertion(types, 11, 9, true);
        list12 = (List<? extends String>[]) list9;
        delegateBooleanAssertion(types, 9, 12, false);
        list9 = list12;
        delegateBooleanAssertion(types, 12, 9, true);
        list13 = (List<? super String>[]) list9;
        delegateBooleanAssertion(types, 9, 13, false);
        list9 = list13;
        delegateBooleanAssertion(types, 13, 9, true);
//        list10 = list10;
        delegateBooleanAssertion(types, 10, 10, true);
        // list11 = list10;
        delegateBooleanAssertion(types, 10, 11, false);
        // list10 = list11;
        delegateBooleanAssertion(types, 11, 10, false);
        // list12 = list10;
        delegateBooleanAssertion(types, 10, 12, false);
        // list10 = list12;
        delegateBooleanAssertion(types, 12, 10, false);
        list13 = list10;
        delegateBooleanAssertion(types, 10, 13, true);
        list10 = (List<? super Object>[]) list13;
        delegateBooleanAssertion(types, 13, 10, false);
//        list11 = list11;
        delegateBooleanAssertion(types, 11, 11, true);
        list12 = list11;
        delegateBooleanAssertion(types, 11, 12, true);
        list11 = (List<String>[]) list12;
        delegateBooleanAssertion(types, 12, 11, false);
        list13 = list11;
        delegateBooleanAssertion(types, 11, 13, true);
        list11 = (List<String>[]) list13;
        delegateBooleanAssertion(types, 13, 11, false);
//        list12 = list12;
        delegateBooleanAssertion(types, 12, 12, true);
        list13 = (List<? super String>[]) list12;
        delegateBooleanAssertion(types, 12, 13, false);
        list12 = (List<? extends String>[]) list13;
        delegateBooleanAssertion(types, 13, 12, false);
//        list13 = list13;
        delegateBooleanAssertion(types, 13, 13, true);
        Type disType = getClass().getField("dis").getGenericType();
        // Reporter.log( ( ( ParameterizedType ) disType
        // ).getOwnerType().getClass().toString() );
        Type datType = getClass().getField("dat").getGenericType();
        Type daType = getClass().getField("da").getGenericType();
        Type uhderType = getClass().getField("uhder").getGenericType();
        Type dingType = getClass().getField("ding").getGenericType();
        Type testerType = getClass().getField("tester").getGenericType();
        Type tester2Type = getClass().getField("tester2").getGenericType();
        Type dat2Type = getClass().getField("dat2").getGenericType();
        Type dat3Type = getClass().getField("dat3").getGenericType();
        dis = dat;
        Assert.assertTrue(TypeUtils.isAssignable(datType, disType));
        // dis = da;
        Assert.assertFalse(TypeUtils.isAssignable(daType, disType));
        dis = uhder;
        Assert.assertTrue(TypeUtils.isAssignable(uhderType, disType));
        dis = ding;
        Assert.assertFalse(String.format("type %s not assignable to %s!", dingType, disType),
                TypeUtils.isAssignable(dingType, disType));
        dis = tester;
        Assert.assertTrue(TypeUtils.isAssignable(testerType, disType));
        // dis = tester2;
        Assert.assertFalse(TypeUtils.isAssignable(tester2Type, disType));
        // dat = dat2;
        Assert.assertFalse(TypeUtils.isAssignable(dat2Type, datType));
        // dat2 = dat;
        Assert.assertFalse(TypeUtils.isAssignable(datType, dat2Type));
        // dat = dat3;
        Assert.assertFalse(TypeUtils.isAssignable(dat3Type, datType));
        char ch = 0;
        boolean bo = false;
        byte by = 0;
        short sh = 0;
        int in = 0;
        long lo = 0;
        float fl = 0;
        double du = 0;
        du = ch;
        Assert.assertTrue(TypeUtils.isAssignable(char.class, double.class));
        du = by;
        Assert.assertTrue(TypeUtils.isAssignable(byte.class, double.class));
        du = sh;
        Assert.assertTrue(TypeUtils.isAssignable(short.class, double.class));
        du = in;
        Assert.assertTrue(TypeUtils.isAssignable(int.class, double.class));
        du = lo;
        Assert.assertTrue(TypeUtils.isAssignable(long.class, double.class));
        du = fl;
        Assert.assertTrue(TypeUtils.isAssignable(float.class, double.class));
        lo = in;
        Assert.assertTrue(TypeUtils.isAssignable(int.class, long.class));
        lo = Integer.valueOf(0);
        Assert.assertTrue(TypeUtils.isAssignable(Integer.class, long.class));
        // Long lngW = 1;
        Assert.assertFalse(TypeUtils.isAssignable(int.class, Long.class));
        // lngW = Integer.valueOf( 0 );
        Assert.assertFalse(TypeUtils.isAssignable(Integer.class, Long.class));
        in = Integer.valueOf(0);
        Assert.assertTrue(TypeUtils.isAssignable(Integer.class, int.class));
        Integer inte = in;
        Assert.assertTrue(TypeUtils.isAssignable(int.class, Integer.class));
        Assert.assertTrue(TypeUtils.isAssignable(int.class, Number.class));
        Assert.assertTrue(TypeUtils.isAssignable(int.class, Object.class));
        Type intComparableType = getClass().getField("intComparable").getGenericType();
        intComparable = 1;
        Assert.assertTrue(TypeUtils.isAssignable(int.class, intComparableType));
        Assert.assertTrue(TypeUtils.isAssignable(int.class, Comparable.class));
        Serializable ser = 1;
        Assert.assertTrue(TypeUtils.isAssignable(int.class, Serializable.class));
        Type longComparableType = getClass().getField("longComparable").getGenericType();
        // longComparable = 1;
        Assert.assertFalse(TypeUtils.isAssignable(int.class, longComparableType));
        // longComparable = Integer.valueOf( 0 );
        Assert.assertFalse(TypeUtils.isAssignable(Integer.class, longComparableType));
        // int[] ia;
        // long[] la = ia;
        Assert.assertFalse(TypeUtils.isAssignable(int[].class, long[].class));
        Integer[] ia = null;
        Type caType = getClass().getField("intWildcardComparable").getGenericType();
        intWildcardComparable = ia;
        Assert.assertTrue(TypeUtils.isAssignable(Integer[].class, caType));
        // int[] ina = ia;
        Assert.assertFalse(TypeUtils.isAssignable(Integer[].class, int[].class));
        int[] ina = null;
        Object[] oa;
        // oa = ina;
        Assert.assertFalse(TypeUtils.isAssignable(int[].class, Object[].class));
        oa = new Integer[0];
        Assert.assertTrue(TypeUtils.isAssignable(Integer[].class, Object[].class));
        Type bClassType = AClass.class.getField("bClass").getGenericType();
        Type cClassType = AClass.class.getField("cClass").getGenericType();
        Type dClassType = AClass.class.getField("dClass").getGenericType();
        Type eClassType = AClass.class.getField("eClass").getGenericType();
        Type fClassType = AClass.class.getField("fClass").getGenericType();
        AClass aClass = new AClass(new AAClass<String>());
        aClass.bClass = aClass.cClass;
        Assert.assertTrue(TypeUtils.isAssignable(cClassType, bClassType));
        aClass.bClass = aClass.dClass;
        Assert.assertTrue(TypeUtils.isAssignable(dClassType, bClassType));
        aClass.bClass = aClass.eClass;
        Assert.assertTrue(TypeUtils.isAssignable(eClassType, bClassType));
        aClass.bClass = aClass.fClass;
        Assert.assertTrue(TypeUtils.isAssignable(fClassType, bClassType));
        aClass.cClass = aClass.dClass;
        Assert.assertTrue(TypeUtils.isAssignable(dClassType, cClassType));
        aClass.cClass = aClass.eClass;
        Assert.assertTrue(TypeUtils.isAssignable(eClassType, cClassType));
        aClass.cClass = aClass.fClass;
        Assert.assertTrue(TypeUtils.isAssignable(fClassType, cClassType));
        aClass.dClass = aClass.eClass;
        Assert.assertTrue(TypeUtils.isAssignable(eClassType, dClassType));
        aClass.dClass = aClass.fClass;
        Assert.assertTrue(TypeUtils.isAssignable(fClassType, dClassType));
        aClass.eClass = aClass.fClass;
        Assert.assertTrue(TypeUtils.isAssignable(fClassType, eClassType));
    }

    public void delegateBooleanAssertion(Type[] types, int i2, int i1, boolean expected) {
        Type type1 = types[i1];
        Type type2 = types[i2];
        boolean isAssignable = TypeUtils.isAssignable(type2, type1);

        if (expected) {
            Assert.assertTrue("[" + i1 + ", " + i2 + "]: From "
                    + StringEscapeUtils.escapeHtml4(String.valueOf(type2)) + " to "
                    + StringEscapeUtils.escapeHtml4(String.valueOf(type1)), isAssignable);
        } else {
            Assert.assertFalse("[" + i1 + ", " + i2 + "]: From "
                    + StringEscapeUtils.escapeHtml4(String.valueOf(type2)) + " to "
                    + StringEscapeUtils.escapeHtml4(String.valueOf(type1)), isAssignable);
        }
    }

    @SuppressWarnings("boxing") // boxing is deliberate here
    @Test
    public void testIsInstance() throws SecurityException, NoSuchFieldException {
        Type intComparableType = getClass().getField("intComparable").getGenericType();
        Type uriComparableType = getClass().getField("uriComparable").getGenericType();
        intComparable = 1;
        Assert.assertTrue(TypeUtils.isInstance(1, intComparableType));
        // uriComparable = 1;
        Assert.assertFalse(TypeUtils.isInstance(1, uriComparableType));
    }

    @Test
    public void testGetTypeArguments() {
        Map<TypeVariable<?>, Type> typeVarAssigns;
        TypeVariable<?> treeSetTypeVar;
        Type typeArg;

        typeVarAssigns = TypeUtils.getTypeArguments(Integer.class, Comparable.class);
        treeSetTypeVar = Comparable.class.getTypeParameters()[0];
        Assert.assertTrue("Type var assigns for Comparable from Integer: " + typeVarAssigns,
                typeVarAssigns.containsKey(treeSetTypeVar));
        typeArg = typeVarAssigns.get(treeSetTypeVar);
        Assert.assertEquals("Type argument of Comparable from Integer: " + typeArg, Integer.class,
                typeVarAssigns.get(treeSetTypeVar));

        typeVarAssigns = TypeUtils.getTypeArguments(int.class, Comparable.class);
        treeSetTypeVar = Comparable.class.getTypeParameters()[0];
        Assert.assertTrue("Type var assigns for Comparable from int: " + typeVarAssigns,
                typeVarAssigns.containsKey(treeSetTypeVar));
        typeArg = typeVarAssigns.get(treeSetTypeVar);
        Assert.assertEquals("Type argument of Comparable from int: " + typeArg, Integer.class,
                typeVarAssigns.get(treeSetTypeVar));

        Collection<Integer> col = Arrays.asList(new Integer[0]);
        typeVarAssigns = TypeUtils.getTypeArguments(List.class, Collection.class);
        treeSetTypeVar = Comparable.class.getTypeParameters()[0];
        Assert.assertFalse("Type var assigns for Collection from List: " + typeVarAssigns,
                typeVarAssigns.containsKey(treeSetTypeVar));

        typeVarAssigns = TypeUtils.getTypeArguments(AAAClass.BBBClass.class, AAClass.BBClass.class);
        Assert.assertTrue(typeVarAssigns.size() == 2);
        Assert.assertEquals(String.class, typeVarAssigns.get(AAClass.class.getTypeParameters()[0]));
        Assert.assertEquals(String.class, typeVarAssigns.get(AAClass.BBClass.class.getTypeParameters()[0]));

        typeVarAssigns = TypeUtils.getTypeArguments(Other.class, This.class);
        Assert.assertEquals(2, typeVarAssigns.size());
        Assert.assertEquals(String.class, typeVarAssigns.get(This.class.getTypeParameters()[0]));
        Assert.assertEquals(Other.class.getTypeParameters()[0], typeVarAssigns.get(This.class.getTypeParameters()[1]));
    }

    @Test
    public void testTypesSatisfyVariables() throws SecurityException, NoSuchFieldException,
            NoSuchMethodException {
        Map<TypeVariable<?>, Type> typeVarAssigns = new HashMap<TypeVariable<?>, Type>();
        Integer max = TypeUtilsTest.stub();
        typeVarAssigns.put(getClass().getMethod("stub").getTypeParameters()[0], Integer.class);
        Assert.assertTrue(TypeUtils.typesSatisfyVariables(typeVarAssigns));
        typeVarAssigns.clear();
        typeVarAssigns.put(getClass().getMethod("stub2").getTypeParameters()[0], Integer.class);
        Assert.assertTrue(TypeUtils.typesSatisfyVariables(typeVarAssigns));
        typeVarAssigns.clear();
        typeVarAssigns.put(getClass().getMethod("stub3").getTypeParameters()[0], Integer.class);
        Assert.assertTrue(TypeUtils.typesSatisfyVariables(typeVarAssigns));
    }

    @Test
    public void testDetermineTypeVariableAssignments() throws SecurityException,
            NoSuchFieldException, NoSuchMethodException {
        ParameterizedType iterableType = (ParameterizedType) getClass().getField("iterable")
                .getGenericType();
        Map<TypeVariable<?>, Type> typeVarAssigns = TypeUtils.determineTypeArguments(TreeSet.class,
                iterableType);
        TypeVariable<?> treeSetTypeVar = TreeSet.class.getTypeParameters()[0];
        Assert.assertTrue(typeVarAssigns.containsKey(treeSetTypeVar));
        Assert.assertEquals(iterableType.getActualTypeArguments()[0], typeVarAssigns
                .get(treeSetTypeVar));
    }

    @Test
    public void testGetRawType() throws SecurityException, NoSuchFieldException {
        Type stringParentFieldType = GenericTypeHolder.class.getDeclaredField("stringParent")
                .getGenericType();
        Type integerParentFieldType = GenericTypeHolder.class.getDeclaredField("integerParent")
                .getGenericType();
        Type foosFieldType = GenericTypeHolder.class.getDeclaredField("foos").getGenericType();
        Type genericParentT = GenericParent.class.getTypeParameters()[0];
        Assert.assertEquals(GenericParent.class, TypeUtils.getRawType(stringParentFieldType, null));
        Assert
                .assertEquals(GenericParent.class, TypeUtils.getRawType(integerParentFieldType,
                        null));
        Assert.assertEquals(List.class, TypeUtils.getRawType(foosFieldType, null));
        Assert.assertEquals(String.class, TypeUtils.getRawType(genericParentT,
                StringParameterizedChild.class));
        Assert.assertEquals(String.class, TypeUtils.getRawType(genericParentT,
                stringParentFieldType));
        Assert.assertEquals(Foo.class, TypeUtils.getRawType(Iterable.class.getTypeParameters()[0],
                foosFieldType));
        Assert.assertEquals(Foo.class, TypeUtils.getRawType(List.class.getTypeParameters()[0],
                foosFieldType));
        Assert.assertNull(TypeUtils.getRawType(genericParentT, GenericParent.class));
        Assert.assertEquals(GenericParent[].class, TypeUtils.getRawType(GenericTypeHolder.class
                .getDeclaredField("barParents").getGenericType(), null));
    }

    @Test
    public void testIsArrayTypeClasses() {
        Assert.assertTrue(TypeUtils.isArrayType(boolean[].class));
        Assert.assertTrue(TypeUtils.isArrayType(byte[].class));
        Assert.assertTrue(TypeUtils.isArrayType(short[].class));
        Assert.assertTrue(TypeUtils.isArrayType(int[].class));
        Assert.assertTrue(TypeUtils.isArrayType(char[].class));
        Assert.assertTrue(TypeUtils.isArrayType(long[].class));
        Assert.assertTrue(TypeUtils.isArrayType(float[].class));
        Assert.assertTrue(TypeUtils.isArrayType(double[].class));
        Assert.assertTrue(TypeUtils.isArrayType(Object[].class));
        Assert.assertTrue(TypeUtils.isArrayType(String[].class));

        Assert.assertFalse(TypeUtils.isArrayType(boolean.class));
        Assert.assertFalse(TypeUtils.isArrayType(byte.class));
        Assert.assertFalse(TypeUtils.isArrayType(short.class));
        Assert.assertFalse(TypeUtils.isArrayType(int.class));
        Assert.assertFalse(TypeUtils.isArrayType(char.class));
        Assert.assertFalse(TypeUtils.isArrayType(long.class));
        Assert.assertFalse(TypeUtils.isArrayType(float.class));
        Assert.assertFalse(TypeUtils.isArrayType(double.class));
        Assert.assertFalse(TypeUtils.isArrayType(Object.class));
        Assert.assertFalse(TypeUtils.isArrayType(String.class));
    }

    @Test
    public void testIsArrayGenericTypes() throws Exception {
        Method method = getClass().getMethod("dummyMethod", List.class, List.class, List.class,
                List.class, List.class, List.class, List.class, List[].class, List[].class,
                List[].class, List[].class, List[].class, List[].class, List[].class);

        Type[] types = method.getGenericParameterTypes();

        Assert.assertFalse(TypeUtils.isArrayType(types[0]));
        Assert.assertFalse(TypeUtils.isArrayType(types[1]));
        Assert.assertFalse(TypeUtils.isArrayType(types[2]));
        Assert.assertFalse(TypeUtils.isArrayType(types[3]));
        Assert.assertFalse(TypeUtils.isArrayType(types[4]));
        Assert.assertFalse(TypeUtils.isArrayType(types[5]));
        Assert.assertFalse(TypeUtils.isArrayType(types[6]));
        Assert.assertTrue(TypeUtils.isArrayType(types[7]));
        Assert.assertTrue(TypeUtils.isArrayType(types[8]));
        Assert.assertTrue(TypeUtils.isArrayType(types[9]));
        Assert.assertTrue(TypeUtils.isArrayType(types[10]));
        Assert.assertTrue(TypeUtils.isArrayType(types[11]));
        Assert.assertTrue(TypeUtils.isArrayType(types[12]));
        Assert.assertTrue(TypeUtils.isArrayType(types[13]));
    }

    @Test
    public void testGetPrimitiveArrayComponentType() throws Exception {
        Assert.assertEquals(boolean.class, TypeUtils.getArrayComponentType(boolean[].class));
        Assert.assertEquals(byte.class, TypeUtils.getArrayComponentType(byte[].class));
        Assert.assertEquals(short.class, TypeUtils.getArrayComponentType(short[].class));
        Assert.assertEquals(int.class, TypeUtils.getArrayComponentType(int[].class));
        Assert.assertEquals(char.class, TypeUtils.getArrayComponentType(char[].class));
        Assert.assertEquals(long.class, TypeUtils.getArrayComponentType(long[].class));
        Assert.assertEquals(float.class, TypeUtils.getArrayComponentType(float[].class));
        Assert.assertEquals(double.class, TypeUtils.getArrayComponentType(double[].class));

        Assert.assertNull(TypeUtils.getArrayComponentType(boolean.class));
        Assert.assertNull(TypeUtils.getArrayComponentType(byte.class));
        Assert.assertNull(TypeUtils.getArrayComponentType(short.class));
        Assert.assertNull(TypeUtils.getArrayComponentType(int.class));
        Assert.assertNull(TypeUtils.getArrayComponentType(char.class));
        Assert.assertNull(TypeUtils.getArrayComponentType(long.class));
        Assert.assertNull(TypeUtils.getArrayComponentType(float.class));
        Assert.assertNull(TypeUtils.getArrayComponentType(double.class));
    }

    @Test
    public void testGetArrayComponentType() throws Exception {
        Method method = getClass().getMethod("dummyMethod", List.class, List.class, List.class,
                List.class, List.class, List.class, List.class, List[].class, List[].class,
                List[].class, List[].class, List[].class, List[].class, List[].class);

        Type[] types = method.getGenericParameterTypes();

        Assert.assertNull(TypeUtils.getArrayComponentType(types[0]));
        Assert.assertNull(TypeUtils.getArrayComponentType(types[1]));
        Assert.assertNull(TypeUtils.getArrayComponentType(types[2]));
        Assert.assertNull(TypeUtils.getArrayComponentType(types[3]));
        Assert.assertNull(TypeUtils.getArrayComponentType(types[4]));
        Assert.assertNull(TypeUtils.getArrayComponentType(types[5]));
        Assert.assertNull(TypeUtils.getArrayComponentType(types[6]));
        Assert.assertEquals(types[0], TypeUtils.getArrayComponentType(types[7]));
        Assert.assertEquals(types[1], TypeUtils.getArrayComponentType(types[8]));
        Assert.assertEquals(types[2], TypeUtils.getArrayComponentType(types[9]));
        Assert.assertEquals(types[3], TypeUtils.getArrayComponentType(types[10]));
        Assert.assertEquals(types[4], TypeUtils.getArrayComponentType(types[11]));
        Assert.assertEquals(types[5], TypeUtils.getArrayComponentType(types[12]));
        Assert.assertEquals(types[6], TypeUtils.getArrayComponentType(types[13]));
    }

    @Test
    public void testLang820() throws Exception {
        Type[] typeArray = {String.class, String.class};
        Type[] expectedArray = {String.class};
        Assert.assertArrayEquals(expectedArray, TypeUtils.normalizeUpperBounds(typeArray));
    }

    public Iterable<? extends Map<Integer, ? extends Collection<?>>> iterable;

    public static <G extends Comparable<G>> G stub() {
        return null;
    }

    public static <G extends Comparable<? super G>> G stub2() {
        return null;
    }

    public static <T extends Comparable<? extends T>> T stub3() {
        return null;
    }
}

class AAClass<T> {

    public class BBClass<S> {
    }
}

class AAAClass extends AAClass<String> {
    public class BBBClass extends BBClass<String> {
    }
}

@SuppressWarnings("rawtypes")
//raw types, where used, are used purposely
class AClass extends AAClass<String>.BBClass<Number> {

    public AClass(AAClass<String> enclosingInstance) {
        enclosingInstance.super();
    }

    public class BClass<T> {
    }

    public class CClass<T> extends BClass {
    }

    public class DClass<T> extends CClass<T> {
    }

    public class EClass<T> extends DClass {
    }

    public class FClass extends EClass<String> {
    }

    public class GClass<T extends BClass<? extends T> & AInterface<AInterface<? super T>>> {
    }

    public BClass<Number> bClass;

    public CClass<? extends String> cClass;

    public DClass<String> dClass;

    public EClass<String> eClass;

    public FClass fClass;

    public GClass gClass;

    public interface AInterface<T> {
    }
}

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

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

import org.apache.commons.lang3.reflect.testbed.Foo;
import org.apache.commons.lang3.reflect.testbed.GenericParent;
import org.apache.commons.lang3.reflect.testbed.GenericTypeHolder;
import org.apache.commons.lang3.reflect.testbed.StringParameterizedChild;
import org.junit.jupiter.api.Test;

/**
 * Test TypeUtils
 */
@SuppressWarnings({ "unchecked", "unused" , "rawtypes" })
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

    public static Comparable<?> wildcardComparable;

    public static URI uri;

    public static List<String>[] stringListArray;

    public void dummyMethod(final List list0, final List<Object> list1, final List<?> list2,
            final List<? super Object> list3, final List<String> list4, final List<? extends String> list5,
            final List<? super String> list6, final List[] list7, final List<Object>[] list8, final List<?>[] list9,
            final List<? super Object>[] list10, final List<String>[] list11, final List<? extends String>[] list12,
            final List<? super String>[] list13) {
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
        final Class<?> clazz = getClass();
        final Method method = clazz.getMethod("dummyMethod", List.class, List.class, List.class,
                List.class, List.class, List.class, List.class, List[].class, List[].class,
                List[].class, List[].class, List[].class, List[].class, List[].class);
        final Type[] types = method.getGenericParameterTypes();
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
        final Type disType = getClass().getField("dis").getGenericType();
        // Reporter.log( ( ( ParameterizedType ) disType
        // ).getOwnerType().getClass().toString() );
        final Type datType = getClass().getField("dat").getGenericType();
        final Type daType = getClass().getField("da").getGenericType();
        final Type uhderType = getClass().getField("uhder").getGenericType();
        final Type dingType = getClass().getField("ding").getGenericType();
        final Type testerType = getClass().getField("tester").getGenericType();
        final Type tester2Type = getClass().getField("tester2").getGenericType();
        final Type dat2Type = getClass().getField("dat2").getGenericType();
        final Type dat3Type = getClass().getField("dat3").getGenericType();
        dis = dat;
        assertTrue(TypeUtils.isAssignable(datType, disType));
        // dis = da;
        assertFalse(TypeUtils.isAssignable(daType, disType));
        dis = uhder;
        assertTrue(TypeUtils.isAssignable(uhderType, disType));
        dis = ding;
        assertFalse(TypeUtils.isAssignable(dingType, disType),
                String.format("type %s not assignable to %s!", dingType, disType));
        dis = tester;
        assertTrue(TypeUtils.isAssignable(testerType, disType));
        // dis = tester2;
        assertFalse(TypeUtils.isAssignable(tester2Type, disType));
        // dat = dat2;
        assertFalse(TypeUtils.isAssignable(dat2Type, datType));
        // dat2 = dat;
        assertFalse(TypeUtils.isAssignable(datType, dat2Type));
        // dat = dat3;
        assertFalse(TypeUtils.isAssignable(dat3Type, datType));
        final char ch = 0;
        final boolean bo = false;
        final byte by = 0;
        final short sh = 0;
        int in = 0;
        long lo = 0;
        final float fl = 0;
        double du = 0;
        du = ch;
        assertTrue(TypeUtils.isAssignable(char.class, double.class));
        du = by;
        assertTrue(TypeUtils.isAssignable(byte.class, double.class));
        du = sh;
        assertTrue(TypeUtils.isAssignable(short.class, double.class));
        du = in;
        assertTrue(TypeUtils.isAssignable(int.class, double.class));
        du = lo;
        assertTrue(TypeUtils.isAssignable(long.class, double.class));
        du = fl;
        assertTrue(TypeUtils.isAssignable(float.class, double.class));
        lo = in;
        assertTrue(TypeUtils.isAssignable(int.class, long.class));
        lo = Integer.valueOf(0);
        assertTrue(TypeUtils.isAssignable(Integer.class, long.class));
        // Long lngW = 1;
        assertFalse(TypeUtils.isAssignable(int.class, Long.class));
        // lngW = Integer.valueOf( 0 );
        assertFalse(TypeUtils.isAssignable(Integer.class, Long.class));
        in = Integer.valueOf(0);
        assertTrue(TypeUtils.isAssignable(Integer.class, int.class));
        final Integer inte = in;
        assertTrue(TypeUtils.isAssignable(int.class, Integer.class));
        assertTrue(TypeUtils.isAssignable(int.class, Number.class));
        assertTrue(TypeUtils.isAssignable(int.class, Object.class));
        final Type intComparableType = getClass().getField("intComparable").getGenericType();
        intComparable = 1;
        assertTrue(TypeUtils.isAssignable(int.class, intComparableType));
        assertTrue(TypeUtils.isAssignable(int.class, Comparable.class));
        final Serializable ser = 1;
        assertTrue(TypeUtils.isAssignable(int.class, Serializable.class));
        final Type longComparableType = getClass().getField("longComparable").getGenericType();
        // longComparable = 1;
        assertFalse(TypeUtils.isAssignable(int.class, longComparableType));
        // longComparable = Integer.valueOf( 0 );
        assertFalse(TypeUtils.isAssignable(Integer.class, longComparableType));
        // int[] ia;
        // long[] la = ia;
        assertFalse(TypeUtils.isAssignable(int[].class, long[].class));
        final Integer[] ia = null;
        final Type caType = getClass().getField("intWildcardComparable").getGenericType();
        intWildcardComparable = ia;
        assertTrue(TypeUtils.isAssignable(Integer[].class, caType));
        // int[] ina = ia;
        assertFalse(TypeUtils.isAssignable(Integer[].class, int[].class));
        final int[] ina = null;
        Object[] oa;
        // oa = ina;
        assertFalse(TypeUtils.isAssignable(int[].class, Object[].class));
        oa = new Integer[0];
        assertTrue(TypeUtils.isAssignable(Integer[].class, Object[].class));
        final Type bClassType = AClass.class.getField("bClass").getGenericType();
        final Type cClassType = AClass.class.getField("cClass").getGenericType();
        final Type dClassType = AClass.class.getField("dClass").getGenericType();
        final Type eClassType = AClass.class.getField("eClass").getGenericType();
        final Type fClassType = AClass.class.getField("fClass").getGenericType();
        final AClass aClass = new AClass(new AAClass<>());
        aClass.bClass = aClass.cClass;
        assertTrue(TypeUtils.isAssignable(cClassType, bClassType));
        aClass.bClass = aClass.dClass;
        assertTrue(TypeUtils.isAssignable(dClassType, bClassType));
        aClass.bClass = aClass.eClass;
        assertTrue(TypeUtils.isAssignable(eClassType, bClassType));
        aClass.bClass = aClass.fClass;
        assertTrue(TypeUtils.isAssignable(fClassType, bClassType));
        aClass.cClass = aClass.dClass;
        assertTrue(TypeUtils.isAssignable(dClassType, cClassType));
        aClass.cClass = aClass.eClass;
        assertTrue(TypeUtils.isAssignable(eClassType, cClassType));
        aClass.cClass = aClass.fClass;
        assertTrue(TypeUtils.isAssignable(fClassType, cClassType));
        aClass.dClass = aClass.eClass;
        assertTrue(TypeUtils.isAssignable(eClassType, dClassType));
        aClass.dClass = aClass.fClass;
        assertTrue(TypeUtils.isAssignable(fClassType, dClassType));
        aClass.eClass = aClass.fClass;
        assertTrue(TypeUtils.isAssignable(fClassType, eClassType));
    }

    public void delegateBooleanAssertion(final Type[] types, final int i2, final int i1, final boolean expected) {
        final Type type1 = types[i1];
        final Type type2 = types[i2];
        final boolean isAssignable = TypeUtils.isAssignable(type2, type1);

        if (expected) {
            assertTrue(isAssignable,
                    "[" + i1 + ", " + i2 + "]: From "
                                + String.valueOf(type2) + " to "
                                + String.valueOf(type1));
        } else {
            assertFalse(isAssignable,
                    "[" + i1 + ", " + i2 + "]: From "
                                + String.valueOf(type2) + " to "
                                + String.valueOf(type1));
        }
    }

    @SuppressWarnings("boxing") // boxing is deliberate here
    @Test
    public void testIsInstance() throws SecurityException, NoSuchFieldException {
        final Type intComparableType = getClass().getField("intComparable").getGenericType();
        final Type uriComparableType = getClass().getField("uriComparable").getGenericType();
        intComparable = 1;
        assertTrue(TypeUtils.isInstance(1, intComparableType));
        // uriComparable = 1;
        assertFalse(TypeUtils.isInstance(1, uriComparableType));
    }

    @Test
    public void testGetTypeArguments() {
        Map<TypeVariable<?>, Type> typeVarAssigns;
        TypeVariable<?> treeSetTypeVar;
        Type typeArg;

        typeVarAssigns = TypeUtils.getTypeArguments(Integer.class, Comparable.class);
        treeSetTypeVar = Comparable.class.getTypeParameters()[0];
        assertTrue(typeVarAssigns.containsKey(treeSetTypeVar),
                "Type var assigns for Comparable from Integer: " + typeVarAssigns);
        typeArg = typeVarAssigns.get(treeSetTypeVar);
        assertEquals(Integer.class, typeVarAssigns.get(treeSetTypeVar),
                "Type argument of Comparable from Integer: " + typeArg);

        typeVarAssigns = TypeUtils.getTypeArguments(int.class, Comparable.class);
        treeSetTypeVar = Comparable.class.getTypeParameters()[0];
        assertTrue(typeVarAssigns.containsKey(treeSetTypeVar),
                "Type var assigns for Comparable from int: " + typeVarAssigns);
        typeArg = typeVarAssigns.get(treeSetTypeVar);
        assertEquals(Integer.class, typeVarAssigns.get(treeSetTypeVar),
                "Type argument of Comparable from int: " + typeArg);

        final Collection<Integer> col = Arrays.asList(new Integer[0]);
        typeVarAssigns = TypeUtils.getTypeArguments(List.class, Collection.class);
        treeSetTypeVar = Comparable.class.getTypeParameters()[0];
        assertFalse(typeVarAssigns.containsKey(treeSetTypeVar),
                "Type var assigns for Collection from List: " + typeVarAssigns);

        typeVarAssigns = TypeUtils.getTypeArguments(AAAClass.BBBClass.class, AAClass.BBClass.class);
        assertEquals(2, typeVarAssigns.size());
        assertEquals(String.class, typeVarAssigns.get(AAClass.class.getTypeParameters()[0]));
        assertEquals(String.class, typeVarAssigns.get(AAClass.BBClass.class.getTypeParameters()[0]));

        typeVarAssigns = TypeUtils.getTypeArguments(Other.class, This.class);
        assertEquals(2, typeVarAssigns.size());
        assertEquals(String.class, typeVarAssigns.get(This.class.getTypeParameters()[0]));
        assertEquals(Other.class.getTypeParameters()[0], typeVarAssigns.get(This.class.getTypeParameters()[1]));

        typeVarAssigns = TypeUtils.getTypeArguments(And.class, This.class);
        assertEquals(2, typeVarAssigns.size());
        assertEquals(Number.class, typeVarAssigns.get(This.class.getTypeParameters()[0]));
        assertEquals(Number.class, typeVarAssigns.get(This.class.getTypeParameters()[1]));

        typeVarAssigns = TypeUtils.getTypeArguments(Thing.class, Other.class);
        assertEquals(2, typeVarAssigns.size());
        assertEquals(getClass().getTypeParameters()[0], typeVarAssigns.get(getClass().getTypeParameters()[0]));
        assertEquals(getClass().getTypeParameters()[0], typeVarAssigns.get(Other.class.getTypeParameters()[0]));
    }

    @Test
    public void testTypesSatisfyVariables() throws SecurityException,
            NoSuchMethodException {
        final Map<TypeVariable<?>, Type> typeVarAssigns = new HashMap<>();
        final Integer max = TypeUtilsTest.<Integer> stub();
        typeVarAssigns.put(getClass().getMethod("stub").getTypeParameters()[0], Integer.class);
        assertTrue(TypeUtils.typesSatisfyVariables(typeVarAssigns));
        typeVarAssigns.clear();
        typeVarAssigns.put(getClass().getMethod("stub2").getTypeParameters()[0], Integer.class);
        assertTrue(TypeUtils.typesSatisfyVariables(typeVarAssigns));
        typeVarAssigns.clear();
        typeVarAssigns.put(getClass().getMethod("stub3").getTypeParameters()[0], Integer.class);
        assertTrue(TypeUtils.typesSatisfyVariables(typeVarAssigns));
    }

    @Test
    public void testDetermineTypeVariableAssignments() throws SecurityException,
            NoSuchFieldException {
        final ParameterizedType iterableType = (ParameterizedType) getClass().getField("iterable")
                .getGenericType();
        final Map<TypeVariable<?>, Type> typeVarAssigns = TypeUtils.determineTypeArguments(TreeSet.class,
                iterableType);
        final TypeVariable<?> treeSetTypeVar = TreeSet.class.getTypeParameters()[0];
        assertTrue(typeVarAssigns.containsKey(treeSetTypeVar));
        assertEquals(iterableType.getActualTypeArguments()[0], typeVarAssigns
                .get(treeSetTypeVar));
    }

    @Test
    public void testGetRawType() throws SecurityException, NoSuchFieldException {
        final Type stringParentFieldType = GenericTypeHolder.class.getDeclaredField("stringParent")
                .getGenericType();
        final Type integerParentFieldType = GenericTypeHolder.class.getDeclaredField("integerParent")
                .getGenericType();
        final Type foosFieldType = GenericTypeHolder.class.getDeclaredField("foos").getGenericType();
        final Type genericParentT = GenericParent.class.getTypeParameters()[0];
        assertEquals(GenericParent.class, TypeUtils.getRawType(stringParentFieldType, null));
        assertEquals(GenericParent.class, TypeUtils.getRawType(integerParentFieldType,
                        null));
        assertEquals(List.class, TypeUtils.getRawType(foosFieldType, null));
        assertEquals(String.class, TypeUtils.getRawType(genericParentT,
                StringParameterizedChild.class));
        assertEquals(String.class, TypeUtils.getRawType(genericParentT,
                stringParentFieldType));
        assertEquals(Foo.class, TypeUtils.getRawType(Iterable.class.getTypeParameters()[0],
                foosFieldType));
        assertEquals(Foo.class, TypeUtils.getRawType(List.class.getTypeParameters()[0],
                foosFieldType));
        assertNull(TypeUtils.getRawType(genericParentT, GenericParent.class));
        assertEquals(GenericParent[].class, TypeUtils.getRawType(GenericTypeHolder.class
                .getDeclaredField("barParents").getGenericType(), null));
    }

    @Test
    public void testIsArrayTypeClasses() {
        assertTrue(TypeUtils.isArrayType(boolean[].class));
        assertTrue(TypeUtils.isArrayType(byte[].class));
        assertTrue(TypeUtils.isArrayType(short[].class));
        assertTrue(TypeUtils.isArrayType(int[].class));
        assertTrue(TypeUtils.isArrayType(char[].class));
        assertTrue(TypeUtils.isArrayType(long[].class));
        assertTrue(TypeUtils.isArrayType(float[].class));
        assertTrue(TypeUtils.isArrayType(double[].class));
        assertTrue(TypeUtils.isArrayType(Object[].class));
        assertTrue(TypeUtils.isArrayType(String[].class));

        assertFalse(TypeUtils.isArrayType(boolean.class));
        assertFalse(TypeUtils.isArrayType(byte.class));
        assertFalse(TypeUtils.isArrayType(short.class));
        assertFalse(TypeUtils.isArrayType(int.class));
        assertFalse(TypeUtils.isArrayType(char.class));
        assertFalse(TypeUtils.isArrayType(long.class));
        assertFalse(TypeUtils.isArrayType(float.class));
        assertFalse(TypeUtils.isArrayType(double.class));
        assertFalse(TypeUtils.isArrayType(Object.class));
        assertFalse(TypeUtils.isArrayType(String.class));
    }

    @Test
    public void testIsArrayGenericTypes() throws Exception {
        final Method method = getClass().getMethod("dummyMethod", List.class, List.class, List.class,
                List.class, List.class, List.class, List.class, List[].class, List[].class,
                List[].class, List[].class, List[].class, List[].class, List[].class);

        final Type[] types = method.getGenericParameterTypes();

        assertFalse(TypeUtils.isArrayType(types[0]));
        assertFalse(TypeUtils.isArrayType(types[1]));
        assertFalse(TypeUtils.isArrayType(types[2]));
        assertFalse(TypeUtils.isArrayType(types[3]));
        assertFalse(TypeUtils.isArrayType(types[4]));
        assertFalse(TypeUtils.isArrayType(types[5]));
        assertFalse(TypeUtils.isArrayType(types[6]));
        assertTrue(TypeUtils.isArrayType(types[7]));
        assertTrue(TypeUtils.isArrayType(types[8]));
        assertTrue(TypeUtils.isArrayType(types[9]));
        assertTrue(TypeUtils.isArrayType(types[10]));
        assertTrue(TypeUtils.isArrayType(types[11]));
        assertTrue(TypeUtils.isArrayType(types[12]));
        assertTrue(TypeUtils.isArrayType(types[13]));
    }

    @Test
    public void testGetPrimitiveArrayComponentType() {
        assertEquals(boolean.class, TypeUtils.getArrayComponentType(boolean[].class));
        assertEquals(byte.class, TypeUtils.getArrayComponentType(byte[].class));
        assertEquals(short.class, TypeUtils.getArrayComponentType(short[].class));
        assertEquals(int.class, TypeUtils.getArrayComponentType(int[].class));
        assertEquals(char.class, TypeUtils.getArrayComponentType(char[].class));
        assertEquals(long.class, TypeUtils.getArrayComponentType(long[].class));
        assertEquals(float.class, TypeUtils.getArrayComponentType(float[].class));
        assertEquals(double.class, TypeUtils.getArrayComponentType(double[].class));

        assertNull(TypeUtils.getArrayComponentType(boolean.class));
        assertNull(TypeUtils.getArrayComponentType(byte.class));
        assertNull(TypeUtils.getArrayComponentType(short.class));
        assertNull(TypeUtils.getArrayComponentType(int.class));
        assertNull(TypeUtils.getArrayComponentType(char.class));
        assertNull(TypeUtils.getArrayComponentType(long.class));
        assertNull(TypeUtils.getArrayComponentType(float.class));
        assertNull(TypeUtils.getArrayComponentType(double.class));
    }

    @Test
    public void testGetArrayComponentType() throws Exception {
        final Method method = getClass().getMethod("dummyMethod", List.class, List.class, List.class,
                List.class, List.class, List.class, List.class, List[].class, List[].class,
                List[].class, List[].class, List[].class, List[].class, List[].class);

        final Type[] types = method.getGenericParameterTypes();

        assertNull(TypeUtils.getArrayComponentType(types[0]));
        assertNull(TypeUtils.getArrayComponentType(types[1]));
        assertNull(TypeUtils.getArrayComponentType(types[2]));
        assertNull(TypeUtils.getArrayComponentType(types[3]));
        assertNull(TypeUtils.getArrayComponentType(types[4]));
        assertNull(TypeUtils.getArrayComponentType(types[5]));
        assertNull(TypeUtils.getArrayComponentType(types[6]));
        assertEquals(types[0], TypeUtils.getArrayComponentType(types[7]));
        assertEquals(types[1], TypeUtils.getArrayComponentType(types[8]));
        assertEquals(types[2], TypeUtils.getArrayComponentType(types[9]));
        assertEquals(types[3], TypeUtils.getArrayComponentType(types[10]));
        assertEquals(types[4], TypeUtils.getArrayComponentType(types[11]));
        assertEquals(types[5], TypeUtils.getArrayComponentType(types[12]));
        assertEquals(types[6], TypeUtils.getArrayComponentType(types[13]));
    }

    @Test
    public void testLang820() {
        final Type[] typeArray = {String.class, String.class};
        final Type[] expectedArray = {String.class};
        assertArrayEquals(expectedArray, TypeUtils.normalizeUpperBounds(typeArray));
    }

    @Test
    public void testParameterize() throws Exception {
        final ParameterizedType stringComparableType = TypeUtils.parameterize(Comparable.class, String.class);
        assertTrue(TypeUtils.equals(getClass().getField("stringComparable").getGenericType(),
            stringComparableType));
        assertEquals("java.lang.Comparable<java.lang.String>", stringComparableType.toString());
    }

    @Test
    public void testParameterizeNarrowerTypeArray() {
        final TypeVariable<?>[] variables = ArrayList.class.getTypeParameters();
        final ParameterizedType parameterizedType = TypeUtils.parameterize(ArrayList.class, variables);
        final Map<TypeVariable<?>, Type> mapping = Collections.<TypeVariable<?>, Type>singletonMap(variables[0], String.class);
        final Type unrolled = TypeUtils.unrollVariables(mapping, parameterizedType);
        assertEquals(TypeUtils.parameterize(ArrayList.class, String.class), unrolled);
    }

    @Test
    public void testParameterizeWithOwner() throws Exception {
        final Type owner = TypeUtils.parameterize(TypeUtilsTest.class, String.class);
        final ParameterizedType dat2Type = TypeUtils.parameterizeWithOwner(owner, That.class, String.class, String.class);
        assertTrue(TypeUtils.equals(getClass().getField("dat2").getGenericType(), dat2Type));
    }

    @Test
    public void testWildcardType() throws Exception {
        final WildcardType simpleWildcard = TypeUtils.wildcardType().withUpperBounds(String.class).build();
        final Field cClass = AClass.class.getField("cClass");
        assertTrue(TypeUtils.equals(((ParameterizedType) cClass.getGenericType()).getActualTypeArguments()[0],
            simpleWildcard));
        assertEquals(String.format("? extends %s", String.class.getName()), TypeUtils.toString(simpleWildcard));
        assertEquals(String.format("? extends %s", String.class.getName()), simpleWildcard.toString());
    }

    @Test
    public void testUnboundedWildcardType() {
        final WildcardType unbounded = TypeUtils.wildcardType().withLowerBounds((Type) null).withUpperBounds().build();
        assertTrue(TypeUtils.equals(TypeUtils.WILDCARD_ALL, unbounded));
        assertArrayEquals(new Type[] { Object.class }, TypeUtils.getImplicitUpperBounds(unbounded));
        assertArrayEquals(new Type[] { null }, TypeUtils.getImplicitLowerBounds(unbounded));
        assertEquals("?", TypeUtils.toString(unbounded));
        assertEquals("?", unbounded.toString());
    }

    @Test
    public void testLowerBoundedWildcardType() {
       final WildcardType lowerBounded = TypeUtils.wildcardType().withLowerBounds(java.sql.Date.class).build();
       assertEquals(String.format("? super %s", java.sql.Date.class.getName()), TypeUtils.toString(lowerBounded));
       assertEquals(String.format("? super %s", java.sql.Date.class.getName()), lowerBounded.toString());

       final TypeVariable<Class<Iterable>> iterableT0 = Iterable.class.getTypeParameters()[0];
       final WildcardType lowerTypeVariable = TypeUtils.wildcardType().withLowerBounds(iterableT0).build();
       assertEquals(String.format("? super %s", iterableT0.getName()), TypeUtils.toString(lowerTypeVariable));
       assertEquals(String.format("? super %s", iterableT0.getName()), lowerTypeVariable.toString());
    }

    @Test
    public void testLang1114() throws Exception {
        final Type nonWildcardType = getClass().getDeclaredField("wildcardComparable").getGenericType();
        final Type wildcardType = ((ParameterizedType)nonWildcardType).getActualTypeArguments()[0];

        assertFalse(TypeUtils.equals(wildcardType, nonWildcardType));
        assertFalse(TypeUtils.equals(nonWildcardType, wildcardType));
    }

    @Test
    public void testGenericArrayType() throws Exception {
        final Type expected = getClass().getField("intWildcardComparable").getGenericType();
        final GenericArrayType actual =
            TypeUtils.genericArrayType(TypeUtils.parameterize(Comparable.class, TypeUtils.wildcardType()
                .withUpperBounds(Integer.class).build()));
        assertTrue(TypeUtils.equals(expected, actual));
        assertEquals("java.lang.Comparable<? extends java.lang.Integer>[]", actual.toString());
    }

    @Test
    public void testToStringLang1311() {
        assertEquals("int[]", TypeUtils.toString(int[].class));
        assertEquals("java.lang.Integer[]", TypeUtils.toString(Integer[].class));
        final Field stringListField = FieldUtils.getDeclaredField(getClass(), "stringListArray");
        assertEquals("java.util.List<java.lang.String>[]", TypeUtils.toString(stringListField.getGenericType()));
    }

    @Test
    public void testToLongString() {
        assertEquals(getClass().getName() + ":B", TypeUtils.toLongString(getClass().getTypeParameters()[0]));
    }

    @Test
    public void testWrap() {
        final Type t = getClass().getTypeParameters()[0];
        assertTrue(TypeUtils.equals(t, TypeUtils.wrap(t).getType()));

        assertEquals(String.class, TypeUtils.wrap(String.class).getType());
    }

    public static class ClassWithSuperClassWithGenericType extends ArrayList<Object> {
        private static final long serialVersionUID = 1L;

        public static <U> Iterable<U> methodWithGenericReturnType() {
            return null;
        }
    }

    @Test
    public void testLANG1190() throws Exception {
        final Type fromType = ClassWithSuperClassWithGenericType.class.getDeclaredMethod("methodWithGenericReturnType").getGenericReturnType();
        final Type failingToType = TypeUtils.wildcardType().withLowerBounds(ClassWithSuperClassWithGenericType.class).build();

        assertTrue(TypeUtils.isAssignable(fromType, failingToType));
    }

    @Test
    public void testLANG1348() throws Exception {
        final Method method = Enum.class.getMethod("valueOf", Class.class, String.class);
        assertEquals("T extends java.lang.Enum<T>", TypeUtils.toString(method.getGenericReturnType()));
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

    AClass(final AAClass<String> enclosingInstance) {
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

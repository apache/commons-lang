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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

import org.apache.commons.lang3.reflect.testbed.Foo;
import org.apache.commons.lang3.reflect.testbed.GenericParent;
import org.apache.commons.lang3.reflect.testbed.GenericTypeHolder;
import org.apache.commons.lang3.reflect.testbed.StringParameterizedChild;
import org.junit.Assert;
import org.junit.Test;

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
        final char ch = 0;
        final boolean bo = false;
        final byte by = 0;
        final short sh = 0;
        int in = 0;
        long lo = 0;
        final float fl = 0;
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
        final Integer inte = in;
        Assert.assertTrue(TypeUtils.isAssignable(int.class, Integer.class));
        Assert.assertTrue(TypeUtils.isAssignable(int.class, Number.class));
        Assert.assertTrue(TypeUtils.isAssignable(int.class, Object.class));
        final Type intComparableType = getClass().getField("intComparable").getGenericType();
        intComparable = 1;
        Assert.assertTrue(TypeUtils.isAssignable(int.class, intComparableType));
        Assert.assertTrue(TypeUtils.isAssignable(int.class, Comparable.class));
        final Serializable ser = 1;
        Assert.assertTrue(TypeUtils.isAssignable(int.class, Serializable.class));
        final Type longComparableType = getClass().getField("longComparable").getGenericType();
        // longComparable = 1;
        Assert.assertFalse(TypeUtils.isAssignable(int.class, longComparableType));
        // longComparable = Integer.valueOf( 0 );
        Assert.assertFalse(TypeUtils.isAssignable(Integer.class, longComparableType));
        // int[] ia;
        // long[] la = ia;
        Assert.assertFalse(TypeUtils.isAssignable(int[].class, long[].class));
        final Integer[] ia = null;
        final Type caType = getClass().getField("intWildcardComparable").getGenericType();
        intWildcardComparable = ia;
        Assert.assertTrue(TypeUtils.isAssignable(Integer[].class, caType));
        // int[] ina = ia;
        Assert.assertFalse(TypeUtils.isAssignable(Integer[].class, int[].class));
        final int[] ina = null;
        Object[] oa;
        // oa = ina;
        Assert.assertFalse(TypeUtils.isAssignable(int[].class, Object[].class));
        oa = new Integer[0];
        Assert.assertTrue(TypeUtils.isAssignable(Integer[].class, Object[].class));
        final Type bClassType = AClass.class.getField("bClass").getGenericType();
        final Type cClassType = AClass.class.getField("cClass").getGenericType();
        final Type dClassType = AClass.class.getField("dClass").getGenericType();
        final Type eClassType = AClass.class.getField("eClass").getGenericType();
        final Type fClassType = AClass.class.getField("fClass").getGenericType();
        final AClass aClass = new AClass(new AAClass<String>());
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

    public void delegateBooleanAssertion(final Type[] types, final int i2, final int i1, final boolean expected) {
        final Type type1 = types[i1];
        final Type type2 = types[i2];
        final boolean isAssignable = TypeUtils.isAssignable(type2, type1);

        if (expected) {
            Assert.assertTrue("[" + i1 + ", " + i2 + "]: From "
                    + String.valueOf(type2) + " to "
                    + String.valueOf(type1), isAssignable);
        } else {
            Assert.assertFalse("[" + i1 + ", " + i2 + "]: From "
                    + String.valueOf(type2) + " to "
                    + String.valueOf(type1), isAssignable);
        }
    }

    @SuppressWarnings("boxing") // boxing is deliberate here
    @Test
    public void testIsInstance() throws SecurityException, NoSuchFieldException {
        final Type intComparableType = getClass().getField("intComparable").getGenericType();
        final Type uriComparableType = getClass().getField("uriComparable").getGenericType();
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

        final Collection<Integer> col = Arrays.asList(new Integer[0]);
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

        typeVarAssigns = TypeUtils.getTypeArguments(And.class, This.class);
        Assert.assertEquals(2, typeVarAssigns.size());
        Assert.assertEquals(Number.class, typeVarAssigns.get(This.class.getTypeParameters()[0]));
        Assert.assertEquals(Number.class, typeVarAssigns.get(This.class.getTypeParameters()[1]));

        typeVarAssigns = TypeUtils.getTypeArguments(Thing.class, Other.class);
        Assert.assertEquals(2, typeVarAssigns.size());
        Assert.assertEquals(getClass().getTypeParameters()[0], typeVarAssigns.get(getClass().getTypeParameters()[0]));
        Assert.assertEquals(getClass().getTypeParameters()[0], typeVarAssigns.get(Other.class.getTypeParameters()[0]));
    }

    @Test
    public void testTypesSatisfyVariables() throws SecurityException, NoSuchFieldException,
            NoSuchMethodException {
        final Map<TypeVariable<?>, Type> typeVarAssigns = new HashMap<>();
        final Integer max = TypeUtilsTest.<Integer> stub();
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
        final ParameterizedType iterableType = (ParameterizedType) getClass().getField("iterable")
                .getGenericType();
        final Map<TypeVariable<?>, Type> typeVarAssigns = TypeUtils.determineTypeArguments(TreeSet.class,
                iterableType);
        final TypeVariable<?> treeSetTypeVar = TreeSet.class.getTypeParameters()[0];
        Assert.assertTrue(typeVarAssigns.containsKey(treeSetTypeVar));
        Assert.assertEquals(iterableType.getActualTypeArguments()[0], typeVarAssigns
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
        final Method method = getClass().getMethod("dummyMethod", List.class, List.class, List.class,
                List.class, List.class, List.class, List.class, List[].class, List[].class,
                List[].class, List[].class, List[].class, List[].class, List[].class);

        final Type[] types = method.getGenericParameterTypes();

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
        final Method method = getClass().getMethod("dummyMethod", List.class, List.class, List.class,
                List.class, List.class, List.class, List.class, List[].class, List[].class,
                List[].class, List[].class, List[].class, List[].class, List[].class);

        final Type[] types = method.getGenericParameterTypes();

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
        final Type[] typeArray = {String.class, String.class};
        final Type[] expectedArray = {String.class};
        Assert.assertArrayEquals(expectedArray, TypeUtils.normalizeUpperBounds(typeArray));
    }

    @Test
    public void testParameterize() throws Exception {
        final ParameterizedType stringComparableType = TypeUtils.parameterize(Comparable.class, String.class);
        Assert.assertTrue(TypeUtils.equals(getClass().getField("stringComparable").getGenericType(),
            stringComparableType));
        Assert.assertEquals("java.lang.Comparable<java.lang.String>", stringComparableType.toString());
    }

    @Test
    public void testParameterizeWithOwner() throws Exception {
        final Type owner = TypeUtils.parameterize(TypeUtilsTest.class, String.class);
        final ParameterizedType dat2Type = TypeUtils.parameterizeWithOwner(owner, That.class, String.class, String.class);
        Assert.assertTrue(TypeUtils.equals(getClass().getField("dat2").getGenericType(), dat2Type));
    }

    @Test
    public void testWildcardType() throws Exception {
        final WildcardType simpleWildcard = TypeUtils.wildcardType().withUpperBounds(String.class).build();
        final Field cClass = AClass.class.getField("cClass");
        Assert.assertTrue(TypeUtils.equals(((ParameterizedType) cClass.getGenericType()).getActualTypeArguments()[0],
            simpleWildcard));
        Assert.assertEquals(String.format("? extends %s", String.class.getName()), TypeUtils.toString(simpleWildcard));
        Assert.assertEquals(String.format("? extends %s", String.class.getName()), simpleWildcard.toString());
    }

    @Test
    public void testUnboundedWildcardType() {
        final WildcardType unbounded = TypeUtils.wildcardType().withLowerBounds((Type) null).withUpperBounds().build();
        Assert.assertTrue(TypeUtils.equals(TypeUtils.WILDCARD_ALL, unbounded));
        Assert.assertArrayEquals(new Type[] { Object.class }, TypeUtils.getImplicitUpperBounds(unbounded));
        Assert.assertArrayEquals(new Type[] { null }, TypeUtils.getImplicitLowerBounds(unbounded));
        Assert.assertEquals("?", TypeUtils.toString(unbounded));
        Assert.assertEquals("?", unbounded.toString());
    }

    @Test
    public void testLowerBoundedWildcardType() {
       final WildcardType lowerBounded = TypeUtils.wildcardType().withLowerBounds(java.sql.Date.class).build();
       Assert.assertEquals(String.format("? super %s", java.sql.Date.class.getName()), TypeUtils.toString(lowerBounded));
       Assert.assertEquals(String.format("? super %s", java.sql.Date.class.getName()), lowerBounded.toString());

       final TypeVariable<Class<Iterable>> iterableT0 = Iterable.class.getTypeParameters()[0];
       final WildcardType lowerTypeVariable = TypeUtils.wildcardType().withLowerBounds(iterableT0).build();
       Assert.assertEquals(String.format("? super %s", iterableT0.getName()), TypeUtils.toString(lowerTypeVariable));
       Assert.assertEquals(String.format("? super %s", iterableT0.getName()), lowerTypeVariable.toString());
    }

    @Test
    public void testLang1114() throws Exception {
        final Type nonWildcardType = getClass().getDeclaredField("wildcardComparable").getGenericType();
        final Type wildcardType = ((ParameterizedType)nonWildcardType).getActualTypeArguments()[0];

        Assert.assertFalse(TypeUtils.equals(wildcardType, nonWildcardType));
        Assert.assertFalse(TypeUtils.equals(nonWildcardType, wildcardType));
    }

    @Test
    public void testGenericArrayType() throws Exception {
        final Type expected = getClass().getField("intWildcardComparable").getGenericType();
        final GenericArrayType actual =
            TypeUtils.genericArrayType(TypeUtils.parameterize(Comparable.class, TypeUtils.wildcardType()
                .withUpperBounds(Integer.class).build()));
        Assert.assertTrue(TypeUtils.equals(expected, actual));
        Assert.assertEquals("java.lang.Comparable<? extends java.lang.Integer>[]", actual.toString());
    }

    @Test
    public void testToStringLang1311() {
        Assert.assertEquals("int[]", TypeUtils.toString(int[].class));
        Assert.assertEquals("java.lang.Integer[]", TypeUtils.toString(Integer[].class));
        Field stringListField = FieldUtils.getDeclaredField(getClass(), "stringListArray");
        Assert.assertEquals("java.util.List<java.lang.String>[]", TypeUtils.toString(stringListField.getGenericType()));
    }

    @Test
    public void testToLongString() {
        Assert.assertEquals(getClass().getName() + ":B", TypeUtils.toLongString(getClass().getTypeParameters()[0]));
    }

    @Test
    public void testWrap() {
        final Type t = getClass().getTypeParameters()[0];
        Assert.assertTrue(TypeUtils.equals(t, TypeUtils.wrap(t).getType()));

        Assert.assertEquals(String.class, TypeUtils.wrap(String.class).getType());
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

        Assert.assertTrue(TypeUtils.isAssignable(fromType, failingToType));
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

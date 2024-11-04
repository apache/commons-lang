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
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.awt.Insets;
import java.io.Serializable;
import java.lang.reflect.Constructor;
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
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.TreeSet;
import java.util.stream.Stream;

import org.apache.commons.lang3.AbstractLangTest;
import org.apache.commons.lang3.reflect.testbed.Foo;
import org.apache.commons.lang3.reflect.testbed.GenericParent;
import org.apache.commons.lang3.reflect.testbed.GenericTypeHolder;
import org.apache.commons.lang3.reflect.testbed.StringParameterizedChild;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Test fixture for https://issues.apache.org/jira/browse/LANG-1524
 *
 * @param <T> Test fixture type, unused type parameter for test.
 */
class AAAAClass<T extends AAAAClass.BBBBClass.CCCClass> {
    public static class BBBBClass {
        public static class CCCClass {
            // empty
        }
    }
}

final class AAAClass extends AAClass<String> {
    public class BBBClass extends BBClass<String> {
        // empty
    }
}

/**
 * Test fixture.
 *
 * @param <T> Test fixture type, unused type parameter for test.
 */
class AAClass<T> {

    /**
     * Test fixture.
     *
     * @param <S> Test fixture type, unused type parameter for test.
     */
    public class BBClass<S> {
        // empty
    }
}

@SuppressWarnings("rawtypes")
//raw types, where used, are used purposely
final class AClass extends AAClass<String>.BBClass<Number> {

    @SuppressWarnings("unused") // Unused type parameter for test
    public interface AInterface<T> {
        // empty
    }

    @SuppressWarnings("unused") // Unused type parameter for test
    public class BClass<T> {
        // empty
    }

    @SuppressWarnings("unused") // Unused type parameter for test
    public class CClass<T> extends BClass {
        // empty
    }

    @SuppressWarnings("unused") // Unused type parameter for test
    public class DClass<T> extends CClass<T> {
        // empty
    }

    @SuppressWarnings("unused") // Unused type parameter for test
    public class EClass<T> extends DClass {
        // empty
    }

    public class FClass extends EClass<String> {
        // empty
    }

    public class GClass<T extends BClass<? extends T> & AInterface<AInterface<? super T>>> {
        // empty
    }

    public BClass<Number> bClass;

    public CClass<? extends String> cClass;

    public DClass<String> dClass;

    public EClass<String> eClass;

    public FClass fClass;

    public GClass gClass;

    AClass(final AAClass<String> enclosingInstance) {
        enclosingInstance.super();
    }
}
@SuppressWarnings("rawtypes")
abstract class Test1<G> {
    public abstract Object m0();
    public abstract String[] m1();
    public abstract <K, V> Map<? extends K, V[]> m10();
    public abstract <K, V> Map<? extends K, List<V[]>> m11();
    public abstract List m12();
    public abstract Map m13();
    public abstract Properties m14();
    public abstract G m15();
    public abstract List<G> m16();
    public abstract Enum m17();
    public abstract <E> E[] m2();
    public abstract <E> List<? extends E> m3();
    public abstract <E extends Enum<E>> List<? extends Enum<E>> m4();
    public abstract List<? extends Enum<?>> m5();
    public abstract List<? super Enum<?>> m6();
    public abstract List<?> m7();
    public abstract Map<? extends Enum<?>, ? super Enum<?>> m8();
    public abstract <K, V> Map<? extends K, ? super V[]> m9();
}

/**
 * Tests {@link TypeUtils}.
 *
 * @param <B> Type for test fixtures.
 */
@SuppressWarnings({ "unused", "rawtypes" })
// raw types, where used, are used purposely
public class TypeUtilsTest<B> extends AbstractLangTest {

    public interface And<K, V> extends This<Number, Number> {
        // empty
    }

    public static class ClassWithSuperClassWithGenericType extends ArrayList<Object> {
        private static final long serialVersionUID = 1L;

        public static <U> Iterable<U> methodWithGenericReturnType() {
            return null;
        }
    }

    /** This non-static inner class is parameterized. */
    private class MyInnerClass<T> {
        // empty
    }

    public class Other<T> implements This<String, T> {
        // empty
    }

    public class Tester implements This<String, B> {
        // empty
    }

    public class That<K, V> implements This<K, V> {
        // empty
    }

    public class The<K, V> extends That<Number, Number> implements And<String, String> {
        // empty
    }

    public class Thing<Q> extends Other<B> {
        // empty
    }

    public interface This<K, V> {
        // empty
    }

    public static Comparable<Integer> intComparable;

    public static Comparable<Long> longComparable;

    public static Comparable<String> stringComparable;

    public static List<String>[] stringListArray;

    public static URI uri;

    public static Comparable<URI> uriComparable;

    public static Comparable<?> wildcardComparable;

    public static <G extends Comparable<G>> G stub() {
        return null;
    }

    public static <G extends Comparable<? super G>> G stub2() {
        return null;
    }

    public static <T extends Comparable<? extends T>> T stub3() {
        return null;
    }

    static Stream<Type> testTypeToString() {
        // @formatter:off
        return Stream.of(Comparator.class, Comparable.class, ArrayList.class, HashMap.class)
                .flatMap(cls -> Stream.of(cls.getDeclaredMethods()))
                .flatMap(m ->
                    Stream.concat(Stream.of(m.getGenericExceptionTypes()),
                    Stream.concat(Stream.of(m.getGenericParameterTypes()),
                    Stream.concat(Stream.of(m.getGenericReturnType()), Stream.of(m.getTypeParameters())))));
        // @formatter:on
    }

    public The<String, String> da;

    public That<String, String> dat;

    public TypeUtilsTest<String>.That<String, String> dat2;

    public TypeUtilsTest<Number>.That<String, String> dat3;

    public Thing ding;

    public This<String, String> dis;

    public Comparable<? extends Integer>[] intWildcardComparable;

    public Iterable<? extends Map<Integer, ? extends Collection<?>>> iterable;

    public TypeUtilsTest<String>.Tester tester;

    public Tester tester2;

    public Other<String> uhder;

    /** The inner class is used as a return type from a method. */
    private <U> MyInnerClass<U> aMethod() {
        return null;
    }

    @Test
    public void test_LANG_1114() throws NoSuchFieldException {
        final Type nonWildcardType = getClass().getDeclaredField("wildcardComparable").getGenericType();
        final Type wildcardType = ((ParameterizedType) nonWildcardType).getActualTypeArguments()[0];

        assertFalse(TypeUtils.equals(wildcardType, nonWildcardType));
        assertFalse(TypeUtils.equals(nonWildcardType, wildcardType));
    }

    @Test
    public void test_LANG_1190() throws NoSuchMethodException {
        final Type fromType = ClassWithSuperClassWithGenericType.class.getDeclaredMethod("methodWithGenericReturnType").getGenericReturnType();
        final Type failingToType = TypeUtils.wildcardType().withLowerBounds(ClassWithSuperClassWithGenericType.class).build();

        assertTrue(TypeUtils.isAssignable(fromType, failingToType));
    }

    @Test
    public void test_LANG_1348() throws NoSuchMethodException {
        final Method method = Enum.class.getMethod("valueOf", Class.class, String.class);
        assertEquals("T extends java.lang.Enum<T>", TypeUtils.toString(method.getGenericReturnType()));
    }

    @Test
    public void test_LANG_1524() {
        assertEquals("AAAAClass(cycle).BBBBClass.CCCClass", TypeUtils.toString(AAAAClass.BBBBClass.CCCClass.class));
        assertEquals("AAAAClass(cycle).BBBBClass", TypeUtils.toString(AAAAClass.BBBBClass.class));
        assertEquals("AAAAClass(cycle)", TypeUtils.toString(AAAAClass.class));
    }

    /**
     * Tests https://issues.apache.org/jira/projects/LANG/issues/LANG-1698
     *
     * <pre>{@code
     * java.lang.StackOverflowError
    at org.apache.commons.lang3.reflect.TypeUtils.typeVariableToString(TypeUtils.java:1785)
    at org.apache.commons.lang3.reflect.TypeUtils.toString(TypeUtils.java:1737)
    at org.apache.commons.lang3.reflect.TypeUtils.toString(TypeUtils.java:1714)
    at org.apache.commons.lang3.reflect.TypeUtils.appendAllTo(TypeUtils.java:302)
    at org.apache.commons.lang3.reflect.TypeUtils.wildcardTypeToString(TypeUtils.java:1902)
    at org.apache.commons.lang3.reflect.TypeUtils.toString(TypeUtils.java:1734)
    at org.apache.commons.lang3.reflect.TypeUtils.toString(TypeUtils.java:1714)
    at org.apache.commons.lang3.reflect.TypeUtils.appendAllTo(TypeUtils.java:302)
    at org.apache.commons.lang3.reflect.TypeUtils.parameterizedTypeToString(TypeUtils.java:1604)
    at org.apache.commons.lang3.reflect.TypeUtils.toString(TypeUtils.java:1731)
    at org.apache.commons.lang3.reflect.TypeUtils.toString(TypeUtils.java:1714)
    at org.apache.commons.lang3.reflect.TypeUtils.appendAllTo(TypeUtils.java:302)
    at org.apache.commons.lang3.reflect.TypeUtils.typeVariableToString(TypeUtils.java:1789)
     * }
     * </pre>
     */
    @Test
    public void test_LANG_1698() {
        final ParameterizedType comparing = (ParameterizedType) Arrays.stream(Comparator.class.getDeclaredMethods())
                .filter(k -> k.getName().equals("comparing")).findFirst()
                .orElse(Comparator.class.getDeclaredMethods()[0]).getGenericParameterTypes()[0];
        final String typeName = TypeUtils
                .parameterize((Class<?>) comparing.getRawType(), comparing.getActualTypeArguments()).getTypeName();
        assertEquals("java.util.function.Function<? super T, ? extends U>", typeName);
    }

    @Test
    public void test_LANG_1702() throws NoSuchMethodException, SecurityException {
        final Type type = TypeUtilsTest.class.getDeclaredMethod("aMethod").getGenericReturnType();

        // any map will do
        final Map<TypeVariable<?>, Type> typeArguments = Collections.emptyMap();

        // this fails with a stack overflow
        final Type unrolledType = TypeUtils.unrollVariables(typeArguments, type);
    }

    @Test
    public void test_LANG_820() {
        final Type[] typeArray = {String.class, String.class};
        final Type[] expectedArray = {String.class};
        assertArrayEquals(expectedArray, TypeUtils.normalizeUpperBounds(typeArray));
    }

    @Test
    public void testContainsTypeVariables() throws NoSuchMethodException {
        assertFalse(TypeUtils.containsTypeVariables(Test1.class.getMethod("m0").getGenericReturnType()));
        assertFalse(TypeUtils.containsTypeVariables(Test1.class.getMethod("m1").getGenericReturnType()));
        assertTrue(TypeUtils.containsTypeVariables(Test1.class.getMethod("m2").getGenericReturnType()));
        assertTrue(TypeUtils.containsTypeVariables(Test1.class.getMethod("m3").getGenericReturnType()));
        assertTrue(TypeUtils.containsTypeVariables(Test1.class.getMethod("m4").getGenericReturnType()));
        assertFalse(TypeUtils.containsTypeVariables(Test1.class.getMethod("m5").getGenericReturnType()));
        assertFalse(TypeUtils.containsTypeVariables(Test1.class.getMethod("m6").getGenericReturnType()));
        assertFalse(TypeUtils.containsTypeVariables(Test1.class.getMethod("m7").getGenericReturnType()));
        assertFalse(TypeUtils.containsTypeVariables(Test1.class.getMethod("m8").getGenericReturnType()));
        assertTrue(TypeUtils.containsTypeVariables(Test1.class.getMethod("m9").getGenericReturnType()));
        assertTrue(TypeUtils.containsTypeVariables(Test1.class.getMethod("m10").getGenericReturnType()));
        assertTrue(TypeUtils.containsTypeVariables(Test1.class.getMethod("m11").getGenericReturnType()));
        assertTrue(TypeUtils.containsTypeVariables(Test1.class.getMethod("m12").getGenericReturnType()));
        assertTrue(TypeUtils.containsTypeVariables(Test1.class.getMethod("m13").getGenericReturnType()));
        assertFalse(TypeUtils.containsTypeVariables(Test1.class.getMethod("m14").getGenericReturnType()));
        assertTrue(TypeUtils.containsTypeVariables(Test1.class.getMethod("m15").getGenericReturnType()));
        assertTrue(TypeUtils.containsTypeVariables(Test1.class.getMethod("m16").getGenericReturnType()));
        assertTrue(TypeUtils.containsTypeVariables(Test1.class.getMethod("m17").getGenericReturnType()));
    }

    @Test
    public void testContainsTypeVariablesPr437() throws Exception {
        abstract class Test2<G> {
            public abstract Object m0();
            public abstract String[] m1();
            public abstract <K, V> Map<? extends K, V[]> m10();
            public abstract <K, V> Map<? extends K, List<V[]>> m11();
            public abstract List m12();
            public abstract Map m13();
            public abstract Properties m14();
            public abstract G m15();
            public abstract List<G> m16();
            public abstract Enum m17();
            public abstract <E> E[] m2();
            public abstract <E> List<? extends E> m3();
            public abstract <E extends Enum<E>> List<? extends Enum<E>> m4();
            public abstract List<? extends Enum<?>> m5();
            public abstract List<? super Enum<?>> m6();
            public abstract List<?> m7();
            public abstract Map<? extends Enum<?>, ? super Enum<?>> m8();
            public abstract <K, V> Map<? extends K, ? super V[]> m9();
        }

        assertFalse(TypeUtils.containsTypeVariables(Test2.class.getMethod("m0").getGenericReturnType()));
        assertFalse(TypeUtils.containsTypeVariables(Test2.class.getMethod("m1").getGenericReturnType()));
        assertTrue(TypeUtils.containsTypeVariables(Test2.class.getMethod("m2").getGenericReturnType()));
        assertTrue(TypeUtils.containsTypeVariables(Test2.class.getMethod("m3").getGenericReturnType()));
        assertTrue(TypeUtils.containsTypeVariables(Test2.class.getMethod("m4").getGenericReturnType()));
        assertFalse(TypeUtils.containsTypeVariables(Test2.class.getMethod("m5").getGenericReturnType()));
        assertFalse(TypeUtils.containsTypeVariables(Test2.class.getMethod("m6").getGenericReturnType()));
        assertFalse(TypeUtils.containsTypeVariables(Test2.class.getMethod("m7").getGenericReturnType()));
        assertFalse(TypeUtils.containsTypeVariables(Test2.class.getMethod("m8").getGenericReturnType()));
        assertTrue(TypeUtils.containsTypeVariables(Test2.class.getMethod("m9").getGenericReturnType()));
        assertTrue(TypeUtils.containsTypeVariables(Test2.class.getMethod("m10").getGenericReturnType()));
        assertTrue(TypeUtils.containsTypeVariables(Test2.class.getMethod("m11").getGenericReturnType()));
        assertTrue(TypeUtils.containsTypeVariables(Test2.class.getMethod("m12").getGenericReturnType()));
        assertTrue(TypeUtils.containsTypeVariables(Test2.class.getMethod("m13").getGenericReturnType()));
        assertFalse(TypeUtils.containsTypeVariables(Test2.class.getMethod("m14").getGenericReturnType()));
        assertTrue(TypeUtils.containsTypeVariables(Test2.class.getMethod("m15").getGenericReturnType()));
        assertTrue(TypeUtils.containsTypeVariables(Test2.class.getMethod("m16").getGenericReturnType()));
        assertTrue(TypeUtils.containsTypeVariables(Test2.class.getMethod("m17").getGenericReturnType()));
    }

    @Test
    public void testDetermineTypeVariableAssignments() throws NoSuchFieldException {
        final ParameterizedType iterableType = (ParameterizedType) getClass().getField("iterable")
                .getGenericType();
        final Map<TypeVariable<?>, Type> typeVarAssigns = TypeUtils.determineTypeArguments(TreeSet.class,
                iterableType);
        final TypeVariable<?> treeSetTypeVar = TreeSet.class.getTypeParameters()[0];
        assertTrue(typeVarAssigns.containsKey(treeSetTypeVar));
        assertEquals(iterableType.getActualTypeArguments()[0], typeVarAssigns
                .get(treeSetTypeVar));

        assertThrows(NullPointerException.class,
                () -> TypeUtils.determineTypeArguments(TreeSet.class, null));
        assertThrows(NullPointerException.class,
                () -> TypeUtils.determineTypeArguments(null, iterableType));
    }

    @Test
    public void testGenericArrayType() throws NoSuchFieldException {
        final Type expected = getClass().getField("intWildcardComparable").getGenericType();
        final GenericArrayType actual =
            TypeUtils.genericArrayType(TypeUtils.parameterize(Comparable.class, TypeUtils.wildcardType()
                .withUpperBounds(Integer.class).build()));
        assertTrue(TypeUtils.equals(expected, actual));
        assertEquals("java.lang.Comparable<? extends java.lang.Integer>[]", actual.toString());
    }

    @Test
    public void testGetArrayComponentType() throws NoSuchFieldException {
        final Type rawListType = GenericTypeHolder.class.getDeclaredField("rawList").getGenericType();
        final Type objectListType = GenericTypeHolder.class.getDeclaredField("objectList").getGenericType();
        final Type unboundListType = GenericTypeHolder.class.getDeclaredField("unboundList").getGenericType();
        final Type superObjectListType = GenericTypeHolder.class.getDeclaredField("superObjectList").getGenericType();
        final Type stringListType = GenericTypeHolder.class.getDeclaredField("stringList").getGenericType();
        final Type subStringListType = GenericTypeHolder.class.getDeclaredField("subStringList").getGenericType();
        final Type superStringListType = GenericTypeHolder.class.getDeclaredField("superStringList").getGenericType();

        assertNull(TypeUtils.getArrayComponentType(rawListType));
        assertNull(TypeUtils.getArrayComponentType(objectListType));
        assertNull(TypeUtils.getArrayComponentType(unboundListType));
        assertNull(TypeUtils.getArrayComponentType(superObjectListType));
        assertNull(TypeUtils.getArrayComponentType(stringListType));
        assertNull(TypeUtils.getArrayComponentType(subStringListType));
        assertNull(TypeUtils.getArrayComponentType(superStringListType));

        final Type rawListTypeArray = GenericTypeHolder.class.getDeclaredField("rawListArray").getGenericType();
        final Type objectListTypeArray = GenericTypeHolder.class.getDeclaredField("objectListArray").getGenericType();
        final Type unboundListTypeArray = GenericTypeHolder.class.getDeclaredField("unboundListArray").getGenericType();
        final Type superObjectListTypeArray = GenericTypeHolder.class.getDeclaredField("superObjectListArray").getGenericType();
        final Type stringListTypeArray = GenericTypeHolder.class.getDeclaredField("stringListArray").getGenericType();
        final Type subStringListTypeArray = GenericTypeHolder.class.getDeclaredField("subStringListArray").getGenericType();
        final Type superStringListTypeArray = GenericTypeHolder.class.getDeclaredField("superStringListArray").getGenericType();

        assertEquals(rawListType, TypeUtils.getArrayComponentType(rawListTypeArray));
        assertEquals(objectListType, TypeUtils.getArrayComponentType(objectListTypeArray));
        assertEquals(unboundListType, TypeUtils.getArrayComponentType(unboundListTypeArray));
        assertEquals(superObjectListType, TypeUtils.getArrayComponentType(superObjectListTypeArray));
        assertEquals(stringListType, TypeUtils.getArrayComponentType(stringListTypeArray));
        assertEquals(subStringListType, TypeUtils.getArrayComponentType(subStringListTypeArray));
        assertEquals(superStringListType, TypeUtils.getArrayComponentType(superStringListTypeArray));
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
    public void testGetRawType() throws NoSuchFieldException {
        final Type stringParentFieldType = GenericTypeHolder.class.getDeclaredField("stringParent").getGenericType();
        final Type integerParentFieldType = GenericTypeHolder.class.getDeclaredField("integerParent").getGenericType();
        final Type foosFieldType = GenericTypeHolder.class.getDeclaredField("foos").getGenericType();
        final Type genericParentT = GenericParent.class.getTypeParameters()[0];
        assertEquals(GenericParent.class, TypeUtils.getRawType(stringParentFieldType, null));
        assertEquals(GenericParent.class, TypeUtils.getRawType(integerParentFieldType, null));
        assertEquals(List.class, TypeUtils.getRawType(foosFieldType, null));
        assertEquals(String.class, TypeUtils.getRawType(genericParentT, StringParameterizedChild.class));
        assertEquals(String.class, TypeUtils.getRawType(genericParentT, stringParentFieldType));
        assertEquals(Foo.class, TypeUtils.getRawType(Iterable.class.getTypeParameters()[0], foosFieldType));
        assertEquals(Foo.class, TypeUtils.getRawType(List.class.getTypeParameters()[0], foosFieldType));
        assertNull(TypeUtils.getRawType(genericParentT, GenericParent.class));
        assertEquals(GenericParent[].class, TypeUtils.getRawType(GenericTypeHolder.class.getDeclaredField("barParents").getGenericType(), null));
    }

    /**
     * Tests https://issues.apache.org/jira/browse/LANG-1697
     */
    @Test
    public void testGetRawType_LANG_1697() {
        assertEquals(int[].class, TypeUtils.getRawType(TypeUtils.genericArrayType(Integer.TYPE), Integer.TYPE));
        // LANG-1697:
        assertNull(TypeUtils.getRawType(TypeUtils.genericArrayType(TypeUtils.WILDCARD_ALL), null));
        // TODO: Is this correct?
        assertNull(TypeUtils.getRawType(TypeUtils.genericArrayType(TypeUtils.WILDCARD_ALL), TypeUtils.WILDCARD_ALL));
        // TODO: Is this correct?
        assertNull(TypeUtils.getRawType(TypeUtils.genericArrayType(TypeUtils.WILDCARD_ALL), Integer.TYPE));
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

        final Collection<Integer> col = Collections.emptyList();
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
    public void testIsArrayGenericTypes() throws NoSuchFieldException {
        final Type rawListType = GenericTypeHolder.class.getDeclaredField("rawList").getGenericType();
        final Type objectListType = GenericTypeHolder.class.getDeclaredField("objectList").getGenericType();
        final Type unboundListType = GenericTypeHolder.class.getDeclaredField("unboundList").getGenericType();
        final Type superObjectListType = GenericTypeHolder.class.getDeclaredField("superObjectList").getGenericType();
        final Type stringListType = GenericTypeHolder.class.getDeclaredField("stringList").getGenericType();
        final Type subStringListType = GenericTypeHolder.class.getDeclaredField("subStringList").getGenericType();
        final Type superStringListType = GenericTypeHolder.class.getDeclaredField("superStringList").getGenericType();

        assertFalse(TypeUtils.isArrayType(rawListType));
        assertFalse(TypeUtils.isArrayType(objectListType));
        assertFalse(TypeUtils.isArrayType(unboundListType));
        assertFalse(TypeUtils.isArrayType(superObjectListType));
        assertFalse(TypeUtils.isArrayType(stringListType));
        assertFalse(TypeUtils.isArrayType(subStringListType));
        assertFalse(TypeUtils.isArrayType(superStringListType));

        final Type rawListTypeArray = GenericTypeHolder.class.getDeclaredField("rawListArray").getGenericType();
        final Type objectListTypeArray = GenericTypeHolder.class.getDeclaredField("objectListArray").getGenericType();
        final Type unboundListTypeArray = GenericTypeHolder.class.getDeclaredField("unboundListArray").getGenericType();
        final Type superObjectListTypeArray = GenericTypeHolder.class.getDeclaredField("superObjectListArray").getGenericType();
        final Type stringListTypeArray = GenericTypeHolder.class.getDeclaredField("stringListArray").getGenericType();
        final Type subStringListTypeArray = GenericTypeHolder.class.getDeclaredField("subStringListArray").getGenericType();
        final Type superStringListTypeArray = GenericTypeHolder.class.getDeclaredField("superStringListArray").getGenericType();

        assertTrue(TypeUtils.isArrayType(rawListTypeArray));
        assertTrue(TypeUtils.isArrayType(objectListTypeArray));
        assertTrue(TypeUtils.isArrayType(unboundListTypeArray));
        assertTrue(TypeUtils.isArrayType(superObjectListTypeArray));
        assertTrue(TypeUtils.isArrayType(stringListTypeArray));
        assertTrue(TypeUtils.isArrayType(subStringListTypeArray));
        assertTrue(TypeUtils.isArrayType(superStringListTypeArray));
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
    public void testIsAssignableClasses() {
        assertTrue(TypeUtils.isAssignable(char.class, double.class));
        assertTrue(TypeUtils.isAssignable(byte.class, double.class));
        assertTrue(TypeUtils.isAssignable(short.class, double.class));
        assertTrue(TypeUtils.isAssignable(int.class, double.class));
        assertTrue(TypeUtils.isAssignable(long.class, double.class));
        assertTrue(TypeUtils.isAssignable(float.class, double.class));

        assertTrue(TypeUtils.isAssignable(int.class, long.class));
        assertTrue(TypeUtils.isAssignable(Integer.class, long.class));
        assertFalse(TypeUtils.isAssignable(int.class, Long.class));
        assertFalse(TypeUtils.isAssignable(Integer.class, Long.class));
        assertTrue(TypeUtils.isAssignable(Integer.class, int.class));
        assertTrue(TypeUtils.isAssignable(int.class, Integer.class));
        assertTrue(TypeUtils.isAssignable(int.class, Number.class));
        assertTrue(TypeUtils.isAssignable(int.class, Object.class));
        assertTrue(TypeUtils.isAssignable(int.class, Comparable.class));
        assertTrue(TypeUtils.isAssignable(int.class, Serializable.class));

        assertFalse(TypeUtils.isAssignable(int[].class, long[].class));
        assertFalse(TypeUtils.isAssignable(Integer[].class, int[].class));
        assertFalse(TypeUtils.isAssignable(int[].class, Object[].class));
        assertTrue(TypeUtils.isAssignable(Integer[].class, Object[].class));
    }

    @Test
    public void testIsAssignableDirectClassHierarchy() throws NoSuchFieldException {
        final Type bClassType = AClass.class.getField("bClass").getGenericType(); // B is superclass
        final Type cClassType = AClass.class.getField("cClass").getGenericType(); // C subclass of B
        final Type dClassType = AClass.class.getField("dClass").getGenericType(); // D subclass of C
        final Type eClassType = AClass.class.getField("eClass").getGenericType(); // E subclass of D
        final Type fClassType = AClass.class.getField("fClass").getGenericType(); // F subclass of E

        assertTrue(TypeUtils.isAssignable(cClassType, bClassType));
        assertTrue(TypeUtils.isAssignable(dClassType, bClassType));
        assertTrue(TypeUtils.isAssignable(eClassType, bClassType));
        assertTrue(TypeUtils.isAssignable(fClassType, bClassType));

        assertTrue(TypeUtils.isAssignable(dClassType, cClassType));
        assertTrue(TypeUtils.isAssignable(eClassType, cClassType));
        assertTrue(TypeUtils.isAssignable(fClassType, cClassType));

        assertTrue(TypeUtils.isAssignable(eClassType, dClassType));
        assertTrue(TypeUtils.isAssignable(fClassType, dClassType));

        assertTrue(TypeUtils.isAssignable(fClassType, eClassType));
    }

    @Test
    public void testIsAssignableGenericArrayTypeToObject() {
        final Class<Constructor> rawClass = Constructor.class;
        final Class<Insets> typeArgClass = Insets.class;
        // Builds a ParameterizedType for Constructor<Insets>
        final ParameterizedType paramType = TypeUtils.parameterize(rawClass, typeArgClass);
        assertEquals(rawClass, paramType.getRawType());
        assertEquals(typeArgClass, paramType.getActualTypeArguments()[0]);

        assertTrue(Object.class.isAssignableFrom(paramType.getClass()));
        assertFalse(paramType.getClass().isAssignableFrom(Object.class));

        final Type testType = Object.class;
        assertTrue(TypeUtils.isAssignable(paramType, testType),
                () -> String.format("TypeUtils.isAssignable(%s, %s)", paramType, testType));
        assertFalse(TypeUtils.isAssignable(testType, paramType),
                () -> String.format("TypeUtils.isAssignable(%s, %s)", testType, paramType));
    }

    @Test
    public void testIsAssignableGenericArrayTypeToParameterizedType() {
        final Class<Constructor> rawClass = Constructor.class;
        final Class<Insets> typeArgClass = Insets.class;
        // Builds a ParameterizedType for Constructor<Insets>
        final ParameterizedType paramType = TypeUtils.parameterize(rawClass, typeArgClass);
        assertEquals(rawClass, paramType.getRawType());
        assertEquals(typeArgClass, paramType.getActualTypeArguments()[0]);

        assertFalse(GenericArrayType.class.isAssignableFrom(paramType.getClass()));
        assertFalse(paramType.getClass().isAssignableFrom(GenericArrayType.class));

        final GenericArrayType testType = TypeUtils.genericArrayType(paramType);
        assertFalse(TypeUtils.isAssignable(paramType, testType),
                () -> String.format("TypeUtils.isAssignable(%s, %s)", paramType, testType));
        assertFalse(TypeUtils.isAssignable(testType, paramType),
                () -> String.format("TypeUtils.isAssignable(%s, %s)", testType, paramType));
    }

    @Test
    @Disabled("TODO")
    public void testIsAssignableGenericArrayTypeToWildcardType() {
        final Class<Constructor> rawClass = Constructor.class;
        final Class<Insets> typeArgClass = Insets.class;
        // Builds a ParameterizedType for Constructor<Insets>
        final ParameterizedType paramType = TypeUtils.parameterize(rawClass, typeArgClass);
        assertEquals(rawClass, paramType.getRawType());
        assertEquals(typeArgClass, paramType.getActualTypeArguments()[0]);

        assertFalse(WildcardType.class.isAssignableFrom(paramType.getClass()));
        assertFalse(paramType.getClass().isAssignableFrom(WildcardType.class));

        final WildcardType testType = TypeUtils.WILDCARD_ALL;
        // TODO This test returns true unlike the test above.
        // Is this a bug in this test or in the main code?
        assertFalse(TypeUtils.isAssignable(paramType, testType),
                () -> String.format("TypeUtils.isAssignable(%s, %s)", paramType, testType));
        assertFalse(TypeUtils.isAssignable(testType, paramType),
                () -> String.format("TypeUtils.isAssignable(%s, %s)", testType, paramType));
    }

    @Test
    public void testIsAssignableGenericClassHierarchy() throws NoSuchFieldException {
        /*
         *            <<This>>
         *      /      /     \     \
         * <<And>>   That   Other   Tester
         *      \   /         |
         *       The        Thing
         */
        final Type disType = getClass().getField("dis").getGenericType();       // This is superinterface
        final Type datType = getClass().getField("dat").getGenericType();       // That implements This
        final Type dat2Type = getClass().getField("dat2").getGenericType();
        final Type dat3Type = getClass().getField("dat3").getGenericType();
        final Type daType = getClass().getField("da").getGenericType();         // The extends That and implements And
        final Type uhderType = getClass().getField("uhder").getGenericType();   // Other implements This
        final Type dingType = getClass().getField("ding").getGenericType();     // Thing extends Other
        final Type testerType = getClass().getField("tester").getGenericType(); // Tester implements This
        final Type tester2Type = getClass().getField("tester2").getGenericType();

        assertTrue(TypeUtils.isAssignable(datType, disType));
        assertFalse(TypeUtils.isAssignable(daType, disType));
        assertTrue(TypeUtils.isAssignable(uhderType, disType));
        assertFalse(TypeUtils.isAssignable(dingType, disType));
        assertTrue(TypeUtils.isAssignable(testerType, disType));
        assertFalse(TypeUtils.isAssignable(tester2Type, disType));

        assertFalse(TypeUtils.isAssignable(dat2Type, datType));
        assertFalse(TypeUtils.isAssignable(datType, dat2Type));
        assertFalse(TypeUtils.isAssignable(dat3Type, datType));
    }

    @Test
    public void testIsAssignableGenericComparableTypes() throws NoSuchFieldException {
        final Type intComparableType = getClass().getField("intComparable").getGenericType();
        assertTrue(TypeUtils.isAssignable(int.class, intComparableType));

        final Type longComparableType = getClass().getField("longComparable").getGenericType();
        assertFalse(TypeUtils.isAssignable(int.class, longComparableType));
        assertFalse(TypeUtils.isAssignable(Integer.class, longComparableType));

        final Type intComparableArrayType = getClass().getField("intWildcardComparable").getGenericType();
        assertTrue(TypeUtils.isAssignable(Integer[].class, intComparableArrayType));
    }

    @Test
    public void testIsAssignableGenericListArrays() throws NoSuchFieldException {
        final Type rawListTypeArray = GenericTypeHolder.class.getDeclaredField("rawListArray").getGenericType();
        final Type objectListTypeArray = GenericTypeHolder.class.getDeclaredField("objectListArray").getGenericType();
        final Type unboundListTypeArray = GenericTypeHolder.class.getDeclaredField("unboundListArray").getGenericType();
        final Type superObjectListTypeArray = GenericTypeHolder.class.getDeclaredField("superObjectListArray").getGenericType();
        final Type stringListTypeArray = GenericTypeHolder.class.getDeclaredField("stringListArray").getGenericType();
        final Type subStringListTypeArray = GenericTypeHolder.class.getDeclaredField("subStringListArray").getGenericType();
        final Type superStringListTypeArray = GenericTypeHolder.class.getDeclaredField("superStringListArray").getGenericType();

        assertTrue(TypeUtils.isAssignable(rawListTypeArray, rawListTypeArray));
        assertTrue(TypeUtils.isAssignable(rawListTypeArray, objectListTypeArray));
        assertTrue(TypeUtils.isAssignable(objectListTypeArray, rawListTypeArray));
        assertTrue(TypeUtils.isAssignable(rawListTypeArray, unboundListTypeArray));
        assertTrue(TypeUtils.isAssignable(unboundListTypeArray, rawListTypeArray));
        assertTrue(TypeUtils.isAssignable(rawListTypeArray, superObjectListTypeArray));
        assertTrue(TypeUtils.isAssignable(superObjectListTypeArray, rawListTypeArray));
        assertTrue(TypeUtils.isAssignable(rawListTypeArray, stringListTypeArray));
        assertTrue(TypeUtils.isAssignable(stringListTypeArray, rawListTypeArray));
        assertTrue(TypeUtils.isAssignable(rawListTypeArray, subStringListTypeArray));
        assertTrue(TypeUtils.isAssignable(subStringListTypeArray, rawListTypeArray));
        assertTrue(TypeUtils.isAssignable(rawListTypeArray, superStringListTypeArray));
        assertTrue(TypeUtils.isAssignable(superStringListTypeArray, rawListTypeArray));

        assertTrue(TypeUtils.isAssignable(objectListTypeArray, objectListTypeArray));
        assertTrue(TypeUtils.isAssignable(objectListTypeArray, unboundListTypeArray));
        assertFalse(TypeUtils.isAssignable(unboundListTypeArray, objectListTypeArray));
        assertTrue(TypeUtils.isAssignable(objectListTypeArray, superObjectListTypeArray));
        assertFalse(TypeUtils.isAssignable(superObjectListTypeArray, objectListTypeArray));
        assertFalse(TypeUtils.isAssignable(objectListTypeArray, stringListTypeArray));
        assertFalse(TypeUtils.isAssignable(stringListTypeArray, objectListTypeArray));
        assertFalse(TypeUtils.isAssignable(objectListTypeArray, subStringListTypeArray));
        assertFalse(TypeUtils.isAssignable(subStringListTypeArray, objectListTypeArray));
        assertTrue(TypeUtils.isAssignable(objectListTypeArray, superStringListTypeArray));
        assertFalse(TypeUtils.isAssignable(superStringListTypeArray, objectListTypeArray));

        assertTrue(TypeUtils.isAssignable(unboundListTypeArray, unboundListTypeArray));
        assertFalse(TypeUtils.isAssignable(unboundListTypeArray, superObjectListTypeArray));
        assertTrue(TypeUtils.isAssignable(superObjectListTypeArray, unboundListTypeArray));
        assertFalse(TypeUtils.isAssignable(unboundListTypeArray, stringListTypeArray));
        assertTrue(TypeUtils.isAssignable(stringListTypeArray, unboundListTypeArray));
        assertFalse(TypeUtils.isAssignable(unboundListTypeArray, subStringListTypeArray));
        assertTrue(TypeUtils.isAssignable(subStringListTypeArray, unboundListTypeArray));
        assertFalse(TypeUtils.isAssignable(unboundListTypeArray, superStringListTypeArray));
        assertTrue(TypeUtils.isAssignable(superStringListTypeArray, unboundListTypeArray));

        assertTrue(TypeUtils.isAssignable(superObjectListTypeArray, superObjectListTypeArray));
        assertFalse(TypeUtils.isAssignable(superObjectListTypeArray, stringListTypeArray));
        assertFalse(TypeUtils.isAssignable(stringListTypeArray, superObjectListTypeArray));
        assertFalse(TypeUtils.isAssignable(superObjectListTypeArray, subStringListTypeArray));
        assertFalse(TypeUtils.isAssignable(subStringListTypeArray, superObjectListTypeArray));
        assertTrue(TypeUtils.isAssignable(superObjectListTypeArray, superStringListTypeArray));
        assertFalse(TypeUtils.isAssignable(superStringListTypeArray, superObjectListTypeArray));

        assertTrue(TypeUtils.isAssignable(stringListTypeArray, stringListTypeArray));
        assertTrue(TypeUtils.isAssignable(stringListTypeArray, subStringListTypeArray));
        assertFalse(TypeUtils.isAssignable(subStringListTypeArray, stringListTypeArray));
        assertTrue(TypeUtils.isAssignable(stringListTypeArray, superStringListTypeArray));
        assertFalse(TypeUtils.isAssignable(superStringListTypeArray, stringListTypeArray));

        assertTrue(TypeUtils.isAssignable(subStringListTypeArray, subStringListTypeArray));
        assertFalse(TypeUtils.isAssignable(subStringListTypeArray, superStringListTypeArray));
        assertFalse(TypeUtils.isAssignable(superStringListTypeArray, subStringListTypeArray));
        assertTrue(TypeUtils.isAssignable(superStringListTypeArray, superStringListTypeArray));
    }

    @Test
    public void testIsAssignableGenericListTypes() throws NoSuchFieldException {
        final Type rawListType = GenericTypeHolder.class.getDeclaredField("rawList").getGenericType();
        final Type objectListType = GenericTypeHolder.class.getDeclaredField("objectList").getGenericType();
        final Type unboundListType = GenericTypeHolder.class.getDeclaredField("unboundList").getGenericType();
        final Type superObjectListType = GenericTypeHolder.class.getDeclaredField("superObjectList").getGenericType();
        final Type stringListType = GenericTypeHolder.class.getDeclaredField("stringList").getGenericType();
        final Type subStringListType = GenericTypeHolder.class.getDeclaredField("subStringList").getGenericType();
        final Type superStringListType = GenericTypeHolder.class.getDeclaredField("superStringList").getGenericType();

        assertTrue(TypeUtils.isAssignable(rawListType, rawListType));
        assertTrue(TypeUtils.isAssignable(rawListType, objectListType));
        assertTrue(TypeUtils.isAssignable(objectListType, rawListType));
        assertTrue(TypeUtils.isAssignable(rawListType, unboundListType));
        assertTrue(TypeUtils.isAssignable(unboundListType, rawListType));
        assertTrue(TypeUtils.isAssignable(rawListType, superObjectListType));
        assertTrue(TypeUtils.isAssignable(superObjectListType, rawListType));
        assertTrue(TypeUtils.isAssignable(rawListType, stringListType));
        assertTrue(TypeUtils.isAssignable(stringListType, rawListType));
        assertTrue(TypeUtils.isAssignable(rawListType, subStringListType));
        assertTrue(TypeUtils.isAssignable(subStringListType, rawListType));
        assertTrue(TypeUtils.isAssignable(rawListType, superStringListType));
        assertTrue(TypeUtils.isAssignable(superStringListType, rawListType));

        assertTrue(TypeUtils.isAssignable(objectListType, objectListType));
        assertTrue(TypeUtils.isAssignable(objectListType, unboundListType));
        assertFalse(TypeUtils.isAssignable(unboundListType, objectListType));
        assertTrue(TypeUtils.isAssignable(objectListType, superObjectListType));
        assertFalse(TypeUtils.isAssignable(superObjectListType, objectListType));
        assertFalse(TypeUtils.isAssignable(objectListType, stringListType));
        assertFalse(TypeUtils.isAssignable(stringListType, objectListType));
        assertFalse(TypeUtils.isAssignable(objectListType, subStringListType));
        assertFalse(TypeUtils.isAssignable(subStringListType, objectListType));
        assertTrue(TypeUtils.isAssignable(objectListType, superStringListType));
        assertFalse(TypeUtils.isAssignable(superStringListType, objectListType));

        assertTrue(TypeUtils.isAssignable(unboundListType, unboundListType));
        assertFalse(TypeUtils.isAssignable(unboundListType, superObjectListType));
        assertTrue(TypeUtils.isAssignable(superObjectListType, unboundListType));
        assertFalse(TypeUtils.isAssignable(unboundListType, stringListType));
        assertTrue(TypeUtils.isAssignable(stringListType, unboundListType));
        assertFalse(TypeUtils.isAssignable(unboundListType, subStringListType));
        assertTrue(TypeUtils.isAssignable(subStringListType, unboundListType));
        assertFalse(TypeUtils.isAssignable(unboundListType, superStringListType));
        assertTrue(TypeUtils.isAssignable(superStringListType, unboundListType));

        assertTrue(TypeUtils.isAssignable(superObjectListType, superObjectListType));
        assertFalse(TypeUtils.isAssignable(superObjectListType, stringListType));
        assertFalse(TypeUtils.isAssignable(stringListType, superObjectListType));
        assertFalse(TypeUtils.isAssignable(superObjectListType, subStringListType));
        assertFalse(TypeUtils.isAssignable(subStringListType, superObjectListType));
        assertTrue(TypeUtils.isAssignable(superObjectListType, superStringListType));
        assertFalse(TypeUtils.isAssignable(superStringListType, superObjectListType));

        assertTrue(TypeUtils.isAssignable(stringListType, stringListType));
        assertTrue(TypeUtils.isAssignable(stringListType, subStringListType));
        assertFalse(TypeUtils.isAssignable(subStringListType, stringListType));
        assertTrue(TypeUtils.isAssignable(stringListType, superStringListType));
        assertFalse(TypeUtils.isAssignable(superStringListType, stringListType));

        assertTrue(TypeUtils.isAssignable(subStringListType, subStringListType));
        assertFalse(TypeUtils.isAssignable(subStringListType, superStringListType));
        assertFalse(TypeUtils.isAssignable(superStringListType, subStringListType));
        assertTrue(TypeUtils.isAssignable(superStringListType, superStringListType));
    }

    @SuppressWarnings("boxing") // boxing is deliberate here
    @Test
    public void testIsInstance() throws NoSuchFieldException {
        final Type intComparableType = getClass().getField("intComparable").getGenericType();
        final Type uriComparableType = getClass().getField("uriComparable").getGenericType();
        assertTrue(TypeUtils.isInstance(1, intComparableType));
        assertFalse(TypeUtils.isInstance(1, uriComparableType));
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
    public void testParameterize() throws NoSuchFieldException {
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
    public void testParameterizeNullPointerException() {
        assertThrows(NullPointerException.class, () -> TypeUtils.parameterize(null, Collections.emptyMap()));
        final Map<TypeVariable<?>, Type> nullTypeVariableMap = null;
        assertThrows(NullPointerException.class, () -> TypeUtils.parameterize(String.class, nullTypeVariableMap));
    }

    @Test
    public void testParameterizeVarArgsNullPointerException() {
        assertThrows(NullPointerException.class, () -> TypeUtils.parameterize(null));
    }

    @Test
    public void testParameterizeWithOwner() throws NoSuchFieldException {
        final Type owner = TypeUtils.parameterize(TypeUtilsTest.class, String.class);
        final ParameterizedType dat2Type = TypeUtils.parameterizeWithOwner(owner, That.class, String.class, String.class);
        assertTrue(TypeUtils.equals(getClass().getField("dat2").getGenericType(), dat2Type));
    }

    @Test
    public void testParameterizeWithOwner3ArgsNullPointerException() {
        final Type owner = TypeUtils.parameterize(TypeUtilsTest.class, String.class);
        assertThrows(NullPointerException.class, () -> TypeUtils.parameterizeWithOwner(owner, null, String.class));
        final Map<TypeVariable<?>, Type> nullTypeVariableMap = null;
        assertThrows(NullPointerException.class, () -> TypeUtils.parameterizeWithOwner(owner, That.class, nullTypeVariableMap));
    }

    @Test
    public void testParameterizeWithOwnerVarArgsNullPointerException() {
        final Type owner = TypeUtils.parameterize(TypeUtilsTest.class, String.class);
        assertThrows(NullPointerException.class, () -> TypeUtils.parameterizeWithOwner(owner, null));
    }

    @Test
    public void testToLongString() {
        assertEquals(getClass().getName() + ":B", TypeUtils.toLongString(getClass().getTypeParameters()[0]));

        assertThrows(NullPointerException.class, () -> TypeUtils.toLongString(null));
    }

    @Test
    public void testToString_LANG_1311() {
        assertEquals("int[]", TypeUtils.toString(int[].class));
        assertEquals("java.lang.Integer[]", TypeUtils.toString(Integer[].class));
        final Field stringListField = FieldUtils.getDeclaredField(getClass(), "stringListArray");
        assertEquals("java.util.List<java.lang.String>[]", TypeUtils.toString(stringListField.getGenericType()));
    }

    @Test
    public void testTypesSatisfyVariables() throws NoSuchMethodException {
        final Map<TypeVariable<?>, Type> typeVarAssigns = new HashMap<>();
        final Integer max = TypeUtilsTest.<Integer>stub();
        typeVarAssigns.put(getClass().getMethod("stub").getTypeParameters()[0], Integer.class);
        assertTrue(TypeUtils.typesSatisfyVariables(typeVarAssigns));
        typeVarAssigns.clear();
        typeVarAssigns.put(getClass().getMethod("stub2").getTypeParameters()[0], Integer.class);
        assertTrue(TypeUtils.typesSatisfyVariables(typeVarAssigns));
        typeVarAssigns.clear();
        typeVarAssigns.put(getClass().getMethod("stub3").getTypeParameters()[0], Integer.class);
        assertTrue(TypeUtils.typesSatisfyVariables(typeVarAssigns));

        assertThrows(NullPointerException.class, () -> TypeUtils.typesSatisfyVariables(null));
    }

    @ParameterizedTest
    @MethodSource
    public void testTypeToString(Type type) {
        // No stack overflow
        assertNotNull(TypeUtils.toString(type));
    }

    @Test
    public void testUnboundedWildcardType() {
        final WildcardType unbounded = TypeUtils.wildcardType().withLowerBounds((Type) null).withUpperBounds().build();
        assertTrue(TypeUtils.equals(TypeUtils.WILDCARD_ALL, unbounded));
        assertArrayEquals(new Type[] { Object.class }, TypeUtils.getImplicitUpperBounds(unbounded));
        assertArrayEquals(new Type[] { null }, TypeUtils.getImplicitLowerBounds(unbounded));
        assertEquals("?", TypeUtils.toString(unbounded));
        assertEquals("?", unbounded.toString());

        assertThrows(NullPointerException.class,
                () -> TypeUtils.getImplicitLowerBounds(null));
        assertThrows(NullPointerException.class,
                () -> TypeUtils.getImplicitUpperBounds(null));
    }

    @Test
    public void testWildcardType() throws NoSuchFieldException {
        final WildcardType simpleWildcard = TypeUtils.wildcardType().withUpperBounds(String.class).build();
        final Field cClass = AClass.class.getField("cClass");
        assertTrue(TypeUtils.equals(((ParameterizedType) cClass.getGenericType()).getActualTypeArguments()[0],
            simpleWildcard));
        assertEquals(String.format("? extends %s", String.class.getName()), TypeUtils.toString(simpleWildcard));
        assertEquals(String.format("? extends %s", String.class.getName()), simpleWildcard.toString());
    }

    @Test
    public void testWrap() {
        final Type t = getClass().getTypeParameters()[0];
        assertTrue(TypeUtils.equals(t, TypeUtils.wrap(t).getType()));

        assertEquals(String.class, TypeUtils.wrap(String.class).getType());
    }
}

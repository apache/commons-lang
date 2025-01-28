package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Disabled;
import java.util.List;
import java.util.Arrays;
import java.lang.reflect.Method;
import org.mockito.MockedStatic;
import java.util.ArrayList;
import java.util.Comparator;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInRelativeOrder;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.mockito.Mockito.mockStatic;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class ClassUtilsBaseRockGeneratedTest {

    //BaseRock generated method id: ${comparatorTest}, hash: 64473B1C0F7FEFFE7C0C79EF824D3B2C
    @Test()
    void comparatorTest() {
        
        //Act Statement(s)
        Comparator<Class<?>> result = ClassUtils.comparator();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${convertClassesToClassNamesWhenClassesIsNull}, hash: 03E1B595207FFFCDE78CDDFDA579AC34
    @Test()
    void convertClassesToClassNamesWhenClassesIsNull() {
        /* Branches:
         * (classes == null) : true
         */
         //Arrange Statement(s)
        List<Class<?>> list = null;
        
        //Act Statement(s)
        List<String> result = ClassUtils.convertClassesToClassNames(list);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${convertClassesToClassNamesWhenClassesIsNotNull}, hash: 353DE677C71F98F9766FDE95D856C2C3
    @Test()
    void convertClassesToClassNamesWhenClassesIsNotNull() {
        /* Branches:
         * (classes == null) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.getName(Object.class, (String) null)).thenReturn("return_of_getName1");
            List<Class<?>> anyList = new ArrayList<>();
            anyList.add(Object.class);
            //Act Statement(s)
            List<String> result = ClassUtils.convertClassesToClassNames(anyList);
            List<String> stringResultList = new ArrayList<>(Arrays.asList("return_of_getName1"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.size(), equalTo(1));
                assertThat(result, containsInRelativeOrder(stringResultList.toArray()));
                classUtils.verify(() -> ClassUtils.getName(Object.class, (String) null), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${convertClassNamesToClassesWhenClassNamesIsNull}, hash: 47EFB64FCA0154E9F685FEC986965BDC
    @Test()
    void convertClassNamesToClassesWhenClassNamesIsNull() {
        /* Branches:
         * (classNames == null) : true
         */
         //Arrange Statement(s)
        List<String> list = null;
        
        //Act Statement(s)
        List<Class<?>> result = ClassUtils.convertClassNamesToClasses(list);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${convertClassNamesToClassesWhenClassNamesIsNotNull}, hash: 77BE13E193C727212EF0667D9E18A50A
    @Test()
    void convertClassNamesToClassesWhenClassNamesIsNotNull() {
        /* Branches:
         * (classNames == null) : false
         */
         //Arrange Statement(s)
        List<String> stringList = new ArrayList<>(Arrays.asList("java.lang.Object"));
        
        //Act Statement(s)
        List<Class<?>> result = ClassUtils.convertClassNamesToClasses(stringList);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result.size(), equalTo(1));
            assertThat(result.get(0), is(instanceOf(Class.class)));
        });
    }

    //BaseRock generated method id: ${convertClassNamesToClassesWhenCaughtException}, hash: 1460DDAFF8563831C5DEF3DB5C302125
    @Test()
    void convertClassNamesToClassesWhenCaughtException() throws ClassNotFoundException {
        /* Branches:
         * (classNames == null) : false
         * (catch-exception (Exception)) : true  #  inside lambda$convertClassNamesToClasses$3 method
         */
         //Arrange Statement(s)
        try (MockedStatic<Class> _class = mockStatic(Class.class)) {
            RuntimeException runtimeException = new RuntimeException();
            _class.when(() -> Class.forName("classNamesItem1")).thenThrow(runtimeException);
            List<String> stringList = new ArrayList<>(Arrays.asList("classNamesItem1"));
            //Act Statement(s)
            List<Class<?>> result = ClassUtils.convertClassNamesToClasses(stringList);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.size(), equalTo(1));
                _class.verify(() -> Class.forName("classNamesItem1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getAbbreviatedNameWhenClsIsNull}, hash: D3CD457170731A8C7D9BAD0560F29EEA
    @Test()
    void getAbbreviatedNameWhenClsIsNull() {
        /* Branches:
         * (cls == null) : true
         */
         //Arrange Statement(s)
        Class<?> _class = null;
        
        //Act Statement(s)
        String result = ClassUtils.getAbbreviatedName(_class, 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${getAbbreviatedNameWhenClsIsNotNull}, hash: E30A34E9AABE8644102C442F1C9C8A59
    @Test()
    void getAbbreviatedNameWhenClsIsNotNull() {
        /* Branches:
         * (cls == null) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.getAbbreviatedName("java.lang.Object", 0)).thenReturn("return_of_getAbbreviatedName1");
            //Act Statement(s)
            String result = ClassUtils.getAbbreviatedName(Object.class, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getAbbreviatedName1"));
                classUtils.verify(() -> ClassUtils.getAbbreviatedName("java.lang.Object", 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getAbbreviatedName1WhenLengthHintLessThanOrEqualsTo0ThrowsIllegalArgumentException}, hash: 2DE5752AA3ABAAEE8235EB097CC9CC54
    @Test()
    void getAbbreviatedName1WhenLengthHintLessThanOrEqualsTo0ThrowsIllegalArgumentException() {
        /* Branches:
         * (lengthHint <= 0) : true
         */
         //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("len must be > 0");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            ClassUtils.getAbbreviatedName("java.lang.String", -1);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getAbbreviatedName1WhenClassNameIsNull}, hash: E5B34B3851B6961076EF8EDBA361DD73
    @Test()
    void getAbbreviatedName1WhenClassNameIsNull() {
        /* Branches:
         * (lengthHint <= 0) : false
         * (className == null) : true
         */
         
        //Act Statement(s)
        String result = ClassUtils.getAbbreviatedName((String) null, 1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${getAbbreviatedName1WhenClassNameLengthLessThanOrEqualsToLengthHint}, hash: B93E1CFBEE305994000DEA93A7030DAC
    @Test()
    void getAbbreviatedName1WhenClassNameLengthLessThanOrEqualsToLengthHint() {
        /* Branches:
         * (lengthHint <= 0) : false
         * (className == null) : false
         * (className.length() <= lengthHint) : true
         */
         
        //Act Statement(s)
        String result = ClassUtils.getAbbreviatedName("", 1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${getAbbreviatedName1WhenSourceLessThanAbbreviatedLength}, hash: 9619783AFFBC01106BC5BF89E3331B20
    @Test()
    void getAbbreviatedName1WhenSourceLessThanAbbreviatedLength() {
        /* Branches:
         * (lengthHint <= 0) : false
         * (className == null) : false
         * (className.length() <= lengthHint) : false
         * (source < abbreviated.length) : true
         * (source < abbreviated.length) : true
         * (abbreviated[source] != '.') : true
         * (source >= originalLength) : false  #  inside useFull method
         * (runAheadTarget + originalLength - source <= desiredLength) : true  #  inside useFull method
         * (useFull(runAheadTarget, source, abbreviated.length, lengthHint)) : true
         * (source < abbreviated.length) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        String result = ClassUtils.getAbbreviatedName("java.lang.String", 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${getAbbreviatedName1WhenTargetNotGreaterThanRunAheadTargetAndSourceLessThanAbbreviatedLength}, hash: 1C0FD95351096178E47F753F93DBE87B
    @Test()
    void getAbbreviatedName1WhenTargetNotGreaterThanRunAheadTargetAndSourceLessThanAbbreviatedLength() {
        /* Branches:
         * (lengthHint <= 0) : false
         * (className == null) : false
         * (className.length() <= lengthHint) : false
         * (source < abbreviated.length) : true
         * (source < abbreviated.length) : true
         * (abbreviated[source] != '.') : true
         * (source >= originalLength) : false  #  inside useFull method
         * (runAheadTarget + originalLength - source <= desiredLength) : false  #  inside useFull method
         * (useFull(runAheadTarget, source, abbreviated.length, lengthHint)) : false
         * (target > runAheadTarget) : false
         * (source < abbreviated.length) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        String result = ClassUtils.getAbbreviatedName("AB", 1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${getAllInterfacesWhenClsIsNull}, hash: 337B81A0760B809C3B2BD410A2691A14
    @Test()
    void getAllInterfacesWhenClsIsNull() {
        /* Branches:
         * (cls == null) : true
         */
         //Arrange Statement(s)
        Class<?> _class = null;
        
        //Act Statement(s)
        List<Class<?>> result = ClassUtils.getAllInterfaces(_class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getAllInterfacesWhenInterfacesFoundAddI}, hash: 99EB2DDF5D4D3C7BB37DF36CC04EE981
    @Test()
    void getAllInterfacesWhenInterfacesFoundAddI() {
        /* Branches:
         * (cls == null) : false
         * (cls != null) : true  #  inside getAllInterfaces method
         * (for-each(interfaces)) : true  #  inside getAllInterfaces method
         * (interfacesFound.add(i)) : true  #  inside getAllInterfaces method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        List<Class<?>> result = ClassUtils.getAllInterfaces(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getAllSuperclassesWhenClsIsNull}, hash: 0ED48D71E4ADFF3DC3543806CDCFC22A
    @Test()
    void getAllSuperclassesWhenClsIsNull() {
        /* Branches:
         * (cls == null) : true
         */
         //Arrange Statement(s)
        Class<?> _class = null;
        
        //Act Statement(s)
        List<Class<?>> result = ClassUtils.getAllSuperclasses(_class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getAllSuperclassesWhenSuperclassIsNotNull}, hash: 627AB4F1B2AAFCA1B73151AAED5DBB40
    @Test()
    void getAllSuperclassesWhenSuperclassIsNotNull() {
        /* Branches:
         * (cls == null) : false
         * (superclass != null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        List<Class<?>> result = ClassUtils.getAllSuperclasses(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result.size(), equalTo(1));
            assertThat(result.get(0), is(instanceOf(Class.class)));
        });
    }

    //BaseRock generated method id: ${getCanonicalNameTest}, hash: F597FB6F882B64F9FDF7D4F913D4828C
    @Test()
    void getCanonicalNameTest() {
        //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.getCanonicalName(Object.class, "")).thenReturn("return_of_getCanonicalName1");
            //Act Statement(s)
            String result = ClassUtils.getCanonicalName(Object.class);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getCanonicalName1"));
                classUtils.verify(() -> ClassUtils.getCanonicalName(Object.class, ""), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getCanonicalName1WhenClsIsNull}, hash: 21AF61630A068B5434CE0C326C188F6A
    @Test()
    void getCanonicalName1WhenClsIsNull() {
        /* Branches:
         * (cls == null) : true
         */
         //Arrange Statement(s)
        Class<?> _class = null;
        
        //Act Statement(s)
        String result = ClassUtils.getCanonicalName(_class, "valueIfNull1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("valueIfNull1")));
    }

    //BaseRock generated method id: ${getCanonicalName1WhenCanonicalNameIsNull}, hash: A48AAC81C724E969F71C96391531B48C
    @Test()
    void getCanonicalName1WhenCanonicalNameIsNull() {
        /* Branches:
         * (cls == null) : false
         * (canonicalName == null) : true
         */
         
        //Act Statement(s)
        String result = ClassUtils.getCanonicalName(Object.class, "valueIfNull1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("valueIfNull1")));
    }

    //BaseRock generated method id: ${getCanonicalName1WhenCanonicalNameIsNotNull}, hash: 898A4F0ED22490E3E9B295D0929319EB
    @Test()
    void getCanonicalName1WhenCanonicalNameIsNotNull() {
        /* Branches:
         * (cls == null) : false
         * (canonicalName == null) : false
         */
         
        //Act Statement(s)
        String result = ClassUtils.getCanonicalName(Object.class, "valueIfNull1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("java.lang.Object")));
    }

    //BaseRock generated method id: ${getCanonicalName2Test}, hash: 61DD91FB5398D04AD275BCDDC4016345
    @Test()
    void getCanonicalName2Test() {
        //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            classUtils.when(() -> ClassUtils.getCanonicalName(object, "")).thenReturn("return_of_getCanonicalName1");
            //Act Statement(s)
            String result = ClassUtils.getCanonicalName(object);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getCanonicalName1"));
                classUtils.verify(() -> ClassUtils.getCanonicalName(object, ""), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getCanonicalName3WhenObjectIsNull}, hash: C877AD52BE634D3A413FB928DEF9F456
    @Test()
    void getCanonicalName3WhenObjectIsNull() {
        /* Branches:
         * (object == null) : true
         */
         //Arrange Statement(s)
        Object object = null;
        
        //Act Statement(s)
        String result = ClassUtils.getCanonicalName(object, "valueIfNull1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("valueIfNull1")));
    }

    //BaseRock generated method id: ${getCanonicalName3WhenCanonicalNameIsNull}, hash: 025DDCD55395BCBC272378570FF88606
    @Test()
    void getCanonicalName3WhenCanonicalNameIsNull() {
        /* Branches:
         * (object == null) : false
         * (canonicalName == null) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        
        //Act Statement(s)
        String result = ClassUtils.getCanonicalName(object, "valueIfNull1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("valueIfNull1")));
    }

    //BaseRock generated method id: ${getCanonicalName3WhenCanonicalNameIsNotNull}, hash: D825E899FBEFCE6E76A90D5D410335F1
    @Test()
    void getCanonicalName3WhenCanonicalNameIsNotNull() {
        /* Branches:
         * (object == null) : false
         * (canonicalName == null) : false
         */
         //Arrange Statement(s)
        Object object = new Object();
        
        //Act Statement(s)
        String result = ClassUtils.getCanonicalName(object, "valueIfNull1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("java.lang.Object")));
    }

    //BaseRock generated method id: ${getClassTest}, hash: D084152FA963F17073DE3C1D9B85C0E0
    @Test()
    void getClassTest() throws ClassNotFoundException {
        //Arrange Statement(s)
        /*try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
    classUtils.when(() -> ClassUtils.getClass((ClassLoader) any(), eq("java.lang.String"), eq(true))).thenReturn(Object.class);
    ClassLoader classLoader = ClassLoader.getPlatformClassLoader();
    //Act Statement(s)
    Class<?> result = ClassUtils.getClass(classLoader, "java.lang.String");
    //Assert statement(s)
    assertAll("result", () -> {
        assertThat(result, equalTo(Object.class));
        classUtils.verify(() -> ClassUtils.getClass((ClassLoader) any(), eq("java.lang.String"), eq(true)), atLeast(1));
    });
}*/
    }

    //BaseRock generated method id: ${getClass1WhenClazzIsNotNull}, hash: 5CE4BB48C5F339BFB866D6FB4D4CE560
    @Test()
    void getClass1WhenClazzIsNotNull() throws ClassNotFoundException {
        /* Branches:
         * (clazz != null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        //ClassLoader classLoader = ClassLoader.getPlatformClassLoader();
        //Act Statement(s)
        //Class<?> result = ClassUtils.getClass(classLoader, "java.lang.String", false);
        //Assert statement(s)
        //assertAll("result", () -> assertThat(result, equalTo(Object.class)));
    }

    //BaseRock generated method id: ${getClass1WhenLastDotIndexEqualsMinus1ThrowsClassNotFoundException}, hash: 5661172AE8B71BA6E38FFBDCB7726521
    @Test()
    void getClass1WhenLastDotIndexEqualsMinus1ThrowsClassNotFoundException() throws ClassNotFoundException {
        /* Branches:
         * (clazz != null) : true
         * (catch-exception (ClassNotFoundException)) : true
         * (lastDotIndex != -1) : true
         * (lastDotIndex != -1) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        //ClassLoader classLoader = ClassLoader.getPlatformClassLoader();
        //ClassNotFoundException classNotFoundException = new ClassNotFoundException("s1");
        //Act Statement(s)
        /*final ClassNotFoundException result = assertThrows(ClassNotFoundException.class, () -> {
    ClassUtils.getClass(classLoader, "java.lang.String", false);
});*/
        //Assert statement(s)
        /*assertAll("result", () -> {
    assertThat(result, is(notNullValue()));
    assertThat(result.getMessage(), equalTo(classNotFoundException.getMessage()));
});*/
    }

    //BaseRock generated method id: ${getClass1WhenAbbreviationIsNotNull}, hash: E90E60AE1FF4DC5785ACD026AAE41303
    @Test()
    void getClass1WhenAbbreviationIsNotNull() throws ClassNotFoundException {
        /* Branches:
         * (clazz != null) : false
         * (canonicalName.endsWith(arrayMarker)) : true  #  inside toCanonicalName method
         * (canonicalName.endsWith(arrayMarker)) : true  #  inside toCanonicalName method
         * (abbreviation != null) : true  #  inside toCanonicalName method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        //ClassLoader classLoader = ClassLoader.getPlatformClassLoader();
        //Act Statement(s)
        //Class<?> result = ClassUtils.getClass(classLoader, (String) null, false);
        //Assert statement(s)
        //assertAll("result", () -> assertThat(result, equalTo(Object.class)));
    }

    //BaseRock generated method id: ${getClass1WhenAbbreviationIsNull}, hash: 760BB4C7AD8534864083E26845AD9764
    @Test()
    void getClass1WhenAbbreviationIsNull() throws ClassNotFoundException {
        /* Branches:
         * (clazz != null) : false
         * (canonicalName.endsWith(arrayMarker)) : true  #  inside toCanonicalName method
         * (canonicalName.endsWith(arrayMarker)) : true  #  inside toCanonicalName method
         * (abbreviation != null) : false  #  inside toCanonicalName method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        //ClassLoader classLoader = ClassLoader.getPlatformClassLoader();
        //Act Statement(s)
        //Class<?> result = ClassUtils.getClass(classLoader, (String) null, false);
        //Assert statement(s)
        //assertAll("result", () -> assertThat(result, equalTo(Object.class)));
    }

    //BaseRock generated method id: ${getClass1WhenLastDotIndexNotEqualsMinus1AndLastDotIndexEqualsMinus1ThrowsClassNotFoundException}, hash: 22ECDC4E9A13666E79DE8D44C3E15D79
    @Test()
    void getClass1WhenLastDotIndexNotEqualsMinus1AndLastDotIndexEqualsMinus1ThrowsClassNotFoundException() throws ClassNotFoundException {
        /* Branches:
         * (clazz != null) : false
         * (canonicalName.endsWith(arrayMarker)) : true  #  inside toCanonicalName method
         * (canonicalName.endsWith(arrayMarker)) : true  #  inside toCanonicalName method
         * (abbreviation != null) : true  #  inside toCanonicalName method
         * (catch-exception (ClassNotFoundException)) : true
         * (lastDotIndex != -1) : true
         * (lastDotIndex != -1) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        /*try (MockedStatic<Class> _class = mockStatic(Class.class)) {
    _class.when(() -> Class.forName(eq("string25"), eq(false), (ClassLoader) any())).thenReturn(Object.class);
    ClassLoader classLoader = ClassLoader.getPlatformClassLoader();
    //Act Statement(s)
    final ClassNotFoundException result = assertThrows(ClassNotFoundException.class, () -> {
        ClassUtils.getClass(classLoader, (String) null, false);
    });
    ClassNotFoundException classNotFoundException = new ClassNotFoundException("s1");
    //Assert statement(s)
    assertAll("result", () -> {
        assertThat(result, is(notNullValue()));
        assertThat(result.getMessage(), equalTo(classNotFoundException.getMessage()));
        _class.verify(() -> Class.forName(eq("string25"), eq(false), (ClassLoader) any()));
    });
}*/
    }

    //BaseRock generated method id: ${getClass1WhenLastDotIndexEqualsMinus1AndLastDotIndexEqualsMinus13ThrowsClassNotFoundException}, hash: 083BDCFB12C5D7F09F00048E2912EAA8
    @Test()
    void getClass1WhenLastDotIndexEqualsMinus1AndLastDotIndexEqualsMinus13ThrowsClassNotFoundException() throws ClassNotFoundException {
        /* Branches:
         * (clazz != null) : false
         * (canonicalName.endsWith(arrayMarker)) : true  #  inside toCanonicalName method
         * (canonicalName.endsWith(arrayMarker)) : true  #  inside toCanonicalName method
         * (abbreviation != null) : false  #  inside toCanonicalName method
         * (catch-exception (ClassNotFoundException)) : true
         * (lastDotIndex != -1) : false
         * (lastDotIndex != -1) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        /*try (MockedStatic<Class> _class = mockStatic(Class.class)) {
    _class.when(() -> Class.forName(eq("[Lint;"), eq(false), (ClassLoader) any())).thenReturn(Object.class);
    ClassLoader classLoader = ClassLoader.getPlatformClassLoader();
    //Act Statement(s)
    final ClassNotFoundException result = assertThrows(ClassNotFoundException.class, () -> {
        ClassUtils.getClass(classLoader, (String) null, false);
    });
    ClassNotFoundException classNotFoundException = new ClassNotFoundException((String) null);
    //Assert statement(s)
    assertAll("result", () -> {
        assertThat(result, is(notNullValue()));
        assertThat(result.getMessage(), equalTo(classNotFoundException.getMessage()));
        _class.verify(() -> Class.forName(eq("[Lint;"), eq(false), (ClassLoader) any()));
    });
}*/
    }

    //BaseRock generated method id: ${getClass2Test}, hash: 287DFACCB10755EE79AA215D8D9AFAA3
    @Test()
    void getClass2Test() throws ClassNotFoundException {
        //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.getClass("java.lang.String", true)).thenReturn(Object.class);
            //Act Statement(s)
            Class<?> result = ClassUtils.getClass("java.lang.String");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Object.class));
                classUtils.verify(() -> ClassUtils.getClass("java.lang.String", true), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getClass3WhenContextCLIsNotNull}, hash: 9978642E71F5AB9FB16D5350E393D5DA
    @Test()
    void getClass3WhenContextCLIsNotNull() throws ClassNotFoundException {
        /* Branches:
         * (contextCL == null) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.getClass((ClassLoader) any(), eq("java.lang.String"), eq(false))).thenReturn(Object.class);
            //Act Statement(s)
            Class<?> result = ClassUtils.getClass("java.lang.String", false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Object.class));
                classUtils.verify(() -> ClassUtils.getClass((ClassLoader) any(), eq("java.lang.String"), eq(false)), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getComponentTypeWhenClsIsNull}, hash: A0A0B91181739B950675B2B3436C4203
    @Test()
    void getComponentTypeWhenClsIsNull() {
        /* Branches:
         * (cls == null) : true
         */
         //Arrange Statement(s)
        Class<Object[]> _class = null;
        
        //Act Statement(s)
        Class result = ClassUtils.getComponentType(_class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getComponentTypeWhenClsIsNotNull}, hash: 1C4BD3A5ECF52741089401C5136D6F67
    @Test()
    void getComponentTypeWhenClsIsNotNull() {
        /* Branches:
         * (cls == null) : false
         */
         
        //Act Statement(s)
        Class result = ClassUtils.getComponentType(Object[].class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Object.class)));
    }

    //BaseRock generated method id: ${getNameTest}, hash: CA7FED4B1F9A0DC9261C1A2A16AFE21B
    @Test()
    void getNameTest() {
        //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.getName(Object.class, "")).thenReturn("return_of_getName1");
            //Act Statement(s)
            String result = ClassUtils.getName(Object.class);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getName1"));
                classUtils.verify(() -> ClassUtils.getName(Object.class, ""), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getName1WhenClsIsNull}, hash: 10BFF2F9601CD3E5F103F248C512CA2B
    @Test()
    void getName1WhenClsIsNull() {
        /* Branches:
         * (cls == null) : true
         */
         //Arrange Statement(s)
        Class<?> _class = null;
        
        //Act Statement(s)
        String result = ClassUtils.getName(_class, "valueIfNull1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("valueIfNull1")));
    }

    //BaseRock generated method id: ${getName1WhenClsIsNotNull}, hash: A70089B77BC69EC6405C7A03BE7AE728
    @Test()
    void getName1WhenClsIsNotNull() {
        /* Branches:
         * (cls == null) : false
         */
         
        //Act Statement(s)
        String result = ClassUtils.getName(Object.class, "valueIfNull1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("java.lang.Object")));
    }

    //BaseRock generated method id: ${getName2Test}, hash: 1661DB68A3C8C256747CB604D8BD6C50
    @Test()
    void getName2Test() {
        //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            classUtils.when(() -> ClassUtils.getName(object, "")).thenReturn("return_of_getName1");
            //Act Statement(s)
            String result = ClassUtils.getName(object);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getName1"));
                classUtils.verify(() -> ClassUtils.getName(object, ""), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getName3WhenObjectIsNull}, hash: 2A5514C80DDA1B94503EB558329F411C
    @Test()
    void getName3WhenObjectIsNull() {
        /* Branches:
         * (object == null) : true
         */
         //Arrange Statement(s)
        Object object = null;
        
        //Act Statement(s)
        String result = ClassUtils.getName(object, "valueIfNull1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("valueIfNull1")));
    }

    //BaseRock generated method id: ${getName3WhenObjectIsNotNull}, hash: B0A0FBB3572E2C6599CF1412FF555D4B
    @Test()
    void getName3WhenObjectIsNotNull() {
        /* Branches:
         * (object == null) : false
         */
         //Arrange Statement(s)
        Object object = new Object();
        
        //Act Statement(s)
        String result = ClassUtils.getName(object, "valueIfNull1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("java.lang.Object")));
    }

    //BaseRock generated method id: ${getPackageCanonicalNameWhenClsIsNull}, hash: A15E88C21C286165A81FD52CD3821EA2
    @Test()
    void getPackageCanonicalNameWhenClsIsNull() {
        /* Branches:
         * (cls == null) : true
         */
         //Arrange Statement(s)
        Class<?> _class = null;
        
        //Act Statement(s)
        String result = ClassUtils.getPackageCanonicalName(_class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${getPackageCanonicalNameWhenClsIsNotNull}, hash: 02F6F761AD2159A0FBA8610AEF0924AB
    @Test()
    void getPackageCanonicalNameWhenClsIsNotNull() {
        /* Branches:
         * (cls == null) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.getPackageCanonicalName("java.lang.Object")).thenReturn("return_of_getPackageCanonicalName1");
            //Act Statement(s)
            String result = ClassUtils.getPackageCanonicalName(Object.class);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getPackageCanonicalName1"));
                classUtils.verify(() -> ClassUtils.getPackageCanonicalName("java.lang.Object"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getPackageCanonicalName1WhenObjectIsNull}, hash: 517E01165E0A5F75D91928925EF27593
    @Test()
    void getPackageCanonicalName1WhenObjectIsNull() {
        /* Branches:
         * (object == null) : true
         */
         //Arrange Statement(s)
        Object object = null;
        
        //Act Statement(s)
        String result = ClassUtils.getPackageCanonicalName(object, "valueIfNull1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("valueIfNull1")));
    }

    //BaseRock generated method id: ${getPackageCanonicalName1WhenObjectIsNotNull}, hash: AE04351E54E126EB0048356929140AF9
    @Test()
    void getPackageCanonicalName1WhenObjectIsNotNull() {
        /* Branches:
         * (object == null) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.getPackageCanonicalName("java.lang.Object")).thenReturn("return_of_getPackageCanonicalName1");
            Object object = new Object();
            //Act Statement(s)
            String result = ClassUtils.getPackageCanonicalName(object, "valueIfNull1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getPackageCanonicalName1"));
                classUtils.verify(() -> ClassUtils.getPackageCanonicalName("java.lang.Object"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getPackageCanonicalName2Test}, hash: DC81D59ADFE053560B7F3BD4A68BF36D
    @Disabled()
    @Test()
    void getPackageCanonicalName2Test() {
        //TODO: Please change the modifier of getCanonicalName from private to default to isolate the test case scenario
        //Act Statement(s)
        //Assert statement(s)
        //classUtils.when(() -> ClassUtils.getCanonicalName("name1")).thenReturn("return_of_getCanonicalName1");
        //classUtils.when(() -> ClassUtils.getPackageName("return_of_getCanonicalName1")).thenReturn("return_of_getPackageName1");
        //String result = ClassUtils.getPackageCanonicalName("name1");
        //assertAll("result", () -> {    assertThat(result, equalTo("return_of_getPackageName1"));    classUtils.verify(() -> ClassUtils.getPackageName("return_of_getCanonicalName1"), atLeast(1));});
    }

    //BaseRock generated method id: ${getPackageNameWhenClsIsNull}, hash: EF62074CC6F9E83EAE140696AB571EFE
    @Test()
    void getPackageNameWhenClsIsNull() {
        /* Branches:
         * (cls == null) : true
         */
         //Arrange Statement(s)
        Class<?> _class = null;
        
        //Act Statement(s)
        String result = ClassUtils.getPackageName(_class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${getPackageNameWhenClsIsNotNull}, hash: EB7261020BE8D3A1CC1E8EFD4323D402
    @Test()
    void getPackageNameWhenClsIsNotNull() {
        /* Branches:
         * (cls == null) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.getPackageName("java.lang.Object")).thenReturn("return_of_getPackageName1");
            //Act Statement(s)
            String result = ClassUtils.getPackageName(Object.class);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getPackageName1"));
                classUtils.verify(() -> ClassUtils.getPackageName("java.lang.Object"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getPackageName1WhenObjectIsNull}, hash: 77C805741544875C5193A0648B4FB649
    @Test()
    void getPackageName1WhenObjectIsNull() {
        /* Branches:
         * (object == null) : true
         */
         //Arrange Statement(s)
        Object object = null;
        
        //Act Statement(s)
        String result = ClassUtils.getPackageName(object, "valueIfNull1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("valueIfNull1")));
    }

    //BaseRock generated method id: ${getPackageName1WhenObjectIsNotNull}, hash: 8C9D34CF0FA945D1D1B2F66A9AD77A3B
    @Test()
    void getPackageName1WhenObjectIsNotNull() {
        /* Branches:
         * (object == null) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.getPackageName(Object.class)).thenReturn("return_of_getPackageName1");
            Object object = new Object();
            //Act Statement(s)
            String result = ClassUtils.getPackageName(object, "valueIfNull1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getPackageName1"));
                classUtils.verify(() -> ClassUtils.getPackageName(Object.class), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getPackageName2WhenStringUtilsIsEmptyClassName}, hash: 5809422CB4B53EB6A1AC05038248CD7C
    @Test()
    void getPackageName2WhenStringUtilsIsEmptyClassName() {
        /* Branches:
         * (StringUtils.isEmpty(className)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        String result = ClassUtils.getPackageName("java.lang.String");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${getPackageName2WhenClassNameCharAtClassNameLengthMinus1Equals___AndIEqualsMinus1}, hash: C976BEE3D3C60DEFC7F3FC06C8D50B8C
    @Test()
    void getPackageName2WhenClassNameCharAtClassNameLengthMinus1Equals___AndIEqualsMinus1() {
        /* Branches:
         * (StringUtils.isEmpty(className)) : false
         * (className.charAt(0) == '[') : true
         * (className.charAt(0) == 'L') : true
         * (className.charAt(className.length() - 1) == ';') : true
         * (i == -1) : true
         */
         
        //Act Statement(s)
        String result = ClassUtils.getPackageName("[L;");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${getPackageName2WhenINotEqualsMinus1}, hash: B830D1DBB7382F09857D2D533C1212AD
    @Test()
    void getPackageName2WhenINotEqualsMinus1() {
        /* Branches:
         * (StringUtils.isEmpty(className)) : false
         * (className.charAt(0) == '[') : true
         * (className.charAt(0) == 'L') : true
         * (className.charAt(className.length() - 1) == ';') : true
         * (i == -1) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        String result = ClassUtils.getPackageName("java.lang.String");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${getPrimitiveClassTest}, hash: 458300CA7BE42E01CF04599077F9F187
    @Test()
    void getPrimitiveClassTest() {
        
        //Act Statement(s)
        Class<?> result = ClassUtils.getPrimitiveClass("java.lang.String");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getPublicMethodWhenIsPublicDeclaredMethodGetDeclaringClass}, hash: B1334F5C850DB4BEB73D8C83D9BDF7E7
    @Test()
    void getPublicMethodWhenIsPublicDeclaredMethodGetDeclaringClass() throws NoSuchMethodException {
        /* Branches:
         * (isPublic(declaredMethod.getDeclaringClass())) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.isPublic(Object.class)).thenReturn(true);
            Class<?>[] classArray = new Class[] {};
            //Act Statement(s)
            Method result = ClassUtils.getPublicMethod(Object.class, "methodName1", classArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                classUtils.verify(() -> ClassUtils.isPublic(Object.class), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getPublicMethodWhenIsPublicNotCandidateClassThrowsNoSuchMethodException}, hash: 7A82A21C79C60434E060448AB128EC41
    @Test()
    void getPublicMethodWhenIsPublicNotCandidateClassThrowsNoSuchMethodException() throws NoSuchMethodException {
        /* Branches:
         * (isPublic(declaredMethod.getDeclaringClass())) : false
         * (for-each(candidateClasses)) : true
         * (!isPublic(candidateClass)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.isPublic(Object.class)).thenReturn(false);
            List<Class<?>> anyList = new ArrayList<>();
            anyList.add(Object.class);
            classUtils.when(() -> ClassUtils.getAllInterfaces(Object.class)).thenReturn(anyList);
            List<Class<?>> anyList2 = new ArrayList<>();
            classUtils.when(() -> ClassUtils.getAllSuperclasses(Object.class)).thenReturn(anyList2);
            Class<?>[] classArray = new Class[] {};
            //Act Statement(s)
            final NoSuchMethodException result = assertThrows(NoSuchMethodException.class, () -> {
                ClassUtils.getPublicMethod(Object.class, "A", classArray);
            });
            NoSuchMethodException noSuchMethodException = new NoSuchMethodException("Can't find a public method for A B");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(noSuchMethodException.getMessage()));
                classUtils.verify(() -> ClassUtils.isPublic(Object.class), atLeast(2));
                classUtils.verify(() -> ClassUtils.getAllInterfaces(Object.class), atLeast(1));
                classUtils.verify(() -> ClassUtils.getAllSuperclasses(Object.class), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getPublicMethodWhenIsPublicNotCandidateClassAndCaughtNoSuchMethodExceptionThrowsNoSuchMethodException}, hash: 8EF392FEDC8E6BA249CCDD722ADC7A0C
    @Test()
    void getPublicMethodWhenIsPublicNotCandidateClassAndCaughtNoSuchMethodExceptionThrowsNoSuchMethodException() throws NoSuchMethodException {
        /* Branches:
         * (isPublic(declaredMethod.getDeclaringClass())) : false
         * (for-each(candidateClasses)) : true
         * (!isPublic(candidateClass)) : true
         * (catch-exception (NoSuchMethodException)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.isPublic(Object.class)).thenReturn(false);
            List<Class<?>> anyList = new ArrayList<>();
            anyList.add(Object.class);
            classUtils.when(() -> ClassUtils.getAllInterfaces(Object.class)).thenReturn(anyList);
            List<Class<?>> anyList2 = new ArrayList<>();
            classUtils.when(() -> ClassUtils.getAllSuperclasses(Object.class)).thenReturn(anyList2);
            Class<?>[] classArray = new Class[] {};
            //Act Statement(s)
            final NoSuchMethodException result = assertThrows(NoSuchMethodException.class, () -> {
                ClassUtils.getPublicMethod(Object.class, "A", classArray);
            });
            NoSuchMethodException noSuchMethodException = new NoSuchMethodException("Can't find a public method for A B");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(noSuchMethodException.getMessage()));
                classUtils.verify(() -> ClassUtils.isPublic(Object.class), atLeast(2));
                classUtils.verify(() -> ClassUtils.getAllInterfaces(Object.class), atLeast(1));
                classUtils.verify(() -> ClassUtils.getAllSuperclasses(Object.class), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getPublicMethodWhenIsPublicCandidateClassAndCaughtNoSuchMethodExceptionThrowsNoSuchMethodException}, hash: 32531A1D5CAA9F2CFB58093C05AC292F
    @Test()
    void getPublicMethodWhenIsPublicCandidateClassAndCaughtNoSuchMethodExceptionThrowsNoSuchMethodException() throws NoSuchMethodException {
        /* Branches:
         * (isPublic(declaredMethod.getDeclaringClass())) : false
         * (for-each(candidateClasses)) : true
         * (!isPublic(candidateClass)) : false
         * (catch-exception (NoSuchMethodException)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            List<Class<?>> anyList = new ArrayList<>();
            anyList.add(Object.class);
            classUtils.when(() -> ClassUtils.getAllInterfaces(Object.class)).thenReturn(anyList);
            List<Class<?>> anyList2 = new ArrayList<>();
            classUtils.when(() -> ClassUtils.getAllSuperclasses(Object.class)).thenReturn(anyList2);
            classUtils.when(() -> ClassUtils.isPublic(Object.class)).thenReturn(false).thenReturn(true);
            Class<?>[] classArray = new Class[] {};
            //Act Statement(s)
            final NoSuchMethodException result = assertThrows(NoSuchMethodException.class, () -> {
                ClassUtils.getPublicMethod(Object.class, "A", classArray);
            });
            NoSuchMethodException noSuchMethodException = new NoSuchMethodException("Can't find a public method for A B");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(noSuchMethodException.getMessage()));
                classUtils.verify(() -> ClassUtils.isPublic(Object.class), atLeast(2));
                classUtils.verify(() -> ClassUtils.getAllInterfaces(Object.class), atLeast(1));
                classUtils.verify(() -> ClassUtils.getAllSuperclasses(Object.class), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getPublicMethodWhenModifierIsPublicCandidateMethodGetDeclaringClassGetModifiers}, hash: C1B1542F2030DC275BC0CB8F280EF9AE
    @Test()
    void getPublicMethodWhenModifierIsPublicCandidateMethodGetDeclaringClassGetModifiers() throws NoSuchMethodException {
        /* Branches:
         * (isPublic(declaredMethod.getDeclaringClass())) : false
         * (for-each(candidateClasses)) : true
         * (!isPublic(candidateClass)) : false
         * (Modifier.isPublic(candidateMethod.getDeclaringClass().getModifiers())) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            List<Class<?>> anyList = new ArrayList<>();
            anyList.add(Object.class);
            classUtils.when(() -> ClassUtils.getAllInterfaces(Object.class)).thenReturn(anyList);
            List<Class<?>> anyList2 = new ArrayList<>();
            classUtils.when(() -> ClassUtils.getAllSuperclasses(Object.class)).thenReturn(anyList2);
            classUtils.when(() -> ClassUtils.isPublic(Object.class)).thenReturn(false).thenReturn(true);
            Class<?>[] classArray = new Class[] {};
            //Act Statement(s)
            Method result = ClassUtils.getPublicMethod(Object.class, "methodName1", classArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                classUtils.verify(() -> ClassUtils.isPublic(Object.class), atLeast(2));
                classUtils.verify(() -> ClassUtils.getAllInterfaces(Object.class), atLeast(1));
                classUtils.verify(() -> ClassUtils.getAllSuperclasses(Object.class), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getPublicMethodWhenModifierNotIsPublicCandidateMethodGetDeclaringClassGetModifiersThrowsNoSuchMethodException}, hash: 8BF3955D65A6FF8B3043194F78783FBC
    @Test()
    void getPublicMethodWhenModifierNotIsPublicCandidateMethodGetDeclaringClassGetModifiersThrowsNoSuchMethodException() throws NoSuchMethodException {
        /* Branches:
         * (isPublic(declaredMethod.getDeclaringClass())) : false
         * (for-each(candidateClasses)) : true
         * (!isPublic(candidateClass)) : false
         * (Modifier.isPublic(candidateMethod.getDeclaringClass().getModifiers())) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.isPublic(Object.class)).thenReturn(false);
            List<Class<?>> anyList = new ArrayList<>();
            classUtils.when(() -> ClassUtils.getAllInterfaces(Object.class)).thenReturn(anyList);
            List<Class<?>> anyList2 = new ArrayList<>();
            classUtils.when(() -> ClassUtils.getAllSuperclasses(Object.class)).thenReturn(anyList2);
            Class<?>[] classArray = new Class[] {};
            //Act Statement(s)
            final NoSuchMethodException result = assertThrows(NoSuchMethodException.class, () -> {
                ClassUtils.getPublicMethod(Object.class, "methodName1", classArray);
            });
            NoSuchMethodException noSuchMethodException = new NoSuchMethodException("s1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(noSuchMethodException.getMessage()));
                classUtils.verify(() -> ClassUtils.isPublic(Object.class), atLeast(2));
                classUtils.verify(() -> ClassUtils.getAllInterfaces(Object.class), atLeast(1));
                classUtils.verify(() -> ClassUtils.getAllSuperclasses(Object.class), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getShortCanonicalNameWhenClsIsNull}, hash: A4CD4193EA7D16004F733F1D7CC6FCDF
    @Test()
    void getShortCanonicalNameWhenClsIsNull() {
        /* Branches:
         * (cls == null) : true
         */
         //Arrange Statement(s)
        Class<?> _class = null;
        
        //Act Statement(s)
        String result = ClassUtils.getShortCanonicalName(_class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${getShortCanonicalNameWhenClsIsNotNull}, hash: AAB9AC68EED1141D7E3BE7314C51E460
    @Test()
    void getShortCanonicalNameWhenClsIsNotNull() {
        /* Branches:
         * (cls == null) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.getShortCanonicalName("java.lang.Object")).thenReturn("return_of_getShortCanonicalName1");
            //Act Statement(s)
            String result = ClassUtils.getShortCanonicalName(Object.class);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getShortCanonicalName1"));
                classUtils.verify(() -> ClassUtils.getShortCanonicalName("java.lang.Object"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getShortCanonicalName1WhenObjectIsNull}, hash: 2D8A760F7308CAB427A1B80746765227
    @Test()
    void getShortCanonicalName1WhenObjectIsNull() {
        /* Branches:
         * (object == null) : true
         */
         //Arrange Statement(s)
        Object object = null;
        
        //Act Statement(s)
        String result = ClassUtils.getShortCanonicalName(object, "valueIfNull1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("valueIfNull1")));
    }

    //BaseRock generated method id: ${getShortCanonicalName1WhenObjectIsNotNull}, hash: 5AAF1BF9F1DED125FF52E55EE55F4759
    @Test()
    void getShortCanonicalName1WhenObjectIsNotNull() {
        /* Branches:
         * (object == null) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.getShortCanonicalName("java.lang.Object")).thenReturn("return_of_getShortCanonicalName1");
            Object object = new Object();
            //Act Statement(s)
            String result = ClassUtils.getShortCanonicalName(object, "valueIfNull1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getShortCanonicalName1"));
                classUtils.verify(() -> ClassUtils.getShortCanonicalName("java.lang.Object"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getShortCanonicalName2Test}, hash: 82BC8412F58EB8BDF508DDAA6CC2353B
    @Disabled()
    @Test()
    void getShortCanonicalName2Test() {
        //TODO: Please change the modifier of getCanonicalName from private to default to isolate the test case scenario
        //Act Statement(s)
        //Assert statement(s)
        //classUtils.when(() -> ClassUtils.getCanonicalName("canonicalName1")).thenReturn("return_of_getCanonicalName1");
        //classUtils.when(() -> ClassUtils.getShortClassName("return_of_getCanonicalName1")).thenReturn("return_of_getShortClassName1");
        //String result = ClassUtils.getShortCanonicalName("canonicalName1");
        //assertAll("result", () -> {    assertThat(result, equalTo("return_of_getShortClassName1"));    classUtils.verify(() -> ClassUtils.getShortClassName("return_of_getCanonicalName1"), atLeast(1));});
    }

    //BaseRock generated method id: ${getShortClassNameWhenClsIsNull}, hash: 2488D8C1C29EF868F8CB267263BCE88E
    @Test()
    void getShortClassNameWhenClsIsNull() {
        /* Branches:
         * (cls == null) : true
         */
         //Arrange Statement(s)
        Class<?> _class = null;
        
        //Act Statement(s)
        String result = ClassUtils.getShortClassName(_class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${getShortClassNameWhenClsIsNotNull}, hash: B3BA73AB88B3CDC7D3202C9F6195407A
    @Test()
    void getShortClassNameWhenClsIsNotNull() {
        /* Branches:
         * (cls == null) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.getShortClassName("java.lang.Object")).thenReturn("return_of_getShortClassName1");
            //Act Statement(s)
            String result = ClassUtils.getShortClassName(Object.class);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getShortClassName1"));
                classUtils.verify(() -> ClassUtils.getShortClassName("java.lang.Object"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getShortClassName1WhenObjectIsNull}, hash: 1B86AA7A69D9D9FC4C2D0E2D64C47290
    @Test()
    void getShortClassName1WhenObjectIsNull() {
        /* Branches:
         * (object == null) : true
         */
         //Arrange Statement(s)
        Object object = null;
        
        //Act Statement(s)
        String result = ClassUtils.getShortClassName(object, "valueIfNull1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("valueIfNull1")));
    }

    //BaseRock generated method id: ${getShortClassName1WhenObjectIsNotNull}, hash: 4FBA3F8E8A1C4C57DB5C059F421E67C7
    @Test()
    void getShortClassName1WhenObjectIsNotNull() {
        /* Branches:
         * (object == null) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.getShortClassName(Object.class)).thenReturn("return_of_getShortClassName1");
            Object object = new Object();
            //Act Statement(s)
            String result = ClassUtils.getShortClassName(object, "valueIfNull1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getShortClassName1"));
                classUtils.verify(() -> ClassUtils.getShortClassName(Object.class), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getShortClassName2WhenStringUtilsIsEmptyClassName}, hash: 05549E87212711555E6BFBEB0D0322DD
    @Test()
    void getShortClassName2WhenStringUtilsIsEmptyClassName() {
        /* Branches:
         * (StringUtils.isEmpty(className)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        String result = ClassUtils.getShortClassName("java.lang.String");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${getShortClassName2WhenReverseAbbreviationMapContainsKeyClassNameAndLastDotIdxEqualsMinus1AndInnerIdxNotEqualsMinus1}, hash: 2BC721439A4BCDF28E1E5A2A622B3A31
    @Test()
    void getShortClassName2WhenReverseAbbreviationMapContainsKeyClassNameAndLastDotIdxEqualsMinus1AndInnerIdxNotEqualsMinus1() {
        /* Branches:
         * (StringUtils.isEmpty(className)) : false
         * (className.startsWith("[")) : true
         * (className.charAt(0) == '[') : true
         * (className.charAt(0) == 'L') : true
         * (className.charAt(className.length() - 1) == ';') : true
         * (reverseAbbreviationMap.containsKey(className)) : true
         * (lastDotIdx == -1) : true
         * (innerIdx != -1) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        String result = ClassUtils.getShortClassName("java.lang.String");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${getShortClassName2WhenLastDotIdxNotEqualsMinus1AndInnerIdxEqualsMinus1}, hash: 92AC806A5E0C520853BB44303DF67B88
    @Test()
    void getShortClassName2WhenLastDotIdxNotEqualsMinus1AndInnerIdxEqualsMinus1() {
        /* Branches:
         * (StringUtils.isEmpty(className)) : false
         * (className.startsWith("[")) : true
         * (className.charAt(0) == '[') : true
         * (className.charAt(0) == 'L') : true
         * (className.charAt(className.length() - 1) == ';') : true
         * (reverseAbbreviationMap.containsKey(className)) : true
         * (lastDotIdx == -1) : false
         * (innerIdx != -1) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        String result = ClassUtils.getShortClassName("java.lang.String");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${getSimpleNameTest}, hash: E4DF145463DC1F27C1D49D9D0EFF8ED4
    @Test()
    void getSimpleNameTest() {
        //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.getSimpleName(Object.class, "")).thenReturn("return_of_getSimpleName1");
            //Act Statement(s)
            String result = ClassUtils.getSimpleName(Object.class);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getSimpleName1"));
                classUtils.verify(() -> ClassUtils.getSimpleName(Object.class, ""), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getSimpleName1WhenClsIsNull}, hash: 71C2B76B5453A3BFDA63D5BCC1269AB2
    @Test()
    void getSimpleName1WhenClsIsNull() {
        /* Branches:
         * (cls == null) : true
         */
         //Arrange Statement(s)
        Class<?> _class = null;
        
        //Act Statement(s)
        String result = ClassUtils.getSimpleName(_class, "valueIfNull1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("valueIfNull1")));
    }

    //BaseRock generated method id: ${getSimpleName1WhenClsIsNotNull}, hash: 4D7EE35D06311A205898A9BD32EC5122
    @Test()
    void getSimpleName1WhenClsIsNotNull() {
        /* Branches:
         * (cls == null) : false
         */
         
        //Act Statement(s)
        String result = ClassUtils.getSimpleName(Object.class, "valueIfNull1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("Object")));
    }

    //BaseRock generated method id: ${getSimpleName2Test}, hash: 0483E83AF51CB2A057E1F1733AFE9875
    @Test()
    void getSimpleName2Test() {
        //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            classUtils.when(() -> ClassUtils.getSimpleName(object, "")).thenReturn("return_of_getSimpleName1");
            //Act Statement(s)
            String result = ClassUtils.getSimpleName(object);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_getSimpleName1"));
                classUtils.verify(() -> ClassUtils.getSimpleName(object, ""), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getSimpleName3WhenObjectIsNull}, hash: 4643E7F7DCB866F1195AEC06735975B5
    @Test()
    void getSimpleName3WhenObjectIsNull() {
        /* Branches:
         * (object == null) : true
         */
         //Arrange Statement(s)
        Object object = null;
        
        //Act Statement(s)
        String result = ClassUtils.getSimpleName(object, "valueIfNull1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("valueIfNull1")));
    }

    //BaseRock generated method id: ${getSimpleName3WhenObjectIsNotNull}, hash: 439F7C4BB474BD1E8337A26FA62BFD9C
    @Test()
    void getSimpleName3WhenObjectIsNotNull() {
        /* Branches:
         * (object == null) : false
         */
         //Arrange Statement(s)
        Object object = new Object();
        
        //Act Statement(s)
        String result = ClassUtils.getSimpleName(object, "valueIfNull1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("Object")));
    }

    //BaseRock generated method id: ${hierarchyTest}, hash: 324D9AAD4138A7B9EFFB0D3DD4EF4EE7
    @Test()
    void hierarchyTest() {
        //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            Iterable<Class<?>> iterable = new ArrayList<>();
            classUtils.when(() -> ClassUtils.hierarchy(Object.class, ClassUtils.Interfaces.EXCLUDE)).thenReturn(iterable);
            //Act Statement(s)
            Iterable<Class<?>> result = ClassUtils.hierarchy(Object.class);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(iterable));
                classUtils.verify(() -> ClassUtils.hierarchy(Object.class, ClassUtils.Interfaces.EXCLUDE), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${hierarchy1WhenInterfacesBehaviorNotEqualsInterfacesINCLUDE}, hash: 8516757A195FF361638FDAAAAF390A80
    @Test()
    void hierarchy1WhenInterfacesBehaviorNotEqualsInterfacesINCLUDE() {
        /* Branches:
         * (interfacesBehavior != Interfaces.INCLUDE) : true
         */
         
        //Act Statement(s)
        Iterable<Class<?>> result = ClassUtils.hierarchy(Object.class, ClassUtils.Interfaces.EXCLUDE);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${hierarchy1WhenInterfacesBehaviorEqualsInterfacesINCLUDE}, hash: D067251F8057ED037EE859E7F707EF0A
    @Test()
    void hierarchy1WhenInterfacesBehaviorEqualsInterfacesINCLUDE() {
        /* Branches:
         * (interfacesBehavior != Interfaces.INCLUDE) : false
         */
         
        //Act Statement(s)
        Iterable<Class<?>> result = ClassUtils.hierarchy(Object.class, ClassUtils.Interfaces.INCLUDE);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${isAssignableWhenIsAssignableClsToClassTrue}, hash: E0102D56B365F7A92717F9C7147A48AB
    @Test()
    void isAssignableWhenIsAssignableClsToClassTrue() {
        /* Branches:
         * (isAssignable(cls, toClass, true)) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.isAssignable(Object.class, Object.class, true)).thenReturn(true);
            //Act Statement(s)
            boolean result = ClassUtils.isAssignable(Object.class, Object.class);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                classUtils.verify(() -> ClassUtils.isAssignable(Object.class, Object.class, true), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAssignableWhenIsAssignableNotClsToClassTrue}, hash: 116EB4236949FF433BB2F81032255482
    @Test()
    void isAssignableWhenIsAssignableNotClsToClassTrue() {
        /* Branches:
         * (isAssignable(cls, toClass, true)) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.isAssignable(Object.class, Object.class, true)).thenReturn(false);
            //Act Statement(s)
            boolean result = ClassUtils.isAssignable(Object.class, Object.class);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                classUtils.verify(() -> ClassUtils.isAssignable(Object.class, Object.class, true), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAssignable1WhenToClassIsNull}, hash: 97E4F4362A5F77A1BDE0A0DB824C6500
    @Test()
    void isAssignable1WhenToClassIsNull() {
        /* Branches:
         * (toClass == null) : true
         */
         //Arrange Statement(s)
        Class<?> _class = null;
        
        //Act Statement(s)
        boolean result = ClassUtils.isAssignable(Object.class, _class, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAssignable1WhenToClassNotIsPrimitive}, hash: EA4CD1F930F43C430B3882F5E18A89A9
    @Test()
    void isAssignable1WhenToClassNotIsPrimitive() {
        /* Branches:
         * (toClass == null) : false
         * (cls == null) : true
         * (!toClass.isPrimitive()) : true
         */
         //Arrange Statement(s)
        Class<?> _class = null;
        
        //Act Statement(s)
        boolean result = ClassUtils.isAssignable(_class, Object.class, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAssignable1WhenToClassIsPrimitive}, hash: 4ED82AA0672B37014AF9C4636EA095BC
    @Test()
    void isAssignable1WhenToClassIsPrimitive() {
        /* Branches:
         * (toClass == null) : false
         * (cls == null) : true
         * (!toClass.isPrimitive()) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Class<?> _class = null;
        
        //Act Statement(s)
        boolean result = ClassUtils.isAssignable(_class, Object.class, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAssignable1WhenClsIsNull}, hash: 3E3E0AAD0FA4D36172E5624AB11430DE
    @Test()
    void isAssignable1WhenClsIsNull() {
        /* Branches:
         * (toClass == null) : false
         * (cls == null) : false
         * (autoboxing) : true
         * (cls.isPrimitive()) : true
         * (!toClass.isPrimitive()) : true
         * (cls == null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.primitiveToWrapper(Object.class)).thenReturn(null);
            //Act Statement(s)
            boolean result = ClassUtils.isAssignable(Object.class, Object.class, true);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                classUtils.verify(() -> ClassUtils.primitiveToWrapper(Object.class), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAssignable1WhenClsNotIsPrimitiveAndClsIsNull}, hash: 22EA61A80C63EE3CE29D3618996B7263
    @Test()
    void isAssignable1WhenClsNotIsPrimitiveAndClsIsNull() {
        /* Branches:
         * (toClass == null) : false
         * (cls == null) : false
         * (autoboxing) : true
         * (cls.isPrimitive()) : true
         * (!toClass.isPrimitive()) : true
         * (cls == null) : false
         * (toClass.isPrimitive()) : true
         * (!cls.isPrimitive()) : true
         * (cls == null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.primitiveToWrapper(Object.class)).thenReturn(Object.class);
            //Act Statement(s)
            boolean result = ClassUtils.isAssignable(Object.class, Object.class, true);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                classUtils.verify(() -> ClassUtils.primitiveToWrapper(Object.class), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAssignable1WhenClsIsNotNullAndClsEqualsToClass}, hash: C4CE534E772DD0F3980215E979AA8E69
    @Test()
    void isAssignable1WhenClsIsNotNullAndClsEqualsToClass() {
        /* Branches:
         * (toClass == null) : false
         * (cls == null) : false
         * (autoboxing) : true
         * (cls.isPrimitive()) : true
         * (!toClass.isPrimitive()) : true
         * (cls == null) : false
         * (toClass.isPrimitive()) : true
         * (!cls.isPrimitive()) : true
         * (cls == null) : false
         * (cls.equals(toClass)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.primitiveToWrapper(Object.class)).thenReturn(Object.class);
            //Act Statement(s)
            boolean result = ClassUtils.isAssignable(Object.class, Object.class, true);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                classUtils.verify(() -> ClassUtils.primitiveToWrapper(Object.class), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAssignable1WhenClsNotEqualsToClassAndClsIsPrimitiveAndToClassNotIsPrimitive}, hash: 0FBE9AD3F7EEF91D4E3B288C021983E5
    @Test()
    void isAssignable1WhenClsNotEqualsToClassAndClsIsPrimitiveAndToClassNotIsPrimitive() {
        /* Branches:
         * (toClass == null) : false
         * (cls == null) : false
         * (autoboxing) : true
         * (cls.isPrimitive()) : true
         * (!toClass.isPrimitive()) : true
         * (cls == null) : false
         * (toClass.isPrimitive()) : true
         * (!cls.isPrimitive()) : true
         * (cls == null) : false
         * (cls.equals(toClass)) : false
         * (cls.isPrimitive()) : true
         * (!toClass.isPrimitive()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.primitiveToWrapper(Object.class)).thenReturn(Object.class);
            //Act Statement(s)
            boolean result = ClassUtils.isAssignable(Object.class, Object.class, true);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                classUtils.verify(() -> ClassUtils.primitiveToWrapper(Object.class), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAssignable1WhenClsNotIsPrimitiveAndToClassIsAssignableFromCls}, hash: B1295034767E6DC20944FE32F953C2D9
    @Test()
    void isAssignable1WhenClsNotIsPrimitiveAndToClassIsAssignableFromCls() {
        /* Branches:
         * (toClass == null) : false
         * (cls == null) : false
         * (autoboxing) : true
         * (cls.isPrimitive()) : true
         * (!toClass.isPrimitive()) : true
         * (cls == null) : false
         * (toClass.isPrimitive()) : true
         * (!cls.isPrimitive()) : true
         * (cls == null) : false
         * (cls.equals(toClass)) : false
         * (cls.isPrimitive()) : false
         * (toClass.isAssignableFrom(cls)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.primitiveToWrapper(Object.class)).thenReturn(Object.class);
            //Act Statement(s)
            boolean result = ClassUtils.isAssignable(Object.class, Object.class, true);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                classUtils.verify(() -> ClassUtils.primitiveToWrapper(Object.class), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAssignable1WhenClsNotIsPrimitiveAndToClassNotIsAssignableFromCls}, hash: A8D624944B59D29890AD379E094AEEFA
    @Test()
    void isAssignable1WhenClsNotIsPrimitiveAndToClassNotIsAssignableFromCls() {
        /* Branches:
         * (toClass == null) : false
         * (cls == null) : false
         * (autoboxing) : true
         * (cls.isPrimitive()) : true
         * (!toClass.isPrimitive()) : true
         * (cls == null) : false
         * (toClass.isPrimitive()) : true
         * (!cls.isPrimitive()) : true
         * (cls == null) : false
         * (cls.equals(toClass)) : false
         * (cls.isPrimitive()) : false
         * (toClass.isAssignableFrom(cls)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.primitiveToWrapper(Object.class)).thenReturn(Object.class);
            //Act Statement(s)
            boolean result = ClassUtils.isAssignable(Object.class, Object.class, true);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                classUtils.verify(() -> ClassUtils.primitiveToWrapper(Object.class), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAssignable1WhenDoubleTYPEEqualsToClass}, hash: A0469498956BC6C4C3E1E28958B82F90
    @Test()
    void isAssignable1WhenDoubleTYPEEqualsToClass() {
        /* Branches:
         * (toClass == null) : false
         * (cls == null) : false
         * (autoboxing) : true
         * (cls.isPrimitive()) : true
         * (!toClass.isPrimitive()) : true
         * (cls == null) : false
         * (toClass.isPrimitive()) : true
         * (!cls.isPrimitive()) : false
         * (cls.equals(toClass)) : false
         * (cls.isPrimitive()) : true
         * (!toClass.isPrimitive()) : false
         * (Integer.TYPE.equals(cls)) : true
         * (Long.TYPE.equals(toClass)) : false
         * (Float.TYPE.equals(toClass)) : false
         * (Double.TYPE.equals(toClass)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.primitiveToWrapper(Object.class)).thenReturn(Object.class);
            //Act Statement(s)
            boolean result = ClassUtils.isAssignable(Object.class, Object.class, true);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                classUtils.verify(() -> ClassUtils.primitiveToWrapper(Object.class), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAssignable1WhenFloatTYPEEqualsClsAndDoubleTYPEEqualsToClass}, hash: 4D89F6BB7091B905D243BE65F6BBD9AA
    @Test()
    void isAssignable1WhenFloatTYPEEqualsClsAndDoubleTYPEEqualsToClass() {
        /* Branches:
         * (toClass == null) : false
         * (cls == null) : false
         * (autoboxing) : true
         * (cls.isPrimitive()) : true
         * (!toClass.isPrimitive()) : true
         * (cls == null) : false
         * (toClass.isPrimitive()) : true
         * (!cls.isPrimitive()) : false
         * (cls.equals(toClass)) : false
         * (cls.isPrimitive()) : true
         * (!toClass.isPrimitive()) : false
         * (Integer.TYPE.equals(cls)) : false
         * (Long.TYPE.equals(cls)) : false
         * (Boolean.TYPE.equals(cls)) : false
         * (Double.TYPE.equals(cls)) : false
         * (Float.TYPE.equals(cls)) : true
         * (Double.TYPE.equals(toClass)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.primitiveToWrapper(Object.class)).thenReturn(Object.class);
            //Act Statement(s)
            boolean result = ClassUtils.isAssignable(Object.class, Object.class, true);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                classUtils.verify(() -> ClassUtils.primitiveToWrapper(Object.class), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAssignable1WhenDoubleTYPENotEqualsToClass}, hash: 180411CFCA566A2D4C7E6C5093496554
    @Test()
    void isAssignable1WhenDoubleTYPENotEqualsToClass() {
        /* Branches:
         * (toClass == null) : false
         * (cls == null) : false
         * (autoboxing) : true
         * (cls.isPrimitive()) : true
         * (!toClass.isPrimitive()) : true
         * (cls == null) : false
         * (toClass.isPrimitive()) : true
         * (!cls.isPrimitive()) : false
         * (cls.equals(toClass)) : false
         * (cls.isPrimitive()) : true
         * (!toClass.isPrimitive()) : false
         * (Integer.TYPE.equals(cls)) : false
         * (Long.TYPE.equals(cls)) : false
         * (Boolean.TYPE.equals(cls)) : false
         * (Double.TYPE.equals(cls)) : false
         * (Float.TYPE.equals(cls)) : true
         * (Double.TYPE.equals(toClass)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.primitiveToWrapper(Object.class)).thenReturn(Object.class);
            //Act Statement(s)
            boolean result = ClassUtils.isAssignable(Object.class, Object.class, true);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                classUtils.verify(() -> ClassUtils.primitiveToWrapper(Object.class), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAssignable2WhenIsAssignableClassArrayToClassArrayTrue}, hash: 33DE2D1FE7ABD6D6763FD2731ED29031
    @Test()
    void isAssignable2WhenIsAssignableClassArrayToClassArrayTrue() {
        /* Branches:
         * (isAssignable(classArray, toClassArray, true)) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            Class<?>[] classArray = new Class[] {};
            Class<?>[] classArray2 = new Class[] {};
            classUtils.when(() -> ClassUtils.isAssignable(classArray, classArray2, true)).thenReturn(true);
            //Act Statement(s)
            boolean result = ClassUtils.isAssignable(classArray, classArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                classUtils.verify(() -> ClassUtils.isAssignable(classArray, classArray2, true), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAssignable2WhenIsAssignableNotClassArrayToClassArrayTrue}, hash: C7DE7FA8F699FED586D7715841B61376
    @Test()
    void isAssignable2WhenIsAssignableNotClassArrayToClassArrayTrue() {
        /* Branches:
         * (isAssignable(classArray, toClassArray, true)) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            Class<?>[] classArray = new Class[] {};
            Class<?>[] classArray2 = new Class[] {};
            classUtils.when(() -> ClassUtils.isAssignable(classArray, classArray2, true)).thenReturn(false);
            //Act Statement(s)
            boolean result = ClassUtils.isAssignable(classArray, classArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                classUtils.verify(() -> ClassUtils.isAssignable(classArray, classArray2, true), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAssignable3WhenArrayUtilsNotIsSameLengthClassArrayToClassArray}, hash: DE87FCB020676BDDEE6A76B215EF0040
    @Test()
    void isAssignable3WhenArrayUtilsNotIsSameLengthClassArrayToClassArray() {
        /* Branches:
         * (!ArrayUtils.isSameLength(classArray, toClassArray)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Class<?>[] classArray = new Class[] {};
        Class<?>[] classArray2 = new Class[] {};
        
        //Act Statement(s)
        boolean result = ClassUtils.isAssignable(classArray, classArray2, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAssignable3WhenIsAssignableNotIIndexOfClassArrayIIndexOfToClassArrayAutoboxing}, hash: AE99132D3D9E2ADB11ABEE206E129BB8
    @Test()
    void isAssignable3WhenIsAssignableNotIIndexOfClassArrayIIndexOfToClassArrayAutoboxing() {
        /* Branches:
         * (!ArrayUtils.isSameLength(classArray, toClassArray)) : false
         * (i < classArray.length) : true
         * (!isAssignable(classArray[i], toClassArray[i], autoboxing)) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.isAssignable(Object.class, Object.class, false)).thenReturn(false);
            Class<?>[] classArray = new Class[] {};
            Class<?>[] classArray2 = new Class[] {};
            //Act Statement(s)
            boolean result = ClassUtils.isAssignable(classArray, classArray2, false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                classUtils.verify(() -> ClassUtils.isAssignable(Object.class, Object.class, false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isAssignable3WhenIsAssignableIIndexOfClassArrayIIndexOfToClassArrayAutoboxing}, hash: 9BC88D7D5C08573B72E059CB438F2285
    @Test()
    void isAssignable3WhenIsAssignableIIndexOfClassArrayIIndexOfToClassArrayAutoboxing() {
        /* Branches:
         * (!ArrayUtils.isSameLength(classArray, toClassArray)) : false
         * (i < classArray.length) : true
         * (!isAssignable(classArray[i], toClassArray[i], autoboxing)) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.isAssignable(Object.class, Object.class, false)).thenReturn(true);
            Class<?>[] classArray = new Class[] {};
            Class<?>[] classArray2 = new Class[] {};
            //Act Statement(s)
            boolean result = ClassUtils.isAssignable(classArray, classArray2, false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                classUtils.verify(() -> ClassUtils.isAssignable(Object.class, Object.class, false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isInnerClassWhenClsGetEnclosingClassIsNotNull}, hash: 03E8CCDFF74E6A6992CDB215E81B0A81
    @Test()
    void isInnerClassWhenClsGetEnclosingClassIsNotNull() {
        /* Branches:
         * (cls != null) : true
         * (cls.getEnclosingClass() != null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = ClassUtils.isInnerClass(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isInnerClassWhenClsGetEnclosingClassIsNull}, hash: 22F45A1978B04EF0BAA177748FE8CD1A
    @Test()
    void isInnerClassWhenClsGetEnclosingClassIsNull() {
        /* Branches:
         * (cls != null) : true
         * (cls.getEnclosingClass() != null) : false
         */
         
        //Act Statement(s)
        boolean result = ClassUtils.isInnerClass(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isPrimitiveOrWrapperWhenTypeIsNull}, hash: 1564113848388C04CD7466FA7A427E04
    @Test()
    void isPrimitiveOrWrapperWhenTypeIsNull() {
        /* Branches:
         * (type == null) : true
         */
         //Arrange Statement(s)
        Class<?> _class = null;
        
        //Act Statement(s)
        boolean result = ClassUtils.isPrimitiveOrWrapper(_class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isPrimitiveOrWrapperWhenIsPrimitiveWrapperType}, hash: 6CD341B0F61D9449ED00D8A58FF3B04C
    @Test()
    void isPrimitiveOrWrapperWhenIsPrimitiveWrapperType() {
        /* Branches:
         * (type == null) : false
         * (type.isPrimitive()) : false
         * (isPrimitiveWrapper(type)) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.isPrimitiveWrapper(Object.class)).thenReturn(true);
            //Act Statement(s)
            boolean result = ClassUtils.isPrimitiveOrWrapper(Object.class);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                classUtils.verify(() -> ClassUtils.isPrimitiveWrapper(Object.class), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isPrimitiveOrWrapperWhenIsPrimitiveWrapperNotType}, hash: 1F0F8E1FB40463949C809BC20DC97E79
    @Test()
    void isPrimitiveOrWrapperWhenIsPrimitiveWrapperNotType() {
        /* Branches:
         * (type == null) : false
         * (type.isPrimitive()) : false
         * (isPrimitiveWrapper(type)) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.isPrimitiveWrapper(Object.class)).thenReturn(false);
            //Act Statement(s)
            boolean result = ClassUtils.isPrimitiveOrWrapper(Object.class);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                classUtils.verify(() -> ClassUtils.isPrimitiveWrapper(Object.class), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isPrimitiveWrapperWhenWrapperPrimitiveMapContainsKeyType}, hash: 4EE985EAEDC0D9907888B734D9B63136
    @Test()
    void isPrimitiveWrapperWhenWrapperPrimitiveMapContainsKeyType() {
        /* Branches:
         * (wrapperPrimitiveMap.containsKey(type)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = ClassUtils.isPrimitiveWrapper(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isPrimitiveWrapperWhenWrapperPrimitiveMapNotContainsKeyType}, hash: 049137F3C9624FB38C21146C09807692
    @Test()
    void isPrimitiveWrapperWhenWrapperPrimitiveMapNotContainsKeyType() {
        /* Branches:
         * (wrapperPrimitiveMap.containsKey(type)) : false
         */
         
        //Act Statement(s)
        boolean result = ClassUtils.isPrimitiveWrapper(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isPublicWhenModifierIsPublicClsGetModifiers}, hash: 27B0367341974FF4DA6DF209354D9E87
    @Test()
    void isPublicWhenModifierIsPublicClsGetModifiers() {
        /* Branches:
         * (Modifier.isPublic(cls.getModifiers())) : true
         */
         
        //Act Statement(s)
        boolean result = ClassUtils.isPublic(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isPublicWhenModifierNotIsPublicClsGetModifiers}, hash: 23B8CD6FAB091AE5EF8F79C0E084B51E
    @Test()
    void isPublicWhenModifierNotIsPublicClsGetModifiers() {
        /* Branches:
         * (Modifier.isPublic(cls.getModifiers())) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = ClassUtils.isPublic(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${primitivesToWrappersWhenClassesIsNull}, hash: 45C1CBFD4A8C73ACEB8FB78DD5D1DB0D
    @Test()
    void primitivesToWrappersWhenClassesIsNull() {
        /* Branches:
         * (classes == null) : true
         */
         //Arrange Statement(s)
        Class<?>[] _class = null;
        
        //Act Statement(s)
        Class<?>[] result = ClassUtils.primitivesToWrappers(_class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${primitivesToWrappersWhenClassesLengthEquals0}, hash: 5FDC0E6417DBCC1C546612C349D7928D
    @Test()
    void primitivesToWrappersWhenClassesLengthEquals0() {
        /* Branches:
         * (classes == null) : false
         * (classes.length == 0) : true
         */
         //Arrange Statement(s)
        Class<?>[] classArray = new Class[] {};
        
        //Act Statement(s)
        Class<?>[] result = ClassUtils.primitivesToWrappers(classArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(classArray)));
    }

    //BaseRock generated method id: ${primitivesToWrappersWhenClassesLengthNotEquals0}, hash: DD19E18DEDEBE2F430BE096205E20AF2
    @Test()
    void primitivesToWrappersWhenClassesLengthNotEquals0() {
        /* Branches:
         * (classes == null) : false
         * (classes.length == 0) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<ClassUtils> classUtils = mockStatic(ClassUtils.class, CALLS_REAL_METHODS)) {
            classUtils.when(() -> ClassUtils.primitiveToWrapper(Object.class)).thenReturn(Object.class);
            Class<?>[] classArray = new Class[] { Object.class };
            //Act Statement(s)
            Class<?>[] result = ClassUtils.primitivesToWrappers(classArray);
            Class<?>[] classResultArray = new Class[] { (Class) null };
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(classResultArray));
                classUtils.verify(() -> ClassUtils.primitiveToWrapper(Object.class), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${primitiveToWrapperWhenClsIsPrimitive}, hash: D793BDF110F09ADCF1B9B3FCEE0A2407
    @Test()
    void primitiveToWrapperWhenClsIsPrimitive() {
        /* Branches:
         * (cls != null) : true
         * (cls.isPrimitive()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Class<?> result = ClassUtils.primitiveToWrapper(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Object.class)));
    }

    //BaseRock generated method id: ${toClassWhenArrayIsNull}, hash: DB30F0D0E55FE43F19E9D4AAAE661569
    @Test()
    void toClassWhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
         //Arrange Statement(s)
        Object[] object = null;
        
        //Act Statement(s)
        Class<?>[] result = ClassUtils.toClass(object);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toClassWhenArrayLengthEquals0}, hash: C4707F2BADB5F9B41CFDBAF90A321344
    @Test()
    void toClassWhenArrayLengthEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : true
         */
         //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        
        //Act Statement(s)
        Class<?>[] result = ClassUtils.toClass(objectArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toClassWhenIIndexOfArrayIsNull}, hash: EF73909CFE6E42C8BE915B24EC9CD450
    @Test()
    void toClassWhenIIndexOfArrayIsNull() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         * (array[i] == null) : true  #  inside lambda$toClass$7 method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object[] objectArray = new Object[] { (Object) null };
        
        //Act Statement(s)
        Class<?>[] result = ClassUtils.toClass(objectArray);
        Class<?>[] classResultArray = new Class[] { (Class) null };
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(classResultArray)));
    }

    //BaseRock generated method id: ${toClassWhenIIndexOfArrayIsNotNull}, hash: BE3B577483226A8440CE81C67715B23C
    @Test()
    void toClassWhenIIndexOfArrayIsNotNull() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         * (array[i] == null) : false  #  inside lambda$toClass$7 method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        Object[] objectArray = new Object[] { object };
        
        //Act Statement(s)
        Class<?>[] result = ClassUtils.toClass(objectArray);
        Class<?>[] classResultArray = new Class[] { (Class) null };
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(classResultArray)));
    }

    //BaseRock generated method id: ${wrappersToPrimitivesWhenClassesIsNull}, hash: F572F080A3B9D8A31762635E2A448A53
    @Test()
    void wrappersToPrimitivesWhenClassesIsNull() {
        /* Branches:
         * (classes == null) : true
         */
         //Arrange Statement(s)
        Class<?>[] _class = null;
        
        //Act Statement(s)
        Class<?>[] result = ClassUtils.wrappersToPrimitives(_class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${wrappersToPrimitivesWhenClassesLengthEquals0}, hash: 74960CD403AA73B644707194090B9943
    @Test()
    void wrappersToPrimitivesWhenClassesLengthEquals0() {
        /* Branches:
         * (classes == null) : false
         * (classes.length == 0) : true
         */
         //Arrange Statement(s)
        Class<?>[] classArray = new Class[] {};
        
        //Act Statement(s)
        Class<?>[] result = ClassUtils.wrappersToPrimitives(classArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(classArray)));
    }

    //BaseRock generated method id: ${wrappersToPrimitivesWhenClassesLengthNotEquals0}, hash: 6295755271554EE4EBDC4E1EB2466C56
    @Test()
    void wrappersToPrimitivesWhenClassesLengthNotEquals0() {
        /* Branches:
         * (classes == null) : false
         * (classes.length == 0) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Class<?>[] classArray = new Class[] { Object.class };
        
        //Act Statement(s)
        Class<?>[] result = ClassUtils.wrappersToPrimitives(classArray);
        Class<?>[] classResultArray = new Class[] { (Class) null };
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(classResultArray)));
    }

    //BaseRock generated method id: ${wrapperToPrimitiveTest}, hash: 004F2A352B9EF53EB0BDC0D00F07D0E2
    @Test()
    void wrapperToPrimitiveTest() {
        
        //Act Statement(s)
        Class<?> result = ClassUtils.wrapperToPrimitive(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Object.class)));
    }
}
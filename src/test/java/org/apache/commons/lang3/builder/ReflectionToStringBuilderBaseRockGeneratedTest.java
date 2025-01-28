package org.apache.commons.lang3.builder;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.lang.reflect.Field;
import java.util.Collection;
import org.mockito.MockedStatic;
import java.util.ArrayList;
import static org.mockito.Mockito.doNothing;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.mockito.Mockito.mockStatic;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class ReflectionToStringBuilderBaseRockGeneratedTest {

    private final Field fieldMock = mock(Field.class);

    //BaseRock generated method id: ${toNoNullStringArrayWhenCollectionIsNull}, hash: DC88C847AC569443DCFD163A248A7601
    @Test()
    void toNoNullStringArrayWhenCollectionIsNull() {
        /* Branches:
         * (collection == null) : true
         */
         //Arrange Statement(s)
        Collection<String> collection = null;
        
        //Act Statement(s)
        String[] result = ReflectionToStringBuilder.toNoNullStringArray(collection);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toNoNullStringArrayWhenCollectionIsNotNull}, hash: 211ACCA68390A3ADDF14D5A63129F421
    @Test()
    void toNoNullStringArrayWhenCollectionIsNotNull() {
        /* Branches:
         * (collection == null) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Collection<String> collection = new ArrayList<>();
        
        //Act Statement(s)
        String[] result = ReflectionToStringBuilder.toNoNullStringArray(collection);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toNoNullStringArray1Test}, hash: 6588E84936A38BCF5FD636C2BF4781D3
    @Test()
    void toNoNullStringArray1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        
        //Act Statement(s)
        String[] result = ReflectionToStringBuilder.toNoNullStringArray(objectArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toString1Test}, hash: 46099A9CA7C85643916F0C0300941A1C
    @Test()
    void toString1Test() {
        //Arrange Statement(s)
        try (MockedStatic<ReflectionToStringBuilder> reflectionToStringBuilder = mockStatic(ReflectionToStringBuilder.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            reflectionToStringBuilder.when(() -> ReflectionToStringBuilder.toString(object, (ToStringStyle) null, false, false, (Class) null)).thenReturn("return_of_toString1");
            //Act Statement(s)
            String result = ReflectionToStringBuilder.toString(object);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_toString1"));
                reflectionToStringBuilder.verify(() -> ReflectionToStringBuilder.toString(object, (ToStringStyle) null, false, false, (Class) null), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toString2Test}, hash: 9EA52928ABEDA1B3CA3B2B131AFCE239
    @Test()
    void toString2Test() {
        //Arrange Statement(s)
        try (MockedStatic<ReflectionToStringBuilder> reflectionToStringBuilder = mockStatic(ReflectionToStringBuilder.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
            reflectionToStringBuilder.when(() -> ReflectionToStringBuilder.toString(object, standardToStringStyle, false, false, (Class) null)).thenReturn("return_of_toString1");
            //Act Statement(s)
            String result = ReflectionToStringBuilder.toString(object, standardToStringStyle);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_toString1"));
                reflectionToStringBuilder.verify(() -> ReflectionToStringBuilder.toString(object, standardToStringStyle, false, false, (Class) null), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toString3Test}, hash: 557E4155F4FFE2FFDA3AB5E7B7A3A1CE
    @Test()
    void toString3Test() {
        //Arrange Statement(s)
        try (MockedStatic<ReflectionToStringBuilder> reflectionToStringBuilder = mockStatic(ReflectionToStringBuilder.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
            reflectionToStringBuilder.when(() -> ReflectionToStringBuilder.toString(object, standardToStringStyle, false, false, (Class) null)).thenReturn("return_of_toString1");
            //Act Statement(s)
            String result = ReflectionToStringBuilder.toString(object, standardToStringStyle, false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_toString1"));
                reflectionToStringBuilder.verify(() -> ReflectionToStringBuilder.toString(object, standardToStringStyle, false, false, (Class) null), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toString4Test}, hash: FCAC5B95E1625E7C51A551AAA8C47FFF
    @Test()
    void toString4Test() {
        //Arrange Statement(s)
        try (MockedStatic<ReflectionToStringBuilder> reflectionToStringBuilder = mockStatic(ReflectionToStringBuilder.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
            reflectionToStringBuilder.when(() -> ReflectionToStringBuilder.toString(object, standardToStringStyle, false, false, (Class) null)).thenReturn("return_of_toString1");
            //Act Statement(s)
            String result = ReflectionToStringBuilder.toString(object, standardToStringStyle, false, false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_toString1"));
                reflectionToStringBuilder.verify(() -> ReflectionToStringBuilder.toString(object, standardToStringStyle, false, false, (Class) null), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toString5WhenDefaultBranchThrowsNullPointerException}, hash: A855155D065B6B99715D082249CCB647
    @Test()
    void toString5WhenDefaultBranchThrowsNullPointerException() {
        /* Branches:
         * (branch expression (line 246)) : false  #  inside <init> method
         * (branch expression (line 249)) : false  #  inside <init> method
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringStyle toStringStyle = null;
        //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            ReflectionToStringBuilder.toString(object, toStringStyle, false, false, false, Object.class);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${toString5WhenClazzNotIsInstanceObjectThrowsIllegalArgumentException}, hash: FB4FA265FB14F3AB02CF6D81EE849D55
    @Test()
    void toString5WhenClazzNotIsInstanceObjectThrowsIllegalArgumentException() {
        /* Branches:
         * (branch expression (line 246)) : false  #  inside <init> method
         * (branch expression (line 249)) : false  #  inside <init> method
         * (clazz != null) : true  #  inside setUpToClass method
         * (object != null) : true  #  inside setUpToClass method
         * (!clazz.isInstance(object)) : true  #  inside setUpToClass method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Specified class is not a superclass of the object");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            ReflectionToStringBuilder.toString(object, standardToStringStyle, false, false, false, Object.class);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${toString5WhenClazzIsInstanceObject}, hash: 724C4F9BF589677667679EA002DEA74A
    @Test()
    void toString5WhenClazzIsInstanceObject() {
        /* Branches:
         * (branch expression (line 246)) : false  #  inside <init> method
         * (branch expression (line 249)) : false  #  inside <init> method
         * (clazz != null) : true  #  inside setUpToClass method
         * (object != null) : true  #  inside setUpToClass method
         * (!clazz.isInstance(object)) : false  #  inside setUpToClass method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        
        //Act Statement(s)
        String result = ReflectionToStringBuilder.toString(object, standardToStringStyle, false, false, false, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${toString6WhenDefaultBranchThrowsNullPointerException}, hash: 799861D54FEC372BBB7EAD93CC7A797C
    @Test()
    void toString6WhenDefaultBranchThrowsNullPointerException() {
        /* Branches:
         * (branch expression (line 246)) : false  #  inside <init> method
         * (branch expression (line 249)) : false  #  inside <init> method
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringStyle toStringStyle = null;
        //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            ReflectionToStringBuilder.toString(object, toStringStyle, false, false, Object.class);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${toString6WhenClazzNotIsInstanceObjectThrowsIllegalArgumentException}, hash: 99B35E5282B02986C91BD1247667A27B
    @Test()
    void toString6WhenClazzNotIsInstanceObjectThrowsIllegalArgumentException() {
        /* Branches:
         * (branch expression (line 246)) : false  #  inside <init> method
         * (branch expression (line 249)) : false  #  inside <init> method
         * (clazz != null) : true  #  inside setUpToClass method
         * (object != null) : true  #  inside setUpToClass method
         * (!clazz.isInstance(object)) : true  #  inside setUpToClass method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Specified class is not a superclass of the object");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            ReflectionToStringBuilder.toString(object, standardToStringStyle, false, false, Object.class);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${toString6WhenClazzIsInstanceObject}, hash: A6297FB6B7F522B02E2E114AEB847CDB
    @Test()
    void toString6WhenClazzIsInstanceObject() {
        /* Branches:
         * (branch expression (line 246)) : false  #  inside <init> method
         * (branch expression (line 249)) : false  #  inside <init> method
         * (clazz != null) : true  #  inside setUpToClass method
         * (object != null) : true  #  inside setUpToClass method
         * (!clazz.isInstance(object)) : false  #  inside setUpToClass method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        
        //Act Statement(s)
        String result = ReflectionToStringBuilder.toString(object, standardToStringStyle, false, false, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${toStringExcludeTest}, hash: 7DA0BBCCF6B08188B864364CF70E8DAB
    @Test()
    void toStringExcludeTest() {
        //Arrange Statement(s)
        try (MockedStatic<ReflectionToStringBuilder> reflectionToStringBuilder = mockStatic(ReflectionToStringBuilder.class, CALLS_REAL_METHODS)) {
            String[] stringArray = new String[] {};
            reflectionToStringBuilder.when(() -> ReflectionToStringBuilder.toNoNullStringArray(anyCollection())).thenReturn(stringArray);
            Object object = new Object();
            reflectionToStringBuilder.when(() -> ReflectionToStringBuilder.toStringExclude(object, stringArray)).thenReturn("return_of_toStringExclude1");
            Collection<String> collection = new ArrayList<>();
            //Act Statement(s)
            String result = ReflectionToStringBuilder.toStringExclude(object, collection);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_toStringExclude1"));
                reflectionToStringBuilder.verify(() -> ReflectionToStringBuilder.toNoNullStringArray(anyCollection()), atLeast(1));
                reflectionToStringBuilder.verify(() -> ReflectionToStringBuilder.toStringExclude(object, stringArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toStringExclude1WhenDefaultBranchThrowsNullPointerException}, hash: 57377AC21E6D7D590998561B186D7F2F
    @Test()
    void toStringExclude1WhenDefaultBranchThrowsNullPointerException() {
        /* Branches:
         * (branch expression (line 246)) : false  #  inside <init> method
         * (branch expression (line 249)) : false  #  inside <init> method
         */
         //Arrange Statement(s)
        Object object = new Object();
        String[] stringArray = new String[] {};
        //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            ReflectionToStringBuilder.toStringExclude(object, stringArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${toStringIncludeTest}, hash: DF2A52818BFC0036716579AD1C14BF20
    @Test()
    void toStringIncludeTest() {
        //Arrange Statement(s)
        try (MockedStatic<ReflectionToStringBuilder> reflectionToStringBuilder = mockStatic(ReflectionToStringBuilder.class, CALLS_REAL_METHODS)) {
            String[] stringArray = new String[] {};
            reflectionToStringBuilder.when(() -> ReflectionToStringBuilder.toNoNullStringArray(anyCollection())).thenReturn(stringArray);
            Object object = new Object();
            reflectionToStringBuilder.when(() -> ReflectionToStringBuilder.toStringInclude(object, stringArray)).thenReturn("return_of_toStringInclude1");
            Collection<String> collection = new ArrayList<>();
            //Act Statement(s)
            String result = ReflectionToStringBuilder.toStringInclude(object, collection);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_toStringInclude1"));
                reflectionToStringBuilder.verify(() -> ReflectionToStringBuilder.toNoNullStringArray(anyCollection()), atLeast(1));
                reflectionToStringBuilder.verify(() -> ReflectionToStringBuilder.toStringInclude(object, stringArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toStringInclude1WhenDefaultBranchThrowsNullPointerException}, hash: 51AA7C9638CA9146C1C143813101C78C
    @Test()
    void toStringInclude1WhenDefaultBranchThrowsNullPointerException() {
        /* Branches:
         * (branch expression (line 246)) : false  #  inside <init> method
         * (branch expression (line 249)) : false  #  inside <init> method
         */
         //Arrange Statement(s)
        Object object = new Object();
        String[] stringArray = new String[] {};
        //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            ReflectionToStringBuilder.toStringInclude(object, stringArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${acceptWhenFieldGetNameIndexOfClassUtilsINNER_CLASS_SEPARATOR_CHARNotEqualsMinus1}, hash: 15214D0FA8E756AF25B29EFF745822FF
    @Test()
    void acceptWhenFieldGetNameIndexOfClassUtilsINNER_CLASS_SEPARATOR_CHARNotEqualsMinus1() {
        /* Branches:
         * (field.getName().indexOf(ClassUtils.INNER_CLASS_SEPARATOR_CHAR) != -1) : true
         */
         //Arrange Statement(s)
        doReturn("$").when(fieldMock).getName();
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, false, false, false);
        
        //Act Statement(s)
        boolean result = target.accept(fieldMock);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.FALSE));
            verify(fieldMock).getName();
        });
    }

    //BaseRock generated method id: ${acceptWhenIsAppendTransientsNot}, hash: 240A51E110AED3C4552CF168C881C1C0
    @Test()
    void acceptWhenIsAppendTransientsNot() {
        /* Branches:
         * (field.getName().indexOf(ClassUtils.INNER_CLASS_SEPARATOR_CHAR) != -1) : false
         * (Modifier.isTransient(field.getModifiers())) : true
         * (this.appendTransients) : false  #  inside isAppendTransients method
         * (!isAppendTransients()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, false, false, false);
        
        //Act Statement(s)
        boolean result = target.accept(fieldMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${acceptWhenThisNotAppendStaticsAndIsAppendStaticsNot}, hash: 896ED5A50123F56C54906923E95F7A8F
    @Test()
    void acceptWhenThisNotAppendStaticsAndIsAppendStaticsNot() {
        /* Branches:
         * (field.getName().indexOf(ClassUtils.INNER_CLASS_SEPARATOR_CHAR) != -1) : false
         * (Modifier.isTransient(field.getModifiers())) : true
         * (this.appendTransients) : true  #  inside isAppendTransients method
         * (!isAppendTransients()) : false
         * (Modifier.isStatic(field.getModifiers())) : true
         * (this.appendStatics) : false  #  inside isAppendStatics method
         * (!isAppendStatics()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, true, false, false);
        
        //Act Statement(s)
        boolean result = target.accept(fieldMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${acceptWhenThisExcludeFieldNamesIsNullAndArrayUtilsNotIsNotEmptyIncludeFieldNamesAndFieldNotIsAnnotationPresentToStringE}, hash: 22F3C5097AB68E899F5BA379A03BE385
    @Test()
    void acceptWhenThisExcludeFieldNamesIsNullAndArrayUtilsNotIsNotEmptyIncludeFieldNamesAndFieldNotIsAnnotationPresentToStringE() {
        /* Branches:
         * (field.getName().indexOf(ClassUtils.INNER_CLASS_SEPARATOR_CHAR) != -1) : false
         * (Modifier.isTransient(field.getModifiers())) : true
         * (this.appendTransients) : true  #  inside isAppendTransients method
         * (!isAppendTransients()) : false
         * (Modifier.isStatic(field.getModifiers())) : true
         * (this.appendStatics) : true  #  inside isAppendStatics method
         * (!isAppendStatics()) : false
         * (this.excludeFieldNames != null) : false
         * (ArrayUtils.isNotEmpty(includeFieldNames)) : false
         * (!field.isAnnotationPresent(ToStringExclude.class)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, true, true, false);
        
        //Act Statement(s)
        boolean result = target.accept(fieldMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${acceptWhenArrayUtilsNotIsNotEmptyIncludeFieldNamesAndFieldIsAnnotationPresentToStringExclude}, hash: E5E7ACAFC70E3A7C575F3A710A638A3B
    @Test()
    void acceptWhenArrayUtilsNotIsNotEmptyIncludeFieldNamesAndFieldIsAnnotationPresentToStringExclude() {
        /* Branches:
         * (field.getName().indexOf(ClassUtils.INNER_CLASS_SEPARATOR_CHAR) != -1) : false
         * (Modifier.isTransient(field.getModifiers())) : true
         * (this.appendTransients) : true  #  inside isAppendTransients method
         * (!isAppendTransients()) : false
         * (Modifier.isStatic(field.getModifiers())) : true
         * (this.appendStatics) : true  #  inside isAppendStatics method
         * (!isAppendStatics()) : false
         * (this.excludeFieldNames != null) : false
         * (ArrayUtils.isNotEmpty(includeFieldNames)) : false
         * (!field.isAnnotationPresent(ToStringExclude.class)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, true, true, false);
        
        //Act Statement(s)
        boolean result = target.accept(fieldMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${appendFieldsInWhenClazzIsArray}, hash: FEA98CB80FA28F88FEE9F84C965E563F
    @Test()
    void appendFieldsInWhenClazzIsArray() {
        /* Branches:
         * (clazz.isArray()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = spy(new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, false, false, false));
        Object object2 = new Object();
        ReflectionToStringBuilder reflectionToStringBuilder = new ReflectionToStringBuilder(object2);
        doReturn(reflectionToStringBuilder).when(target).reflectionAppendArray(object);
        
        //Act Statement(s)
        target.appendFieldsIn(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> verify(target).reflectionAppendArray(object));
    }

    //BaseRock generated method id: ${appendFieldsInWhenCaughtIllegalAccessException}, hash: 41B8CFA14BF77EB27475C69DCE6A4778
    @Test()
    void appendFieldsInWhenCaughtIllegalAccessException() {
        /* Branches:
         * (clazz.isArray()) : false
         * (for-each(fields)) : true
         * (accept(field)) : false
         * (catch-exception (IllegalAccessException)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = spy(new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, false, false, false));
        doReturn(false).when(target).accept(fieldMock);
        
        //Act Statement(s)
        target.appendFieldsIn(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> verify(target).accept(fieldMock));
    }

    //BaseRock generated method id: ${appendFieldsInWhenFieldValueIsNotNullAndFieldNotIsAnnotationPresentToStringSummary}, hash: D31285318155E265DFAF53DF243292FD
    @Test()
    void appendFieldsInWhenFieldValueIsNotNullAndFieldNotIsAnnotationPresentToStringSummary() throws IllegalAccessException {
        /* Branches:
         * (clazz.isArray()) : false
         * (for-each(fields)) : true
         * (accept(field)) : true
         * (!excludeNullValues) : false
         * (fieldValue != null) : true
         * (!field.isAnnotationPresent(ToStringSummary.class)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = spy(new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, false, false, true));
        doReturn(true).when(target).accept(fieldMock);
        Object object2 = new Object();
        doReturn(object2).when(target).getValue(fieldMock);
        Object object3 = new Object();
        ToStringBuilder toStringBuilder = new ToStringBuilder(object3);
        doReturn(toStringBuilder).when(target).append("string2", object2, true);
        
        //Act Statement(s)
        target.appendFieldsIn(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).accept(fieldMock);
            verify(target).getValue(fieldMock);
            verify(target).append("string2", object2, true);
        });
    }

    //BaseRock generated method id: ${appendFieldsInWhenFieldValueIsNotNullAndFieldIsAnnotationPresentToStringSummary}, hash: D8947D158BE3E9A392E5D43D599400CF
    @Test()
    void appendFieldsInWhenFieldValueIsNotNullAndFieldIsAnnotationPresentToStringSummary() throws IllegalAccessException {
        /* Branches:
         * (clazz.isArray()) : false
         * (for-each(fields)) : true
         * (accept(field)) : true
         * (!excludeNullValues) : false
         * (fieldValue != null) : true
         * (!field.isAnnotationPresent(ToStringSummary.class)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = spy(new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, false, false, true));
        doReturn(true).when(target).accept(fieldMock);
        Object object2 = new Object();
        doReturn(object2).when(target).getValue(fieldMock);
        Object object3 = new Object();
        ToStringBuilder toStringBuilder = new ToStringBuilder(object3);
        doReturn(toStringBuilder).when(target).append("string2", object2, false);
        
        //Act Statement(s)
        target.appendFieldsIn(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).accept(fieldMock);
            verify(target).getValue(fieldMock);
            verify(target).append("string2", object2, false);
        });
    }

    //BaseRock generated method id: ${appendFieldsInWhenFieldNotIsAnnotationPresentToStringSummaryAndCaughtIllegalAccessExceptionThrowsIllegalStateException}, hash: 271C9C4160300CD7B5DBF995494B1ABC
    @Test()
    void appendFieldsInWhenFieldNotIsAnnotationPresentToStringSummaryAndCaughtIllegalAccessExceptionThrowsIllegalStateException() throws IllegalAccessException {
        /* Branches:
         * (clazz.isArray()) : false
         * (for-each(fields)) : true
         * (accept(field)) : true
         * (!excludeNullValues) : false
         * (fieldValue != null) : true
         * (!field.isAnnotationPresent(ToStringSummary.class)) : true
         * (catch-exception (IllegalAccessException)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = spy(new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, false, false, true));
        doReturn(true).when(target).accept(fieldMock);
        Object object2 = new Object();
        doReturn(object2).when(target).getValue(fieldMock);
        Object object3 = new Object();
        ToStringBuilder toStringBuilder = new ToStringBuilder(object3);
        doReturn(toStringBuilder).when(target).append("string2", object2, true);
        IllegalAccessException illegalAccessException = new IllegalAccessException();
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            target.appendFieldsIn(Object.class);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getCause(), is(instanceOf(illegalAccessException.getClass())));
            verify(target).accept(fieldMock);
            verify(target).getValue(fieldMock);
            verify(target).append("string2", object2, true);
        });
    }

    //BaseRock generated method id: ${getExcludeFieldNamesTest}, hash: 79F4F18F1D3A1BAB5A8295CEDCBBB67F
    @Test()
    void getExcludeFieldNamesTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, false, false, false);
        
        //Act Statement(s)
        String[] result = target.getExcludeFieldNames();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${getIncludeFieldNamesTest}, hash: 8C974EA52BEE2D80B0FF232BD7CCE06C
    @Test()
    void getIncludeFieldNamesTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, false, false, false);
        
        //Act Statement(s)
        String[] result = target.getIncludeFieldNames();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${getUpToClassTest}, hash: 379CCE903EA65ED79DF96019D5AF717C
    @Test()
    void getUpToClassTest() {
        //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, false, false, false);
        
        //Act Statement(s)
        Class<?> result = target.getUpToClass();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Object.class)));
    }

    //BaseRock generated method id: ${getValueTest}, hash: 403A81920ED2CF3B4682CA719747C410
    @Test()
    void getValueTest() throws IllegalAccessException, IllegalArgumentException {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        doReturn(object).when(fieldMock).get(object2);
        Object object3 = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = spy(new ReflectionToStringBuilder(object3, standardToStringStyle, (StringBuffer) null, Object.class, false, false, false));
        doReturn(object2).when(target).getObject();
        
        //Act Statement(s)
        Object result = target.getValue(fieldMock);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(object));
            verify(fieldMock).get(object2);
            verify(target).getObject();
        });
    }

    //BaseRock generated method id: ${isAppendStaticsWhenThisAppendStatics}, hash: D7C12A03BED97B45CE446DA09386FC72
    @Test()
    void isAppendStaticsWhenThisAppendStatics() {
        /* Branches:
         * (this.appendStatics) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, false, true, false);
        
        //Act Statement(s)
        boolean result = target.isAppendStatics();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAppendStaticsWhenThisNotAppendStatics}, hash: 9B07AA5885952CA4F64C688A053DF8C0
    @Test()
    void isAppendStaticsWhenThisNotAppendStatics() {
        /* Branches:
         * (this.appendStatics) : false
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, false, false, false);
        
        //Act Statement(s)
        boolean result = target.isAppendStatics();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAppendTransientsWhenThisAppendTransients}, hash: 9B45A9EA299D725762F6F884DA872108
    @Test()
    void isAppendTransientsWhenThisAppendTransients() {
        /* Branches:
         * (this.appendTransients) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, true, false, false);
        
        //Act Statement(s)
        boolean result = target.isAppendTransients();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAppendTransientsWhenThisNotAppendTransients}, hash: 226C35F77B7301FA319B1257878FEA4F
    @Test()
    void isAppendTransientsWhenThisNotAppendTransients() {
        /* Branches:
         * (this.appendTransients) : false
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, false, false, false);
        
        //Act Statement(s)
        boolean result = target.isAppendTransients();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isExcludeNullValuesWhenThisExcludeNullValues}, hash: 01B018EA702814A5ED27B852945328F2
    @Test()
    void isExcludeNullValuesWhenThisExcludeNullValues() {
        /* Branches:
         * (this.excludeNullValues) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, false, false, true);
        
        //Act Statement(s)
        boolean result = target.isExcludeNullValues();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isExcludeNullValuesWhenThisNotExcludeNullValues}, hash: 5BCEE84B56AF7B8053F6AB407303245E
    @Test()
    void isExcludeNullValuesWhenThisNotExcludeNullValues() {
        /* Branches:
         * (this.excludeNullValues) : false
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, false, false, false);
        
        //Act Statement(s)
        boolean result = target.isExcludeNullValues();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${reflectionAppendArrayTest}, hash: 588C14BAEA7DEA2BDA1936352A625260
    @Test()
    void reflectionAppendArrayTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = spy(new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, false, false, false));
        StringBuffer stringBuffer = new StringBuffer();
        doReturn(stringBuffer).when(target).getStringBuffer();
        Object object2 = new Object();
        
        //Act Statement(s)
        ReflectionToStringBuilder result = target.reflectionAppendArray(object2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).getStringBuffer();
        });
    }

    //BaseRock generated method id: ${setAppendStaticsTest}, hash: 4776E069A555ABF540377E2075031E35
    @Test()
    void setAppendStaticsTest() {
        //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, false, false, false);
        
        //Act Statement(s)
        target.setAppendStatics(false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(target.isAppendStatics(), equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${setAppendTransientsTest}, hash: 094C73753FDB01C533C713AE94610D0C
    @Test()
    void setAppendTransientsTest() {
        //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, false, false, false);
        
        //Act Statement(s)
        target.setAppendTransients(false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(target.isAppendTransients(), equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${setExcludeFieldNamesWhenExcludeFieldNamesParamIsNull}, hash: 77F368EE26C6A0EA7807BCF8C07592BE
    @Test()
    void setExcludeFieldNamesWhenExcludeFieldNamesParamIsNull() {
        /* Branches:
         * (excludeFieldNamesParam == null) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, false, false, false);
        String[] string = null;
        
        //Act Statement(s)
        ReflectionToStringBuilder result = target.setExcludeFieldNames(string);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${setExcludeFieldNamesWhenExcludeFieldNamesParamIsNotNull}, hash: 182258E65BD1764D526B8DFB28505209
    @Test()
    void setExcludeFieldNamesWhenExcludeFieldNamesParamIsNotNull() {
        /* Branches:
         * (excludeFieldNamesParam == null) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, false, false, false);
        String[] stringArray = new String[] {};
        
        //Act Statement(s)
        ReflectionToStringBuilder result = target.setExcludeFieldNames(stringArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${setExcludeNullValuesTest}, hash: 488E2D2EDDB1118E2FEED2B7EED97E49
    @Test()
    void setExcludeNullValuesTest() {
        //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, false, false, false);
        
        //Act Statement(s)
        target.setExcludeNullValues(false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(target.isExcludeNullValues(), equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${setIncludeFieldNamesWhenIncludeFieldNamesParamIsNull}, hash: 5D471B54D555D28C4B9F59A1388E7662
    @Test()
    void setIncludeFieldNamesWhenIncludeFieldNamesParamIsNull() {
        /* Branches:
         * (includeFieldNamesParam == null) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, false, false, false);
        String[] string = null;
        
        //Act Statement(s)
        ReflectionToStringBuilder result = target.setIncludeFieldNames(string);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${setIncludeFieldNamesWhenIncludeFieldNamesParamIsNotNull}, hash: 6B4994CD39E522D3BBA245C88F4EAD89
    @Test()
    void setIncludeFieldNamesWhenIncludeFieldNamesParamIsNotNull() {
        /* Branches:
         * (includeFieldNamesParam == null) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, false, false, false);
        String[] stringArray = new String[] {};
        
        //Act Statement(s)
        ReflectionToStringBuilder result = target.setIncludeFieldNames(stringArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${setUpToClassWhenClazzNotIsInstanceObjectThrowsIllegalArgumentException}, hash: C3E22E5FF3B1FBE154B08D2AB75050F5
    @Test()
    void setUpToClassWhenClazzNotIsInstanceObjectThrowsIllegalArgumentException() {
        /* Branches:
         * (clazz != null) : true
         * (object != null) : true
         * (!clazz.isInstance(object)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, false, false, false);
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Specified class is not a superclass of the object");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            target.setUpToClass(Object.class);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${setUpToClassWhenClazzIsInstanceObject}, hash: 2577FE1C023F0D99D997CB6B71CC91BE
    @Test()
    void setUpToClassWhenClazzIsInstanceObject() {
        /* Branches:
         * (clazz != null) : true
         * (object != null) : true
         * (!clazz.isInstance(object)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, false, false, false);
        
        //Act Statement(s)
        target.setUpToClass(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(target.getUpToClass(), equalTo(Object.class)));
    }

    //BaseRock generated method id: ${toStringWhenGetObjectIsNull}, hash: 2D2A31FEA5C4F5C29B4C9320ACD4EAF0
    @Test()
    void toStringWhenGetObjectIsNull() {
        /* Branches:
         * (getObject() == null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = new ReflectionToStringBuilder((Object) null, standardToStringStyle, (StringBuffer) null, Object.class, false, false, false);
        
        //Act Statement(s)
        String result = target.toString();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${toStringWhenClazzNotEqualsGetUpToClassAndGetObjectIsNotNull}, hash: 72CED4F2702E9D7C126679F5C91E825E
    @Test()
    void toStringWhenClazzNotEqualsGetUpToClassAndGetObjectIsNotNull() {
        /* Branches:
         * (getObject() == null) : false
         * (ArrayUtils.containsAny(this.excludeFieldNames, (Object[]) this.includeFieldNames)) : false  #  inside validate method
         * (clazz.getSuperclass() != null) : true
         * (clazz != getUpToClass()) : true
         * (getObject() == null) : false  #  inside toString method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        ReflectionToStringBuilder target = spy(new ReflectionToStringBuilder(object, standardToStringStyle, (StringBuffer) null, Object.class, false, false, false));
        doNothing().when(target).appendFieldsIn(Object.class);
        StringBuffer stringBuffer = new StringBuffer();
        StringBuffer stringBuffer2 = new StringBuffer();
        doReturn(stringBuffer, stringBuffer2).when(target).getStringBuffer();
        
        //Act Statement(s)
        String result = target.toString();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("stringBuffer5"));
            verify(target, times(2)).appendFieldsIn(Object.class);
            verify(target, times(2)).getStringBuffer();
        });
    }
}

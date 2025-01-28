package org.apache.commons.lang3.builder;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.List;
import org.apache.commons.lang3.tuple.MutablePair;
import java.util.Set;
import java.util.Collection;
import org.mockito.MockedStatic;
import org.apache.commons.lang3.tuple.Pair;
import java.util.ArrayList;
import org.apache.commons.lang3.tuple.ImmutablePair;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.Mockito.verify;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.doReturn;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class EqualsBuilderBaseRockGeneratedTest {

    private final IDKey iDKeyMock = mock(IDKey.class);

    private final IDKey iDKeyMock2 = mock(IDKey.class);

    //BaseRock generated method id: ${getRegisterPairTest}, hash: 628EFD71C526AFE83E1582637DA78E9B
    @Test()
    void getRegisterPairTest() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        
        //Act Statement(s)
        Pair<IDKey, IDKey> result = EqualsBuilder.getRegisterPair(object, object2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getRegistryTest}, hash: CCA00C415F371891DBEFD94C712C1C42
    @Test()
    void getRegistryTest() {
        
        //Act Statement(s)
        Set<Pair<IDKey, IDKey>> result = EqualsBuilder.getRegistry();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${isRegisteredWhenRegistryContainsSwappedPair}, hash: 494037496C6B263F906B998A2086D4E8
    @Test()
    void isRegisteredWhenRegistryContainsSwappedPair() {
        /* Branches:
         * (registry != null) : true
         * (registry.contains(pair)) : false
         * (registry.contains(swappedPair)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<EqualsBuilder> equalsBuilder = mockStatic(EqualsBuilder.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            equalsBuilder.when(() -> EqualsBuilder.getRegisterPair(object, object2)).thenReturn(new ImmutablePair<>(iDKeyMock, iDKeyMock2));
            //Act Statement(s)
            boolean result = EqualsBuilder.isRegistered(object, object2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                equalsBuilder.verify(() -> EqualsBuilder.getRegisterPair(object, object2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isRegisteredWhenRegistryNotContainsSwappedPair}, hash: CF45B9C32D4AC6455BFC882A2B53C67B
    @Test()
    void isRegisteredWhenRegistryNotContainsSwappedPair() {
        /* Branches:
         * (registry != null) : true
         * (registry.contains(pair)) : false
         * (registry.contains(swappedPair)) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<EqualsBuilder> equalsBuilder = mockStatic(EqualsBuilder.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            equalsBuilder.when(() -> EqualsBuilder.getRegisterPair(object, object2)).thenReturn(new ImmutablePair<>(iDKeyMock, iDKeyMock2));
            //Act Statement(s)
            boolean result = EqualsBuilder.isRegistered(object, object2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                equalsBuilder.verify(() -> EqualsBuilder.getRegisterPair(object, object2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${reflectionEqualsWhenReflectionEqualsLhsRhsTestTransientsNull}, hash: 4268483476745494CDAD6223DA459C2B
    @Test()
    void reflectionEqualsWhenReflectionEqualsLhsRhsTestTransientsNull() {
        /* Branches:
         * (reflectionEquals(lhs, rhs, testTransients, null)) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<EqualsBuilder> equalsBuilder = mockStatic(EqualsBuilder.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            String[] stringArray = new String[] {};
            equalsBuilder.when(() -> EqualsBuilder.reflectionEquals(object, object2, false, (Class) null, stringArray)).thenReturn(true);
            //Act Statement(s)
            boolean result = EqualsBuilder.reflectionEquals(object, object2, false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                equalsBuilder.verify(() -> EqualsBuilder.reflectionEquals(object, object2, false, (Class) null, stringArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${reflectionEqualsWhenReflectionEqualsNotLhsRhsTestTransientsNull}, hash: C6702EBA528007C4EAD01A2D19A62E23
    @Test()
    void reflectionEqualsWhenReflectionEqualsNotLhsRhsTestTransientsNull() {
        /* Branches:
         * (reflectionEquals(lhs, rhs, testTransients, null)) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<EqualsBuilder> equalsBuilder = mockStatic(EqualsBuilder.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            String[] stringArray = new String[] {};
            equalsBuilder.when(() -> EqualsBuilder.reflectionEquals(object, object2, false, (Class) null, stringArray)).thenReturn(false);
            //Act Statement(s)
            boolean result = EqualsBuilder.reflectionEquals(object, object2, false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                equalsBuilder.verify(() -> EqualsBuilder.reflectionEquals(object, object2, false, (Class) null, stringArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${reflectionEquals1WhenLhsEqualsRhs}, hash: 000E6498F51B4D4243D8550C9325E751
    @Test()
    void reflectionEquals1WhenLhsEqualsRhs() {
        /* Branches:
         * (lhs == rhs) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        String[] stringArray = new String[] {};
        
        //Act Statement(s)
        boolean result = EqualsBuilder.reflectionEquals(object, object, false, Object.class, false, stringArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${reflectionEquals1WhenRhsIsNull}, hash: 5A3F15EA74AEC1F529D15CB0E042DEDC
    @Test()
    void reflectionEquals1WhenRhsIsNull() {
        /* Branches:
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        Object object2 = null;
        String[] stringArray = new String[] {};
        
        //Act Statement(s)
        boolean result = EqualsBuilder.reflectionEquals(object, object2, false, Object.class, false, stringArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${reflectionEquals1WhenNewEqualsBuilderSetExcludeFieldsExcludeFieldsSetReflectUpToClassReflectUpToClassSetTestTransientsT}, hash: 31374556A01F8896D65A1B6A8C756D9F
    @Test()
    void reflectionEquals1WhenNewEqualsBuilderSetExcludeFieldsExcludeFieldsSetReflectUpToClassReflectUpToClassSetTestTransientsT() {
        /* Branches:
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (new EqualsBuilder().setExcludeFields(excludeFields).setReflectUpToClass(reflectUpToClass).setTestTransients(testTransients).setTestRecursive(testRecursive).reflectionAppend(lhs, rhs).isEquals()) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: object of type EqualsBuilder - Method: setReflectUpToClass
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        String[] stringArray = new String[] {};
        
        //Act Statement(s)
        boolean result = EqualsBuilder.reflectionEquals(object, object2, false, Object.class, false, stringArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${reflectionEquals1WhenNewEqualsBuilderSetExcludeFieldsExcludeFieldsSetReflectUpToClassReflectUpToClassSetTestTransientsT2}, hash: 575194B00E33FC0BD9CD60D605FCB050
    @Test()
    void reflectionEquals1WhenNewEqualsBuilderSetExcludeFieldsExcludeFieldsSetReflectUpToClassReflectUpToClassSetTestTransientsT2() {
        /* Branches:
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (new EqualsBuilder().setExcludeFields(excludeFields).setReflectUpToClass(reflectUpToClass).setTestTransients(testTransients).setTestRecursive(testRecursive).reflectionAppend(lhs, rhs).isEquals()) : false
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: object of type EqualsBuilder - Method: setReflectUpToClass
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        String[] stringArray = new String[] {};
        
        //Act Statement(s)
        boolean result = EqualsBuilder.reflectionEquals(object, object2, false, Object.class, false, stringArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${reflectionEquals2WhenReflectionEqualsLhsRhsTestTransientsReflectUpToClassFalseExcludeFields}, hash: 97FCE8DF6DA79FD96E5166E08291A1AE
    @Test()
    void reflectionEquals2WhenReflectionEqualsLhsRhsTestTransientsReflectUpToClassFalseExcludeFields() {
        /* Branches:
         * (reflectionEquals(lhs, rhs, testTransients, reflectUpToClass, false, excludeFields)) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<EqualsBuilder> equalsBuilder = mockStatic(EqualsBuilder.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            String[] stringArray = new String[] {};
            equalsBuilder.when(() -> EqualsBuilder.reflectionEquals(object, object2, false, Object.class, false, stringArray)).thenReturn(true);
            //Act Statement(s)
            boolean result = EqualsBuilder.reflectionEquals(object, object2, false, Object.class, stringArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                equalsBuilder.verify(() -> EqualsBuilder.reflectionEquals(object, object2, false, Object.class, false, stringArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${reflectionEquals2WhenReflectionEqualsNotLhsRhsTestTransientsReflectUpToClassFalseExcludeFields}, hash: 2D4581E15B7C3C0A3617CBD3ACE00800
    @Test()
    void reflectionEquals2WhenReflectionEqualsNotLhsRhsTestTransientsReflectUpToClassFalseExcludeFields() {
        /* Branches:
         * (reflectionEquals(lhs, rhs, testTransients, reflectUpToClass, false, excludeFields)) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<EqualsBuilder> equalsBuilder = mockStatic(EqualsBuilder.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            String[] stringArray = new String[] {};
            equalsBuilder.when(() -> EqualsBuilder.reflectionEquals(object, object2, false, Object.class, false, stringArray)).thenReturn(false);
            //Act Statement(s)
            boolean result = EqualsBuilder.reflectionEquals(object, object2, false, Object.class, stringArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                equalsBuilder.verify(() -> EqualsBuilder.reflectionEquals(object, object2, false, Object.class, false, stringArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${reflectionEquals3WhenReflectionEqualsLhsRhsReflectionToStringBuilderToNoNullStringArrayExcludeFields}, hash: B6EFA2241CE9D1D501817B4A9F99EB2C
    @Test()
    void reflectionEquals3WhenReflectionEqualsLhsRhsReflectionToStringBuilderToNoNullStringArrayExcludeFields() {
        /* Branches:
         * (reflectionEquals(lhs, rhs, ReflectionToStringBuilder.toNoNullStringArray(excludeFields))) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<EqualsBuilder> equalsBuilder = mockStatic(EqualsBuilder.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            String[] stringArray = new String[] {};
            equalsBuilder.when(() -> EqualsBuilder.reflectionEquals(object, object2, stringArray)).thenReturn(true);
            Collection<String> collection = new ArrayList<>();
            //Act Statement(s)
            boolean result = EqualsBuilder.reflectionEquals(object, object2, collection);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                equalsBuilder.verify(() -> EqualsBuilder.reflectionEquals(object, object2, stringArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${reflectionEquals3WhenReflectionEqualsNotLhsRhsReflectionToStringBuilderToNoNullStringArrayExcludeFields}, hash: 9F34F807AEC016BE2A120BD1447D5929
    @Test()
    void reflectionEquals3WhenReflectionEqualsNotLhsRhsReflectionToStringBuilderToNoNullStringArrayExcludeFields() {
        /* Branches:
         * (reflectionEquals(lhs, rhs, ReflectionToStringBuilder.toNoNullStringArray(excludeFields))) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<EqualsBuilder> equalsBuilder = mockStatic(EqualsBuilder.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            String[] stringArray = new String[] {};
            equalsBuilder.when(() -> EqualsBuilder.reflectionEquals(object, object2, stringArray)).thenReturn(false);
            Collection<String> collection = new ArrayList<>();
            //Act Statement(s)
            boolean result = EqualsBuilder.reflectionEquals(object, object2, collection);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                equalsBuilder.verify(() -> EqualsBuilder.reflectionEquals(object, object2, stringArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${reflectionEquals4WhenReflectionEqualsLhsRhsFalseNullExcludeFields}, hash: 5295CCD731B004EF951A7670AFFFF07E
    @Test()
    void reflectionEquals4WhenReflectionEqualsLhsRhsFalseNullExcludeFields() {
        /* Branches:
         * (reflectionEquals(lhs, rhs, false, null, excludeFields)) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<EqualsBuilder> equalsBuilder = mockStatic(EqualsBuilder.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            String[] stringArray = new String[] {};
            equalsBuilder.when(() -> EqualsBuilder.reflectionEquals(object, object2, false, (Class) null, stringArray)).thenReturn(true);
            //Act Statement(s)
            boolean result = EqualsBuilder.reflectionEquals(object, object2, stringArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                equalsBuilder.verify(() -> EqualsBuilder.reflectionEquals(object, object2, false, (Class) null, stringArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${reflectionEquals4WhenReflectionEqualsNotLhsRhsFalseNullExcludeFields}, hash: 1F819D45AE8EB6790EF87697CE296838
    @Test()
    void reflectionEquals4WhenReflectionEqualsNotLhsRhsFalseNullExcludeFields() {
        /* Branches:
         * (reflectionEquals(lhs, rhs, false, null, excludeFields)) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<EqualsBuilder> equalsBuilder = mockStatic(EqualsBuilder.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            String[] stringArray = new String[] {};
            equalsBuilder.when(() -> EqualsBuilder.reflectionEquals(object, object2, false, (Class) null, stringArray)).thenReturn(false);
            //Act Statement(s)
            boolean result = EqualsBuilder.reflectionEquals(object, object2, stringArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                equalsBuilder.verify(() -> EqualsBuilder.reflectionEquals(object, object2, false, (Class) null, stringArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${appendWhenLhsEqualsRhs}, hash: 7E5215A8BE7EF9AB7503D0D5170E5B5A
    @Test()
    void appendWhenLhsEqualsRhs() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        
        //Act Statement(s)
        EqualsBuilder result = target.append(false, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${appendWhenLhsNotEqualsRhs}, hash: 581C364B978ACBBBEFA0CB00C3619B34
    @Test()
    void appendWhenLhsNotEqualsRhs() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        
        //Act Statement(s)
        EqualsBuilder result = target.append(true, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append1WhenLhsEqualsRhs}, hash: E26679976ECD8C9327DA085477B0408E
    @Test()
    void append1WhenLhsEqualsRhs() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        boolean[] booleanArray = new boolean[] {};
        
        //Act Statement(s)
        EqualsBuilder result = target.append(booleanArray, booleanArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append1WhenRhsIsNull}, hash: DFFEBFDFE86885746AE925111671FCB9
    @Test()
    void append1WhenRhsIsNull() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        boolean[] booleanArray = new boolean[] {};
        boolean[] _boolean = null;
        
        //Act Statement(s)
        EqualsBuilder result = target.append(booleanArray, _boolean);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append1WhenLhsLengthNotEqualsRhsLength}, hash: 5A4021AE47854D5E0A24431B4AE18CB3
    @Test()
    void append1WhenLhsLengthNotEqualsRhsLength() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        boolean[] booleanArray = new boolean[] { false };
        boolean[] booleanArray2 = new boolean[] {};
        
        //Act Statement(s)
        EqualsBuilder result = target.append(booleanArray, booleanArray2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append1WhenIsEquals}, hash: A0359A37F00B8EE419384685AB9C0B1A
    @Test()
    void append1WhenIsEquals() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : false
         * (i < lhs.length) : true
         * (isEquals) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = spy(new EqualsBuilder());
        EqualsBuilder equalsBuilder = new EqualsBuilder();
        doReturn(equalsBuilder).when(target).append(false, false);
        boolean[] booleanArray = new boolean[] { false };
        boolean[] booleanArray2 = new boolean[] { false };
        
        //Act Statement(s)
        EqualsBuilder result = target.append(booleanArray, booleanArray2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append(false, false);
        });
    }

    //BaseRock generated method id: ${append2WhenLhsEqualsRhs}, hash: E4197DA6C6100183B7B310A728266C4C
    @Test()
    void append2WhenLhsEqualsRhs() {
        /* Branches:
         * (isEquals) : true
         * (lhs == rhs) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        
        //Act Statement(s)
        EqualsBuilder result = target.append((byte) 1, (byte) 1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append2WhenLhsNotEqualsRhs}, hash: 55EC4611758C63D123E8D368094C752D
    @Test()
    void append2WhenLhsNotEqualsRhs() {
        /* Branches:
         * (isEquals) : true
         * (lhs == rhs) : false
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        
        //Act Statement(s)
        EqualsBuilder result = target.append((byte) 1, (byte) 2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append3WhenLhsEqualsRhs}, hash: 3FD365E0F1C7BE41C637A0D0A826DD62
    @Test()
    void append3WhenLhsEqualsRhs() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        byte[] byteArray = new byte[] {};
        
        //Act Statement(s)
        EqualsBuilder result = target.append(byteArray, byteArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append3WhenRhsIsNull}, hash: 8F01A0770D45556E235FC271441B308B
    @Test()
    void append3WhenRhsIsNull() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        byte[] byteArray = new byte[] {};
        byte[] _byte = null;
        
        //Act Statement(s)
        EqualsBuilder result = target.append(byteArray, _byte);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append3WhenLhsLengthNotEqualsRhsLength}, hash: EECCFF22DFA0A5E92BEA1862A4263CB4
    @Test()
    void append3WhenLhsLengthNotEqualsRhsLength() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        byte[] byteArray = new byte[] { (byte) 0 };
        byte[] byteArray2 = new byte[] {};
        
        //Act Statement(s)
        EqualsBuilder result = target.append(byteArray, byteArray2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append3WhenIsEquals}, hash: 2280D641267C7FB21DD25336CE6257FD
    @Test()
    void append3WhenIsEquals() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : false
         * (i < lhs.length) : true
         * (isEquals) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = spy(new EqualsBuilder());
        EqualsBuilder equalsBuilder = new EqualsBuilder();
        doReturn(equalsBuilder).when(target).append((byte) 0, (byte) 0);
        byte[] byteArray = new byte[] { (byte) 0 };
        byte[] byteArray2 = new byte[] { (byte) 0 };
        
        //Act Statement(s)
        EqualsBuilder result = target.append(byteArray, byteArray2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append((byte) 0, (byte) 0);
        });
    }

    //BaseRock generated method id: ${append4WhenLhsEqualsRhs}, hash: D3E134FF63528C3CC9AD304667F45CD9
    @Test()
    void append4WhenLhsEqualsRhs() {
        /* Branches:
         * (isEquals) : true
         * (lhs == rhs) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        
        //Act Statement(s)
        EqualsBuilder result = target.append('A', 'A');
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append4WhenLhsNotEqualsRhs}, hash: 337E493D2350FA728E0E9E2CA90FDF73
    @Test()
    void append4WhenLhsNotEqualsRhs() {
        /* Branches:
         * (isEquals) : true
         * (lhs == rhs) : false
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        
        //Act Statement(s)
        EqualsBuilder result = target.append('A', 'B');
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append5WhenLhsEqualsRhs}, hash: E44041504DF4BAB02D2E39780023860D
    @Test()
    void append5WhenLhsEqualsRhs() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        char[] charArray = new char[] {};
        
        //Act Statement(s)
        EqualsBuilder result = target.append(charArray, charArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append5WhenRhsIsNull}, hash: B9FC77AF27F80E7E6F9FF8F0D4D22FD3
    @Test()
    void append5WhenRhsIsNull() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        char[] charArray = new char[] {};
        char[] _char = null;
        
        //Act Statement(s)
        EqualsBuilder result = target.append(charArray, _char);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append5WhenLhsLengthNotEqualsRhsLength}, hash: 3A049605BBEEE4DEDF4138D266CB6696
    @Test()
    void append5WhenLhsLengthNotEqualsRhsLength() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        char[] charArray = new char[] { 'A' };
        char[] charArray2 = new char[] {};
        
        //Act Statement(s)
        EqualsBuilder result = target.append(charArray, charArray2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append5WhenIsEquals}, hash: DC3C3B353F06FD765E0DF58CB381FD90
    @Test()
    void append5WhenIsEquals() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : false
         * (i < lhs.length) : true
         * (isEquals) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = spy(new EqualsBuilder());
        EqualsBuilder equalsBuilder = new EqualsBuilder();
        doReturn(equalsBuilder).when(target).append('A', 'A');
        char[] charArray = new char[] { 'A' };
        char[] charArray2 = new char[] { 'A' };
        
        //Act Statement(s)
        EqualsBuilder result = target.append(charArray, charArray2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append('A', 'A');
        });
    }

    //BaseRock generated method id: ${append6WhenIsEquals}, hash: F5AA198B1A56785BAC1FEDF6ED926EBB
    @Test()
    void append6WhenIsEquals() {
        /* Branches:
         * (isEquals) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = spy(new EqualsBuilder());
        EqualsBuilder equalsBuilder = new EqualsBuilder();
        doReturn(equalsBuilder).when(target).append(4607182418800017408L, 4602678819172646912L);
        
        //Act Statement(s)
        EqualsBuilder result = target.append(Double.parseDouble("1.0"), Double.parseDouble("0.5"));
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(equalsBuilder));
            verify(target).append(4607182418800017408L, 4602678819172646912L);
        });
    }

    //BaseRock generated method id: ${append7WhenLhsEqualsRhs}, hash: 5B381E8215A58DDC5CC6958202E593B9
    @Test()
    void append7WhenLhsEqualsRhs() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        double[] doubleArray = new double[] {};
        
        //Act Statement(s)
        EqualsBuilder result = target.append(doubleArray, doubleArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append7WhenRhsIsNull}, hash: D0EC308B199C006DBA21CC37601C6A40
    @Test()
    void append7WhenRhsIsNull() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        double[] doubleArray = new double[] {};
        double[] _double = null;
        
        //Act Statement(s)
        EqualsBuilder result = target.append(doubleArray, _double);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append7WhenLhsLengthNotEqualsRhsLength}, hash: 960E86D58599C4016B3893081AEDD9B2
    @Test()
    void append7WhenLhsLengthNotEqualsRhsLength() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        double[] doubleArray = new double[] { Double.parseDouble("0") };
        double[] doubleArray2 = new double[] {};
        
        //Act Statement(s)
        EqualsBuilder result = target.append(doubleArray, doubleArray2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append7WhenIsEquals}, hash: 018C86E2ED4A55C77ACD979D7EF17F77
    @Test()
    void append7WhenIsEquals() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : false
         * (i < lhs.length) : true
         * (isEquals) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = spy(new EqualsBuilder());
        EqualsBuilder equalsBuilder = new EqualsBuilder();
        doReturn(equalsBuilder).when(target).append(Double.parseDouble("0"), Double.parseDouble("0"));
        double[] doubleArray = new double[] { Double.parseDouble("0") };
        double[] doubleArray2 = new double[] { Double.parseDouble("0") };
        
        //Act Statement(s)
        EqualsBuilder result = target.append(doubleArray, doubleArray2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append(Double.parseDouble("0"), Double.parseDouble("0"));
        });
    }

    //BaseRock generated method id: ${append8WhenIsEquals}, hash: 4EE66229112B6C5FD3A3E27338FD9E37
    @Test()
    void append8WhenIsEquals() {
        /* Branches:
         * (isEquals) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = spy(new EqualsBuilder());
        EqualsBuilder equalsBuilder = new EqualsBuilder();
        doReturn(equalsBuilder).when(target).append(1065353216, 1056964608);
        
        //Act Statement(s)
        EqualsBuilder result = target.append(Float.parseFloat("1.0"), Float.parseFloat("0.5"));
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(equalsBuilder));
            verify(target).append(1065353216, 1056964608);
        });
    }

    //BaseRock generated method id: ${append9WhenLhsEqualsRhs}, hash: 52C024039347B3D2519E1E0D7967A172
    @Test()
    void append9WhenLhsEqualsRhs() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        float[] floatArray = new float[] {};
        
        //Act Statement(s)
        EqualsBuilder result = target.append(floatArray, floatArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append9WhenRhsIsNull}, hash: 585A7D31699AB383AFD2609E4D1FD7EF
    @Test()
    void append9WhenRhsIsNull() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        float[] floatArray = new float[] {};
        float[] _float = null;
        
        //Act Statement(s)
        EqualsBuilder result = target.append(floatArray, _float);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append9WhenLhsLengthNotEqualsRhsLength}, hash: 2A5EAD387302E701D66670E7B29DD69F
    @Test()
    void append9WhenLhsLengthNotEqualsRhsLength() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        float[] floatArray = new float[] { Float.parseFloat("0") };
        float[] floatArray2 = new float[] {};
        
        //Act Statement(s)
        EqualsBuilder result = target.append(floatArray, floatArray2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append9WhenIsEquals}, hash: 41385A6F8B5EED579205D6235132DE29
    @Test()
    void append9WhenIsEquals() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : false
         * (i < lhs.length) : true
         * (isEquals) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = spy(new EqualsBuilder());
        EqualsBuilder equalsBuilder = new EqualsBuilder();
        doReturn(equalsBuilder).when(target).append(Float.parseFloat("0"), Float.parseFloat("0"));
        float[] floatArray = new float[] { Float.parseFloat("0") };
        float[] floatArray2 = new float[] { Float.parseFloat("0") };
        
        //Act Statement(s)
        EqualsBuilder result = target.append(floatArray, floatArray2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append(Float.parseFloat("0"), Float.parseFloat("0"));
        });
    }

    //BaseRock generated method id: ${append10WhenLhsEqualsRhs}, hash: 9DB7760AEA431ED13361EA53F9BCD1F6
    @Test()
    void append10WhenLhsEqualsRhs() {
        /* Branches:
         * (isEquals) : true
         * (lhs == rhs) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        
        //Act Statement(s)
        EqualsBuilder result = target.append(1, 1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append10WhenLhsNotEqualsRhs}, hash: 4EE6AE807EF8B0DFEEE7F177ECD3129B
    @Test()
    void append10WhenLhsNotEqualsRhs() {
        /* Branches:
         * (isEquals) : true
         * (lhs == rhs) : false
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        
        //Act Statement(s)
        EqualsBuilder result = target.append(1, 2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append11WhenLhsEqualsRhs}, hash: FF308B552870937FD5C3824FD60DC64C
    @Test()
    void append11WhenLhsEqualsRhs() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        int[] intArray = new int[] {};
        
        //Act Statement(s)
        EqualsBuilder result = target.append(intArray, intArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append11WhenRhsIsNull}, hash: 5354AA558B584168F0FBE2F26D8C77BA
    @Test()
    void append11WhenRhsIsNull() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        int[] intArray = new int[] {};
        int[] _int = null;
        
        //Act Statement(s)
        EqualsBuilder result = target.append(intArray, _int);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append11WhenLhsLengthNotEqualsRhsLength}, hash: 92C11457F6157C23C373DC804AF23ECF
    @Test()
    void append11WhenLhsLengthNotEqualsRhsLength() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        int[] intArray = new int[] { 0 };
        int[] intArray2 = new int[] {};
        
        //Act Statement(s)
        EqualsBuilder result = target.append(intArray, intArray2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append11WhenIsEquals}, hash: 12A0CA6CE6ABE88A78362C116C7DEBC3
    @Test()
    void append11WhenIsEquals() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : false
         * (i < lhs.length) : true
         * (isEquals) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = spy(new EqualsBuilder());
        EqualsBuilder equalsBuilder = new EqualsBuilder();
        doReturn(equalsBuilder).when(target).append(0, 0);
        int[] intArray = new int[] { 0 };
        int[] intArray2 = new int[] { 0 };
        
        //Act Statement(s)
        EqualsBuilder result = target.append(intArray, intArray2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append(0, 0);
        });
    }

    //BaseRock generated method id: ${append12WhenLhsEqualsRhs}, hash: C02713E5DCCC92810F531A0651BF5A91
    @Test()
    void append12WhenLhsEqualsRhs() {
        /* Branches:
         * (isEquals) : true
         * (lhs == rhs) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        
        //Act Statement(s)
        EqualsBuilder result = target.append(1L, 1L);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append12WhenLhsNotEqualsRhs}, hash: CDE0405E8240DB61B2E27A5A1DE030C0
    @Test()
    void append12WhenLhsNotEqualsRhs() {
        /* Branches:
         * (isEquals) : true
         * (lhs == rhs) : false
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        
        //Act Statement(s)
        EqualsBuilder result = target.append(1L, 2L);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append13WhenLhsEqualsRhs}, hash: 536E8D7680439B297FEFE951AD8D6554
    @Test()
    void append13WhenLhsEqualsRhs() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        long[] longArray = new long[] {};
        
        //Act Statement(s)
        EqualsBuilder result = target.append(longArray, longArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append13WhenRhsIsNull}, hash: 8993F39FF0A9AABEE53EB26D9A1BC003
    @Test()
    void append13WhenRhsIsNull() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        long[] longArray = new long[] {};
        long[] _long = null;
        
        //Act Statement(s)
        EqualsBuilder result = target.append(longArray, _long);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append13WhenLhsLengthNotEqualsRhsLength}, hash: 9D417E8CF936AE398D3CBD89538F9484
    @Test()
    void append13WhenLhsLengthNotEqualsRhsLength() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        long[] longArray = new long[] { 0L };
        long[] longArray2 = new long[] {};
        
        //Act Statement(s)
        EqualsBuilder result = target.append(longArray, longArray2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append13WhenIsEquals}, hash: 06C576080F3C5143BF51925D83093900
    @Test()
    void append13WhenIsEquals() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : false
         * (i < lhs.length) : true
         * (isEquals) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = spy(new EqualsBuilder());
        EqualsBuilder equalsBuilder = new EqualsBuilder();
        doReturn(equalsBuilder).when(target).append(0L, 0L);
        long[] longArray = new long[] { 0L };
        long[] longArray2 = new long[] { 0L };
        
        //Act Statement(s)
        EqualsBuilder result = target.append(longArray, longArray2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append(0L, 0L);
        });
    }

    //BaseRock generated method id: ${append14WhenLhsEqualsRhs}, hash: 44C4A26F0D84F078C8672ED42D89AE2A
    @Test()
    void append14WhenLhsEqualsRhs() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        Object object = new Object();
        
        //Act Statement(s)
        EqualsBuilder result = target.append(object, object);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append14WhenRhsIsNull}, hash: DA9DF1C27EE79087509636280E31848B
    @Test()
    void append14WhenRhsIsNull() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        Object object = new Object();
        Object object2 = null;
        
        //Act Statement(s)
        EqualsBuilder result = target.append(object, object2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append14WhenLhsGetClassNotEqualsRhsGetClass}, hash: B59BA3F2D18D3AF11E4639E21ABFA036
    @Test()
    void append14WhenLhsGetClassNotEqualsRhsGetClass() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhsClass.isArray()) : true
         * (lhs.getClass() != rhs.getClass()) : true  #  inside appendArray method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        Object object = new Object();
        Object object2 = new Object();
        
        //Act Statement(s)
        EqualsBuilder result = target.append(object, object2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append14WhenNotTestRecursive}, hash: AB23B33E47134777CEBA0CBBA33FA820
    @Test()
    void append14WhenNotTestRecursive() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhsClass.isArray()) : false
         * (testRecursive) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        Object object = new Object();
        Object object2 = new Object();
        
        //Act Statement(s)
        EqualsBuilder result = target.append(object, object2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append14WhenLhsInstanceOfLongArray}, hash: BC89683A2EC1122C22436B284A0DEAC7
    @Test()
    void append14WhenLhsInstanceOfLongArray() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhsClass.isArray()) : true
         * (lhs.getClass() != rhs.getClass()) : false  #  inside appendArray method
         * (lhs instanceof long[]) : true  #  inside appendArray method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        EqualsBuilder target = spy(new EqualsBuilder());
        EqualsBuilder equalsBuilder = new EqualsBuilder();
        long[] longArray = new long[] {};
        long[] longArray2 = new long[] {};
        doReturn(equalsBuilder).when(target).append(longArray, longArray2);
        
        //Act Statement(s)
        EqualsBuilder result = target.append((Object) longArray, (Object) longArray2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append(longArray, longArray2);
        });
    }

    //BaseRock generated method id: ${append14WhenLhsInstanceOfIntArray}, hash: 319FA18586A2B533E8B816CC7FBC6C67
    @Test()
    void append14WhenLhsInstanceOfIntArray() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhsClass.isArray()) : true
         * (lhs.getClass() != rhs.getClass()) : false  #  inside appendArray method
         * (lhs instanceof long[]) : false  #  inside appendArray method
         * (lhs instanceof int[]) : true  #  inside appendArray method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        EqualsBuilder target = spy(new EqualsBuilder());
        EqualsBuilder equalsBuilder = new EqualsBuilder();
        int[] intArray = new int[] {};
        int[] intArray2 = new int[] {};
        doReturn(equalsBuilder).when(target).append(intArray, intArray2);
        
        //Act Statement(s)
        EqualsBuilder result = target.append((Object) intArray, (Object) intArray2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append(intArray, intArray2);
        });
    }

    //BaseRock generated method id: ${append14WhenLhsInstanceOfShortArray}, hash: 23ECEE8B3B76CFABA50E76F5EC88DCEF
    @Test()
    void append14WhenLhsInstanceOfShortArray() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhsClass.isArray()) : true
         * (lhs.getClass() != rhs.getClass()) : false  #  inside appendArray method
         * (lhs instanceof long[]) : false  #  inside appendArray method
         * (lhs instanceof int[]) : false  #  inside appendArray method
         * (lhs instanceof short[]) : true  #  inside appendArray method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        short[] shortArray = new short[] {};
        short[] shortArray2 = new short[] {};
        
        //Act Statement(s)
        EqualsBuilder result = target.append((Object) shortArray, (Object) shortArray2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append14WhenLhsInstanceOfCharArray}, hash: 06A3592C066863752FC02B6ACE538A0A
    @Test()
    void append14WhenLhsInstanceOfCharArray() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhsClass.isArray()) : true
         * (lhs.getClass() != rhs.getClass()) : false  #  inside appendArray method
         * (lhs instanceof long[]) : false  #  inside appendArray method
         * (lhs instanceof int[]) : false  #  inside appendArray method
         * (lhs instanceof short[]) : false  #  inside appendArray method
         * (lhs instanceof char[]) : true  #  inside appendArray method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        EqualsBuilder target = spy(new EqualsBuilder());
        EqualsBuilder equalsBuilder = new EqualsBuilder();
        char[] charArray = new char[] {};
        char[] charArray2 = new char[] {};
        doReturn(equalsBuilder).when(target).append(charArray, charArray2);
        
        //Act Statement(s)
        EqualsBuilder result = target.append((Object) charArray, (Object) charArray2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append(charArray, charArray2);
        });
    }

    //BaseRock generated method id: ${append14WhenLhsInstanceOfByteArray}, hash: EE92FFC0A75646314381A8A094A01F66
    @Test()
    void append14WhenLhsInstanceOfByteArray() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhsClass.isArray()) : true
         * (lhs.getClass() != rhs.getClass()) : false  #  inside appendArray method
         * (lhs instanceof long[]) : false  #  inside appendArray method
         * (lhs instanceof int[]) : false  #  inside appendArray method
         * (lhs instanceof short[]) : false  #  inside appendArray method
         * (lhs instanceof char[]) : false  #  inside appendArray method
         * (lhs instanceof byte[]) : true  #  inside appendArray method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        EqualsBuilder target = spy(new EqualsBuilder());
        EqualsBuilder equalsBuilder = new EqualsBuilder();
        byte[] byteArray = new byte[] {};
        byte[] byteArray2 = new byte[] {};
        doReturn(equalsBuilder).when(target).append(byteArray, byteArray2);
        
        //Act Statement(s)
        EqualsBuilder result = target.append((Object) byteArray, (Object) byteArray2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append(byteArray, byteArray2);
        });
    }

    //BaseRock generated method id: ${append14WhenLhsInstanceOfDoubleArray}, hash: EB486235CC198369DA10BD9DE69F4605
    @Test()
    void append14WhenLhsInstanceOfDoubleArray() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhsClass.isArray()) : true
         * (lhs.getClass() != rhs.getClass()) : false  #  inside appendArray method
         * (lhs instanceof long[]) : false  #  inside appendArray method
         * (lhs instanceof int[]) : false  #  inside appendArray method
         * (lhs instanceof short[]) : false  #  inside appendArray method
         * (lhs instanceof char[]) : false  #  inside appendArray method
         * (lhs instanceof byte[]) : false  #  inside appendArray method
         * (lhs instanceof double[]) : true  #  inside appendArray method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        EqualsBuilder target = spy(new EqualsBuilder());
        EqualsBuilder equalsBuilder = new EqualsBuilder();
        double[] doubleArray = new double[] {};
        double[] doubleArray2 = new double[] {};
        doReturn(equalsBuilder).when(target).append(doubleArray, doubleArray2);
        
        //Act Statement(s)
        EqualsBuilder result = target.append((Object) doubleArray, (Object) doubleArray2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append(doubleArray, doubleArray2);
        });
    }

    //BaseRock generated method id: ${append14WhenLhsInstanceOfFloatArray}, hash: EAD5A626C7499A08F8FD4A350833F0DB
    @Test()
    void append14WhenLhsInstanceOfFloatArray() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhsClass.isArray()) : true
         * (lhs.getClass() != rhs.getClass()) : false  #  inside appendArray method
         * (lhs instanceof long[]) : false  #  inside appendArray method
         * (lhs instanceof int[]) : false  #  inside appendArray method
         * (lhs instanceof short[]) : false  #  inside appendArray method
         * (lhs instanceof char[]) : false  #  inside appendArray method
         * (lhs instanceof byte[]) : false  #  inside appendArray method
         * (lhs instanceof double[]) : false  #  inside appendArray method
         * (lhs instanceof float[]) : true  #  inside appendArray method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        EqualsBuilder target = spy(new EqualsBuilder());
        EqualsBuilder equalsBuilder = new EqualsBuilder();
        float[] floatArray = new float[] {};
        float[] floatArray2 = new float[] {};
        doReturn(equalsBuilder).when(target).append(floatArray, floatArray2);
        
        //Act Statement(s)
        EqualsBuilder result = target.append((Object) floatArray, (Object) floatArray2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append(floatArray, floatArray2);
        });
    }

    //BaseRock generated method id: ${append14WhenLhsInstanceOfBooleanArray}, hash: B724405B7D4DF219B6D7ED095F5E00ED
    @Test()
    void append14WhenLhsInstanceOfBooleanArray() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhsClass.isArray()) : true
         * (lhs.getClass() != rhs.getClass()) : false  #  inside appendArray method
         * (lhs instanceof long[]) : false  #  inside appendArray method
         * (lhs instanceof int[]) : false  #  inside appendArray method
         * (lhs instanceof short[]) : false  #  inside appendArray method
         * (lhs instanceof char[]) : false  #  inside appendArray method
         * (lhs instanceof byte[]) : false  #  inside appendArray method
         * (lhs instanceof double[]) : false  #  inside appendArray method
         * (lhs instanceof float[]) : false  #  inside appendArray method
         * (lhs instanceof boolean[]) : true  #  inside appendArray method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        EqualsBuilder target = spy(new EqualsBuilder());
        EqualsBuilder equalsBuilder = new EqualsBuilder();
        boolean[] booleanArray = new boolean[] {};
        boolean[] booleanArray2 = new boolean[] {};
        doReturn(equalsBuilder).when(target).append(booleanArray, booleanArray2);
        
        //Act Statement(s)
        EqualsBuilder result = target.append((Object) booleanArray, (Object) booleanArray2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append(booleanArray, booleanArray2);
        });
    }

    //BaseRock generated method id: ${append14WhenLhsNotInstanceOfBooleanArray}, hash: 83AB1B3427815156ECE93F823F734895
    @Test()
    void append14WhenLhsNotInstanceOfBooleanArray() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhsClass.isArray()) : true
         * (lhs.getClass() != rhs.getClass()) : false  #  inside appendArray method
         * (lhs instanceof long[]) : false  #  inside appendArray method
         * (lhs instanceof int[]) : false  #  inside appendArray method
         * (lhs instanceof short[]) : false  #  inside appendArray method
         * (lhs instanceof char[]) : false  #  inside appendArray method
         * (lhs instanceof byte[]) : false  #  inside appendArray method
         * (lhs instanceof double[]) : false  #  inside appendArray method
         * (lhs instanceof float[]) : false  #  inside appendArray method
         * (lhs instanceof boolean[]) : false  #  inside appendArray method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        Object[] objectArray = new Object[] {};
        Object[] objectArray2 = new Object[] {};
        
        //Act Statement(s)
        EqualsBuilder result = target.append((Object) objectArray, (Object) objectArray2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append15WhenLhsEqualsRhs}, hash: 3CA76CFBAD52CC5BC6437B6803FC79FC
    @Test()
    void append15WhenLhsEqualsRhs() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        Object[] objectArray = new Object[] {};
        
        //Act Statement(s)
        EqualsBuilder result = target.append(objectArray, objectArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append15WhenRhsIsNull}, hash: 4C24DA3F697E7633B88FD2BC9EB1318F
    @Test()
    void append15WhenRhsIsNull() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        Object[] objectArray = new Object[] {};
        Object[] object = null;
        
        //Act Statement(s)
        EqualsBuilder result = target.append(objectArray, object);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append15WhenLhsLengthNotEqualsRhsLength}, hash: 4BF5C700A1B304B3C30FC0F95785AA30
    @Test()
    void append15WhenLhsLengthNotEqualsRhsLength() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        Object object = new Object();
        Object[] objectArray = new Object[] { object };
        Object[] objectArray2 = new Object[] {};
        
        //Act Statement(s)
        EqualsBuilder result = target.append(objectArray, objectArray2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append15WhenIsEquals}, hash: C1841B9E6680C43E53EEDFE0C01E69C5
    @Test()
    void append15WhenIsEquals() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : false
         * (i < lhs.length) : true
         * (isEquals) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = spy(new EqualsBuilder());
        EqualsBuilder equalsBuilder = new EqualsBuilder();
        Object object = new Object();
        Object object2 = new Object();
        doReturn(equalsBuilder).when(target).append(object, object2);
        Object[] objectArray = new Object[] { object };
        Object[] objectArray2 = new Object[] { object2 };
        
        //Act Statement(s)
        EqualsBuilder result = target.append(objectArray, objectArray2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append(object, object2);
        });
    }

    //BaseRock generated method id: ${append16WhenLhsEqualsRhs}, hash: 64257D3A50C2125DB185C4AE86D186A6
    @Test()
    void append16WhenLhsEqualsRhs() {
        /* Branches:
         * (isEquals) : true
         * (lhs == rhs) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        
        //Act Statement(s)
        EqualsBuilder result = target.append((short) 1, (short) 1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append16WhenLhsNotEqualsRhs}, hash: 010C7AB53DD7669466482FE54D64BD5B
    @Test()
    void append16WhenLhsNotEqualsRhs() {
        /* Branches:
         * (isEquals) : true
         * (lhs == rhs) : false
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        
        //Act Statement(s)
        EqualsBuilder result = target.append((short) 1, (short) 2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append17WhenLhsEqualsRhs}, hash: B1081BD6250AA0B7F8D54E5583A112C9
    @Test()
    void append17WhenLhsEqualsRhs() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        short[] shortArray = new short[] {};
        
        //Act Statement(s)
        EqualsBuilder result = target.append(shortArray, shortArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append17WhenRhsIsNull}, hash: E2D046B85A6E92E1BA291DECEF2D07D3
    @Test()
    void append17WhenRhsIsNull() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        short[] shortArray = new short[] {};
        short[] _short = null;
        
        //Act Statement(s)
        EqualsBuilder result = target.append(shortArray, _short);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append17WhenLhsLengthNotEqualsRhsLength}, hash: CB9E7F8E3C96DF8ABD49C1F96CDE6AA7
    @Test()
    void append17WhenLhsLengthNotEqualsRhsLength() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        short[] shortArray = new short[] { (short) 0 };
        short[] shortArray2 = new short[] {};
        
        //Act Statement(s)
        EqualsBuilder result = target.append(shortArray, shortArray2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append17WhenIsEquals}, hash: C1161A71AD738820C0D23755AF21DDD6
    @Test()
    void append17WhenIsEquals() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhs.length != rhs.length) : false
         * (i < lhs.length) : true
         * (isEquals) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = spy(new EqualsBuilder());
        EqualsBuilder equalsBuilder = new EqualsBuilder();
        doReturn(equalsBuilder).when(target).append((short) 0, (short) 0);
        short[] shortArray = new short[] { (short) 0 };
        short[] shortArray2 = new short[] { (short) 0 };
        
        //Act Statement(s)
        EqualsBuilder result = target.append(shortArray, shortArray2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append((short) 0, (short) 0);
        });
    }

    //BaseRock generated method id: ${appendSuperWhenIsEquals}, hash: 3AB8A666291CBD573296E308251B7CA4
    @Test()
    void appendSuperWhenIsEquals() {
        /* Branches:
         * (!isEquals) : false
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        
        //Act Statement(s)
        EqualsBuilder result = target.appendSuper(false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${buildTest}, hash: 6D09A9128E5F1AB9FCAFF534F0635EF9
    @Test()
    void buildTest() {
        //Arrange Statement(s)
        EqualsBuilder target = spy(new EqualsBuilder());
        doReturn(false).when(target).isEquals();
        
        //Act Statement(s)
        Boolean result = target.build();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.FALSE));
            verify(target).isEquals();
        });
    }

    //BaseRock generated method id: ${isEqualsTest}, hash: 847B60D31BF13A4079F747F33C1CDA63
    @Test()
    void isEqualsTest() {
        //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        
        //Act Statement(s)
        boolean result = target.isEquals();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${reflectionAppendWhenLhsEqualsRhs}, hash: A1569D658B9D42795DD8143A189B1E52
    @Test()
    void reflectionAppendWhenLhsEqualsRhs() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        Object object = new Object();
        
        //Act Statement(s)
        EqualsBuilder result = target.reflectionAppend(object, object);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${reflectionAppendWhenRhsIsNull}, hash: 93F80DA4BCD8FDC1EDEBDF38B39772E3
    @Test()
    void reflectionAppendWhenRhsIsNull() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : true
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        Object object = new Object();
        Object object2 = null;
        
        //Act Statement(s)
        EqualsBuilder result = target.reflectionAppend(object, object2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${reflectionAppendWhenTestClassIsArray}, hash: D10B49708FBE4060D9CAEFB5DD28E274
    @Test()
    void reflectionAppendWhenTestClassIsArray() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhsClass.isInstance(rhs)) : true
         * (!rhsClass.isInstance(lhs)) : true
         * (testClass.isArray()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        EqualsBuilder target = spy(new EqualsBuilder());
        EqualsBuilder equalsBuilder = new EqualsBuilder();
        Object object = new Object();
        Object object2 = new Object();
        doReturn(equalsBuilder).when(target).append(object, object2);
        
        //Act Statement(s)
        EqualsBuilder result = target.reflectionAppend(object, object2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append(object, object2);
        });
    }

    //BaseRock generated method id: ${reflectionAppendWhenCaughtIllegalArgumentException}, hash: E4B6BE817C3EAE7365E0E3FE50289321
    @Test()
    void reflectionAppendWhenCaughtIllegalArgumentException() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhsClass.isInstance(rhs)) : true
         * (!rhsClass.isInstance(lhs)) : true
         * (testClass.isArray()) : true
         * (catch-exception (IllegalArgumentException)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        EqualsBuilder target = spy(new EqualsBuilder());
        EqualsBuilder equalsBuilder = new EqualsBuilder();
        Object object = new Object();
        Object object2 = new Object();
        doReturn(equalsBuilder).when(target).append(object, object2);
        
        //Act Statement(s)
        EqualsBuilder result = target.reflectionAppend(object, object2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).append(object, object2);
        });
    }

    //BaseRock generated method id: ${reflectionAppendWhenBypassReflectionClassesContainsRhsClass}, hash: FC42EFD24275789E947C54954C1B4A85
    @Test()
    void reflectionAppendWhenBypassReflectionClassesContainsRhsClass() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhsClass.isInstance(rhs)) : true
         * (!rhsClass.isInstance(lhs)) : true
         * (testClass.isArray()) : false
         * (bypassReflectionClasses != null) : true
         * (bypassReflectionClasses.contains(lhsClass)) : false
         * (bypassReflectionClasses.contains(rhsClass)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        Object object = new Object();
        Object object2 = new Object();
        
        //Act Statement(s)
        EqualsBuilder result = target.reflectionAppend(object, object2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${reflectionAppendWhenTestClassNotEqualsReflectUpToClass}, hash: 7D53A98BF807BE39E57E0EE67C047EFB
    @Test()
    void reflectionAppendWhenTestClassNotEqualsReflectUpToClass() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhsClass.isInstance(rhs)) : true
         * (!rhsClass.isInstance(lhs)) : true
         * (testClass.isArray()) : false
         * (bypassReflectionClasses != null) : true
         * (bypassReflectionClasses.contains(lhsClass)) : false
         * (bypassReflectionClasses.contains(rhsClass)) : false
         * (isRegistered(lhs, rhs)) : true  #  inside reflectionAppend method
         * (testClass.getSuperclass() != null) : true
         * (testClass != reflectUpToClass) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<EqualsBuilder> equalsBuilder = mockStatic(EqualsBuilder.class)) {
            Object object = new Object();
            Object object2 = new Object();
            equalsBuilder.when(() -> EqualsBuilder.isRegistered(object, object2)).thenReturn(true);
            EqualsBuilder target = new EqualsBuilder();
            //Act Statement(s)
            EqualsBuilder result = target.reflectionAppend(object, object2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(target));
                equalsBuilder.verify(() -> EqualsBuilder.isRegistered(object, object2), atLeast(2));
            });
        }
    }

    //BaseRock generated method id: ${reflectionAppendWhenTestClassNotEqualsReflectUpToClassAndCaughtIllegalArgumentException}, hash: 0FF3DDF7B3EA148508602345CC4C391A
    @Test()
    void reflectionAppendWhenTestClassNotEqualsReflectUpToClassAndCaughtIllegalArgumentException() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhsClass.isInstance(rhs)) : true
         * (!rhsClass.isInstance(lhs)) : true
         * (testClass.isArray()) : false
         * (bypassReflectionClasses != null) : true
         * (bypassReflectionClasses.contains(lhsClass)) : false
         * (bypassReflectionClasses.contains(rhsClass)) : false
         * (isRegistered(lhs, rhs)) : true  #  inside reflectionAppend method
         * (testClass.getSuperclass() != null) : true
         * (testClass != reflectUpToClass) : true
         * (catch-exception (IllegalArgumentException)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<EqualsBuilder> equalsBuilder = mockStatic(EqualsBuilder.class)) {
            Object object = new Object();
            Object object2 = new Object();
            equalsBuilder.when(() -> EqualsBuilder.isRegistered(object, object2)).thenReturn(true);
            EqualsBuilder target = new EqualsBuilder();
            //Act Statement(s)
            EqualsBuilder result = target.reflectionAppend(object, object2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(target));
                equalsBuilder.verify(() -> EqualsBuilder.isRegistered(object, object2), atLeast(2));
            });
        }
    }

    //BaseRock generated method id: ${reflectionAppendWhenRegistryNotIsEmptyAndTestClassGetSuperclassIsNotNullAndTestClassNotEqualsReflectUpToClass}, hash: 47165F16FE30741F03FCB8B579C9F273
    @Test()
    void reflectionAppendWhenRegistryNotIsEmptyAndTestClassGetSuperclassIsNotNullAndTestClassNotEqualsReflectUpToClass() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhsClass.isInstance(rhs)) : false
         * (rhsClass.isInstance(lhs)) : true
         * (!lhsClass.isInstance(rhs)) : true
         * (testClass.isArray()) : false
         * (bypassReflectionClasses != null) : true
         * (bypassReflectionClasses.contains(lhsClass)) : false
         * (bypassReflectionClasses.contains(rhsClass)) : false
         * (isRegistered(lhs, rhs)) : false  #  inside reflectionAppend method
         * (i < fields.length) : false  #  inside reflectionAppend method
         * (registry.isEmpty()) : false  #  inside unregister method
         * (testClass.getSuperclass() != null) : true
         * (testClass != reflectUpToClass) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<EqualsBuilder> equalsBuilder = mockStatic(EqualsBuilder.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            equalsBuilder.when(() -> EqualsBuilder.isRegistered(object, object2)).thenReturn(false);
            MutablePair<IDKey, IDKey> mutablePair = new MutablePair<>();
            MutablePair<IDKey, IDKey> mutablePair2 = new MutablePair<>();
            MutablePair<IDKey, IDKey> mutablePair3 = new MutablePair<>();
            MutablePair<IDKey, IDKey> mutablePair4 = new MutablePair<>();
            equalsBuilder.when(() -> EqualsBuilder.getRegisterPair(object, object2)).thenReturn(mutablePair).thenReturn(mutablePair2).thenReturn(mutablePair3).thenReturn(mutablePair4);
            EqualsBuilder target = new EqualsBuilder();
            //Act Statement(s)
            EqualsBuilder result = target.reflectionAppend(object, object2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(target));
                equalsBuilder.verify(() -> EqualsBuilder.isRegistered(object, object2), atLeast(2));
                equalsBuilder.verify(() -> EqualsBuilder.getRegisterPair(object, object2), atLeast(4));
            });
        }
    }

    //BaseRock generated method id: ${reflectionAppendWhenRegistryIsEmptyAndTestClassGetSuperclassIsNull}, hash: 4AFF8513252F6C5A986D7564F07D0C78
    @Test()
    void reflectionAppendWhenRegistryIsEmptyAndTestClassGetSuperclassIsNull() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhsClass.isInstance(rhs)) : true
         * (!rhsClass.isInstance(lhs)) : true
         * (testClass.isArray()) : false
         * (bypassReflectionClasses != null) : true
         * (bypassReflectionClasses.contains(lhsClass)) : false
         * (bypassReflectionClasses.contains(rhsClass)) : false
         * (isRegistered(lhs, rhs)) : false  #  inside reflectionAppend method
         * (i < fields.length) : true  #  inside reflectionAppend method
         * (isEquals) : true  #  inside reflectionAppend method
         * (!ArrayUtils.contains(excludeFields, field.getName())) : true  #  inside reflectionAppend method
         * (!field.getName().contains("$")) : true  #  inside reflectionAppend method
         * (testTransients) : false  #  inside reflectionAppend method
         * (!Modifier.isStatic(field.getModifiers())) : true  #  inside reflectionAppend method
         * (!field.isAnnotationPresent(EqualsExclude.class)) : true  #  inside reflectionAppend method
         * (registry.isEmpty()) : true  #  inside unregister method
         * (testClass.getSuperclass() != null) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<EqualsBuilder> equalsBuilder = mockStatic(EqualsBuilder.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            equalsBuilder.when(() -> EqualsBuilder.isRegistered(object, object2)).thenReturn(false);
            MutablePair<IDKey, IDKey> mutablePair = new MutablePair<>();
            MutablePair<IDKey, IDKey> mutablePair2 = new MutablePair<>();
            equalsBuilder.when(() -> EqualsBuilder.getRegisterPair(object, object2)).thenReturn(mutablePair).thenReturn(mutablePair2);
            EqualsBuilder target = spy(new EqualsBuilder());
            EqualsBuilder equalsBuilder2 = new EqualsBuilder();
            Object object3 = new Object();
            Object object4 = new Object();
            doReturn(equalsBuilder2).when(target).append(object3, object4);
            //Act Statement(s)
            EqualsBuilder result = target.reflectionAppend(object, object2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(target));
                equalsBuilder.verify(() -> EqualsBuilder.isRegistered(object, object2), atLeast(1));
                equalsBuilder.verify(() -> EqualsBuilder.getRegisterPair(object, object2), atLeast(2));
                verify(target, atLeast(1)).append(object3, object4);
            });
        }
    }

    //BaseRock generated method id: ${reflectionAppendWhenRegistryIsEmpty}, hash: 1BA4A7D794B2994EFA855BE990FA1A0E
    @Test()
    void reflectionAppendWhenRegistryIsEmpty() {
        /* Branches:
         * (!isEquals) : false
         * (lhs == rhs) : false
         * (lhs == null) : false
         * (rhs == null) : false
         * (lhsClass.isInstance(rhs)) : true
         * (!rhsClass.isInstance(lhs)) : true
         * (testClass.isArray()) : false
         * (bypassReflectionClasses != null) : true
         * (bypassReflectionClasses.contains(lhsClass)) : false
         * (bypassReflectionClasses.contains(rhsClass)) : false
         * (isRegistered(lhs, rhs)) : true  #  inside reflectionAppend method
         * (testClass.getSuperclass() != null) : true
         * (testClass != reflectUpToClass) : true
         * (i < fields.length) : true  #  inside reflectionAppend method
         * (isEquals) : true  #  inside reflectionAppend method
         * (!ArrayUtils.contains(excludeFields, field.getName())) : true  #  inside reflectionAppend method
         * (!field.getName().contains("$")) : true  #  inside reflectionAppend method
         * (testTransients) : false  #  inside reflectionAppend method
         * (!Modifier.isStatic(field.getModifiers())) : true  #  inside reflectionAppend method
         * (!field.isAnnotationPresent(EqualsExclude.class)) : true  #  inside reflectionAppend method
         * (registry.isEmpty()) : true  #  inside unregister method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<EqualsBuilder> equalsBuilder = mockStatic(EqualsBuilder.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            equalsBuilder.when(() -> EqualsBuilder.isRegistered(object, object2)).thenReturn(false);
            MutablePair<IDKey, IDKey> mutablePair = new MutablePair<>();
            MutablePair<IDKey, IDKey> mutablePair2 = new MutablePair<>();
            equalsBuilder.when(() -> EqualsBuilder.getRegisterPair(object, object2)).thenReturn(mutablePair).thenReturn(mutablePair2);
            EqualsBuilder target = spy(new EqualsBuilder());
            EqualsBuilder equalsBuilder2 = new EqualsBuilder();
            Object object3 = new Object();
            Object object4 = new Object();
            doReturn(equalsBuilder2).when(target).append(object3, object4);
            //Act Statement(s)
            EqualsBuilder result = target.reflectionAppend(object, object2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(target));
                equalsBuilder.verify(() -> EqualsBuilder.isRegistered(object, object2), atLeast(2));
                equalsBuilder.verify(() -> EqualsBuilder.getRegisterPair(object, object2), atLeast(2));
                verify(target, atLeast(1)).append(object3, object4);
            });
        }
    }

    //BaseRock generated method id: ${resetTest}, hash: 0E0B0F7152BE8A1FA67062D5A3DC74C3
    @Test()
    void resetTest() {
        //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        
        //Act Statement(s)
        target.reset();
    }

    //BaseRock generated method id: ${setBypassReflectionClassesTest}, hash: BAE17E1AB19D18DF93734188819132B1
    @Test()
    void setBypassReflectionClassesTest() {
        //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        List<Class<?>> anyList = new ArrayList<>();
        
        //Act Statement(s)
        EqualsBuilder result = target.setBypassReflectionClasses(anyList);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${setEqualsTest}, hash: A5D0B44B667B7C686604D3DBBBECDDAF
    @Test()
    void setEqualsTest() {
        //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        
        //Act Statement(s)
        target.setEquals(false);
    }

    //BaseRock generated method id: ${setExcludeFieldsTest}, hash: C92AEA1DBCF692A999C099BDE5DD83CC
    @Test()
    void setExcludeFieldsTest() {
        //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        String[] stringArray = new String[] {};
        
        //Act Statement(s)
        EqualsBuilder result = target.setExcludeFields(stringArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${setReflectUpToClassTest}, hash: 777808C6B29249ADA11F42C3CB6B392D
    @Test()
    void setReflectUpToClassTest() {
        //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        
        //Act Statement(s)
        EqualsBuilder result = target.setReflectUpToClass(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${setTestRecursiveTest}, hash: 80CF5E012E70EB172D3BA54EDDAC2544
    @Test()
    void setTestRecursiveTest() {
        //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        
        //Act Statement(s)
        EqualsBuilder result = target.setTestRecursive(false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${setTestTransientsTest}, hash: 592608CE3B09C10B21520E37A282B006
    @Test()
    void setTestTransientsTest() {
        //Arrange Statement(s)
        EqualsBuilder target = new EqualsBuilder();
        
        //Act Statement(s)
        EqualsBuilder result = target.setTestTransients(false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }
}

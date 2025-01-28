package org.apache.commons.lang3.reflect;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.Type;
import java.lang.reflect.GenericDeclaration;
import java.util.Map;
import java.util.HashMap;
import org.mockito.MockedStatic;
import java.lang.reflect.WildcardType;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.verify;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mockStatic;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class TypeUtilsBaseRockGeneratedTest {

    private final GenericArrayType genericArrayTypeMock = mock(GenericArrayType.class);

    private final GenericArrayType genericArrayTypeMock2 = mock(GenericArrayType.class);

    private final ParameterizedType parameterizedTypeMock = mock(ParameterizedType.class);

    private final ParameterizedType parameterizedTypeMock2 = mock(ParameterizedType.class);

    private final Type typeMock = mock(Type.class);

    private final Type typeMock2 = mock(Type.class);

    private final TypeVariable<?> typeVariableMock = mock(TypeVariable.class);

    private final WildcardType wildcardTypeMock = mock(WildcardType.class);

    //BaseRock generated method id: ${containsTypeVariablesWhenTypeInstanceOfTypeVariable_}, hash: BAB6008176A4CFF1D47963B03917D157
    @Test()
    void containsTypeVariablesWhenTypeInstanceOfTypeVariable_() {
        /* Branches:
         * (type instanceof TypeVariable<?>) : true
         */
         //Arrange Statement(s)
        TypeVariable typeVariableMock = mock(TypeVariable.class);
        
        //Act Statement(s)
        boolean result = TypeUtils.containsTypeVariables(typeVariableMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${containsTypeVariablesWhenTypeGetTypeParametersLengthGreaterThan0}, hash: F8BC24AAB426E6D50F6B962D3A2B5FB5
    @Test()
    void containsTypeVariablesWhenTypeGetTypeParametersLengthGreaterThan0() {
        /* Branches:
         * (type instanceof TypeVariable<?>) : false
         * (type instanceof Class<?>) : true
         * (((Class<?>) type).getTypeParameters().length > 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.containsTypeVariables(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${containsTypeVariablesWhenTypeGetTypeParametersLengthNotGreaterThan0}, hash: 6C67339951D6F91969A73200DCD6A976
    @Test()
    void containsTypeVariablesWhenTypeGetTypeParametersLengthNotGreaterThan0() {
        /* Branches:
         * (type instanceof TypeVariable<?>) : false
         * (type instanceof Class<?>) : true
         * (((Class<?>) type).getTypeParameters().length > 0) : false
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.containsTypeVariables(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${containsTypeVariablesWhenContainsTypeVariablesArg}, hash: 38F948F99047D8EABCD0D1CD0460009B
    @Test()
    void containsTypeVariablesWhenContainsTypeVariablesArg() {
        /* Branches:
         * (type instanceof TypeVariable<?>) : false
         * (type instanceof Class<?>) : false
         * (type instanceof ParameterizedType) : true
         * (for-each(((ParameterizedType) type).getActualTypeArguments())) : true
         * (containsTypeVariables(arg)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.containsTypeVariables(parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${containsTypeVariablesWhenContainsTypeVariablesNotArg}, hash: 56771622AAC801EEF0DEA062E8B74038
    @Test()
    void containsTypeVariablesWhenContainsTypeVariablesNotArg() {
        /* Branches:
         * (type instanceof TypeVariable<?>) : false
         * (type instanceof Class<?>) : false
         * (type instanceof ParameterizedType) : true
         * (for-each(((ParameterizedType) type).getActualTypeArguments())) : true
         * (containsTypeVariables(arg)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.containsTypeVariables(parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${containsTypeVariablesWhenTypeInstanceOfGenericArrayType}, hash: 2D58C276900E7BE7FC2E873090A9DCB1
    @Test()
    void containsTypeVariablesWhenTypeInstanceOfGenericArrayType() {
        /* Branches:
         * (type instanceof TypeVariable<?>) : false
         * (type instanceof Class<?>) : false
         * (type instanceof ParameterizedType) : false
         * (type instanceof WildcardType) : false
         * (type instanceof GenericArrayType) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.containsTypeVariables(genericArrayTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${containsTypeVariablesWhenTypeNotInstanceOfGenericArrayType}, hash: 8826C1DCAFF21CC9EB53A1C8A24109B8
    @Test()
    void containsTypeVariablesWhenTypeNotInstanceOfGenericArrayType() {
        /* Branches:
         * (type instanceof TypeVariable<?>) : false
         * (type instanceof Class<?>) : false
         * (type instanceof ParameterizedType) : false
         * (type instanceof WildcardType) : false
         * (type instanceof GenericArrayType) : false
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.containsTypeVariables(typeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${containsTypeVariablesWhenContainsTypeVariables0IndexOfGetImplicitUpperBoundsWild}, hash: 945329D4A303FD1DC1D240271418C93C
    @Test()
    void containsTypeVariablesWhenContainsTypeVariables0IndexOfGetImplicitUpperBoundsWild() {
        /* Branches:
         * (type instanceof TypeVariable<?>) : false
         * (type instanceof Class<?>) : false
         * (type instanceof ParameterizedType) : false
         * (type instanceof WildcardType) : true
         * (containsTypeVariables(getImplicitLowerBounds(wild)[0])) : false
         * (containsTypeVariables(getImplicitUpperBounds(wild)[0])) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitLowerBounds(wildcardTypeMock)).thenReturn(typeArray);
            Type[] typeArray2 = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray2);
            //Act Statement(s)
            boolean result = TypeUtils.containsTypeVariables(wildcardTypeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                typeUtils.verify(() -> TypeUtils.getImplicitLowerBounds(wildcardTypeMock), atLeast(1));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${containsTypeVariablesWhenContainsTypeVariablesNot0IndexOfGetImplicitUpperBoundsWild}, hash: 614461A485FAF7BA5979D455D5CBD951
    @Test()
    void containsTypeVariablesWhenContainsTypeVariablesNot0IndexOfGetImplicitUpperBoundsWild() {
        /* Branches:
         * (type instanceof TypeVariable<?>) : false
         * (type instanceof Class<?>) : false
         * (type instanceof ParameterizedType) : false
         * (type instanceof WildcardType) : true
         * (containsTypeVariables(getImplicitLowerBounds(wild)[0])) : false
         * (containsTypeVariables(getImplicitUpperBounds(wild)[0])) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitLowerBounds(wildcardTypeMock)).thenReturn(typeArray);
            Type[] typeArray2 = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray2);
            //Act Statement(s)
            boolean result = TypeUtils.containsTypeVariables(wildcardTypeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                typeUtils.verify(() -> TypeUtils.getImplicitLowerBounds(wildcardTypeMock), atLeast(1));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenRawTypeNotInstanceOfClass_ThrowsIllegalStateException}, hash: 86B53B450AE9F319DD9610208D286562
    @Test()
    void determineTypeArgumentsWhenRawTypeNotInstanceOfClass_ThrowsIllegalStateException() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : true  #  inside getRawType method
         */
         //Arrange Statement(s)
        Type typeMock = mock(Type.class, "type8");
        doReturn(typeMock).when(parameterizedTypeMock).getRawType();
        IllegalStateException illegalStateException = new IllegalStateException("Wait... What!? Type of rawType: type8");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
            verify(parameterizedTypeMock).getRawType();
        });
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenIsAssignableNotClsSuperClass}, hash: 3E67C0857F8A7643E61A9021DC87D23E
    @Test()
    void determineTypeArgumentsWhenIsAssignableNotClsSuperClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : true
         */
         //Arrange Statement(s)
        doReturn(null).when(parameterizedTypeMock).getRawType();
        
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(nullValue()));
            verify(parameterizedTypeMock).getRawType();
        });
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenToClassNotIsPrimitiveAndIsAssignableClsSuperClassThrowsNullPointerException}, hash: 85F2C09D7BA6BBABA9C785BB7F9E7665
    @Test()
    void determineTypeArgumentsWhenToClassNotIsPrimitiveAndIsAssignableClsSuperClassThrowsNullPointerException() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : true  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Class<?> _class = null;
        //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            TypeUtils.determineTypeArguments(_class, parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenToClassIsPrimitiveAndIsAssignableNotClsSuperClass}, hash: B2FAB8B897120F7C074EC9AF4A0EE9E7
    @Test()
    void determineTypeArgumentsWhenToClassIsPrimitiveAndIsAssignableNotClsSuperClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : true  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Class<?> _class = null;
        
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.determineTypeArguments(_class, parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenClsEqualsSuperClassThrowsIllegalStateException}, hash: CA2428AB73CA933EA64E5324A0D1A5C3
    @Test()
    void determineTypeArgumentsWhenClsEqualsSuperClassThrowsIllegalStateException() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         */
        Type typeMock = mock(Type.class, "type9");
        doReturn(Object.class, typeMock).when(parameterizedTypeMock).getRawType();
        IllegalStateException illegalStateException = new IllegalStateException("Wait... What!? Type of rawType: type9");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
            verify(parameterizedTypeMock, times(2)).getRawType();
        });
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenIsAssignableNotClsToClass}, hash: 56141EECCBD1D3EA036C910DBD5965AF
    @Test()
    void determineTypeArgumentsWhenIsAssignableNotClsToClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenOwnerTypeInstanceOfParameterizedTypeThrowsIllegalStateException}, hash: F6B3582E2E0F9FDFC31DDDDE5C10FE16
    @Test()
    void determineTypeArgumentsWhenOwnerTypeInstanceOfParameterizedTypeThrowsIllegalStateException() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        IllegalStateException illegalStateException = new IllegalStateException("s1");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenOwnerTypeInstanceOfParameterizedTypeThrowsNullPointerException}, hash: AD404A85D6AB877FCC03ADF5ABB1966C
    @Test()
    void determineTypeArgumentsWhenOwnerTypeInstanceOfParameterizedTypeThrowsNullPointerException() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenToClassEqualsCls}, hash: 86FB0ED6A09107B8A6292FAA58567B6D
    @Test()
    void determineTypeArgumentsWhenToClassEqualsCls() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
        TypeVariable typeVariableMock = mock(TypeVariable.class);
        Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
        anyMap.put(typeVariableMock, typeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(anyMap)));
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenMidTypeInstanceOfParameterizedTypeThrowsIllegalStateException}, hash: DA00926E94A7A9AC8648F8A0B111EF74
    @Test()
    void determineTypeArgumentsWhenMidTypeInstanceOfParameterizedTypeThrowsIllegalStateException() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        IllegalStateException illegalStateException = new IllegalStateException("s1");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenTypeInstanceOfClass_AndIsAssignableNotClsToClass}, hash: 8CED62A0F93F8812DA29F48956ABB398
    @Test()
    void determineTypeArgumentsWhenTypeInstanceOfClass_AndIsAssignableNotClsToClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (!isAssignable(cls, toClass)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenGenericInterfaceIsNullAndTypeInstanceOfClass_AndIsAssignableNotClsToClass}, hash: 24414BD10B5EADEBE3992016200412C6
    @Test()
    void determineTypeArgumentsWhenGenericInterfaceIsNullAndTypeInstanceOfClass_AndIsAssignableNotClsToClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (!isAssignable(cls, toClass)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenIsAssignableClsToClassThrowsNullPointerException}, hash: 127B76526A20A079E41F46646D143B09
    @Test()
    void determineTypeArgumentsWhenIsAssignableClsToClassThrowsNullPointerException() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenTypeNotInstanceOfClass_AndTypeInstanceOfParameterizedType}, hash: 0D021E4C040AE5EC258F3792301CB44B
    @Test()
    void determineTypeArgumentsWhenTypeNotInstanceOfClass_AndTypeInstanceOfParameterizedType() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenToClassIsPrimitive}, hash: 3DAE59E71E45E4B12D4F9EAE431EFC84
    @Test()
    void determineTypeArgumentsWhenToClassIsPrimitive() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenToClassIsArray}, hash: 130EA1A1E7365EA4401967ECFC8AE29D
    @Test()
    void determineTypeArgumentsWhenToClassIsArray() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : true  #  inside getTypeArguments method
         * (toClass.isArray()) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenSubtypeVarAssignsIsNotNullAndToClassNotEqualsCls}, hash: CEC60E051FACEFD3938E9C47F06B6748
    @Test()
    void determineTypeArgumentsWhenSubtypeVarAssignsIsNotNullAndToClassNotEqualsCls() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenToClassNotIsPrimitiveAndSubtypeVarAssignsIsNotNullAndToClassEqualsCls}, hash: 3B529F9DF1E46E792680027F1B825975
    @Test()
    void determineTypeArgumentsWhenToClassNotIsPrimitiveAndSubtypeVarAssignsIsNotNullAndToClassEqualsCls() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenToClassNotEqualsClsThrowsIllegalStateException}, hash: 4BA095728E5472AF606B2A28B8D80BED
    @Test()
    void determineTypeArgumentsWhenToClassNotEqualsClsThrowsIllegalStateException() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        IllegalStateException illegalStateException = new IllegalStateException("s1");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenIsAssignableClsToClassAndClsIsPrimitiveAndToClassNotIsPrimitiveAndSubtypeVarAssignsIsNotNullA}, hash: 99172BAA5170BA75AD879440D9E55C84
    @Test()
    void determineTypeArgumentsWhenIsAssignableClsToClassAndClsIsPrimitiveAndToClassNotIsPrimitiveAndSubtypeVarAssignsIsNotNullA() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenIsAssignableBoundToClass}, hash: 6939E9309B5A985DD6CEF68C687D8234
    @Test()
    void determineTypeArgumentsWhenIsAssignableBoundToClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : false  #  inside getTypeArguments method
         * (type instanceof WildcardType) : true  #  inside getTypeArguments method
         * (for-each(getImplicitUpperBounds((WildcardType) type))) : true  #  inside getTypeArguments method
         * (isAssignable(bound, toClass)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray);
            //Act Statement(s)
            Map<TypeVariable<?>, Type> result = TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.size(), equalTo(0));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenGetImplicitUpperBoundsTypeIsNotEmptyAndIsAssignableBoundToClass}, hash: D8C1949071141DCE81A436C143591417
    @Test()
    void determineTypeArgumentsWhenGetImplicitUpperBoundsTypeIsNotEmptyAndIsAssignableBoundToClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : false  #  inside getTypeArguments method
         * (type instanceof WildcardType) : true  #  inside getTypeArguments method
         * (for-each(getImplicitUpperBounds((WildcardType) type))) : true  #  inside getTypeArguments method
         * (isAssignable(bound, toClass)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray);
            //Act Statement(s)
            Map<TypeVariable<?>, Type> result = TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.size(), equalTo(0));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenTypeInstanceOfWildcardTypeAndGetImplicitUpperBoundsTypeIsNotEmptyAndIsAssignableBoundToClass}, hash: 0DDD1593095ADAB1FF4AC6B91E9FE509
    @Test()
    void determineTypeArgumentsWhenTypeInstanceOfWildcardTypeAndGetImplicitUpperBoundsTypeIsNotEmptyAndIsAssignableBoundToClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : false  #  inside getTypeArguments method
         * (type instanceof WildcardType) : true  #  inside getTypeArguments method
         * (for-each(getImplicitUpperBounds((WildcardType) type))) : true  #  inside getTypeArguments method
         * (isAssignable(bound, toClass)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray);
            //Act Statement(s)
            Map<TypeVariable<?>, Type> result = TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.size(), equalTo(0));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenIsAssignableNotBoundToClass}, hash: 256961366301C3425159C6C2FD8FAA4D
    @Test()
    void determineTypeArgumentsWhenIsAssignableNotBoundToClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : false  #  inside getTypeArguments method
         * (type instanceof WildcardType) : true  #  inside getTypeArguments method
         * (for-each(getImplicitUpperBounds((WildcardType) type))) : true  #  inside getTypeArguments method
         * (isAssignable(bound, toClass)) : false  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray);
            //Act Statement(s)
            Map<TypeVariable<?>, Type> result = TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(nullValue()));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenGetImplicitUpperBoundsTypeIsNotEmptyAndIsAssignableNotBoundToClass}, hash: 89A3E215AADE6EFEDA05FF730562121B
    @Test()
    void determineTypeArgumentsWhenGetImplicitUpperBoundsTypeIsNotEmptyAndIsAssignableNotBoundToClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : false  #  inside getTypeArguments method
         * (type instanceof WildcardType) : true  #  inside getTypeArguments method
         * (for-each(getImplicitUpperBounds((WildcardType) type))) : true  #  inside getTypeArguments method
         * (isAssignable(bound, toClass)) : false  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray);
            //Act Statement(s)
            Map<TypeVariable<?>, Type> result = TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(nullValue()));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenTypeInstanceOfParameterizedTypeThrowsIllegalStateException}, hash: EA2A8EB7980DB357818EF8D1BE1176F0
    @Test()
    void determineTypeArgumentsWhenTypeInstanceOfParameterizedTypeThrowsIllegalStateException() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : false  #  inside getTypeArguments method
         * (type instanceof WildcardType) : true  #  inside getTypeArguments method
         * (for-each(getImplicitUpperBounds((WildcardType) type))) : true  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : true  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray);
            //Act Statement(s)
            final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
                TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
            });
            IllegalStateException illegalStateException = new IllegalStateException("s1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenMidTypeInstanceOfClass_}, hash: 531C07312031EBDCC52AE5A91015167E
    @Test()
    void determineTypeArgumentsWhenMidTypeInstanceOfClass_() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (midType instanceof Class<?>) : true  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenMidTypeNotInstanceOfClass_ThrowsIllegalStateException}, hash: 998B83C1D23DAD88A1ADBE09F1ACF9FB
    @Test()
    void determineTypeArgumentsWhenMidTypeNotInstanceOfClass_ThrowsIllegalStateException() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (midType instanceof Class<?>) : false  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        IllegalStateException illegalStateException = new IllegalStateException("s1");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenIsAssignableGenericInterfaceMidClass}, hash: 76B55288C8D4D22CF095999ADAEAB736
    @Test()
    void determineTypeArgumentsWhenIsAssignableGenericInterfaceMidClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenToClassNotEqualsClsAndIsAssignableGenericInterfaceMidClass}, hash: AB8E9BAA73643119610B4E51D9865DFD
    @Test()
    void determineTypeArgumentsWhenToClassNotEqualsClsAndIsAssignableGenericInterfaceMidClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenIsAssignableNotGenericInterfaceMidClass}, hash: 4A44D5C31F545815DAB49C9A020A3EAA
    @Test()
    void determineTypeArgumentsWhenIsAssignableNotGenericInterfaceMidClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (isAssignable(genericInterface, (Type) midClass)) : false  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenTypeInstanceOfParameterizedTypeAndIsAssignableBoundToClass}, hash: 08FD6C42E59E077F75F6284F2526DEA8
    @Test()
    void determineTypeArgumentsWhenTypeInstanceOfParameterizedTypeAndIsAssignableBoundToClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : false  #  inside getTypeArguments method
         * (type instanceof WildcardType) : true  #  inside getTypeArguments method
         * (for-each(getImplicitUpperBounds((WildcardType) type))) : true  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : true  #  inside isAssignable method
         * (isAssignable(bound, toClass)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray);
            //Act Statement(s)
            Map<TypeVariable<?>, Type> result = TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.size(), equalTo(0));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenIsAssignableBoundToClassAndIsAssignableBoundToClass}, hash: 29610A4E79BCD09A7CC1CCEA3D0A356A
    @Test()
    void determineTypeArgumentsWhenIsAssignableBoundToClassAndIsAssignableBoundToClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : false  #  inside getTypeArguments method
         * (type instanceof WildcardType) : true  #  inside getTypeArguments method
         * (for-each(getImplicitUpperBounds((WildcardType) type))) : true  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside isAssignable method
         * (type instanceof TypeVariable<?>) : true  #  inside isAssignable method
         * (for-each(((TypeVariable<?>) type).getBounds())) : true  #  inside isAssignable method
         * (isAssignable(bound, toClass)) : true  #  inside isAssignable method
         * (isAssignable(bound, toClass)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray);
            //Act Statement(s)
            Map<TypeVariable<?>, Type> result = TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.size(), equalTo(0));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenIsAssignableNotBoundToClassAndIsAssignableNotBoundToClass}, hash: 4BE0FDA1EB59159C784F036A027C61F5
    @Test()
    void determineTypeArgumentsWhenIsAssignableNotBoundToClassAndIsAssignableNotBoundToClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : false  #  inside getTypeArguments method
         * (type instanceof WildcardType) : true  #  inside getTypeArguments method
         * (for-each(getImplicitUpperBounds((WildcardType) type))) : true  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside isAssignable method
         * (type instanceof TypeVariable<?>) : true  #  inside isAssignable method
         * (for-each(((TypeVariable<?>) type).getBounds())) : true  #  inside isAssignable method
         * (isAssignable(bound, toClass)) : false  #  inside isAssignable method
         * (isAssignable(bound, toClass)) : false  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray);
            //Act Statement(s)
            Map<TypeVariable<?>, Type> result = TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(nullValue()));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenIsAssignableTypeGetGenericComponentTypeToClassGetComponentTypeAndIsAssignableBoundToClass}, hash: C34975837DE238473D86C7AA08D6FB4F
    @Test()
    void determineTypeArgumentsWhenIsAssignableTypeGetGenericComponentTypeToClassGetComponentTypeAndIsAssignableBoundToClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : false  #  inside getTypeArguments method
         * (type instanceof WildcardType) : true  #  inside getTypeArguments method
         * (for-each(getImplicitUpperBounds((WildcardType) type))) : true  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside isAssignable method
         * (type instanceof TypeVariable<?>) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : true  #  inside isAssignable method
         * (toClass.equals(Object.class)) : false  #  inside isAssignable method
         * (toClass.isArray()) : true  #  inside isAssignable method
         * (isAssignable(((GenericArrayType) type).getGenericComponentType(), toClass.getComponentType())) : true  #  inside isAssignable method
         * (isAssignable(bound, toClass)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray);
            //Act Statement(s)
            Map<TypeVariable<?>, Type> result = TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.size(), equalTo(0));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${determineTypeArgumentsWhenIsAssignableNotTypeGetGenericComponentTypeToClassGetComponentTypeAndIsAssignableNotBoundToCla}, hash: 62BB58083BC398AF7BD18CCF238F6047
    @Test()
    void determineTypeArgumentsWhenIsAssignableNotTypeGetGenericComponentTypeToClassGetComponentTypeAndIsAssignableNotBoundToCla() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, superClass)) : false
         * (cls.equals(superClass)) : true
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : false  #  inside getTypeArguments method
         * (type instanceof WildcardType) : true  #  inside getTypeArguments method
         * (for-each(getImplicitUpperBounds((WildcardType) type))) : true  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside isAssignable method
         * (type instanceof TypeVariable<?>) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : true  #  inside isAssignable method
         * (toClass.equals(Object.class)) : false  #  inside isAssignable method
         * (toClass.isArray()) : true  #  inside isAssignable method
         * (isAssignable(((GenericArrayType) type).getGenericComponentType(), toClass.getComponentType())) : false  #  inside isAssignable method
         * (isAssignable(bound, toClass)) : false  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray);
            //Act Statement(s)
            Map<TypeVariable<?>, Type> result = TypeUtils.determineTypeArguments(Object.class, parameterizedTypeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(nullValue()));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${equals2WhenObjectsEqualsType1Type2}, hash: 84AAD5CE7C83FC301D110B23138DEDCD
    @Test()
    void equals2WhenObjectsEqualsType1Type2() {
        /* Branches:
         * (Objects.equals(type1, type2)) : true
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.equals(typeMock, typeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equals2WhenType1InstanceOfWildcardType}, hash: AA974DB6D64E8B3C3FFADF16B15699A1
    @Test()
    void equals2WhenType1InstanceOfWildcardType() {
        /* Branches:
         * (Objects.equals(type1, type2)) : false
         * (type1 instanceof ParameterizedType) : false
         * (type1 instanceof GenericArrayType) : false
         * (type1 instanceof WildcardType) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.equals((Type) wildcardTypeMock, typeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equals2WhenType1NotInstanceOfWildcardType}, hash: CF157A867956DF229D2DA4B7FF5439EA
    @Test()
    void equals2WhenType1NotInstanceOfWildcardType() {
        /* Branches:
         * (Objects.equals(type1, type2)) : false
         * (type1 instanceof ParameterizedType) : false
         * (type1 instanceof GenericArrayType) : false
         * (type1 instanceof WildcardType) : false
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.equals(typeMock, typeMock2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equals2WhenEqualsNotParameterizedTypeGetOwnerTypeOtherGetOwnerType}, hash: 17C7016DAAB47C5266BB4CF800C2A7D7
    @Test()
    void equals2WhenEqualsNotParameterizedTypeGetOwnerTypeOtherGetOwnerType() {
        /* Branches:
         * (Objects.equals(type1, type2)) : false
         * (type1 instanceof ParameterizedType) : true
         * (type instanceof ParameterizedType) : true  #  inside equals method
         * (equals(parameterizedType.getRawType(), other.getRawType())) : true  #  inside equals method
         * (equals(parameterizedType.getOwnerType(), other.getOwnerType())) : false  #  inside equals method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.equals((Type) parameterizedTypeMock, (Type) parameterizedTypeMock2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equals2WhenEqualsGenericArrayTypeGetGenericComponentTypeTypeGetGenericComponentType}, hash: 99F27EEC3B9419D3F29CCC52D0932BF5
    @Test()
    void equals2WhenEqualsGenericArrayTypeGetGenericComponentTypeTypeGetGenericComponentType() {
        /* Branches:
         * (Objects.equals(type1, type2)) : false
         * (type1 instanceof ParameterizedType) : false
         * (type1 instanceof GenericArrayType) : true
         * (type instanceof GenericArrayType) : true  #  inside equals method
         * (equals(genericArrayType.getGenericComponentType(), ((GenericArrayType) type).getGenericComponentType())) : true  #  inside equals method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.equals((Type) genericArrayTypeMock, (Type) genericArrayTypeMock2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equals2WhenEqualsNotGenericArrayTypeGetGenericComponentTypeTypeGetGenericComponentType}, hash: BB9121480506803986488CDEA53C9072
    @Test()
    void equals2WhenEqualsNotGenericArrayTypeGetGenericComponentTypeTypeGetGenericComponentType() {
        /* Branches:
         * (Objects.equals(type1, type2)) : false
         * (type1 instanceof ParameterizedType) : false
         * (type1 instanceof GenericArrayType) : true
         * (type instanceof GenericArrayType) : true  #  inside equals method
         * (equals(genericArrayType.getGenericComponentType(), ((GenericArrayType) type).getGenericComponentType())) : false  #  inside equals method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.equals((Type) genericArrayTypeMock, (Type) genericArrayTypeMock2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equals2WhenType1LengthNotEqualsType2Length}, hash: 9F64E24B96335723C088BBB4D4AC8FB8
    @Test()
    void equals2WhenType1LengthNotEqualsType2Length() {
        /* Branches:
         * (Objects.equals(type1, type2)) : false
         * (type1 instanceof ParameterizedType) : true
         * (type instanceof ParameterizedType) : true  #  inside equals method
         * (equals(parameterizedType.getRawType(), other.getRawType())) : true  #  inside equals method
         * (equals(parameterizedType.getOwnerType(), other.getOwnerType())) : true  #  inside equals method
         * (type1.length == type2.length) : false  #  inside equals method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.equals((Type) parameterizedTypeMock, (Type) parameterizedTypeMock2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equals2WhenEqualsNotIIndexOfType1IIndexOfType2}, hash: C370F51940BE188F914701E6D88FADC8
    @Test()
    void equals2WhenEqualsNotIIndexOfType1IIndexOfType2() {
        /* Branches:
         * (Objects.equals(type1, type2)) : false
         * (type1 instanceof ParameterizedType) : true
         * (type instanceof ParameterizedType) : true  #  inside equals method
         * (equals(parameterizedType.getRawType(), other.getRawType())) : true  #  inside equals method
         * (equals(parameterizedType.getOwnerType(), other.getOwnerType())) : true  #  inside equals method
         * (type1.length == type2.length) : true  #  inside equals method
         * (i < type1.length) : true  #  inside equals method
         * (!equals(type1[i], type2[i])) : true  #  inside equals method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            typeUtils.when(() -> TypeUtils.equals(typeMock, typeMock2)).thenReturn(false);
            //Act Statement(s)
            boolean result = TypeUtils.equals((Type) parameterizedTypeMock, (Type) parameterizedTypeMock2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                typeUtils.verify(() -> TypeUtils.equals(typeMock, typeMock2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${equals2WhenEqualsIIndexOfType1IIndexOfType2}, hash: 07E95001B12382F6DEF1183F92652C68
    @Test()
    void equals2WhenEqualsIIndexOfType1IIndexOfType2() {
        /* Branches:
         * (Objects.equals(type1, type2)) : false
         * (type1 instanceof ParameterizedType) : true
         * (type instanceof ParameterizedType) : true  #  inside equals method
         * (equals(parameterizedType.getRawType(), other.getRawType())) : true  #  inside equals method
         * (equals(parameterizedType.getOwnerType(), other.getOwnerType())) : true  #  inside equals method
         * (type1.length == type2.length) : true  #  inside equals method
         * (i < type1.length) : true  #  inside equals method
         * (!equals(type1[i], type2[i])) : false  #  inside equals method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            typeUtils.when(() -> TypeUtils.equals(typeMock, typeMock2)).thenReturn(false);
            //Act Statement(s)
            boolean result = TypeUtils.equals((Type) parameterizedTypeMock, (Type) parameterizedTypeMock2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                typeUtils.verify(() -> TypeUtils.equals(typeMock, typeMock2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${genericArrayTypeTest}, hash: 27BAF297687432ECA96A2783192F2FB6
    @Test()
    void genericArrayTypeTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        GenericArrayType result = TypeUtils.genericArrayType(typeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getArrayComponentTypeWhenClsIsArray}, hash: EAA06A7A70DF636CE412BC8CE276AD20
    @Test()
    void getArrayComponentTypeWhenClsIsArray() {
        /* Branches:
         * (type instanceof Class<?>) : true
         * (cls.isArray()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Type result = TypeUtils.getArrayComponentType(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Object.class)));
    }

    //BaseRock generated method id: ${getArrayComponentTypeWhenClsNotIsArray}, hash: 4CF1BCD6E11BFB198014A5FA915D3301
    @Test()
    void getArrayComponentTypeWhenClsNotIsArray() {
        /* Branches:
         * (type instanceof Class<?>) : true
         * (cls.isArray()) : false
         */
         
        //Act Statement(s)
        Type result = TypeUtils.getArrayComponentType(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getArrayComponentTypeWhenTypeInstanceOfGenericArrayType}, hash: 1350FB8490408D448949E5F318C1CCBF
    @Test()
    void getArrayComponentTypeWhenTypeInstanceOfGenericArrayType() {
        /* Branches:
         * (type instanceof Class<?>) : false
         * (type instanceof GenericArrayType) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Type result = TypeUtils.getArrayComponentType(genericArrayTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getArrayComponentTypeWhenTypeNotInstanceOfGenericArrayType}, hash: 16FE9FF11F7C0821E8F123EE51B1856D
    @Test()
    void getArrayComponentTypeWhenTypeNotInstanceOfGenericArrayType() {
        /* Branches:
         * (type instanceof Class<?>) : false
         * (type instanceof GenericArrayType) : false
         */
         
        //Act Statement(s)
        Type result = TypeUtils.getArrayComponentType(typeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getImplicitBoundsWhenBoundsLengthEquals0}, hash: 6E95BBD54CA9D53A76DED833862DDB2C
    @Test()
    void getImplicitBoundsWhenBoundsLengthEquals0() {
        /* Branches:
         * (bounds.length == 0) : true
         */
         //Arrange Statement(s)
        Type[] typeArray = new Type[] {};
        doReturn(typeArray).when(typeVariableMock).getBounds();
        
        //Act Statement(s)
        Type[] result = TypeUtils.getImplicitBounds(typeVariableMock);
        Type[] typeResultArray = new Type[] { Object.class };
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(typeResultArray));
            verify(typeVariableMock).getBounds();
        });
    }

    //BaseRock generated method id: ${getImplicitBoundsWhenBoundsLengthNotEquals0}, hash: FE49F4CFAB607D611E5B4A994399176A
    @Test()
    void getImplicitBoundsWhenBoundsLengthNotEquals0() {
        /* Branches:
         * (bounds.length == 0) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] { typeMock };
            doReturn(typeArray).when(typeVariableMock).getBounds();
            Type[] typeArray2 = new Type[] {};
            typeUtils.when(() -> TypeUtils.normalizeUpperBounds(typeArray)).thenReturn(typeArray2);
            //Act Statement(s)
            Type[] result = TypeUtils.getImplicitBounds(typeVariableMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(typeArray2));
                verify(typeVariableMock, atLeast(1)).getBounds();
                typeUtils.verify(() -> TypeUtils.normalizeUpperBounds(typeArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getImplicitLowerBoundsWhenBoundsLengthEquals0}, hash: F60FF09848B115FCDF5EA6ABD0588A44
    @Test()
    void getImplicitLowerBoundsWhenBoundsLengthEquals0() {
        /* Branches:
         * (bounds.length == 0) : true
         */
         //Arrange Statement(s)
        Type[] typeArray = new Type[] {};
        doReturn(typeArray).when(wildcardTypeMock).getLowerBounds();
        
        //Act Statement(s)
        Type[] result = TypeUtils.getImplicitLowerBounds(wildcardTypeMock);
        Type[] typeResultArray = new Type[] { (Type) null };
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(typeResultArray));
            verify(wildcardTypeMock).getLowerBounds();
        });
    }

    //BaseRock generated method id: ${getImplicitLowerBoundsWhenBoundsLengthNotEquals0}, hash: 69B63D244624E25889B804FA5E6BDD4F
    @Test()
    void getImplicitLowerBoundsWhenBoundsLengthNotEquals0() {
        /* Branches:
         * (bounds.length == 0) : false
         */
         //Arrange Statement(s)
        Type[] typeArray = new Type[] { typeMock };
        doReturn(typeArray).when(wildcardTypeMock).getLowerBounds();
        
        //Act Statement(s)
        Type[] result = TypeUtils.getImplicitLowerBounds(wildcardTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(typeArray));
            verify(wildcardTypeMock).getLowerBounds();
        });
    }

    //BaseRock generated method id: ${getImplicitUpperBoundsWhenBoundsLengthEquals0}, hash: 2E9C921AB380C656397D38BAEDDC15AB
    @Test()
    void getImplicitUpperBoundsWhenBoundsLengthEquals0() {
        /* Branches:
         * (bounds.length == 0) : true
         */
         //Arrange Statement(s)
        Type[] typeArray = new Type[] {};
        doReturn(typeArray).when(wildcardTypeMock).getUpperBounds();
        
        //Act Statement(s)
        Type[] result = TypeUtils.getImplicitUpperBounds(wildcardTypeMock);
        Type[] typeResultArray = new Type[] { Object.class };
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(typeResultArray));
            verify(wildcardTypeMock).getUpperBounds();
        });
    }

    //BaseRock generated method id: ${getImplicitUpperBoundsWhenBoundsLengthNotEquals0}, hash: 577C8751830F53BFC290704841752535
    @Test()
    void getImplicitUpperBoundsWhenBoundsLengthNotEquals0() {
        /* Branches:
         * (bounds.length == 0) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] { typeMock };
            doReturn(typeArray).when(wildcardTypeMock).getUpperBounds();
            Type[] typeArray2 = new Type[] {};
            typeUtils.when(() -> TypeUtils.normalizeUpperBounds(typeArray)).thenReturn(typeArray2);
            //Act Statement(s)
            Type[] result = TypeUtils.getImplicitUpperBounds(wildcardTypeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(typeArray2));
                verify(wildcardTypeMock, atLeast(1)).getUpperBounds();
                typeUtils.verify(() -> TypeUtils.normalizeUpperBounds(typeArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getRawType1WhenTypeInstanceOfClass_}, hash: 276AE75BACB3B388D141DFA02FA09EEE
    @Test()
    void getRawType1WhenTypeInstanceOfClass_() {
        /* Branches:
         * (type instanceof Class<?>) : true
         */
         
        //Act Statement(s)
        Class<?> result = TypeUtils.getRawType(Object.class, typeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Object.class)));
    }

    //BaseRock generated method id: ${getRawType1WhenRawTypeNotInstanceOfClass_ThrowsIllegalStateException}, hash: 08BDF8DACEF322F73E13693534408A73
    @Test()
    void getRawType1WhenRawTypeNotInstanceOfClass_ThrowsIllegalStateException() {
        /* Branches:
         * (type instanceof Class<?>) : false
         * (type instanceof ParameterizedType) : true
         * (!(rawType instanceof Class<?>)) : true  #  inside getRawType method
         */
         //Arrange Statement(s)
        Type typeMock = mock(Type.class, "type9");
        doReturn(typeMock).when(parameterizedTypeMock).getRawType();
        IllegalStateException illegalStateException = new IllegalStateException("Wait... What!? Type of rawType: type9");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.getRawType(parameterizedTypeMock, typeMock2);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
            verify(parameterizedTypeMock).getRawType();
        });
    }

    //BaseRock generated method id: ${getRawType1WhenRawTypeInstanceOfClass_}, hash: 86019D5130E4E4F5ECC6D07D15F7C017
    @Test()
    void getRawType1WhenRawTypeInstanceOfClass_() {
        /* Branches:
         * (type instanceof Class<?>) : false
         * (type instanceof ParameterizedType) : true
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         */
         //Arrange Statement(s)
        doReturn(Object.class).when(parameterizedTypeMock).getRawType();
        
        //Act Statement(s)
        Class<?> result = TypeUtils.getRawType(parameterizedTypeMock, typeMock);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Object.class));
            verify(parameterizedTypeMock).getRawType();
        });
    }

    //BaseRock generated method id: ${getRawType1WhenAssigningTypeIsNull}, hash: C6D14CBFFA0CA5800FCB2D1B3E5250CD
    @Test()
    void getRawType1WhenAssigningTypeIsNull() {
        /* Branches:
         * (type instanceof Class<?>) : false
         * (type instanceof ParameterizedType) : false
         * (type instanceof TypeVariable<?>) : true
         * (assigningType == null) : true
         */
         //Arrange Statement(s)
        TypeVariable typeVariableMock = mock(TypeVariable.class);
        Type type = null;
        
        //Act Statement(s)
        Class<?> result = TypeUtils.getRawType(typeVariableMock, type);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getRawType1WhenGenericDeclarationNotInstanceOfClass_}, hash: 8BFB1552F755349D95C69C8BB2FAE27D
    @Test()
    void getRawType1WhenGenericDeclarationNotInstanceOfClass_() {
        /* Branches:
         * (type instanceof Class<?>) : false
         * (type instanceof ParameterizedType) : false
         * (type instanceof TypeVariable<?>) : true
         * (assigningType == null) : false
         * (!(genericDeclaration instanceof Class<?>)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TypeVariable typeVariableMock = mock(TypeVariable.class);
        
        //Act Statement(s)
        Class<?> result = TypeUtils.getRawType(typeVariableMock, typeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getRawType1WhenRawComponentTypeIsNotNull}, hash: FA44D5E88724486CEE78E1848E2B6A17
    @Test()
    void getRawType1WhenRawComponentTypeIsNotNull() {
        /* Branches:
         * (type instanceof Class<?>) : false
         * (type instanceof ParameterizedType) : false
         * (type instanceof TypeVariable<?>) : false
         * (type instanceof GenericArrayType) : true
         * (rawComponentType != null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Class<?> result = TypeUtils.getRawType(genericArrayTypeMock, typeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Object.class)));
    }

    //BaseRock generated method id: ${getRawType1WhenRawComponentTypeIsNull}, hash: 558DD293E3C59944BBEC1FEBA3F6A11E
    @Test()
    void getRawType1WhenRawComponentTypeIsNull() {
        /* Branches:
         * (type instanceof Class<?>) : false
         * (type instanceof ParameterizedType) : false
         * (type instanceof TypeVariable<?>) : false
         * (type instanceof GenericArrayType) : true
         * (rawComponentType != null) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Class<?> result = TypeUtils.getRawType(genericArrayTypeMock, typeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getRawType1WhenTypeInstanceOfWildcardType}, hash: 2E55918EAC60D30EDF815CC096C2401F
    @Test()
    void getRawType1WhenTypeInstanceOfWildcardType() {
        /* Branches:
         * (type instanceof Class<?>) : false
         * (type instanceof ParameterizedType) : false
         * (type instanceof TypeVariable<?>) : false
         * (type instanceof GenericArrayType) : false
         * (type instanceof WildcardType) : true
         */
         
        //Act Statement(s)
        Class<?> result = TypeUtils.getRawType(wildcardTypeMock, typeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getRawType1WhenTypeNotInstanceOfWildcardTypeThrowsIllegalArgumentException}, hash: 8970613F7EDE99CA7A2093AC89F87EFA
    @Test()
    void getRawType1WhenTypeNotInstanceOfWildcardTypeThrowsIllegalArgumentException() {
        /* Branches:
         * (type instanceof Class<?>) : false
         * (type instanceof ParameterizedType) : false
         * (type instanceof TypeVariable<?>) : false
         * (type instanceof GenericArrayType) : false
         * (type instanceof WildcardType) : false
         */
         //Arrange Statement(s)
        Type typeMock = mock(Type.class, "type");
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("unknown type: type");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            TypeUtils.getRawType(typeMock, typeMock2);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getRawType1WhenTypeVarAssignsIsNull}, hash: A1E9D7A0D54E8EB1C11620B0C1900E16
    @Test()
    void getRawType1WhenTypeVarAssignsIsNull() {
        /* Branches:
         * (type instanceof Class<?>) : false
         * (type instanceof ParameterizedType) : false
         * (type instanceof TypeVariable<?>) : true
         * (assigningType == null) : false
         * (!(genericDeclaration instanceof Class<?>)) : false
         * (typeVarAssigns == null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TypeVariable typeVariableMock = mock(TypeVariable.class);
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            typeUtils.when(() -> TypeUtils.getTypeArguments(typeMock, Object.class)).thenReturn(null);
            //Act Statement(s)
            Class<?> result = TypeUtils.getRawType(typeVariableMock, typeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(nullValue()));
                typeUtils.verify(() -> TypeUtils.getTypeArguments(typeMock, Object.class), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getRawType1WhenTypeArgumentIsNull}, hash: 1DA8C48793B18E61ABC501B877C071FA
    @Test()
    void getRawType1WhenTypeArgumentIsNull() {
        /* Branches:
         * (type instanceof Class<?>) : false
         * (type instanceof ParameterizedType) : false
         * (type instanceof TypeVariable<?>) : true
         * (assigningType == null) : false
         * (!(genericDeclaration instanceof Class<?>)) : false
         * (typeVarAssigns == null) : false
         * (typeArgument == null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TypeVariable typeVariableMock = mock(TypeVariable.class);
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            typeUtils.when(() -> TypeUtils.getTypeArguments(typeMock, Object.class)).thenReturn(anyMap);
            //Act Statement(s)
            Class<?> result = TypeUtils.getRawType(typeVariableMock, typeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(nullValue()));
                typeUtils.verify(() -> TypeUtils.getTypeArguments(typeMock, Object.class), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getRawType1WhenTypeArgumentIsNotNull}, hash: A6E7B69C12708ACD6C9ABE769DAB9EC0
    @Test()
    void getRawType1WhenTypeArgumentIsNotNull() {
        /* Branches:
         * (type instanceof Class<?>) : false
         * (type instanceof ParameterizedType) : false
         * (type instanceof TypeVariable<?>) : true
         * (assigningType == null) : false
         * (!(genericDeclaration instanceof Class<?>)) : false
         * (typeVarAssigns == null) : false
         * (typeArgument == null) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TypeVariable typeVariableMock = mock(TypeVariable.class);
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            typeUtils.when(() -> TypeUtils.getTypeArguments(typeMock, Object.class)).thenReturn(anyMap);
            //Act Statement(s)
            Class<?> result = TypeUtils.getRawType(typeVariableMock, typeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Object.class));
                typeUtils.verify(() -> TypeUtils.getTypeArguments(typeMock, Object.class), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getTypeArguments1WhenRawTypeNotInstanceOfClass_ThrowsIllegalStateException}, hash: D41BBB162A675E82015B8072AA712565
    @Test()
    void getTypeArguments1WhenRawTypeNotInstanceOfClass_ThrowsIllegalStateException() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : true  #  inside getRawType method
         */
         //Arrange Statement(s)
        Type typeMock = mock(Type.class, "type9");
        doReturn(typeMock).when(parameterizedTypeMock).getRawType();
        IllegalStateException illegalStateException = new IllegalStateException("Wait... What!? Type of rawType: type9");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.getTypeArguments(parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
            verify(parameterizedTypeMock).getRawType();
        });
    }

    //BaseRock generated method id: ${getTypeArguments1WhenRawTypeInstanceOfClass_ThrowsIllegalStateException}, hash: 2CA5B4C85C33CFE35C73FFAACDDFAFA3
    @Test()
    void getTypeArguments1WhenRawTypeInstanceOfClass_ThrowsIllegalStateException() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         */
        Type typeMock = mock(Type.class, "type10");
        doReturn(Object.class, typeMock).when(parameterizedTypeMock).getRawType();
        IllegalStateException illegalStateException = new IllegalStateException("Wait... What!? Type of rawType: type10");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.getTypeArguments(parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
            verify(parameterizedTypeMock, times(2)).getRawType();
        });
    }

    //BaseRock generated method id: ${getTypeArguments1WhenIsAssignableNotClsToClass}, hash: 2D05EA2156D1FC63417DE0B16284F4C0
    @Test()
    void getTypeArguments1WhenIsAssignableNotClsToClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : true  #  inside getTypeArguments method
         */
        doReturn(null, Object.class).when(parameterizedTypeMock).getRawType();
        
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(nullValue()));
            verify(parameterizedTypeMock, times(2)).getRawType();
        });
    }

    //BaseRock generated method id: ${getTypeArguments1WhenToClassIsPrimitiveAndIsAssignableNotClsToClass}, hash: C50EE4E6BC37D5FBC24D8481CDE157CC
    @Test()
    void getTypeArguments1WhenToClassIsPrimitiveAndIsAssignableNotClsToClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : true  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getTypeArguments1WhenOwnerTypeInstanceOfParameterizedTypeThrowsIllegalStateException}, hash: 3376BB30A930AA50ADCCA84B05A86158
    @Test()
    void getTypeArguments1WhenOwnerTypeInstanceOfParameterizedTypeThrowsIllegalStateException() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : true  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        IllegalStateException illegalStateException = new IllegalStateException("s1");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.getTypeArguments(parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getTypeArguments1WhenOwnerTypeInstanceOfParameterizedTypeThrowsNullPointerException}, hash: 36B815D850E08C330E11254343619A6A
    @Test()
    void getTypeArguments1WhenOwnerTypeInstanceOfParameterizedTypeThrowsNullPointerException() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : true  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            TypeUtils.getTypeArguments(parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getTypeArguments1WhenToClassEqualsCls}, hash: 20432772593EF849674C5E9005AB2691
    @Test()
    void getTypeArguments1WhenToClassEqualsCls() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock);
        TypeVariable typeVariableMock = mock(TypeVariable.class);
        Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
        anyMap.put(typeVariableMock, typeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(anyMap)));
    }

    //BaseRock generated method id: ${getTypeArguments1WhenMidTypeInstanceOfParameterizedTypeThrowsIllegalStateException}, hash: CF05EA41DB8507D68FF2ECDFD241842C
    @Test()
    void getTypeArguments1WhenMidTypeInstanceOfParameterizedTypeThrowsIllegalStateException() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        IllegalStateException illegalStateException = new IllegalStateException("s1");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.getTypeArguments(parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getTypeArguments1WhenTypeInstanceOfClass_AndIsAssignableNotClsToClass}, hash: C30664BF8285A2D05B5CEFF9178F7C19
    @Test()
    void getTypeArguments1WhenTypeInstanceOfClass_AndIsAssignableNotClsToClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (!isAssignable(cls, toClass)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getTypeArguments1WhenGenericInterfaceIsNullAndTypeInstanceOfClass_AndIsAssignableNotClsToClass}, hash: DC8E3A4948B8B8C0F02A7ACBA4C32EE2
    @Test()
    void getTypeArguments1WhenGenericInterfaceIsNullAndTypeInstanceOfClass_AndIsAssignableNotClsToClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (!isAssignable(cls, toClass)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getTypeArguments1WhenIsAssignableClsToClassThrowsNullPointerException}, hash: CC0688819A21FCEA922C8C5BDC979741
    @Test()
    void getTypeArguments1WhenIsAssignableClsToClassThrowsNullPointerException() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            TypeUtils.getTypeArguments(parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getTypeArguments1WhenTypeNotInstanceOfClass_AndTypeInstanceOfParameterizedType}, hash: 3747E4B3505026DFECD62408349B8F0E
    @Test()
    void getTypeArguments1WhenTypeNotInstanceOfClass_AndTypeInstanceOfParameterizedType() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getTypeArguments1WhenToClassIsPrimitive}, hash: 23C8EE751C72D4FAE18D80099B387623
    @Test()
    void getTypeArguments1WhenToClassIsPrimitive() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getTypeArguments1WhenToClassIsArray}, hash: 6FEE33AD511E7B60962F6A6F91D1493A
    @Test()
    void getTypeArguments1WhenToClassIsArray() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : true  #  inside getTypeArguments method
         * (toClass.isArray()) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getTypeArguments1WhenSubtypeVarAssignsIsNotNullAndToClassNotEqualsCls}, hash: 404E476F21EC00968C51FCCA4FFCE458
    @Test()
    void getTypeArguments1WhenSubtypeVarAssignsIsNotNullAndToClassNotEqualsCls() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getTypeArguments1WhenToClassNotIsPrimitiveAndSubtypeVarAssignsIsNotNullAndToClassEqualsCls}, hash: 53C2DF6BDD3CA23AE566AF5C2DBA1C29
    @Test()
    void getTypeArguments1WhenToClassNotIsPrimitiveAndSubtypeVarAssignsIsNotNullAndToClassEqualsCls() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getTypeArguments1WhenToClassNotEqualsClsThrowsIllegalStateException}, hash: DADEEA98601DAC279705950478F285C9
    @Test()
    void getTypeArguments1WhenToClassNotEqualsClsThrowsIllegalStateException() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        IllegalStateException illegalStateException = new IllegalStateException("s1");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.getTypeArguments(parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getTypeArguments1WhenIsAssignableClsToClassAndClsIsPrimitiveAndToClassNotIsPrimitiveAndSubtypeVarAssignsIsNotNullAndToC}, hash: C73EA70585DCDA00E6CA262DAF82A715
    @Test()
    void getTypeArguments1WhenIsAssignableClsToClassAndClsIsPrimitiveAndToClassNotIsPrimitiveAndSubtypeVarAssignsIsNotNullAndToC() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getTypeArguments1WhenIsAssignableBoundToClass}, hash: DDB46054D8ADA02DDB97098EA3CD1D03
    @Test()
    void getTypeArguments1WhenIsAssignableBoundToClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : false  #  inside getTypeArguments method
         * (type instanceof WildcardType) : true  #  inside getTypeArguments method
         * (for-each(getImplicitUpperBounds((WildcardType) type))) : true  #  inside getTypeArguments method
         * (isAssignable(bound, toClass)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray);
            //Act Statement(s)
            Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.size(), equalTo(0));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getTypeArguments1WhenGetImplicitUpperBoundsTypeIsNotEmptyAndIsAssignableBoundToClass}, hash: 78DE2CEDE991D6EB7059501A5D50DCA3
    @Test()
    void getTypeArguments1WhenGetImplicitUpperBoundsTypeIsNotEmptyAndIsAssignableBoundToClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : false  #  inside getTypeArguments method
         * (type instanceof WildcardType) : true  #  inside getTypeArguments method
         * (for-each(getImplicitUpperBounds((WildcardType) type))) : true  #  inside getTypeArguments method
         * (isAssignable(bound, toClass)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray);
            //Act Statement(s)
            Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.size(), equalTo(0));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getTypeArguments1WhenTypeInstanceOfWildcardTypeAndGetImplicitUpperBoundsTypeIsNotEmptyAndIsAssignableBoundToClass}, hash: 5C5CD086654170C1D916418CC7B94CDC
    @Test()
    void getTypeArguments1WhenTypeInstanceOfWildcardTypeAndGetImplicitUpperBoundsTypeIsNotEmptyAndIsAssignableBoundToClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : false  #  inside getTypeArguments method
         * (type instanceof WildcardType) : true  #  inside getTypeArguments method
         * (for-each(getImplicitUpperBounds((WildcardType) type))) : true  #  inside getTypeArguments method
         * (isAssignable(bound, toClass)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray);
            //Act Statement(s)
            Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.size(), equalTo(0));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getTypeArguments1WhenIsAssignableNotBoundToClass}, hash: 643D06CAF7056218307471E8A19FA640
    @Test()
    void getTypeArguments1WhenIsAssignableNotBoundToClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : false  #  inside getTypeArguments method
         * (type instanceof WildcardType) : true  #  inside getTypeArguments method
         * (for-each(getImplicitUpperBounds((WildcardType) type))) : true  #  inside getTypeArguments method
         * (isAssignable(bound, toClass)) : false  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray);
            //Act Statement(s)
            Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(nullValue()));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getTypeArguments1WhenGetImplicitUpperBoundsTypeIsNotEmptyAndIsAssignableNotBoundToClass}, hash: F5975E2745E4AB98D9F75BC1EC568684
    @Test()
    void getTypeArguments1WhenGetImplicitUpperBoundsTypeIsNotEmptyAndIsAssignableNotBoundToClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : false  #  inside getTypeArguments method
         * (type instanceof WildcardType) : true  #  inside getTypeArguments method
         * (for-each(getImplicitUpperBounds((WildcardType) type))) : true  #  inside getTypeArguments method
         * (isAssignable(bound, toClass)) : false  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray);
            //Act Statement(s)
            Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(nullValue()));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getTypeArguments1WhenTypeInstanceOfParameterizedTypeThrowsIllegalStateException}, hash: E08B64021973AF247ADC08C3D4CA2420
    @Test()
    void getTypeArguments1WhenTypeInstanceOfParameterizedTypeThrowsIllegalStateException() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : false  #  inside getTypeArguments method
         * (type instanceof WildcardType) : true  #  inside getTypeArguments method
         * (for-each(getImplicitUpperBounds((WildcardType) type))) : true  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : true  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray);
            //Act Statement(s)
            final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
                TypeUtils.getTypeArguments(parameterizedTypeMock);
            });
            IllegalStateException illegalStateException = new IllegalStateException("s1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getTypeArguments1WhenMidTypeInstanceOfClass_}, hash: 86FDF289A2EE24BAA3702563647C61AE
    @Test()
    void getTypeArguments1WhenMidTypeInstanceOfClass_() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (midType instanceof Class<?>) : true  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getTypeArguments1WhenMidTypeNotInstanceOfClass_ThrowsIllegalStateException}, hash: 4A1AD27C1F58EAD3AEC82F3886691820
    @Test()
    void getTypeArguments1WhenMidTypeNotInstanceOfClass_ThrowsIllegalStateException() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (midType instanceof Class<?>) : false  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        IllegalStateException illegalStateException = new IllegalStateException("s1");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.getTypeArguments(parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getTypeArguments1WhenIsAssignableGenericInterfaceMidClass}, hash: 5D807D1AED2DF0261E45998FD5B93203
    @Test()
    void getTypeArguments1WhenIsAssignableGenericInterfaceMidClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getTypeArguments1WhenToClassNotEqualsClsAndIsAssignableGenericInterfaceMidClass}, hash: 2884158815406BAA4873FF9A7E0109BE
    @Test()
    void getTypeArguments1WhenToClassNotEqualsClsAndIsAssignableGenericInterfaceMidClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getTypeArguments1WhenIsAssignableNotGenericInterfaceMidClass}, hash: 10BCB86BD5F33167D49C06D5D0F82E28
    @Test()
    void getTypeArguments1WhenIsAssignableNotGenericInterfaceMidClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (isAssignable(genericInterface, (Type) midClass)) : false  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getTypeArguments1WhenTypeInstanceOfParameterizedTypeAndIsAssignableBoundToClass}, hash: 4BBD7F79E5AB7FAE3E183D7B65B3A0A7
    @Test()
    void getTypeArguments1WhenTypeInstanceOfParameterizedTypeAndIsAssignableBoundToClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : false  #  inside getTypeArguments method
         * (type instanceof WildcardType) : true  #  inside getTypeArguments method
         * (for-each(getImplicitUpperBounds((WildcardType) type))) : true  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : true  #  inside isAssignable method
         * (isAssignable(bound, toClass)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray);
            //Act Statement(s)
            Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.size(), equalTo(0));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getTypeArguments1WhenIsAssignableBoundToClassAndIsAssignableBoundToClass}, hash: 86498AD1D956AC633D1A6E15B8EE0929
    @Test()
    void getTypeArguments1WhenIsAssignableBoundToClassAndIsAssignableBoundToClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : false  #  inside getTypeArguments method
         * (type instanceof WildcardType) : true  #  inside getTypeArguments method
         * (for-each(getImplicitUpperBounds((WildcardType) type))) : true  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside isAssignable method
         * (type instanceof TypeVariable<?>) : true  #  inside isAssignable method
         * (for-each(((TypeVariable<?>) type).getBounds())) : true  #  inside isAssignable method
         * (isAssignable(bound, toClass)) : true  #  inside isAssignable method
         * (isAssignable(bound, toClass)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray);
            //Act Statement(s)
            Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.size(), equalTo(0));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getTypeArguments1WhenIsAssignableNotBoundToClassAndIsAssignableNotBoundToClass}, hash: E5767864A976053FE0B1261B649DA93A
    @Test()
    void getTypeArguments1WhenIsAssignableNotBoundToClassAndIsAssignableNotBoundToClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : false  #  inside getTypeArguments method
         * (type instanceof WildcardType) : true  #  inside getTypeArguments method
         * (for-each(getImplicitUpperBounds((WildcardType) type))) : true  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside isAssignable method
         * (type instanceof TypeVariable<?>) : true  #  inside isAssignable method
         * (for-each(((TypeVariable<?>) type).getBounds())) : true  #  inside isAssignable method
         * (isAssignable(bound, toClass)) : false  #  inside isAssignable method
         * (isAssignable(bound, toClass)) : false  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray);
            //Act Statement(s)
            Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(nullValue()));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getTypeArguments1WhenIsAssignableTypeGetGenericComponentTypeToClassGetComponentTypeAndIsAssignableBoundToClass}, hash: ECF520C0E2B3333D29E415BCB223433A
    @Test()
    void getTypeArguments1WhenIsAssignableTypeGetGenericComponentTypeToClassGetComponentTypeAndIsAssignableBoundToClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : false  #  inside getTypeArguments method
         * (type instanceof WildcardType) : true  #  inside getTypeArguments method
         * (for-each(getImplicitUpperBounds((WildcardType) type))) : true  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside isAssignable method
         * (type instanceof TypeVariable<?>) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : true  #  inside isAssignable method
         * (toClass.equals(Object.class)) : false  #  inside isAssignable method
         * (toClass.isArray()) : true  #  inside isAssignable method
         * (isAssignable(((GenericArrayType) type).getGenericComponentType(), toClass.getComponentType())) : true  #  inside isAssignable method
         * (isAssignable(bound, toClass)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray);
            //Act Statement(s)
            Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.size(), equalTo(0));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getTypeArguments1WhenIsAssignableNotTypeGetGenericComponentTypeToClassGetComponentTypeAndIsAssignableNotBoundToClass}, hash: 18FACB091BAA01C620E827DBD2D6B9D5
    @Test()
    void getTypeArguments1WhenIsAssignableNotTypeGetGenericComponentTypeToClassGetComponentTypeAndIsAssignableNotBoundToClass() {
        /* Branches:
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : false  #  inside getTypeArguments method
         * (type instanceof WildcardType) : true  #  inside getTypeArguments method
         * (for-each(getImplicitUpperBounds((WildcardType) type))) : true  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside isAssignable method
         * (type instanceof TypeVariable<?>) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : true  #  inside isAssignable method
         * (toClass.equals(Object.class)) : false  #  inside isAssignable method
         * (toClass.isArray()) : true  #  inside isAssignable method
         * (isAssignable(((GenericArrayType) type).getGenericComponentType(), toClass.getComponentType())) : false  #  inside isAssignable method
         * (isAssignable(bound, toClass)) : false  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray);
            //Act Statement(s)
            Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(nullValue()));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getTypeArguments3WhenRawTypeNotInstanceOfClass_ThrowsIllegalStateException}, hash: B9343E52457809EE3552BCC35DB7C4E6
    @Test()
    void getTypeArguments3WhenRawTypeNotInstanceOfClass_ThrowsIllegalStateException() {
        /* Branches:
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : true  #  inside getTypeArguments method
         * (!(rawType instanceof Class<?>)) : true  #  inside getRawType method
         */
         //Arrange Statement(s)
        Type typeMock = mock(Type.class, "type9");
        doReturn(typeMock).when(parameterizedTypeMock).getRawType();
        IllegalStateException illegalStateException = new IllegalStateException("Wait... What!? Type of rawType: type9");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.getTypeArguments(parameterizedTypeMock, Object.class);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
            verify(parameterizedTypeMock).getRawType();
        });
    }

    //BaseRock generated method id: ${getTypeArguments3WhenIsAssignableNotClsToClass}, hash: 3F574D91C4C9CA0A56B5D06C35A9364F
    @Test()
    void getTypeArguments3WhenIsAssignableNotClsToClass() {
        /* Branches:
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : true  #  inside getTypeArguments method
         */
         //Arrange Statement(s)
        Class<?> _class = null;
        
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(Object.class, _class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getTypeArguments3WhenToClassIsArray}, hash: 6FE4DF455B6B7AAAEFFF4370604EE39F
    @Test()
    void getTypeArguments3WhenToClassIsArray() {
        /* Branches:
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : true  #  inside getTypeArguments method
         * (toClass.isArray()) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(genericArrayTypeMock, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getTypeArguments3WhenToClassNotIsPrimitiveAndIsAssignableClsToClassThrowsNullPointerException}, hash: 7BBAAB3ECCAA909A8C1D55C098A5F249
    @Test()
    void getTypeArguments3WhenToClassNotIsPrimitiveAndIsAssignableClsToClassThrowsNullPointerException() {
        /* Branches:
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : true  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         */
         //Arrange Statement(s)
        Class _class = null;
        //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            TypeUtils.getTypeArguments(_class, Object.class);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getTypeArguments3WhenToClassIsPrimitiveAndIsAssignableNotClsToClass}, hash: 86B6E4FC9A81886076BF4E9C6540A473
    @Test()
    void getTypeArguments3WhenToClassIsPrimitiveAndIsAssignableNotClsToClass() {
        /* Branches:
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : true  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Class _class = null;
        
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(_class, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getTypeArguments3WhenToClassIsNullAndIsAssignableNotClsToClass}, hash: 9D2F89D748061D56B225BDBC85AFBEE5
    @Test()
    void getTypeArguments3WhenToClassIsNullAndIsAssignableNotClsToClass() {
        /* Branches:
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : true  #  inside getTypeArguments method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : true  #  inside getTypeArguments method
         */
         //Arrange Statement(s)
        doReturn(Object.class).when(parameterizedTypeMock).getRawType();
        Class<?> _class = null;
        
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock, _class);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(nullValue()));
            verify(parameterizedTypeMock).getRawType();
        });
    }

    //BaseRock generated method id: ${getTypeArguments3WhenToClassIsPrimitive}, hash: 817E95CEDF9755A728456FA34541F57F
    @Test()
    void getTypeArguments3WhenToClassIsPrimitive() {
        /* Branches:
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(Object.class, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getTypeArguments3WhenClassUtilsNotIsAssignableTypeToClassAndIsAssignableNotClsToClass}, hash: 74DA7E50C0A163DEC1DB307A108D5C0E
    @Test()
    void getTypeArguments3WhenClassUtilsNotIsAssignableTypeToClassAndIsAssignableNotClsToClass() {
        /* Branches:
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(Object.class, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getTypeArguments3WhenToClassIsNotNullAndToClassIsPrimitiveAndIsAssignableNotClsToClass}, hash: D0D9F073E7CA35EE4BE5387C0B554DD3
    @Test()
    void getTypeArguments3WhenToClassIsNotNullAndToClassIsPrimitiveAndIsAssignableNotClsToClass() {
        /* Branches:
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : true  #  inside getTypeArguments method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : true  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getTypeArguments3WhenOwnerTypeInstanceOfParameterizedTypeThrowsIllegalStateException}, hash: 3C718073198AC1882816AFBA009F7192
    @Test()
    void getTypeArguments3WhenOwnerTypeInstanceOfParameterizedTypeThrowsIllegalStateException() {
        /* Branches:
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : true  #  inside getTypeArguments method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : true  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : true  #  inside getTypeArguments method
         */
         //Arrange Statement(s)
        doReturn(null).when(parameterizedTypeMock).getRawType();
        doReturn(parameterizedTypeMock2).when(parameterizedTypeMock).getOwnerType();
        Type typeMock = mock(Type.class, "type11");
        doReturn(typeMock).when(parameterizedTypeMock2).getRawType();
        IllegalStateException illegalStateException = new IllegalStateException("Wait... What!? Type of rawType: type11");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.getTypeArguments(parameterizedTypeMock, Object.class);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
            verify(parameterizedTypeMock).getRawType();
            verify(parameterizedTypeMock).getOwnerType();
            verify(parameterizedTypeMock2).getRawType();
        });
    }

    //BaseRock generated method id: ${getTypeArguments3WhenOwnerTypeInstanceOfParameterizedTypeThrowsNullPointerException}, hash: B404716C0160E39E1DEF9C7037071388
    @Test()
    void getTypeArguments3WhenOwnerTypeInstanceOfParameterizedTypeThrowsNullPointerException() {
        /* Branches:
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : true  #  inside getTypeArguments method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : true  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            TypeUtils.getTypeArguments(parameterizedTypeMock, Object.class);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getTypeArguments3WhenIsAssignableNotBoundToClass}, hash: FC2645DDD484194BD848FB61B5443749
    @Test()
    void getTypeArguments3WhenIsAssignableNotBoundToClass() {
        /* Branches:
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : false  #  inside getTypeArguments method
         * (type instanceof WildcardType) : true  #  inside getTypeArguments method
         * (for-each(getImplicitUpperBounds((WildcardType) type))) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : true  #  inside isAssignable method
         * (isAssignable(bound, toClass)) : false  #  inside getTypeArguments method
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] { typeMock };
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray);
            Class<?> _class = null;
            //Act Statement(s)
            Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(wildcardTypeMock, _class);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(nullValue()));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getTypeArguments3WhenSubtypeVarAssignsIsNullAndToClassEqualsCls}, hash: 6E5A39363524CACCF7BF0053BB27EDA0
    @Test()
    void getTypeArguments3WhenSubtypeVarAssignsIsNullAndToClassEqualsCls() {
        /* Branches:
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(Object.class, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getTypeArguments3WhenTypeInstanceOfClass_AndClassUtilsNotIsAssignableTypeToClassAndIsAssignableNotClsToClass}, hash: 1E08608548B64E1F6B43A32D0749B146
    @Test()
    void getTypeArguments3WhenTypeInstanceOfClass_AndClassUtilsNotIsAssignableTypeToClassAndIsAssignableNotClsToClass() {
        /* Branches:
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : true  #  inside getTypeArguments method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getTypeArguments3WhenToClassNotIsPrimitiveAndIsAssignableBoundToClass}, hash: F37943912BC0F5CABD12525D1BF25C54
    @Test()
    void getTypeArguments3WhenToClassNotIsPrimitiveAndIsAssignableBoundToClass() {
        /* Branches:
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : false  #  inside getTypeArguments method
         * (type instanceof WildcardType) : true  #  inside getTypeArguments method
         * (for-each(getImplicitUpperBounds((WildcardType) type))) : true  #  inside getTypeArguments method
         * (type == null) : true  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(bound, toClass)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray);
            //Act Statement(s)
            Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(wildcardTypeMock, Object.class);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.size(), equalTo(0));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getTypeArguments3WhenToClassIsPrimitiveAndIsAssignableNotBoundToClass}, hash: 66DF2E17A7ECBA21FAA38108F78C8158
    @Test()
    void getTypeArguments3WhenToClassIsPrimitiveAndIsAssignableNotBoundToClass() {
        /* Branches:
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (type instanceof GenericArrayType) : false  #  inside getTypeArguments method
         * (type instanceof WildcardType) : true  #  inside getTypeArguments method
         * (for-each(getImplicitUpperBounds((WildcardType) type))) : true  #  inside getTypeArguments method
         * (type == null) : true  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(bound, toClass)) : false  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock)).thenReturn(typeArray);
            //Act Statement(s)
            Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(wildcardTypeMock, Object.class);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(nullValue()));
                typeUtils.verify(() -> TypeUtils.getImplicitUpperBounds(wildcardTypeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getTypeArguments3WhenToClassNotEqualsClsAndSuperClassNotIsInterface}, hash: 1A7407D1E0910FA75A5953FE8F300C08
    @Test()
    void getTypeArguments3WhenToClassNotEqualsClsAndSuperClassNotIsInterface() {
        /* Branches:
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : false  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(Object.class, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getTypeArguments3WhenILessThanTypeParamsLengthAndToClassEqualsCls}, hash: 2CBBB1BA692539B2DFFF4A19F7406A0A
    @Test()
    void getTypeArguments3WhenILessThanTypeParamsLengthAndToClassEqualsCls() {
        /* Branches:
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : true  #  inside getTypeArguments method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock, Object.class);
        TypeVariable typeVariableMock = mock(TypeVariable.class);
        Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
        anyMap.put(typeVariableMock, typeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(anyMap)));
    }

    //BaseRock generated method id: ${getTypeArguments3WhenMidTypeInstanceOfParameterizedTypeAndRawTypeNotInstanceOfClass_ThrowsIllegalStateException}, hash: 688E278DB7E5454B9653F8DFBBCB2E8C
    @Test()
    void getTypeArguments3WhenMidTypeInstanceOfParameterizedTypeAndRawTypeNotInstanceOfClass_ThrowsIllegalStateException() {
        /* Branches:
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (!(rawType instanceof Class<?>)) : true  #  inside getRawType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        IllegalStateException illegalStateException = new IllegalStateException("s1");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.getTypeArguments(Object.class, Object.class);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getTypeArguments3WhenMidTypeNotInstanceOfClass_ThrowsIllegalStateException}, hash: CB96B67375E4A8CDF3D34C21F300DA13
    @Test()
    void getTypeArguments3WhenMidTypeNotInstanceOfClass_ThrowsIllegalStateException() {
        /* Branches:
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : false  #  inside getClosestParentType method
         * (midType instanceof Class<?>) : false  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        IllegalStateException illegalStateException = new IllegalStateException("s1");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.getTypeArguments(Object.class, Object.class);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getTypeArguments3WhenMidTypeInstanceOfParameterizedTypeThrowsIllegalStateException}, hash: 75B0A8488E73AF1226FC57ACB5D223DF
    @Test()
    void getTypeArguments3WhenMidTypeInstanceOfParameterizedTypeThrowsIllegalStateException() {
        /* Branches:
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : true  #  inside getTypeArguments method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        IllegalStateException illegalStateException = new IllegalStateException("s1");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.getTypeArguments(parameterizedTypeMock, Object.class);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getTypeArguments3WhenSubtypeVarAssignsIsNullAndINotLessThanTypeParamsLengthAndToClassNotEqualsClsAndSuperClassNotIsInte}, hash: 2EC7372C6EF3D45B4817AD14F9CEDA9A
    @Test()
    void getTypeArguments3WhenSubtypeVarAssignsIsNullAndINotLessThanTypeParamsLengthAndToClassNotEqualsClsAndSuperClassNotIsInte() {
        /* Branches:
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : true  #  inside getTypeArguments method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : false  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getTypeArguments3WhenMidTypeNotInstanceOfParameterizedTypeAndMidTypeNotInstanceOfClass_ThrowsIllegalStateException}, hash: 8536AB89FCC97C3ACC9096409E0CB845
    @Test()
    void getTypeArguments3WhenMidTypeNotInstanceOfParameterizedTypeAndMidTypeNotInstanceOfClass_ThrowsIllegalStateException() {
        /* Branches:
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : true  #  inside getTypeArguments method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : false  #  inside getClosestParentType method
         * (midType instanceof Class<?>) : false  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        IllegalStateException illegalStateException = new IllegalStateException("s1");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.getTypeArguments(parameterizedTypeMock, Object.class);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getTypeArguments3WhenIsAssignableNotMidClassSuperClassAndGenericInterfaceIsNull}, hash: 48831D4816D1655E89038400DD01CB1F
    @Test()
    void getTypeArguments3WhenIsAssignableNotMidClassSuperClassAndGenericInterfaceIsNull() {
        /* Branches:
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(Object.class, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getTypeArguments3WhenTypeInstanceOfClass_AndClassUtilsNotIsAssignableTypeToClassAndIsAssignableNotMidClassSuperClassAnd}, hash: 3945394FDF5883C74C408AFEDEB2A165
    @Test()
    void getTypeArguments3WhenTypeInstanceOfClass_AndClassUtilsNotIsAssignableTypeToClassAndIsAssignableNotMidClassSuperClassAnd() {
        /* Branches:
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : false  #  inside getClosestParentType method
         * (midType instanceof Class<?>) : true  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(Object.class, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getTypeArguments3WhenGenericInterfaceIsNotNull}, hash: D78DB72F144C9C821D3DA381A21DE852
    @Test()
    void getTypeArguments3WhenGenericInterfaceIsNotNull() {
        /* Branches:
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(Object.class, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getTypeArguments3WhenIsAssignableGenericInterfaceMidClassAndGenericInterfaceIsNotNull}, hash: 8FC1B8F8CBA1C75453D22004071FED44
    @Test()
    void getTypeArguments3WhenIsAssignableGenericInterfaceMidClassAndGenericInterfaceIsNotNull() {
        /* Branches:
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(Object.class, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getTypeArguments3WhenToClassIsPrimitiveAndIsAssignableNotMidClassSuperClassAndGenericInterfaceIsNull}, hash: 6D80D7FBE8BCC901697A1A8ECF7A4098
    @Test()
    void getTypeArguments3WhenToClassIsPrimitiveAndIsAssignableNotMidClassSuperClassAndGenericInterfaceIsNull() {
        /* Branches:
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : true  #  inside getTypeArguments method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getTypeArguments3WhenToClassNotIsPrimitiveAndIsAssignableGenericInterfaceMidClassAndGenericInterfaceIsNotNull}, hash: 008F673FFA838B419A83DE521448DA6C
    @Test()
    void getTypeArguments3WhenToClassNotIsPrimitiveAndIsAssignableGenericInterfaceMidClassAndGenericInterfaceIsNotNull() {
        /* Branches:
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : true  #  inside getTypeArguments method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getTypeArguments3WhenIsAssignableMidClassSuperClassAndIsAssignableGenericInterfaceMidClassAndGenericInterfaceIsNotNull}, hash: BBE98918DA16B7965F261EE583758A48
    @Test()
    void getTypeArguments3WhenIsAssignableMidClassSuperClassAndIsAssignableGenericInterfaceMidClassAndGenericInterfaceIsNotNull() {
        /* Branches:
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : true  #  inside getTypeArguments method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getTypeArguments3WhenClassUtilsNotIsAssignableTypeToClassAndIsAssignableNotMidClassSuperClassAndGenericInterfaceIsNull3}, hash: D03B9EB9BF815DD6224DD3B7FBD1D46F
    @Test()
    void getTypeArguments3WhenClassUtilsNotIsAssignableTypeToClassAndIsAssignableNotMidClassSuperClassAndGenericInterfaceIsNull3() {
        /* Branches:
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : true  #  inside getTypeArguments method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : false  #  inside getClosestParentType method
         * (midType instanceof Class<?>) : true  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : false  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getTypeArguments3WhenToClassIsNotNullAndToClassIsPrimitiveAndIsAssignableNotGenericInterfaceMidClassAndGenericInterface}, hash: 8A9D913EFF9B9F4047B63DA6ED1194D9
    @Test()
    void getTypeArguments3WhenToClassIsNotNullAndToClassIsPrimitiveAndIsAssignableNotGenericInterfaceMidClassAndGenericInterface() {
        /* Branches:
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(genericInterface, (Type) midClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(Object.class, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getTypeArguments3WhenToClassIsPrimitiveAndIsAssignableNotGenericInterfaceMidClassAndGenericInterfaceIsNull2}, hash: 7FABDB6D12B15446442F250716822CF0
    @Test()
    void getTypeArguments3WhenToClassIsPrimitiveAndIsAssignableNotGenericInterfaceMidClassAndGenericInterfaceIsNull2() {
        /* Branches:
         * (type instanceof Class<?>) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : true  #  inside getTypeArguments method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (isAssignable(genericInterface, (Type) midClass)) : false  #  inside getClosestParentType method
         * (genericInterface != null) : false  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Map<TypeVariable<?>, Type> result = TypeUtils.getTypeArguments(parameterizedTypeMock, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${isArrayTypeWhenTypeIsArray}, hash: E3FD36B8433CF34DDA402BB621D2ABE8
    @Test()
    void isArrayTypeWhenTypeIsArray() {
        /* Branches:
         * (type instanceof GenericArrayType) : false
         * (type instanceof Class<?>) : true
         * (((Class<?>) type).isArray()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.isArrayType(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isArrayTypeWhenTypeNotIsArray}, hash: 5ED9A682B004D0433E0FA5EE45919B93
    @Test()
    void isArrayTypeWhenTypeNotIsArray() {
        /* Branches:
         * (type instanceof GenericArrayType) : false
         * (type instanceof Class<?>) : true
         * (((Class<?>) type).isArray()) : false
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.isArrayType(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAssignable3WhenTypeIsNull}, hash: A777B66AF1076AE591831FFF264778AC
    @Test()
    void isAssignable3WhenTypeIsNull() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : true  #  inside isAssignable method
         */
         //Arrange Statement(s)
        Type type = null;
        
        //Act Statement(s)
        boolean result = TypeUtils.isAssignable(type, (Type) parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAssignable3WhenToClassNotIsPrimitive}, hash: BD22EBBBD9B76E6BA3C6366AE892D817
    @Test()
    void isAssignable3WhenToClassNotIsPrimitive() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : true  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         */
         //Arrange Statement(s)
        Type type = null;
        
        //Act Statement(s)
        boolean result = TypeUtils.isAssignable(type, (Type) Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAssignable3WhenToClassIsPrimitive}, hash: B7A0AF4EF1082239DF2E68500A213E56
    @Test()
    void isAssignable3WhenToClassIsPrimitive() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : true  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Type type = null;
        
        //Act Statement(s)
        boolean result = TypeUtils.isAssignable(type, (Type) Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAssignable3WhenToClassEqualsType}, hash: E459FE9F2297573CAB95BDFD08018214
    @Test()
    void isAssignable3WhenToClassEqualsType() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.isAssignable((Type) Object.class, (Type) Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAssignable3WhenTypeInstanceOfGenericArrayType}, hash: 9C0792EDDE79E690CAC44177DD53CBC9
    @Test()
    void isAssignable3WhenTypeInstanceOfGenericArrayType() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : true  #  inside isAssignable method
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.isAssignable((Type) genericArrayTypeMock, (Type) parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAssignable3WhenClassUtilsIsAssignableTypeToClass}, hash: 45991174698BF8072EAD35F16D10BE66
    @Test()
    void isAssignable3WhenClassUtilsIsAssignableTypeToClass() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.isAssignable((Type) Object.class, (Type) Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAssignable3WhenClassUtilsNotIsAssignableTypeToClass}, hash: B37A07804F6B8915408A3207EC867636
    @Test()
    void isAssignable3WhenClassUtilsNotIsAssignableTypeToClass() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : false  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.isAssignable((Type) Object.class, (Type) Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAssignable3WhenToParameterizedTypeEqualsType}, hash: 57DB096C9F662A7A7609D05385C6D7A6
    @Test()
    void isAssignable3WhenToParameterizedTypeEqualsType() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : true  #  inside isAssignable method
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.isAssignable((Type) parameterizedTypeMock, (Type) parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAssignable3WhenRawTypeNotInstanceOfClass_ThrowsIllegalStateException}, hash: 2F62D362AD67C341930838D496958609
    @Test()
    void isAssignable3WhenRawTypeNotInstanceOfClass_ThrowsIllegalStateException() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : false  #  inside isAssignable method
         * (type instanceof ParameterizedType) : true  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : true  #  inside getRawType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        IllegalStateException illegalStateException = new IllegalStateException("s1");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.isAssignable((Type) parameterizedTypeMock, (Type) Object.class);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${isAssignable3WhenToParameterizedTypeNotEqualsTypeAndRawTypeNotInstanceOfClass_ThrowsIllegalStateException}, hash: C51C72E0623835F093D7767FC360ABE6
    @Test()
    void isAssignable3WhenToParameterizedTypeNotEqualsTypeAndRawTypeNotInstanceOfClass_ThrowsIllegalStateException() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : true  #  inside getRawType method
         */
         //Arrange Statement(s)
        Type typeMock = mock(Type.class, "type9");
        doReturn(typeMock).when(parameterizedTypeMock).getRawType();
        IllegalStateException illegalStateException = new IllegalStateException("Wait... What!? Type of rawType: type9");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.isAssignable(typeMock2, (Type) parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
            verify(parameterizedTypeMock).getRawType();
        });
    }

    //BaseRock generated method id: ${isAssignable3WhenIsAssignableTypeToType}, hash: E71906C75AA966E27B4E86EE60A6C378
    @Test()
    void isAssignable3WhenIsAssignableTypeToType() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : false  #  inside isAssignable method
         * (type instanceof ParameterizedType) : true  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (isAssignable(type, (Class<?>) toType)) : true  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.isAssignable((Type) parameterizedTypeMock, (Type) Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAssignable3WhenIsAssignableBoundToClass}, hash: AEC92CF7B84267CCC72B8A21BE268B24
    @Test()
    void isAssignable3WhenIsAssignableBoundToClass() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : false  #  inside isAssignable method
         * (type instanceof ParameterizedType) : false  #  inside isAssignable method
         * (type instanceof TypeVariable<?>) : true  #  inside isAssignable method
         * (for-each(((TypeVariable<?>) type).getBounds())) : true  #  inside isAssignable method
         * (isAssignable(bound, toClass)) : true  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TypeVariable typeVariableMock = mock(TypeVariable.class);
        
        //Act Statement(s)
        boolean result = TypeUtils.isAssignable((Type) typeVariableMock, (Type) Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAssignable3WhenIsAssignableNotBoundToClass}, hash: 398FD81AA4A639253C880CBCA380DAD8
    @Test()
    void isAssignable3WhenIsAssignableNotBoundToClass() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : false  #  inside isAssignable method
         * (type instanceof ParameterizedType) : false  #  inside isAssignable method
         * (type instanceof TypeVariable<?>) : true  #  inside isAssignable method
         * (for-each(((TypeVariable<?>) type).getBounds())) : true  #  inside isAssignable method
         * (isAssignable(bound, toClass)) : false  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TypeVariable typeVariableMock = mock(TypeVariable.class);
        
        //Act Statement(s)
        boolean result = TypeUtils.isAssignable((Type) typeVariableMock, (Type) Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAssignable3WhenTypeInstanceOfWildcardType}, hash: F59A53C9B8F458719B9B89E30FC9F9CE
    @Test()
    void isAssignable3WhenTypeInstanceOfWildcardType() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : false  #  inside isAssignable method
         * (type instanceof ParameterizedType) : false  #  inside isAssignable method
         * (type instanceof TypeVariable<?>) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (type instanceof WildcardType) : true  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.isAssignable((Type) wildcardTypeMock, (Type) Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAssignable3WhenTypeNotInstanceOfWildcardTypeThrowsIllegalStateException}, hash: 0450A6778EC843FA359E0F93F35934C3
    @Test()
    void isAssignable3WhenTypeNotInstanceOfWildcardTypeThrowsIllegalStateException() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : false  #  inside isAssignable method
         * (type instanceof ParameterizedType) : false  #  inside isAssignable method
         * (type instanceof TypeVariable<?>) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (type instanceof WildcardType) : false  #  inside isAssignable method
         */
         //Arrange Statement(s)
        Type typeMock = mock(Type.class, "type");
        IllegalStateException illegalStateException = new IllegalStateException("found an unhandled type: type");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.isAssignable(typeMock, (Type) Object.class);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${isAssignable3WhenIsAssignableTypeGetGenericComponentTypeToClassGetComponentType}, hash: DBC50B04DB72D3B5852A53530B2072E7
    @Test()
    void isAssignable3WhenIsAssignableTypeGetGenericComponentTypeToClassGetComponentType() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : false  #  inside isAssignable method
         * (type instanceof ParameterizedType) : false  #  inside isAssignable method
         * (type instanceof TypeVariable<?>) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : true  #  inside isAssignable method
         * (toClass.equals(Object.class)) : false  #  inside isAssignable method
         * (toClass.isArray()) : true  #  inside isAssignable method
         * (isAssignable(((GenericArrayType) type).getGenericComponentType(), toClass.getComponentType())) : true  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.isAssignable((Type) genericArrayTypeMock, (Type) Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAssignable3WhenIsAssignableNotTypeGetGenericComponentTypeToClassGetComponentType}, hash: 871C2E615EB9A9F0C2F04730BF9A20DE
    @Test()
    void isAssignable3WhenIsAssignableNotTypeGetGenericComponentTypeToClassGetComponentType() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : false  #  inside isAssignable method
         * (type instanceof ParameterizedType) : false  #  inside isAssignable method
         * (type instanceof TypeVariable<?>) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : true  #  inside isAssignable method
         * (toClass.equals(Object.class)) : false  #  inside isAssignable method
         * (toClass.isArray()) : true  #  inside isAssignable method
         * (isAssignable(((GenericArrayType) type).getGenericComponentType(), toClass.getComponentType())) : false  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.isAssignable((Type) genericArrayTypeMock, (Type) Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAssignable3WhenFromTypeVarAssignsIsNull}, hash: 98206373AA3B2C29E8464D0DB74D5702
    @Test()
    void isAssignable3WhenFromTypeVarAssignsIsNull() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : true  #  inside getTypeArguments method
         * (fromTypeVarAssigns == null) : true  #  inside isAssignable method
         */
         //Arrange Statement(s)
        doReturn(null).when(parameterizedTypeMock).getRawType();
        
        //Act Statement(s)
        boolean result = TypeUtils.isAssignable((Type) Object.class, (Type) parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.FALSE));
            verify(parameterizedTypeMock).getRawType();
        });
    }

    //BaseRock generated method id: ${isAssignable3WhenFromTypeVarAssignsIsEmpty}, hash: D63EDA2F18DD833C8BC11A073C8E5C2E
    @Test()
    void isAssignable3WhenFromTypeVarAssignsIsEmpty() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : true  #  inside getTypeArguments method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : true  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.isAssignable((Type) Object.class, (Type) parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAssignable3WhenFromTypeVarAssignsIsNotNullAndFromTypeVarAssignsIsEmpty}, hash: 6338301DDB031056F4F9237AFE3B6420
    @Test()
    void isAssignable3WhenFromTypeVarAssignsIsNotNullAndFromTypeVarAssignsIsEmpty() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : true  #  inside getTypeArguments method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : true  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.isAssignable((Type) Object.class, (Type) parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAssignable3WhenMidTypeInstanceOfParameterizedTypeThrowsIllegalStateException}, hash: 2EB22473AF2CDA51DBDBCF3F9DCF2525
    @Test()
    void isAssignable3WhenMidTypeInstanceOfParameterizedTypeThrowsIllegalStateException() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        IllegalStateException illegalStateException = new IllegalStateException("s1");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.isAssignable((Type) Object.class, (Type) parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${isAssignable3WhenFromTypeVarAssignsNotIsEmptyThrowsIllegalStateException}, hash: A16B51E00595F48BF1696DC46F74D717
    @Test()
    void isAssignable3WhenFromTypeVarAssignsNotIsEmptyThrowsIllegalStateException() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        IllegalStateException illegalStateException = new IllegalStateException("s1");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.isAssignable((Type) Object.class, (Type) parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${isAssignable3WhenIsAssignableNotClsToClassThrowsNullPointerException}, hash: D23D207329CCC4D117E6447FEC726F4C
    @Test()
    void isAssignable3WhenIsAssignableNotClsToClassThrowsNullPointerException() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            TypeUtils.isAssignable((Type) Object.class, (Type) parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${isAssignable3WhenOwnerTypeInstanceOfParameterizedTypeThrowsIllegalStateException}, hash: 0D5A231D88BEE4136C3913B454CFD77B
    @Test()
    void isAssignable3WhenOwnerTypeInstanceOfParameterizedTypeThrowsIllegalStateException() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        IllegalStateException illegalStateException = new IllegalStateException("s1");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.isAssignable((Type) Object.class, (Type) parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${isAssignable3WhenOwnerTypeInstanceOfParameterizedTypeThrowsNullPointerException}, hash: CA79111524109541DCC8741C1251EC76
    @Test()
    void isAssignable3WhenOwnerTypeInstanceOfParameterizedTypeThrowsNullPointerException() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            TypeUtils.isAssignable((Type) Object.class, (Type) parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${isAssignable3WhenToClassNotEqualsClsThrowsNullPointerException}, hash: 0637C6B82289E1DC22AB403FC875942F
    @Test()
    void isAssignable3WhenToClassNotEqualsClsThrowsNullPointerException() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            TypeUtils.isAssignable((Type) Object.class, (Type) parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${isAssignable3WhenToClassNotEqualsClsThrowsIllegalStateException}, hash: 85F3E96914FCCBA36E5084A2BBD4A9D0
    @Test()
    void isAssignable3WhenToClassNotEqualsClsThrowsIllegalStateException() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        IllegalStateException illegalStateException = new IllegalStateException("s1");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.isAssignable((Type) Object.class, (Type) parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${isAssignable3WhenINotLessThanTypeParamsLengthAndToClassNotEqualsClsThrowsNullPointerException}, hash: E3CCF7BB322F8C6BEE0B85E657C867C9
    @Test()
    void isAssignable3WhenINotLessThanTypeParamsLengthAndToClassNotEqualsClsThrowsNullPointerException() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            TypeUtils.isAssignable((Type) Object.class, (Type) parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${isAssignable3WhenTypeInstanceOfParameterizedTypeThrowsNullPointerException}, hash: DF987D482AC081DD2F535007C3A618DB
    @Test()
    void isAssignable3WhenTypeInstanceOfParameterizedTypeThrowsNullPointerException() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            TypeUtils.isAssignable((Type) Object.class, (Type) parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${isAssignable3WhenToClassNotEqualsClsAndToTypeVarAssignsKeySetIsEmpty}, hash: 78E11CA79DAEED36721F06D8746D0021
    @Test()
    void isAssignable3WhenToClassNotEqualsClsAndToTypeVarAssignsKeySetIsEmpty() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (for-each(toTypeVarAssigns.keySet())) : false  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.isAssignable((Type) Object.class, (Type) parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAssignable3WhenFromTypeArgInstanceOfClass}, hash: 493863712F6660D5693D792CD40EFB72
    @Test()
    void isAssignable3WhenFromTypeArgInstanceOfClass() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : true  #  inside getTypeArguments method
         * (for-each(toTypeVarAssigns.keySet())) : true  #  inside isAssignable method
         * (!(result instanceof TypeVariable<?>)) : true  #  inside unrollVariableAssignments method
         * (toTypeArg == null) : true  #  inside isAssignable method
         * (fromTypeArg instanceof Class) : true  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.isAssignable((Type) Object.class, (Type) parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAssignable3WhenResultNotEqualsTypeVariableAndToTypeArgIsNotNullAndFromTypeArgIsNull}, hash: 5416BE49B4E28713577F91E0EC468A86
    @Test()
    void isAssignable3WhenResultNotEqualsTypeVariableAndToTypeArgIsNotNullAndFromTypeArgIsNull() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : true  #  inside getTypeArguments method
         * (for-each(toTypeVarAssigns.keySet())) : true  #  inside isAssignable method
         * (!(result instanceof TypeVariable<?>)) : false  #  inside unrollVariableAssignments method
         * (result.equals(typeVariable)) : false  #  inside unrollVariableAssignments method
         * (toTypeArg == null) : false  #  inside isAssignable method
         * (fromTypeArg != null) : false  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.isAssignable((Type) Object.class, (Type) parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAssignable3WhenToTypeArgInstanceOfWildcardTypeNotAndIsAssignableFromTypeArgToTypeArgTypeVarAssigns}, hash: E143124A29A60060205F2F96580A47EC
    @Test()
    void isAssignable3WhenToTypeArgInstanceOfWildcardTypeNotAndIsAssignableFromTypeArgToTypeArgTypeVarAssigns() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : true  #  inside getTypeArguments method
         * (for-each(toTypeVarAssigns.keySet())) : true  #  inside isAssignable method
         * (!(result instanceof TypeVariable<?>)) : false  #  inside unrollVariableAssignments method
         * (result.equals(typeVariable)) : true  #  inside unrollVariableAssignments method
         * (toTypeArg == null) : false  #  inside isAssignable method
         * (fromTypeArg != null) : true  #  inside isAssignable method
         * (toTypeArg != null) : true  #  inside isAssignable method
         * (!toTypeArg.equals(fromTypeArg)) : true  #  inside isAssignable method
         * (!(toTypeArg instanceof WildcardType && isAssignable(fromTypeArg, toTypeArg, typeVarAssigns))) : true  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.isAssignable((Type) Object.class, (Type) parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isAssignable3WhenToTypeArgNotEqualsFromTypeArgAndToTypeArgInstanceOfWildcardTypeNotAndIsAssignableFromTypeArgToTypeArgT}, hash: CEE656D08A566D4C031A0A560820FDF3
    @Test()
    void isAssignable3WhenToTypeArgNotEqualsFromTypeArgAndToTypeArgInstanceOfWildcardTypeNotAndIsAssignableFromTypeArgToTypeArgT() {
        /* Branches:
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : true  #  inside getTypeArguments method
         * (for-each(toTypeVarAssigns.keySet())) : true  #  inside isAssignable method
         * (!(result instanceof TypeVariable<?>)) : false  #  inside unrollVariableAssignments method
         * (result.equals(typeVariable)) : true  #  inside unrollVariableAssignments method
         * (toTypeArg == null) : false  #  inside isAssignable method
         * (fromTypeArg != null) : true  #  inside isAssignable method
         * (toTypeArg != null) : true  #  inside isAssignable method
         * (!toTypeArg.equals(fromTypeArg)) : true  #  inside isAssignable method
         * (!(toTypeArg instanceof WildcardType && isAssignable(fromTypeArg, toTypeArg, typeVarAssigns))) : true  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        boolean result = TypeUtils.isAssignable((Type) Object.class, (Type) parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isInstanceWhenTypeIsNull}, hash: DF5E6DD90D7B010E07AD07CDA8F59409
    @Test()
    void isInstanceWhenTypeIsNull() {
        /* Branches:
         * (type == null) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        Type type = null;
        
        //Act Statement(s)
        boolean result = TypeUtils.isInstance(object, type);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isInstanceWhenTypeNotInstanceOfClass_}, hash: E22573FFC2ACF52C8D740F04D85414D2
    @Test()
    void isInstanceWhenTypeNotInstanceOfClass_() {
        /* Branches:
         * (type == null) : false
         * (value == null) : true
         * (!(type instanceof Class<?>)) : true
         */
         //Arrange Statement(s)
        Object object = null;
        
        //Act Statement(s)
        boolean result = TypeUtils.isInstance(object, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isInstanceWhenTypeInstanceOfClass_}, hash: CD59C6E35C6B60E8615A0734BC065FF2
    @Test()
    void isInstanceWhenTypeInstanceOfClass_() {
        /* Branches:
         * (type == null) : false
         * (value == null) : true
         * (!(type instanceof Class<?>)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = null;
        
        //Act Statement(s)
        boolean result = TypeUtils.isInstance(object, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isInstanceWhenToTypeInstanceOfParameterizedTypeAndTypeIsNull}, hash: 4232BF3260AAB4AC3989CC8F11CC2713
    @Test()
    void isInstanceWhenToTypeInstanceOfParameterizedTypeAndTypeIsNull() {
        /* Branches:
         * (type == null) : false
         * (value == null) : false
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : true  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        
        //Act Statement(s)
        boolean result = TypeUtils.isInstance(object, parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isInstanceWhenToClassNotIsPrimitive}, hash: 7BA389D3613F2F85CCDF5F726BEC4E48
    @Test()
    void isInstanceWhenToClassNotIsPrimitive() {
        /* Branches:
         * (type == null) : false
         * (value == null) : false
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : true  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        
        //Act Statement(s)
        boolean result = TypeUtils.isInstance(object, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isInstanceWhenToClassIsPrimitive}, hash: 171FD6FD84ED7CDCE6F8AB8362A8A0D8
    @Test()
    void isInstanceWhenToClassIsPrimitive() {
        /* Branches:
         * (type == null) : false
         * (value == null) : false
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : true  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        
        //Act Statement(s)
        boolean result = TypeUtils.isInstance(object, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isInstanceWhenToClassEqualsType}, hash: EAAC8D1AB0D64488090D95B3F53D4BEA
    @Test()
    void isInstanceWhenToClassEqualsType() {
        /* Branches:
         * (type == null) : false
         * (value == null) : false
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         */
         //Arrange Statement(s)
        Object object = new Object();
        
        //Act Statement(s)
        boolean result = TypeUtils.isInstance(object, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isInstanceWhenClassUtilsIsAssignableTypeToClass}, hash: 9CEFE5CE0A1D6D4BC08B23100AED5125
    @Test()
    void isInstanceWhenClassUtilsIsAssignableTypeToClass() {
        /* Branches:
         * (type == null) : false
         * (value == null) : false
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        
        //Act Statement(s)
        boolean result = TypeUtils.isInstance(object, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isInstanceWhenClassUtilsNotIsAssignableTypeToClass}, hash: 9C912094ABD6BA074F80496C3F218C24
    @Test()
    void isInstanceWhenClassUtilsNotIsAssignableTypeToClass() {
        /* Branches:
         * (type == null) : false
         * (value == null) : false
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : false  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        
        //Act Statement(s)
        boolean result = TypeUtils.isInstance(object, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isInstanceWhenToParameterizedTypeEqualsType}, hash: 3B35959440EB0A9902E2661AE8BC7657
    @Test()
    void isInstanceWhenToParameterizedTypeEqualsType() {
        /* Branches:
         * (type == null) : false
         * (value == null) : false
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : true  #  inside isAssignable method
         */
         //Arrange Statement(s)
        Object object = new Object();
        
        //Act Statement(s)
        boolean result = TypeUtils.isInstance(object, parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isInstanceWhenRawTypeNotInstanceOfClass_ThrowsIllegalStateException}, hash: F607654EB91626F027684BA7C84CE1BA
    @Test()
    void isInstanceWhenRawTypeNotInstanceOfClass_ThrowsIllegalStateException() {
        /* Branches:
         * (type == null) : false
         * (value == null) : false
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : true  #  inside getRawType method
         */
         //Arrange Statement(s)
        Type typeMock = mock(Type.class, "type9");
        doReturn(typeMock).when(parameterizedTypeMock).getRawType();
        Object object = new Object();
        IllegalStateException illegalStateException = new IllegalStateException("Wait... What!? Type of rawType: type9");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.isInstance(object, parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
            verify(parameterizedTypeMock).getRawType();
        });
    }

    //BaseRock generated method id: ${isInstanceWhenFromTypeVarAssignsIsNull}, hash: A23F29AA81E60A78D20D5ECBD8C3A14F
    @Test()
    void isInstanceWhenFromTypeVarAssignsIsNull() {
        /* Branches:
         * (type == null) : false
         * (value == null) : false
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : true  #  inside getTypeArguments method
         * (fromTypeVarAssigns == null) : true  #  inside isAssignable method
         */
         //Arrange Statement(s)
        doReturn(null).when(parameterizedTypeMock).getRawType();
        Object object = new Object();
        
        //Act Statement(s)
        boolean result = TypeUtils.isInstance(object, parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.FALSE));
            verify(parameterizedTypeMock).getRawType();
        });
    }

    //BaseRock generated method id: ${isInstanceWhenFromTypeVarAssignsIsEmpty}, hash: D6E5A9CA0A980A03C0ACF08EDD9164AD
    @Test()
    void isInstanceWhenFromTypeVarAssignsIsEmpty() {
        /* Branches:
         * (type == null) : false
         * (value == null) : false
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : true  #  inside getTypeArguments method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : true  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        
        //Act Statement(s)
        boolean result = TypeUtils.isInstance(object, parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isInstanceWhenMidTypeInstanceOfParameterizedTypeThrowsIllegalStateException}, hash: 025AD50ACBC54E7F17A98A0760669CAE
    @Test()
    void isInstanceWhenMidTypeInstanceOfParameterizedTypeThrowsIllegalStateException() {
        /* Branches:
         * (type == null) : false
         * (value == null) : false
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        IllegalStateException illegalStateException = new IllegalStateException("s1");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.isInstance(object, parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${isInstanceWhenFromTypeVarAssignsNotIsEmptyThrowsIllegalStateException}, hash: 470AFCC20B6293034C8F5AE16AAD8D6A
    @Test()
    void isInstanceWhenFromTypeVarAssignsNotIsEmptyThrowsIllegalStateException() {
        /* Branches:
         * (type == null) : false
         * (value == null) : false
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        IllegalStateException illegalStateException = new IllegalStateException("s1");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.isInstance(object, parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${isInstanceWhenIsAssignableNotClsToClassThrowsNullPointerException}, hash: 275620AA5DA2D329BCE48E87C93780E0
    @Test()
    void isInstanceWhenIsAssignableNotClsToClassThrowsNullPointerException() {
        /* Branches:
         * (type == null) : false
         * (value == null) : false
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            TypeUtils.isInstance(object, parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${isInstanceWhenOwnerTypeInstanceOfParameterizedTypeThrowsIllegalStateException}, hash: 2831D7826EAE1EED282DE703F55CF9C1
    @Test()
    void isInstanceWhenOwnerTypeInstanceOfParameterizedTypeThrowsIllegalStateException() {
        /* Branches:
         * (type == null) : false
         * (value == null) : false
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        IllegalStateException illegalStateException = new IllegalStateException("s1");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.isInstance(object, parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${isInstanceWhenOwnerTypeInstanceOfParameterizedTypeThrowsNullPointerException}, hash: 3DECE3955833AF6C62FE817708511EC3
    @Test()
    void isInstanceWhenOwnerTypeInstanceOfParameterizedTypeThrowsNullPointerException() {
        /* Branches:
         * (type == null) : false
         * (value == null) : false
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            TypeUtils.isInstance(object, parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${isInstanceWhenToClassNotEqualsClsThrowsNullPointerException}, hash: C41EB75F4C3549A69B7087A53EE860CB
    @Test()
    void isInstanceWhenToClassNotEqualsClsThrowsNullPointerException() {
        /* Branches:
         * (type == null) : false
         * (value == null) : false
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            TypeUtils.isInstance(object, parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${isInstanceWhenToClassNotEqualsClsThrowsIllegalStateException}, hash: 9AA83EA549822F99C970DFDBF0105C9D
    @Test()
    void isInstanceWhenToClassNotEqualsClsThrowsIllegalStateException() {
        /* Branches:
         * (type == null) : false
         * (value == null) : false
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        IllegalStateException illegalStateException = new IllegalStateException("s1");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.isInstance(object, parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${isInstanceWhenINotLessThanTypeParamsLengthAndToClassNotEqualsClsThrowsNullPointerException}, hash: 8D009D95A84EF0CE0D61189686F15327
    @Test()
    void isInstanceWhenINotLessThanTypeParamsLengthAndToClassNotEqualsClsThrowsNullPointerException() {
        /* Branches:
         * (type == null) : false
         * (value == null) : false
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            TypeUtils.isInstance(object, parameterizedTypeMock);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${isInstanceWhenToClassNotEqualsClsAndToTypeVarAssignsKeySetIsEmpty}, hash: 475DB1A34DC0F80AE84C1B94874E1F0C
    @Test()
    void isInstanceWhenToClassNotEqualsClsAndToTypeVarAssignsKeySetIsEmpty() {
        /* Branches:
         * (type == null) : false
         * (value == null) : false
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (for-each(toTypeVarAssigns.keySet())) : false  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        
        //Act Statement(s)
        boolean result = TypeUtils.isInstance(object, parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isInstanceWhenINotLessThanTypeParamsLengthAndToClassNotEqualsClsAndToTypeVarAssignsKeySetIsEmpty}, hash: D5E59362F68246583A0B2A41365C670D
    @Test()
    void isInstanceWhenINotLessThanTypeParamsLengthAndToClassNotEqualsClsAndToTypeVarAssignsKeySetIsEmpty() {
        /* Branches:
         * (type == null) : false
         * (value == null) : false
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (for-each(toTypeVarAssigns.keySet())) : false  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        
        //Act Statement(s)
        boolean result = TypeUtils.isInstance(object, parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isInstanceWhenFromTypeArgInstanceOfClass}, hash: 00134BD56FDC3586704CDF9E4F406E08
    @Test()
    void isInstanceWhenFromTypeArgInstanceOfClass() {
        /* Branches:
         * (type == null) : false
         * (value == null) : false
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : true  #  inside getTypeArguments method
         * (for-each(toTypeVarAssigns.keySet())) : true  #  inside isAssignable method
         * (!(result instanceof TypeVariable<?>)) : true  #  inside unrollVariableAssignments method
         * (toTypeArg == null) : true  #  inside isAssignable method
         * (fromTypeArg instanceof Class) : true  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        
        //Act Statement(s)
        boolean result = TypeUtils.isInstance(object, parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isInstanceWhenResultNotEqualsTypeVariableAndToTypeArgIsNotNullAndFromTypeArgIsNull}, hash: F74AB952454EA1240701C49437499D4C
    @Test()
    void isInstanceWhenResultNotEqualsTypeVariableAndToTypeArgIsNotNullAndFromTypeArgIsNull() {
        /* Branches:
         * (type == null) : false
         * (value == null) : false
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : true  #  inside getTypeArguments method
         * (for-each(toTypeVarAssigns.keySet())) : true  #  inside isAssignable method
         * (!(result instanceof TypeVariable<?>)) : false  #  inside unrollVariableAssignments method
         * (result.equals(typeVariable)) : false  #  inside unrollVariableAssignments method
         * (toTypeArg == null) : false  #  inside isAssignable method
         * (fromTypeArg != null) : false  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        
        //Act Statement(s)
        boolean result = TypeUtils.isInstance(object, parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isInstanceWhenToTypeArgInstanceOfWildcardTypeNotAndIsAssignableFromTypeArgToTypeArgTypeVarAssigns}, hash: C212FA7E76E96E9BABB3A19723008743
    @Test()
    void isInstanceWhenToTypeArgInstanceOfWildcardTypeNotAndIsAssignableFromTypeArgToTypeArgTypeVarAssigns() {
        /* Branches:
         * (type == null) : false
         * (value == null) : false
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : true  #  inside getTypeArguments method
         * (for-each(toTypeVarAssigns.keySet())) : true  #  inside isAssignable method
         * (!(result instanceof TypeVariable<?>)) : false  #  inside unrollVariableAssignments method
         * (result.equals(typeVariable)) : true  #  inside unrollVariableAssignments method
         * (toTypeArg == null) : false  #  inside isAssignable method
         * (fromTypeArg != null) : true  #  inside isAssignable method
         * (toTypeArg != null) : true  #  inside isAssignable method
         * (!toTypeArg.equals(fromTypeArg)) : true  #  inside isAssignable method
         * (!(toTypeArg instanceof WildcardType && isAssignable(fromTypeArg, toTypeArg, typeVarAssigns))) : true  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        
        //Act Statement(s)
        boolean result = TypeUtils.isInstance(object, parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isInstanceWhenToTypeArgNotEqualsFromTypeArgAndToTypeArgInstanceOfWildcardTypeNotAndIsAssignableFromTypeArgToTypeArgType}, hash: 891328CE551F105EA074D9FF51DB7DD7
    @Test()
    void isInstanceWhenToTypeArgNotEqualsFromTypeArgAndToTypeArgInstanceOfWildcardTypeNotAndIsAssignableFromTypeArgToTypeArgType() {
        /* Branches:
         * (type == null) : false
         * (value == null) : false
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : true  #  inside getTypeArguments method
         * (for-each(toTypeVarAssigns.keySet())) : true  #  inside isAssignable method
         * (!(result instanceof TypeVariable<?>)) : false  #  inside unrollVariableAssignments method
         * (result.equals(typeVariable)) : true  #  inside unrollVariableAssignments method
         * (toTypeArg == null) : false  #  inside isAssignable method
         * (fromTypeArg != null) : true  #  inside isAssignable method
         * (toTypeArg != null) : true  #  inside isAssignable method
         * (!toTypeArg.equals(fromTypeArg)) : true  #  inside isAssignable method
         * (!(toTypeArg instanceof WildcardType && isAssignable(fromTypeArg, toTypeArg, typeVarAssigns))) : true  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        
        //Act Statement(s)
        boolean result = TypeUtils.isInstance(object, parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isInstanceWhenToTypeArgInstanceOfWildcardTypeAndIsAssignableFromTypeArgToTypeArgTypeVarAssigns}, hash: AB7DF4E6FE656D124D2733C42CA66A20
    @Test()
    void isInstanceWhenToTypeArgInstanceOfWildcardTypeAndIsAssignableFromTypeArgToTypeArgTypeVarAssigns() {
        /* Branches:
         * (type == null) : false
         * (value == null) : false
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : true  #  inside getTypeArguments method
         * (for-each(toTypeVarAssigns.keySet())) : true  #  inside isAssignable method
         * (!(result instanceof TypeVariable<?>)) : true  #  inside unrollVariableAssignments method
         * (toTypeArg == null) : false  #  inside isAssignable method
         * (fromTypeArg != null) : true  #  inside isAssignable method
         * (toTypeArg != null) : true  #  inside isAssignable method
         * (!toTypeArg.equals(fromTypeArg)) : true  #  inside isAssignable method
         * (!(toTypeArg instanceof WildcardType && isAssignable(fromTypeArg, toTypeArg, typeVarAssigns))) : false  #  inside isAssignable method
         * (!(toTypeArg instanceof WildcardType && isAssignable(fromTypeArg, toTypeArg, typeVarAssigns))) : false  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        
        //Act Statement(s)
        boolean result = TypeUtils.isInstance(object, parameterizedTypeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${normalizeUpperBoundsWhenBoundsLengthLessThan2}, hash: 4D210591047176F1A545C42E8A07438B
    @Test()
    void normalizeUpperBoundsWhenBoundsLengthLessThan2() {
        /* Branches:
         * (bounds.length < 2) : true
         */
         //Arrange Statement(s)
        Type[] typeArray = new Type[] {};
        
        //Act Statement(s)
        Type[] result = TypeUtils.normalizeUpperBounds(typeArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(typeArray)));
    }

    //BaseRock generated method id: ${normalizeUpperBoundsWhenSubtypeFound}, hash: 3E9BC147F5BBA84E9BB07EB9AF38EA08
    @Test()
    void normalizeUpperBoundsWhenSubtypeFound() {
        /* Branches:
         * (bounds.length < 2) : false
         * (for-each(bounds)) : true
         * (for-each(bounds)) : true
         * (type1 != type2) : true
         * (toType == null) : true  #  inside isAssignable method
         * (type == null) : true  #  inside isAssignable method
         * (toClass == null) : true  #  inside isAssignable method
         * (isAssignable(type2, type1, null)) : true
         * (!subtypeFound) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Type[] typeArray = new Type[] {};
        
        //Act Statement(s)
        Type[] result = TypeUtils.normalizeUpperBounds(typeArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${normalizeUpperBoundsWhenToTypeNotInstanceOfTypeVariable_ThrowsIllegalStateException}, hash: A0B2545FC752154F8E39AA667365A2A7
    @Test()
    void normalizeUpperBoundsWhenToTypeNotInstanceOfTypeVariable_ThrowsIllegalStateException() {
        /* Branches:
         * (bounds.length < 2) : false
         * (for-each(bounds)) : true
         * (for-each(bounds)) : true
         * (type1 != type2) : true
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : false  #  inside isAssignable method
         * (toType instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toType instanceof WildcardType) : false  #  inside isAssignable method
         * (toType instanceof TypeVariable<?>) : false  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Type[] typeArray = new Type[] {};
        IllegalStateException illegalStateException = new IllegalStateException("s1");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.normalizeUpperBounds(typeArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${normalizeUpperBoundsWhenIsAssignableType2Type1NullAndSubtypeFound}, hash: 9CE666EE9117336BC5588F9E2553B77B
    @Test()
    void normalizeUpperBoundsWhenIsAssignableType2Type1NullAndSubtypeFound() {
        /* Branches:
         * (bounds.length < 2) : false
         * (for-each(bounds)) : true
         * (for-each(bounds)) : true
         * (type1 != type2) : true
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (isAssignable(type2, type1, null)) : true
         * (!subtypeFound) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Type[] typeArray = new Type[] {};
        
        //Act Statement(s)
        Type[] result = TypeUtils.normalizeUpperBounds(typeArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${normalizeUpperBoundsWhenToTypeInstanceOfWildcardTypeAndIsAssignableType2Type1NullAndSubtypeFound}, hash: 1CF5B1686EDA4804B69B3649670C83BC
    @Test()
    void normalizeUpperBoundsWhenToTypeInstanceOfWildcardTypeAndIsAssignableType2Type1NullAndSubtypeFound() {
        /* Branches:
         * (bounds.length < 2) : false
         * (for-each(bounds)) : true
         * (for-each(bounds)) : true
         * (type1 != type2) : true
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : false  #  inside isAssignable method
         * (toType instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toType instanceof WildcardType) : true  #  inside isAssignable method
         * (isAssignable(type2, type1, null)) : true
         * (!subtypeFound) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Type[] typeArray = new Type[] {};
        
        //Act Statement(s)
        Type[] result = TypeUtils.normalizeUpperBounds(typeArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${normalizeUpperBoundsWhenToTypeInstanceOfTypeVariable_AndIsAssignableNotType2Type1NullAndNotSubtypeFound}, hash: 83D84815E40FC4EF97378E3996CC61FB
    @Test()
    void normalizeUpperBoundsWhenToTypeInstanceOfTypeVariable_AndIsAssignableNotType2Type1NullAndNotSubtypeFound() {
        /* Branches:
         * (bounds.length < 2) : false
         * (for-each(bounds)) : true
         * (for-each(bounds)) : true
         * (type1 != type2) : true
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : false  #  inside isAssignable method
         * (toType instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toType instanceof WildcardType) : false  #  inside isAssignable method
         * (toType instanceof TypeVariable<?>) : true  #  inside isAssignable method
         * (isAssignable(type2, type1, null)) : false
         * (!subtypeFound) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Type[] typeArray = new Type[] {};
        
        //Act Statement(s)
        Type[] result = TypeUtils.normalizeUpperBounds(typeArray);
        TypeVariable typeVariableMock = mock(TypeVariable.class);
        Type[] typeResultArray = new Type[] { typeVariableMock };
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(typeResultArray)));
    }

    //BaseRock generated method id: ${normalizeUpperBoundsWhenToParameterizedTypeEqualsTypeAndIsAssignableType2Type1NullAndSubtypeFound}, hash: AD2655951515BE6FF194A99646415DF8
    @Test()
    void normalizeUpperBoundsWhenToParameterizedTypeEqualsTypeAndIsAssignableType2Type1NullAndSubtypeFound() {
        /* Branches:
         * (bounds.length < 2) : false
         * (for-each(bounds)) : true
         * (for-each(bounds)) : true
         * (type1 != type2) : true
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : true  #  inside isAssignable method
         * (isAssignable(type2, type1, null)) : true
         * (!subtypeFound) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Type[] typeArray = new Type[] {};
        
        //Act Statement(s)
        Type[] result = TypeUtils.normalizeUpperBounds(typeArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${normalizeUpperBoundsWhenToGenericArrayTypeEqualsTypeAndIsAssignableType2Type1NullAndSubtypeFound}, hash: 3D373331C78DAD8E743D6D6126B9EA31
    @Test()
    void normalizeUpperBoundsWhenToGenericArrayTypeEqualsTypeAndIsAssignableType2Type1NullAndSubtypeFound() {
        /* Branches:
         * (bounds.length < 2) : false
         * (for-each(bounds)) : true
         * (for-each(bounds)) : true
         * (type1 != type2) : true
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : false  #  inside isAssignable method
         * (toType instanceof GenericArrayType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toGenericArrayType == null) : false  #  inside isAssignable method
         * (toGenericArrayType.equals(type)) : true  #  inside isAssignable method
         * (isAssignable(type2, type1, null)) : true
         * (!subtypeFound) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Type[] typeArray = new Type[] {};
        
        //Act Statement(s)
        Type[] result = TypeUtils.normalizeUpperBounds(typeArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${parameterizeWhenVariablesIsNotEmpty}, hash: DFCBD818026892E890EB4752C60B98FD
    @Test()
    void parameterizeWhenVariablesIsNotEmpty() {
        /* Branches:
         * (for-each(variables)) : true  #  inside extractTypeArgumentsFrom method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            typeUtils.when(() -> TypeUtils.toString(typeVariableMock)).thenReturn("return_of_toString1");
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.parameterizeWithOwner((Type) null, Object.class, typeArray)).thenReturn(parameterizedTypeMock);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            ParameterizedType result = TypeUtils.parameterize(Object.class, anyMap);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(parameterizedTypeMock));
                typeUtils.verify(() -> TypeUtils.toString(typeVariableMock), atLeast(1));
                typeUtils.verify(() -> TypeUtils.parameterizeWithOwner((Type) null, Object.class, typeArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${parameterize1Test}, hash: 2DECF05DF94A7E3FFDFC1930EE9C6F7E
    @Test()
    void parameterize1Test() {
        //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.parameterizeWithOwner((Type) null, Object.class, typeArray)).thenReturn(parameterizedTypeMock);
            //Act Statement(s)
            ParameterizedType result = TypeUtils.parameterize(Object.class, typeArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(parameterizedTypeMock));
                typeUtils.verify(() -> TypeUtils.parameterizeWithOwner((Type) null, Object.class, typeArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${parameterizeWithOwnerWhenVariablesIsNotEmpty}, hash: 42A1CE30FB194B933B415CCBDC4B4BA6
    @Test()
    void parameterizeWithOwnerWhenVariablesIsNotEmpty() {
        /* Branches:
         * (for-each(variables)) : true  #  inside extractTypeArgumentsFrom method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            typeUtils.when(() -> TypeUtils.toString(typeVariableMock)).thenReturn("return_of_toString1");
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.parameterizeWithOwner(typeMock, Object.class, typeArray)).thenReturn(parameterizedTypeMock);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            ParameterizedType result = TypeUtils.parameterizeWithOwner(typeMock, Object.class, anyMap);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(parameterizedTypeMock));
                typeUtils.verify(() -> TypeUtils.toString(typeVariableMock), atLeast(1));
                typeUtils.verify(() -> TypeUtils.parameterizeWithOwner(typeMock, Object.class, typeArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${parameterizeWithOwner1WhenOwnerIsNullThrowsIllegalArgumentException}, hash: B39C6E1970FA3A140B873AC9EC39975F
    @Test()
    void parameterizeWithOwner1WhenOwnerIsNullThrowsIllegalArgumentException() {
        /* Branches:
         * (rawClass.getEnclosingClass() == null) : true
         * (owner == null) : true
         */
         //Arrange Statement(s)
        Type type = null;
        Type[] typeArray = new Type[] {};
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            TypeUtils.parameterizeWithOwner(type, Object.class, typeArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${parameterizeWithOwner1WhenOwnerIsNotNullThrowsIllegalArgumentException}, hash: 1E7AA8B31E8914E898E56E09F620CCD5
    @Test()
    void parameterizeWithOwner1WhenOwnerIsNotNullThrowsIllegalArgumentException() {
        /* Branches:
         * (rawClass.getEnclosingClass() == null) : true
         * (owner == null) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Type[] typeArray = new Type[] {};
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            TypeUtils.parameterizeWithOwner(typeMock, Object.class, typeArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${parameterizeWithOwner1WhenToClassIsNullThrowsIllegalArgumentException}, hash: CE887144450C834EEEE5E3C387FAB251
    @Test()
    void parameterizeWithOwner1WhenToClassIsNullThrowsIllegalArgumentException() {
        /* Branches:
         * (rawClass.getEnclosingClass() == null) : false
         * (owner == null) : false
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : true  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Type[] typeArray = new Type[] {};
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            TypeUtils.parameterizeWithOwner(typeMock, Object.class, typeArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${parameterizeWithOwner1WhenRawClassGetTypeParametersLengthEqualsTypeArgumentsLengthThrowsIllegalArgumentException}, hash: 7DB94EE17756B3703FEB15EBE8306E81
    @Test()
    void parameterizeWithOwner1WhenRawClassGetTypeParametersLengthEqualsTypeArgumentsLengthThrowsIllegalArgumentException() {
        /* Branches:
         * (rawClass.getEnclosingClass() == null) : false
         * (owner == null) : false
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (rawClass.getTypeParameters().length == typeArguments.length) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Type[] typeArray = new Type[] {};
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            TypeUtils.parameterizeWithOwner(typeMock, Object.class, typeArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${parameterizeWithOwner1WhenClassUtilsNotIsAssignableTypeToClassThrowsIllegalArgumentException}, hash: 82A73B7B9E095EF9C68F759C16647310
    @Test()
    void parameterizeWithOwner1WhenClassUtilsNotIsAssignableTypeToClassThrowsIllegalArgumentException() {
        /* Branches:
         * (rawClass.getEnclosingClass() == null) : false
         * (owner == null) : false
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : false  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Type[] typeArray = new Type[] {};
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            TypeUtils.parameterizeWithOwner((Type) Object.class, Object.class, typeArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${parameterizeWithOwner1WhenRawClassGetTypeParametersLengthNotEqualsTypeArgumentsLengthThrowsIllegalArgumentException}, hash: 5A9EDFBEB9A0EC3F5A6BA9B27B88140B
    @Test()
    void parameterizeWithOwner1WhenRawClassGetTypeParametersLengthNotEqualsTypeArgumentsLengthThrowsIllegalArgumentException() {
        /* Branches:
         * (rawClass.getEnclosingClass() == null) : false
         * (owner == null) : false
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (rawClass.getTypeParameters().length == typeArguments.length) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Type[] typeArray = new Type[] {};
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            TypeUtils.parameterizeWithOwner((Type) Object.class, Object.class, typeArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${parameterizeWithOwner1WhenRawTypeNotInstanceOfClass_ThrowsIllegalStateException}, hash: 772C0589A8DFBACD02E8B4D1A752DB18
    @Test()
    void parameterizeWithOwner1WhenRawTypeNotInstanceOfClass_ThrowsIllegalStateException() {
        /* Branches:
         * (rawClass.getEnclosingClass() == null) : false
         * (owner == null) : false
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : false  #  inside isAssignable method
         * (type instanceof ParameterizedType) : true  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : true  #  inside getRawType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Type[] typeArray = new Type[] {};
        IllegalStateException illegalStateException = new IllegalStateException("s1");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.parameterizeWithOwner((Type) parameterizedTypeMock, Object.class, typeArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${parameterizeWithOwner1WhenRawTypeInstanceOfClass_AndRawClassGetTypeParametersLengthNotEquThrowsIllegalArgumentException}, hash: CE67495202027448B04E42FBD37B3D13
    @Test()
    void parameterizeWithOwner1WhenRawTypeInstanceOfClass_AndRawClassGetTypeParametersLengthNotEquThrowsIllegalArgumentException() {
        /* Branches:
         * (rawClass.getEnclosingClass() == null) : false
         * (owner == null) : false
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : false  #  inside isAssignable method
         * (type instanceof ParameterizedType) : true  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (rawClass.getTypeParameters().length == typeArguments.length) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Type[] typeArray = new Type[] {};
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            TypeUtils.parameterizeWithOwner((Type) parameterizedTypeMock, Object.class, typeArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${parameterizeWithOwner1WhenIsAssignableNotBoundToClassThrowsIllegalArgumentException}, hash: B30A4C29160039767B1C309ED372747E
    @Test()
    void parameterizeWithOwner1WhenIsAssignableNotBoundToClassThrowsIllegalArgumentException() {
        /* Branches:
         * (rawClass.getEnclosingClass() == null) : false
         * (owner == null) : false
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : false  #  inside isAssignable method
         * (type instanceof ParameterizedType) : false  #  inside isAssignable method
         * (type instanceof TypeVariable<?>) : true  #  inside isAssignable method
         * (for-each(((TypeVariable<?>) type).getBounds())) : true  #  inside isAssignable method
         * (isAssignable(bound, toClass)) : false  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TypeVariable typeVariableMock = mock(TypeVariable.class);
        Type[] typeArray = new Type[] {};
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            TypeUtils.parameterizeWithOwner((Type) typeVariableMock, Object.class, typeArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${parameterizeWithOwner1WhenTypeInstanceOfWildcardTypeThrowsIllegalArgumentException}, hash: 40A21F545122E5F47B1A8CE311A725AC
    @Test()
    void parameterizeWithOwner1WhenTypeInstanceOfWildcardTypeThrowsIllegalArgumentException() {
        /* Branches:
         * (rawClass.getEnclosingClass() == null) : false
         * (owner == null) : false
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : false  #  inside isAssignable method
         * (type instanceof ParameterizedType) : false  #  inside isAssignable method
         * (type instanceof TypeVariable<?>) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (type instanceof WildcardType) : true  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Type[] typeArray = new Type[] {};
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            TypeUtils.parameterizeWithOwner((Type) wildcardTypeMock, Object.class, typeArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${parameterizeWithOwner1WhenTypeNotInstanceOfWildcardTypeThrowsIllegalStateException}, hash: 5E99A74F2041FB9AE88E9FC2BFF7A1C5
    @Test()
    void parameterizeWithOwner1WhenTypeNotInstanceOfWildcardTypeThrowsIllegalStateException() {
        /* Branches:
         * (rawClass.getEnclosingClass() == null) : false
         * (owner == null) : false
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : false  #  inside isAssignable method
         * (type instanceof ParameterizedType) : false  #  inside isAssignable method
         * (type instanceof TypeVariable<?>) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (type instanceof WildcardType) : false  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Type typeMock = mock(Type.class, "owner");
        Type[] typeArray = new Type[] {};
        IllegalStateException illegalStateException = new IllegalStateException("s1");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            TypeUtils.parameterizeWithOwner(typeMock, Object.class, typeArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${parameterizeWithOwner1WhenIsAssignableBoundToClassAndRawClassGetTypeParametersLengthNotEqThrowsIllegalArgumentException}, hash: CC7DBB2C3E8A70135375BF67AE131A7F
    @Test()
    void parameterizeWithOwner1WhenIsAssignableBoundToClassAndRawClassGetTypeParametersLengthNotEqThrowsIllegalArgumentException() {
        /* Branches:
         * (rawClass.getEnclosingClass() == null) : false
         * (owner == null) : false
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : false  #  inside isAssignable method
         * (type instanceof ParameterizedType) : false  #  inside isAssignable method
         * (type instanceof TypeVariable<?>) : true  #  inside isAssignable method
         * (for-each(((TypeVariable<?>) type).getBounds())) : true  #  inside isAssignable method
         * (isAssignable(bound, toClass)) : true  #  inside isAssignable method
         * (rawClass.getTypeParameters().length == typeArguments.length) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TypeVariable typeVariableMock = mock(TypeVariable.class);
        Type[] typeArray = new Type[] {};
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            TypeUtils.parameterizeWithOwner((Type) typeVariableMock, Object.class, typeArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${parameterizeWithOwner1WhenIsAssignableNotTypeGetGenericComponentTypeToClassGetComponentTyThrowsIllegalArgumentException}, hash: 434D2082DBDA616AA0DCD0FFF5E8E1B0
    @Test()
    void parameterizeWithOwner1WhenIsAssignableNotTypeGetGenericComponentTypeToClassGetComponentTyThrowsIllegalArgumentException() {
        /* Branches:
         * (rawClass.getEnclosingClass() == null) : false
         * (owner == null) : false
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : false  #  inside isAssignable method
         * (type instanceof ParameterizedType) : false  #  inside isAssignable method
         * (type instanceof TypeVariable<?>) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : true  #  inside isAssignable method
         * (toClass.equals(Object.class)) : false  #  inside isAssignable method
         * (toClass.isArray()) : true  #  inside isAssignable method
         * (isAssignable(((GenericArrayType) type).getGenericComponentType(), toClass.getComponentType())) : false  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Type[] typeArray = new Type[] {};
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            TypeUtils.parameterizeWithOwner((Type) genericArrayTypeMock, Object.class, typeArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${parameterizeWithOwner1WhenIsAssignableTypeGetGenericComponentTypeToClassGetComponentTypeAThrowsIllegalArgumentException}, hash: E4B68C6C34F98DEB98327C2FE3CAE5EC
    @Test()
    void parameterizeWithOwner1WhenIsAssignableTypeGetGenericComponentTypeToClassGetComponentTypeAThrowsIllegalArgumentException() {
        /* Branches:
         * (rawClass.getEnclosingClass() == null) : false
         * (owner == null) : false
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : false  #  inside isAssignable method
         * (type instanceof ParameterizedType) : false  #  inside isAssignable method
         * (type instanceof TypeVariable<?>) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : true  #  inside isAssignable method
         * (toClass.equals(Object.class)) : false  #  inside isAssignable method
         * (toClass.isArray()) : true  #  inside isAssignable method
         * (isAssignable(((GenericArrayType) type).getGenericComponentType(), toClass.getComponentType())) : true  #  inside isAssignable method
         * (rawClass.getTypeParameters().length == typeArguments.length) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Type[] typeArray = new Type[] {};
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            TypeUtils.parameterizeWithOwner((Type) genericArrayTypeMock, Object.class, typeArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${toLongStringWhenBoundsLengthNotGreaterThan0}, hash: C586D92D5154734886B0E64F92D5EF33
    @Test()
    void toLongStringWhenBoundsLengthNotGreaterThan0() {
        /* Branches:
         * (d instanceof Class<?>) : false
         * (d instanceof Type) : false
         * (bounds.length > 0) : false  #  inside typeVariableToString method
         */
         //Arrange Statement(s)
        GenericDeclaration genericDeclarationMock = mock(GenericDeclaration.class, "genericDeclaration");
        doReturn(genericDeclarationMock).when(typeVariableMock).getGenericDeclaration();
        doReturn("").when(typeVariableMock).getName();
        Type[] typeArray = new Type[] {};
        doReturn(typeArray).when(typeVariableMock).getBounds();
        
        //Act Statement(s)
        String result = TypeUtils.toLongString(typeVariableMock);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("genericDeclaration:"));
            verify(typeVariableMock).getGenericDeclaration();
            verify(typeVariableMock).getName();
            verify(typeVariableMock).getBounds();
        });
    }

    //BaseRock generated method id: ${toLongStringWhenNotAppend}, hash: 9BF3484C24FE6EFF95F3ECB3C8B761AE
    @Test()
    void toLongStringWhenNotAppend() {
        /* Branches:
         * (d instanceof Class<?>) : true
         * (c.getEnclosingClass() == null) : true
         * (bounds.length > 0) : true  #  inside typeVariableToString method
         * (!(bounds.length == 1 && Object.class.equals(bounds[0]))) : false  #  inside typeVariableToString method
         * (bound instanceof ParameterizedType) : true  #  inside typeVariableToString method
         * (rawType instanceof Class) : true  #  inside typeVariableToString method
         * (((Class<?>) rawType).isInterface()) : true  #  inside typeVariableToString method
         * (append) : false  #  inside typeVariableToString method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        String result = TypeUtils.toLongString(typeVariableMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${toStringWhenClsIsArray}, hash: 43E701D696F63B3EC5CEA4FDF6C60F57
    @Test()
    void toStringWhenClsIsArray() {
        /* Branches:
         * (type instanceof Class<?>) : true
         * (cls.isArray()) : true  #  inside classToString method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        String result = TypeUtils.toString(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${toStringWhenIsCyclicalCls}, hash: DA029D2076AE08AFDD52DC1661FAB5FE
    @Test()
    void toStringWhenIsCyclicalCls() {
        /* Branches:
         * (type instanceof Class<?>) : true
         * (cls.isArray()) : false  #  inside classToString method
         * (for-each(cls.getTypeParameters())) : true  #  inside isCyclical method
         * (for-each(typeParameter.getAnnotatedBounds())) : true  #  inside isCyclical method
         * (annotatedBound.getType().getTypeName().contains(cls.getName())) : true  #  inside isCyclical method
         * (isCyclical(cls)) : true  #  inside classToString method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        String result = TypeUtils.toString(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${toStringWhenClsGetTypeParametersLengthGreaterThan0}, hash: 93F3CDFD2F0FBF4FA9C72EA765F27C45
    @Test()
    void toStringWhenClsGetTypeParametersLengthGreaterThan0() {
        /* Branches:
         * (type instanceof Class<?>) : true
         * (cls.isArray()) : false  #  inside classToString method
         * (for-each(cls.getTypeParameters())) : true  #  inside isCyclical method
         * (for-each(typeParameter.getAnnotatedBounds())) : true  #  inside isCyclical method
         * (annotatedBound.getType().getTypeName().contains(cls.getName())) : false  #  inside isCyclical method
         * (isCyclical(cls)) : false  #  inside classToString method
         * (cls.getEnclosingClass() != null) : true  #  inside classToString method
         * (cls.getTypeParameters().length > 0) : true  #  inside classToString method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        String result = TypeUtils.toString(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${toStringWhenClsGetEnclosingClassIsNullAndClsGetTypeParametersLengthGreaterThan0}, hash: EDA9E0EDD84F34F7647CDB7E08016A76
    @Test()
    void toStringWhenClsGetEnclosingClassIsNullAndClsGetTypeParametersLengthGreaterThan0() {
        /* Branches:
         * (type instanceof Class<?>) : true
         * (cls.isArray()) : false  #  inside classToString method
         * (for-each(cls.getTypeParameters())) : true  #  inside isCyclical method
         * (for-each(typeParameter.getAnnotatedBounds())) : true  #  inside isCyclical method
         * (annotatedBound.getType().getTypeName().contains(cls.getName())) : false  #  inside isCyclical method
         * (isCyclical(cls)) : false  #  inside classToString method
         * (cls.getEnclosingClass() != null) : false  #  inside classToString method
         * (cls.getTypeParameters().length > 0) : true  #  inside classToString method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        String result = TypeUtils.toString(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenReplacementTypeIsNullThrowsIllegalArgumentException}, hash: 3B4EBB181D77559946B121D5267E71CA
    @Test()
    void typesSatisfyVariablesWhenReplacementTypeIsNullThrowsIllegalArgumentException() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : true  #  inside substituteTypeVariables method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
                TypeUtils.typesSatisfyVariables(anyMap);
            });
            IllegalArgumentException illegalArgumentException = new IllegalArgumentException("s1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenIsAssignableTypeSubstituteTypeVariablesBoundTypeVariableMapTypeVariableMap}, hash: DEE5726872BB3EF792DA6DCB45874697
    @Test()
    void typesSatisfyVariablesWhenIsAssignableTypeSubstituteTypeVariablesBoundTypeVariableMapTypeVariableMap() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : true  #  inside isAssignable method
         * (!isAssignable(type, substituteTypeVariables(bound, typeVariableMap), typeVariableMap)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            boolean result = TypeUtils.typesSatisfyVariables(anyMap);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenToClassNotIsPrimitiveAndIsAssignableTypeSubstituteTypeVariablesBoundTypeVariableMapTypeVariabl}, hash: CD626541002172408E0181D93AB6117E
    @Test()
    void typesSatisfyVariablesWhenToClassNotIsPrimitiveAndIsAssignableTypeSubstituteTypeVariablesBoundTypeVariableMapTypeVariabl() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : true  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (!isAssignable(type, substituteTypeVariables(bound, typeVariableMap), typeVariableMap)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            boolean result = TypeUtils.typesSatisfyVariables(anyMap);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenIsAssignableNotTypeSubstituteTypeVariablesBoundTypeVariableMapTypeVariableMap}, hash: C09D804196BC83DC8019D9591BC6D352
    @Test()
    void typesSatisfyVariablesWhenIsAssignableNotTypeSubstituteTypeVariablesBoundTypeVariableMapTypeVariableMap() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : true  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : false  #  inside isAssignable method
         * (!isAssignable(type, substituteTypeVariables(bound, typeVariableMap), typeVariableMap)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            boolean result = TypeUtils.typesSatisfyVariables(anyMap);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenToClassEqualsTypeAndIsAssignableTypeSubstituteTypeVariablesBoundTypeVariableMapTypeVariableMap}, hash: DE479280E0DEFB99E60E2FEDA1915887
    @Test()
    void typesSatisfyVariablesWhenToClassEqualsTypeAndIsAssignableTypeSubstituteTypeVariablesBoundTypeVariableMapTypeVariableMap() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(type, substituteTypeVariables(bound, typeVariableMap), typeVariableMap)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            boolean result = TypeUtils.typesSatisfyVariables(anyMap);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenTypeInstanceOfGenericArrayTypeAndIsAssignableNotTypeSubstituteTypeVariablesBoundTypeVariableMa}, hash: 88B943014273228EEA00693D6F5D4A0B
    @Test()
    void typesSatisfyVariablesWhenTypeInstanceOfGenericArrayTypeAndIsAssignableNotTypeSubstituteTypeVariablesBoundTypeVariableMa() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : true  #  inside isAssignable method
         * (!isAssignable(type, substituteTypeVariables(bound, typeVariableMap), typeVariableMap)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            boolean result = TypeUtils.typesSatisfyVariables(anyMap);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenClassUtilsIsAssignableTypeToClassAndIsAssignableTypeSubstituteTypeVariablesBoundTypeVariableMa}, hash: 52C872D58FC0A3265D26AEBFC14A3816
    @Test()
    void typesSatisfyVariablesWhenClassUtilsIsAssignableTypeToClassAndIsAssignableTypeSubstituteTypeVariablesBoundTypeVariableMa() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(type, substituteTypeVariables(bound, typeVariableMap), typeVariableMap)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            boolean result = TypeUtils.typesSatisfyVariables(anyMap);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenClassUtilsNotIsAssignableTypeToClassAndIsAssignableNotTypeSubstituteTypeVariablesBoundTypeVari}, hash: B8D1DD9CEFE97EFAA38BC75561D0AE52
    @Test()
    void typesSatisfyVariablesWhenClassUtilsNotIsAssignableTypeToClassAndIsAssignableNotTypeSubstituteTypeVariablesBoundTypeVari() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : false  #  inside isAssignable method
         * (!isAssignable(type, substituteTypeVariables(bound, typeVariableMap), typeVariableMap)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            boolean result = TypeUtils.typesSatisfyVariables(anyMap);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenRawTypeNotInstanceOfClass_ThrowsIllegalStateException}, hash: 37F9A755891334198ECCC4CB45D16151
    @Test()
    void typesSatisfyVariablesWhenRawTypeNotInstanceOfClass_ThrowsIllegalStateException() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : false  #  inside isAssignable method
         * (type instanceof ParameterizedType) : true  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : true  #  inside getRawType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
                TypeUtils.typesSatisfyVariables(anyMap);
            });
            IllegalStateException illegalStateException = new IllegalStateException("s1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenToParameterizedTypeEqualsTypeAndIsAssignableTypeSubstituteTypeVariablesBoundTypeVariableMapTyp}, hash: 5E7B4788C2A60B9B40A1064FF4BCB7BC
    @Test()
    void typesSatisfyVariablesWhenToParameterizedTypeEqualsTypeAndIsAssignableTypeSubstituteTypeVariablesBoundTypeVariableMapTyp() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(type, substituteTypeVariables(bound, typeVariableMap), typeVariableMap)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            boolean result = TypeUtils.typesSatisfyVariables(anyMap);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenToParameterizedTypeNotEqualsTypeAndRawTypeNotInstanceOfClass_ThrowsIllegalStateException}, hash: 4930DF8FDF1EA9371BB93F7E0C9E5405
    @Test()
    void typesSatisfyVariablesWhenToParameterizedTypeNotEqualsTypeAndRawTypeNotInstanceOfClass_ThrowsIllegalStateException() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : true  #  inside getRawType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
                TypeUtils.typesSatisfyVariables(anyMap);
            });
            IllegalStateException illegalStateException = new IllegalStateException("s1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenIsAssignableTypeToTypeAndIsAssignableTypeSubstituteTypeVariablesBoundTypeVariableMapTypeVariab}, hash: 38BDAA2C96A52D2CD45F37441A5D9084
    @Test()
    void typesSatisfyVariablesWhenIsAssignableTypeToTypeAndIsAssignableTypeSubstituteTypeVariablesBoundTypeVariableMapTypeVariab() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : false  #  inside isAssignable method
         * (type instanceof ParameterizedType) : true  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (isAssignable(type, (Class<?>) toType)) : true  #  inside isAssignable method
         * (!isAssignable(type, substituteTypeVariables(bound, typeVariableMap), typeVariableMap)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            boolean result = TypeUtils.typesSatisfyVariables(anyMap);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenTypeNotInstanceOfWildcardTypeThrowsIllegalStateException}, hash: 2D95C75C2E93729A2FFA1DD41BBCEE34
    @Test()
    void typesSatisfyVariablesWhenTypeNotInstanceOfWildcardTypeThrowsIllegalStateException() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : false  #  inside isAssignable method
         * (type instanceof ParameterizedType) : false  #  inside isAssignable method
         * (type instanceof TypeVariable<?>) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (type instanceof WildcardType) : false  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
                TypeUtils.typesSatisfyVariables(anyMap);
            });
            IllegalStateException illegalStateException = new IllegalStateException("s1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenIsAssignableBoundToClassAndIsAssignableTypeSubstituteTypeVariablesBoundTypeVariableMapTypeVari}, hash: E50A204016003510A3564F0C3347CA4F
    @Test()
    void typesSatisfyVariablesWhenIsAssignableBoundToClassAndIsAssignableTypeSubstituteTypeVariablesBoundTypeVariableMapTypeVari() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : false  #  inside isAssignable method
         * (type instanceof ParameterizedType) : false  #  inside isAssignable method
         * (type instanceof TypeVariable<?>) : true  #  inside isAssignable method
         * (for-each(((TypeVariable<?>) type).getBounds())) : true  #  inside isAssignable method
         * (isAssignable(bound, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(type, substituteTypeVariables(bound, typeVariableMap), typeVariableMap)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            boolean result = TypeUtils.typesSatisfyVariables(anyMap);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenIsAssignableNotBoundToClassAndIsAssignableNotTypeSubstituteTypeVariablesBoundTypeVariableMapTy}, hash: C055A2F0ADB0EE478218E773A07384DB
    @Test()
    void typesSatisfyVariablesWhenIsAssignableNotBoundToClassAndIsAssignableNotTypeSubstituteTypeVariablesBoundTypeVariableMapTy() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : false  #  inside isAssignable method
         * (type instanceof ParameterizedType) : false  #  inside isAssignable method
         * (type instanceof TypeVariable<?>) : true  #  inside isAssignable method
         * (for-each(((TypeVariable<?>) type).getBounds())) : true  #  inside isAssignable method
         * (isAssignable(bound, toClass)) : false  #  inside isAssignable method
         * (!isAssignable(type, substituteTypeVariables(bound, typeVariableMap), typeVariableMap)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            boolean result = TypeUtils.typesSatisfyVariables(anyMap);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenTypeInstanceOfWildcardTypeAndIsAssignableNotTypeSubstituteTypeVariablesBoundTypeVariableMapTyp}, hash: AAC565F7CFD4418D800EB646C37E1565
    @Test()
    void typesSatisfyVariablesWhenTypeInstanceOfWildcardTypeAndIsAssignableNotTypeSubstituteTypeVariablesBoundTypeVariableMapTyp() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : false  #  inside isAssignable method
         * (type instanceof ParameterizedType) : false  #  inside isAssignable method
         * (type instanceof TypeVariable<?>) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (type instanceof WildcardType) : true  #  inside isAssignable method
         * (!isAssignable(type, substituteTypeVariables(bound, typeVariableMap), typeVariableMap)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            boolean result = TypeUtils.typesSatisfyVariables(anyMap);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenIsAssignableTypeGetGenericComponentTypeToClassGetComponentTypeAndIsAssignableTypeSubstituteTyp}, hash: 073DD39BCC297B1E756E8B538A06470A
    @Test()
    void typesSatisfyVariablesWhenIsAssignableTypeGetGenericComponentTypeToClassGetComponentTypeAndIsAssignableTypeSubstituteTyp() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : false  #  inside isAssignable method
         * (type instanceof ParameterizedType) : false  #  inside isAssignable method
         * (type instanceof TypeVariable<?>) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : true  #  inside isAssignable method
         * (toClass.equals(Object.class)) : false  #  inside isAssignable method
         * (toClass.isArray()) : true  #  inside isAssignable method
         * (isAssignable(((GenericArrayType) type).getGenericComponentType(), toClass.getComponentType())) : true  #  inside isAssignable method
         * (!isAssignable(type, substituteTypeVariables(bound, typeVariableMap), typeVariableMap)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            boolean result = TypeUtils.typesSatisfyVariables(anyMap);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenIsAssignableNotTypeGetGenericComponentTypeToClassGetComponentTypeAndIsAssignableNotTypeSubstit}, hash: F1B4775591926D19964CF7FAC747C210
    @Test()
    void typesSatisfyVariablesWhenIsAssignableNotTypeGetGenericComponentTypeToClassGetComponentTypeAndIsAssignableNotTypeSubstit() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : false  #  inside isAssignable method
         * (type instanceof ParameterizedType) : false  #  inside isAssignable method
         * (type instanceof TypeVariable<?>) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : true  #  inside isAssignable method
         * (toClass.equals(Object.class)) : false  #  inside isAssignable method
         * (toClass.isArray()) : true  #  inside isAssignable method
         * (isAssignable(((GenericArrayType) type).getGenericComponentType(), toClass.getComponentType())) : false  #  inside isAssignable method
         * (!isAssignable(type, substituteTypeVariables(bound, typeVariableMap), typeVariableMap)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            boolean result = TypeUtils.typesSatisfyVariables(anyMap);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenFromTypeVarAssignsIsNullAndIsAssignableNotTypeSubstituteTypeVariablesBoundTypeVariableMapTypeV}, hash: F07773BCC72B8B27298EE2962AF296C9
    @Test()
    void typesSatisfyVariablesWhenFromTypeVarAssignsIsNullAndIsAssignableNotTypeSubstituteTypeVariablesBoundTypeVariableMapTypeV() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : true  #  inside getTypeArguments method
         * (fromTypeVarAssigns == null) : true  #  inside isAssignable method
         * (!isAssignable(type, substituteTypeVariables(bound, typeVariableMap), typeVariableMap)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            boolean result = TypeUtils.typesSatisfyVariables(anyMap);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenFromTypeVarAssignsIsEmptyAndIsAssignableTypeSubstituteTypeVariablesBoundTypeVariableMapTypeVar}, hash: 684B7DDF36E87AC9EEE8CCC23FABCD65
    @Test()
    void typesSatisfyVariablesWhenFromTypeVarAssignsIsEmptyAndIsAssignableTypeSubstituteTypeVariablesBoundTypeVariableMapTypeVar() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : true  #  inside getTypeArguments method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : true  #  inside isAssignable method
         * (!isAssignable(type, substituteTypeVariables(bound, typeVariableMap), typeVariableMap)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            boolean result = TypeUtils.typesSatisfyVariables(anyMap);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenFromTypeVarAssignsIsEmptyAndIsAssignableTypeSubstituteTypeVariablesBoundTypeVariableMapTypeVar2}, hash: F655ED0AA9E69DDC31B64C5E7701F7E4
    @Test()
    void typesSatisfyVariablesWhenFromTypeVarAssignsIsEmptyAndIsAssignableTypeSubstituteTypeVariablesBoundTypeVariableMapTypeVar2() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : true  #  inside getTypeArguments method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : true  #  inside isAssignable method
         * (!isAssignable(type, substituteTypeVariables(bound, typeVariableMap), typeVariableMap)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            boolean result = TypeUtils.typesSatisfyVariables(anyMap);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenMidTypeInstanceOfParameterizedTypeThrowsIllegalStateException}, hash: 254BDC228524654A641290E8EBA623D6
    @Test()
    void typesSatisfyVariablesWhenMidTypeInstanceOfParameterizedTypeThrowsIllegalStateException() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
                TypeUtils.typesSatisfyVariables(anyMap);
            });
            IllegalStateException illegalStateException = new IllegalStateException("s1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenFromTypeVarAssignsNotIsEmptyThrowsIllegalStateException}, hash: DA0A6612C9101B4C2110FE09DF17B0E8
    @Test()
    void typesSatisfyVariablesWhenFromTypeVarAssignsNotIsEmptyThrowsIllegalStateException() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
                TypeUtils.typesSatisfyVariables(anyMap);
            });
            IllegalStateException illegalStateException = new IllegalStateException("s1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenIsAssignableNotClsToClassThrowsNullPointerException}, hash: 4AB1EA9FABF5906AD891B54DA6BDA792
    @Test()
    void typesSatisfyVariablesWhenIsAssignableNotClsToClassThrowsNullPointerException() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            final NullPointerException result = assertThrows(NullPointerException.class, () -> {
                TypeUtils.typesSatisfyVariables(anyMap);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenOwnerTypeInstanceOfParameterizedTypeThrowsIllegalStateException}, hash: 18C58954DA9FD066AFA9F7561DCF2E3F
    @Test()
    void typesSatisfyVariablesWhenOwnerTypeInstanceOfParameterizedTypeThrowsIllegalStateException() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
                TypeUtils.typesSatisfyVariables(anyMap);
            });
            IllegalStateException illegalStateException = new IllegalStateException("s1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenOwnerTypeInstanceOfParameterizedTypeThrowsNullPointerException}, hash: 674DF1B8971296357F01B061B3E52682
    @Test()
    void typesSatisfyVariablesWhenOwnerTypeInstanceOfParameterizedTypeThrowsNullPointerException() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : true  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            final NullPointerException result = assertThrows(NullPointerException.class, () -> {
                TypeUtils.typesSatisfyVariables(anyMap);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenToClassNotEqualsClsThrowsIllegalStateException}, hash: B06EB4EA1B700DB7B4BB112185824498
    @Test()
    void typesSatisfyVariablesWhenToClassNotEqualsClsThrowsIllegalStateException() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : false  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
                TypeUtils.typesSatisfyVariables(anyMap);
            });
            IllegalStateException illegalStateException = new IllegalStateException("s1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenFromTypeArgInstanceOfClassAndIsAssignableTypeSubstituteTypeVariablesBoundTypeVariableMapTypeVa}, hash: 90BAA4FE89F59A9226DD7A9B16F91A6B
    @Test()
    void typesSatisfyVariablesWhenFromTypeArgInstanceOfClassAndIsAssignableTypeSubstituteTypeVariablesBoundTypeVariableMapTypeVa() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : false  #  inside getTypeArguments method
         * (i < typeParams.length) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : true  #  inside getTypeArguments method
         * (for-each(toTypeVarAssigns.keySet())) : true  #  inside isAssignable method
         * (!(result instanceof TypeVariable<?>)) : true  #  inside unrollVariableAssignments method
         * (toTypeArg == null) : true  #  inside isAssignable method
         * (fromTypeArg instanceof Class) : true  #  inside isAssignable method
         * (!isAssignable(type, substituteTypeVariables(bound, typeVariableMap), typeVariableMap)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            boolean result = TypeUtils.typesSatisfyVariables(anyMap);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenFromTypeArgIsNullAndIsAssignableTypeSubstituteTypeVariablesBoundTypeVariableMapTypeVariableMap3}, hash: DFB69844B17939D888F4EE6453B37755
    @Test()
    void typesSatisfyVariablesWhenFromTypeArgIsNullAndIsAssignableTypeSubstituteTypeVariablesBoundTypeVariableMapTypeVariableMap3() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : false  #  inside getTypeArguments method
         * (i < typeParams.length) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : true  #  inside getTypeArguments method
         * (for-each(toTypeVarAssigns.keySet())) : true  #  inside isAssignable method
         * (!(result instanceof TypeVariable<?>)) : false  #  inside unrollVariableAssignments method
         * (result.equals(typeVariable)) : false  #  inside unrollVariableAssignments method
         * (toTypeArg == null) : false  #  inside isAssignable method
         * (fromTypeArg != null) : false  #  inside isAssignable method
         * (!isAssignable(type, substituteTypeVariables(bound, typeVariableMap), typeVariableMap)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            boolean result = TypeUtils.typesSatisfyVariables(anyMap);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenToTypeArgInstanceOfWildcardTypeNotAndIsAssignableFromTypeArgToTypeArgTypeVarAssignsAndIsAssign}, hash: F190477A03BF061441ABA3F497851EEB
    @Test()
    void typesSatisfyVariablesWhenToTypeArgInstanceOfWildcardTypeNotAndIsAssignableFromTypeArgToTypeArgTypeVarAssignsAndIsAssign() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : false  #  inside getTypeArguments method
         * (i < typeParams.length) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : true  #  inside getTypeArguments method
         * (for-each(toTypeVarAssigns.keySet())) : true  #  inside isAssignable method
         * (!(result instanceof TypeVariable<?>)) : false  #  inside unrollVariableAssignments method
         * (result.equals(typeVariable)) : true  #  inside unrollVariableAssignments method
         * (toTypeArg == null) : false  #  inside isAssignable method
         * (fromTypeArg != null) : true  #  inside isAssignable method
         * (toTypeArg != null) : true  #  inside isAssignable method
         * (!toTypeArg.equals(fromTypeArg)) : true  #  inside isAssignable method
         * (!(toTypeArg instanceof WildcardType && isAssignable(fromTypeArg, toTypeArg, typeVarAssigns))) : true  #  inside isAssignable method
         * (!isAssignable(type, substituteTypeVariables(bound, typeVariableMap), typeVariableMap)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            boolean result = TypeUtils.typesSatisfyVariables(anyMap);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenToTypeArgInstanceOfWildcardTypeNotAndIsAssignableFromTypeArgToTypeArgTypeVarAssignsAndIsAssign2}, hash: 2DCBFC3DDFE0E48BA16B224C55EB0F12
    @Test()
    void typesSatisfyVariablesWhenToTypeArgInstanceOfWildcardTypeNotAndIsAssignableFromTypeArgToTypeArgTypeVarAssignsAndIsAssign2() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : false  #  inside getTypeArguments method
         * (i < typeParams.length) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : true  #  inside getTypeArguments method
         * (for-each(toTypeVarAssigns.keySet())) : true  #  inside isAssignable method
         * (!(result instanceof TypeVariable<?>)) : false  #  inside unrollVariableAssignments method
         * (result.equals(typeVariable)) : true  #  inside unrollVariableAssignments method
         * (toTypeArg == null) : false  #  inside isAssignable method
         * (fromTypeArg != null) : true  #  inside isAssignable method
         * (toTypeArg != null) : true  #  inside isAssignable method
         * (!toTypeArg.equals(fromTypeArg)) : true  #  inside isAssignable method
         * (!(toTypeArg instanceof WildcardType && isAssignable(fromTypeArg, toTypeArg, typeVarAssigns))) : true  #  inside isAssignable method
         * (!isAssignable(type, substituteTypeVariables(bound, typeVariableMap), typeVariableMap)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            boolean result = TypeUtils.typesSatisfyVariables(anyMap);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${typesSatisfyVariablesWhenFromTypeArgIsNotNullAndToTypeArgIsNullAndIsAssignableTypeSubstituteTypeVariablesBoundTypeVaria}, hash: 155C5B83FE2A5AC0937C667C5E9CC343
    @Test()
    void typesSatisfyVariablesWhenFromTypeArgIsNotNullAndToTypeArgIsNullAndIsAssignableTypeSubstituteTypeVariablesBoundTypeVaria() {
        /* Branches:
         * (for-each(typeVariableMap.entrySet())) : true
         * (for-each(getImplicitBounds(typeVar))) : true
         * (type instanceof TypeVariable<?>) : true  #  inside substituteTypeVariables method
         * (typeVarAssigns != null) : true  #  inside substituteTypeVariables method
         * (replacementType == null) : false  #  inside substituteTypeVariables method
         * (toType == null) : false  #  inside isAssignable method
         * (toType instanceof Class<?>) : false  #  inside isAssignable method
         * (toType instanceof ParameterizedType) : true  #  inside isAssignable method
         * (type == null) : false  #  inside isAssignable method
         * (toParameterizedType == null) : false  #  inside isAssignable method
         * (type instanceof GenericArrayType) : false  #  inside isAssignable method
         * (toParameterizedType.equals(type)) : false  #  inside isAssignable method
         * (!(rawType instanceof Class<?>)) : false  #  inside getRawType method
         * (type instanceof Class<?>) : true  #  inside getTypeArguments method
         * (type == null) : false  #  inside isAssignable method
         * (toClass == null) : false  #  inside isAssignable method
         * (toClass.equals(type)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (cls.isPrimitive()) : true  #  inside getTypeArguments method
         * (toClass.isPrimitive()) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : true  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (superClass.isInterface()) : true  #  inside getClosestParentType method
         * (for-each(interfaceTypes)) : true  #  inside getClosestParentType method
         * (midType instanceof ParameterizedType) : true  #  inside getClosestParentType method
         * (toClass == null) : false  #  inside isAssignable method
         * (!toClass.isPrimitive()) : true  #  inside isAssignable method
         * (isAssignable(midClass, superClass)) : true  #  inside getClosestParentType method
         * (isAssignable(genericInterface, (Type) midClass)) : true  #  inside getClosestParentType method
         * (genericInterface != null) : true  #  inside getClosestParentType method
         * (fromTypeVarAssigns == null) : false  #  inside isAssignable method
         * (fromTypeVarAssigns.isEmpty()) : false  #  inside isAssignable method
         * (type instanceof Class<?>) : true  #  inside isAssignable method
         * (ClassUtils.isAssignable((Class<?>) type, toClass)) : true  #  inside isAssignable method
         * (!isAssignable(cls, toClass)) : false  #  inside getTypeArguments method
         * (ownerType instanceof ParameterizedType) : false  #  inside getTypeArguments method
         * (subtypeVarAssigns == null) : false  #  inside getTypeArguments method
         * (i < typeParams.length) : false  #  inside getTypeArguments method
         * (toClass.equals(cls)) : false  #  inside getTypeArguments method
         * (type instanceof ParameterizedType) : true  #  inside getTypeArguments method
         * (for-each(toTypeVarAssigns.keySet())) : true  #  inside isAssignable method
         * (!(result instanceof TypeVariable<?>)) : true  #  inside unrollVariableAssignments method
         * (toTypeArg == null) : true  #  inside isAssignable method
         * (fromTypeArg instanceof Class) : false  #  inside isAssignable method
         * (fromTypeArg != null) : true  #  inside isAssignable method
         * (toTypeArg != null) : false  #  inside isAssignable method
         * (!isAssignable(type, substituteTypeVariables(bound, typeVariableMap), typeVariableMap)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.getImplicitBounds(typeVariableMock)).thenReturn(typeArray);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            //Act Statement(s)
            boolean result = TypeUtils.typesSatisfyVariables(anyMap);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                typeUtils.verify(() -> TypeUtils.getImplicitBounds(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${unrollVariablesWhenTypeInstanceOfTypeVariable_}, hash: B0F02E6A5B964EAFAC7DF72E89A94B91
    @Test()
    void unrollVariablesWhenTypeInstanceOfTypeVariable_() {
        /* Branches:
         * (typeArguments == null) : true
         * (containsTypeVariables(type)) : true
         * (type instanceof TypeVariable<?>) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TypeVariable typeVariableMock = mock(TypeVariable.class);
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            typeUtils.when(() -> TypeUtils.containsTypeVariables(typeVariableMock)).thenReturn(false);
            Map<TypeVariable<?>, Type> map = null;
            //Act Statement(s)
            Type result = TypeUtils.unrollVariables(map, typeVariableMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                typeUtils.verify(() -> TypeUtils.containsTypeVariables(typeVariableMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${unrollVariablesWhenTypeNotInstanceOfWildcardType}, hash: A06B5E4BFCC01FCACF868C423842B5E5
    @Test()
    void unrollVariablesWhenTypeNotInstanceOfWildcardType() {
        /* Branches:
         * (typeArguments == null) : true
         * (containsTypeVariables(type)) : true
         * (type instanceof TypeVariable<?>) : false
         * (type instanceof ParameterizedType) : false
         * (type instanceof WildcardType) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            typeUtils.when(() -> TypeUtils.containsTypeVariables(typeMock)).thenReturn(true);
            Map<TypeVariable<?>, Type> map = null;
            //Act Statement(s)
            Type result = TypeUtils.unrollVariables(map, typeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(typeMock));
                typeUtils.verify(() -> TypeUtils.containsTypeVariables(typeMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${unrollVariablesWhenUnrolledIsNotNull}, hash: 272F70B449DCC02D97D3DCFFC125A42C
    @Test()
    void unrollVariablesWhenUnrolledIsNotNull() {
        /* Branches:
         * (typeArguments == null) : true
         * (containsTypeVariables(type)) : true
         * (type instanceof TypeVariable<?>) : false
         * (type instanceof ParameterizedType) : true
         * (p.getOwnerType() == null) : true
         * (i < args.length) : true
         * (unrolled != null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            typeUtils.when(() -> TypeUtils.containsTypeVariables(parameterizedTypeMock)).thenReturn(false);
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.parameterizeWithOwner(typeMock, Object.class, typeArray)).thenReturn(parameterizedTypeMock2);
            Map<TypeVariable<?>, Type> map = null;
            //Act Statement(s)
            Type result = TypeUtils.unrollVariables(map, parameterizedTypeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(parameterizedTypeMock2));
                typeUtils.verify(() -> TypeUtils.containsTypeVariables(parameterizedTypeMock), atLeast(1));
                typeUtils.verify(() -> TypeUtils.parameterizeWithOwner(typeMock, Object.class, typeArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${unrollVariablesWhenILessThanArgsLengthAndUnrolledIsNotNull}, hash: F035A3CE5907DD8FCB978E219BE2B7D9
    @Test()
    void unrollVariablesWhenILessThanArgsLengthAndUnrolledIsNotNull() {
        /* Branches:
         * (typeArguments == null) : true
         * (containsTypeVariables(type)) : true
         * (type instanceof TypeVariable<?>) : false
         * (type instanceof ParameterizedType) : true
         * (p.getOwnerType() == null) : false
         * (i < args.length) : true
         * (unrolled != null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            typeUtils.when(() -> TypeUtils.containsTypeVariables(parameterizedTypeMock)).thenReturn(false);
            Map<TypeVariable<?>, Type> anyMap = new HashMap<>();
            typeUtils.when(() -> TypeUtils.getTypeArguments(parameterizedTypeMock)).thenReturn(anyMap);
            Type[] typeArray = new Type[] {};
            typeUtils.when(() -> TypeUtils.parameterizeWithOwner(typeMock, Object.class, typeArray)).thenReturn(parameterizedTypeMock2);
            Map<TypeVariable<?>, Type> map = null;
            //Act Statement(s)
            Type result = TypeUtils.unrollVariables(map, parameterizedTypeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(parameterizedTypeMock2));
                typeUtils.verify(() -> TypeUtils.containsTypeVariables(parameterizedTypeMock), atLeast(1));
                typeUtils.verify(() -> TypeUtils.getTypeArguments(parameterizedTypeMock), atLeast(1));
                typeUtils.verify(() -> TypeUtils.parameterizeWithOwner(typeMock, Object.class, typeArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${unrollVariablesWhenUnrolledIsNull}, hash: 491C78514577B11AB019A2222CFC7D3B
    @Test()
    void unrollVariablesWhenUnrolledIsNull() {
        /* Branches:
         * (typeArguments == null) : true
         * (containsTypeVariables(type)) : true
         * (type instanceof TypeVariable<?>) : false
         * (type instanceof ParameterizedType) : false
         * (type instanceof WildcardType) : true
         * (i < result.length) : true  #  inside unrollBounds method
         * (unrolled == null) : true  #  inside unrollBounds method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TypeUtils.WildcardTypeBuilder typeUtilsWildcardTypeBuilderMock = mock(TypeUtils.WildcardTypeBuilder.class);
        try (MockedStatic<TypeUtils> typeUtils = mockStatic(TypeUtils.class, CALLS_REAL_METHODS)) {
            typeUtils.when(() -> TypeUtils.containsTypeVariables(wildcardTypeMock)).thenReturn(false);
            typeUtils.when(() -> TypeUtils.wildcardType()).thenReturn(typeUtilsWildcardTypeBuilderMock);
            Map<TypeVariable<?>, Type> map = null;
            //Act Statement(s)
            Type result = TypeUtils.unrollVariables(map, wildcardTypeMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                typeUtils.verify(() -> TypeUtils.containsTypeVariables(wildcardTypeMock), atLeast(1));
                typeUtils.verify(() -> TypeUtils.wildcardType(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${wildcardTypeTest}, hash: B9B0E916A62F981A2F27F667C3BFB2F6
    @Test()
    void wildcardTypeTest() {
        
        //Act Statement(s)
        TypeUtils.WildcardTypeBuilder result = TypeUtils.wildcardType();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${wrapTest}, hash: AD534969B2482A1C0C6326ACE09D5F80
    @Test()
    void wrapTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Typed result = TypeUtils.wrap(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${wrap1Test}, hash: 33AE4FB648DBE783092A3EEF6CD76BDD
    @Test()
    void wrap1Test() {
        
        //Act Statement(s)
        Typed result = TypeUtils.wrap(typeMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }
}

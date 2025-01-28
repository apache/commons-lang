package org.apache.commons.lang3.reflect;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.List;
import org.mockito.stubbing.Answer;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import org.mockito.MockedStatic;
import java.util.ArrayList;
import static org.mockito.Mockito.doNothing;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doReturn;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.mockito.Mockito.mockStatic;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class FieldUtilsBaseRockGeneratedTest {

    private final Field fieldMock = mock(Field.class);

    //BaseRock generated method id: ${getAllFieldsTest}, hash: DF370375B7787D0CDD93EE08E83319CC
    @Test()
    void getAllFieldsTest() {
        //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            List<Field> fieldList = new ArrayList<>();
            fieldUtils.when(() -> FieldUtils.getAllFieldsList(Object.class)).thenReturn(fieldList);
            //Act Statement(s)
            Field[] result = FieldUtils.getAllFields(Object.class);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.length, equalTo(0));
                fieldUtils.verify(() -> FieldUtils.getAllFieldsList(Object.class), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getAllFieldsListWhenCurrentClassIsNotNull}, hash: 61819F8AFCEA2B99B42AC872C31DAC0A
    @Test()
    void getAllFieldsListWhenCurrentClassIsNotNull() {
        /* Branches:
         * (currentClass != null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        List<Field> result = FieldUtils.getAllFieldsList(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getDeclaredFieldTest}, hash: 65FCC7C782AE7661EA25781B0B0CA822
    @Test()
    void getDeclaredFieldTest() {
        //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            fieldUtils.when(() -> FieldUtils.getDeclaredField(Object.class, "fieldName1", false)).thenReturn(fieldMock);
            //Act Statement(s)
            Field result = FieldUtils.getDeclaredField(Object.class, "fieldName1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(fieldMock));
                fieldUtils.verify(() -> FieldUtils.getDeclaredField(Object.class, "fieldName1", false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getDeclaredField1WhenNotForceAccess}, hash: C20395067CF77C5A372288B67AC42560
    @Test()
    void getDeclaredField1WhenNotForceAccess() {
        /* Branches:
         * (!MemberUtils.isAccessible(field)) : true
         * (!forceAccess) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Field result = FieldUtils.getDeclaredField(Object.class, "A", false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getDeclaredField1WhenForceAccess}, hash: 36ADFCB040468C31155EF0A08B1BC117
    @Test()
    void getDeclaredField1WhenForceAccess() {
        /* Branches:
         * (!MemberUtils.isAccessible(field)) : true
         * (!forceAccess) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Field result = FieldUtils.getDeclaredField(Object.class, "A", true);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getDeclaredField1WhenNotForceAccessAndCaughtNoSuchFieldException}, hash: F6F82355A10B2418A8791033A2366905
    @Test()
    void getDeclaredField1WhenNotForceAccessAndCaughtNoSuchFieldException() {
        /* Branches:
         * (!MemberUtils.isAccessible(field)) : true
         * (!forceAccess) : true
         * (catch-exception (NoSuchFieldException)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Field result = FieldUtils.getDeclaredField(Object.class, "A", false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getFieldTest}, hash: FC2CC53EB9F3C38572FFA339B1BFB6BF
    @Test()
    void getFieldTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            fieldUtils.when(() -> FieldUtils.getField(Object.class, "fieldName1", false)).thenReturn(fieldMock);
            //Act Statement(s)
            Field result = FieldUtils.getField(Object.class, "fieldName1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                fieldUtils.verify(() -> FieldUtils.getField(Object.class, "fieldName1", false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getField1WhenForceAccess}, hash: 9AA2D820DD94D9EA4D220FD50EC8E426
    @Test()
    void getField1WhenForceAccess() {
        /* Branches:
         * (acls != null) : true
         * (!MemberUtils.isPublic(field)) : true
         * (!forceAccess) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Field result = FieldUtils.getField(Object.class, "A", true);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getField1WhenNotForceAccessAndClassUtilsGetAllInterfacesClsIsEmpty}, hash: B3751232CA57460026CD568BA7D95EB8
    @Test()
    void getField1WhenNotForceAccessAndClassUtilsGetAllInterfacesClsIsEmpty() {
        /* Branches:
         * (acls != null) : true
         * (!MemberUtils.isPublic(field)) : true
         * (!forceAccess) : true
         * (for-each(ClassUtils.getAllInterfaces(cls))) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Field result = FieldUtils.getField(Object.class, "", false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getField1WhenNotForceAccessAndClassUtilsGetAllInterfacesClsIsNotEmptyAndMatchIsNullThrowsIllegalArgumentException}, hash: 8158AE59E9F04B386241B0798C34C456
    @Test()
    void getField1WhenNotForceAccessAndClassUtilsGetAllInterfacesClsIsNotEmptyAndMatchIsNullThrowsIllegalArgumentException() {
        /* Branches:
         * (acls != null) : true
         * (!MemberUtils.isPublic(field)) : true
         * (!forceAccess) : true
         * (for-each(ClassUtils.getAllInterfaces(cls))) : true
         * (match == null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            FieldUtils.getField(Object.class, "A", false);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getField1WhenCaughtNoSuchFieldException}, hash: 016988470815734DEC919D24D2281B76
    @Test()
    void getField1WhenCaughtNoSuchFieldException() {
        /* Branches:
         * (acls != null) : true
         * (!MemberUtils.isPublic(field)) : true
         * (!forceAccess) : true
         * (for-each(ClassUtils.getAllInterfaces(cls))) : false
         * (catch-exception (NoSuchFieldException)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        Field result = FieldUtils.getField(Object.class, "", false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getField1WhenClassUtilsGetAllInterfacesClsIsNotEmptyAndMatchIsNull2ThrowsIllegalArgumentException}, hash: 747186B8485AE24801FB8DFE1F955D8D
    @Test()
    void getField1WhenClassUtilsGetAllInterfacesClsIsNotEmptyAndMatchIsNull2ThrowsIllegalArgumentException() {
        /* Branches:
         * (acls != null) : true
         * (!MemberUtils.isPublic(field)) : true
         * (!forceAccess) : true
         * (catch-exception (NoSuchFieldException)) : true
         * (for-each(ClassUtils.getAllInterfaces(cls))) : true
         * (match == null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            FieldUtils.getField(Object.class, "A", false);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getFieldsListWithAnnotationWhenFieldGetAnnotationAnnotationClsIsNotNull}, hash: 196867F649DDA0B69B2AB7CC1C06F74C
    @Test()
    void getFieldsListWithAnnotationWhenFieldGetAnnotationAnnotationClsIsNotNull() {
        /* Branches:
         * (field.getAnnotation(annotationCls) != null) : true  #  inside lambda$getFieldsListWithAnnotation$0 method
         */
         //Arrange Statement(s)
        Annotation annotationMock = mock(Annotation.class);
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            List<Field> fieldList = new ArrayList<>();
            fieldList.add(fieldMock);
            fieldUtils.when(() -> FieldUtils.getAllFieldsList(Object.class)).thenReturn(fieldList);
            doReturn(annotationMock).when(fieldMock).getAnnotation(Annotation.class);
            //Act Statement(s)
            List<Field> result = FieldUtils.getFieldsListWithAnnotation(Object.class, Annotation.class);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.size(), equalTo(1));
                assertThat(result.get(0), is(instanceOf(Field.class)));
                fieldUtils.verify(() -> FieldUtils.getAllFieldsList(Object.class), atLeast(1));
                verify(fieldMock, atLeast(1)).getAnnotation(Annotation.class);
            });
        }
    }

    //BaseRock generated method id: ${getFieldsListWithAnnotationWhenFieldGetAnnotationAnnotationClsIsNull}, hash: 79DFEF41641829F2683E7D6D97F4C506
    @Test()
    void getFieldsListWithAnnotationWhenFieldGetAnnotationAnnotationClsIsNull() {
        /* Branches:
         * (field.getAnnotation(annotationCls) != null) : false  #  inside lambda$getFieldsListWithAnnotation$0 method
         */
         //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            List<Field> fieldList = new ArrayList<>();
            fieldList.add(fieldMock);
            fieldUtils.when(() -> FieldUtils.getAllFieldsList(Object.class)).thenReturn(fieldList);
            doReturn(null).when(fieldMock).getAnnotation(Annotation.class);
            //Act Statement(s)
            List<Field> result = FieldUtils.getFieldsListWithAnnotation(Object.class, Annotation.class);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.size(), equalTo(0));
                fieldUtils.verify(() -> FieldUtils.getAllFieldsList(Object.class), atLeast(1));
                verify(fieldMock, atLeast(1)).getAnnotation(Annotation.class);
            });
        }
    }

    //BaseRock generated method id: ${getFieldsWithAnnotationTest}, hash: AB4F3BB80EE285C2081345A6373CB3DD
    @Test()
    void getFieldsWithAnnotationTest() {
        //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            List<Field> fieldList = new ArrayList<>();
            fieldUtils.when(() -> FieldUtils.getFieldsListWithAnnotation(Object.class, Annotation.class)).thenReturn(fieldList);
            //Act Statement(s)
            Field[] result = FieldUtils.getFieldsWithAnnotation(Object.class, Annotation.class);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.length, equalTo(0));
                fieldUtils.verify(() -> FieldUtils.getFieldsListWithAnnotation(Object.class, Annotation.class), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${readDeclaredFieldTest}, hash: C616F1D0A33EE603B1D091F368CC7A98
    @Test()
    void readDeclaredFieldTest() throws IllegalAccessException {
        //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            fieldUtils.when(() -> FieldUtils.readDeclaredField(object2, "fieldName1", false)).thenReturn(object);
            //Act Statement(s)
            Object result = FieldUtils.readDeclaredField(object2, "fieldName1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(object));
                fieldUtils.verify(() -> FieldUtils.readDeclaredField(object2, "fieldName1", false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${readDeclaredField1WhenFieldIsNotNullThrowsIllegalArgumentException}, hash: A66F571333D1A139E283E35BA877830B
    @Test()
    void readDeclaredField1WhenFieldIsNotNullThrowsIllegalArgumentException() throws IllegalAccessException {
        /* Branches:
         * (field != null) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            fieldUtils.when(() -> FieldUtils.getDeclaredField(Object.class, "fieldName1", false)).thenReturn(fieldMock);
            Object object = new Object();
            //Act Statement(s)
            final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
                FieldUtils.readDeclaredField(object, "fieldName1", false);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                fieldUtils.verify(() -> FieldUtils.getDeclaredField(Object.class, "fieldName1", false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${readDeclaredField1WhenFieldIsNullThrowsIllegalArgumentException}, hash: 7F4F2E194E9C3EC7CC15786E349EED0B
    @Test()
    void readDeclaredField1WhenFieldIsNullThrowsIllegalArgumentException() throws IllegalAccessException {
        /* Branches:
         * (field != null) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            fieldUtils.when(() -> FieldUtils.getDeclaredField(Object.class, "fieldName1", false)).thenReturn(null);
            Object object = new Object();
            //Act Statement(s)
            final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
                FieldUtils.readDeclaredField(object, "fieldName1", false);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                fieldUtils.verify(() -> FieldUtils.getDeclaredField(Object.class, "fieldName1", false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${readDeclaredStaticFieldTest}, hash: CD4D01110369FB2100285C9F18E20D3A
    @Test()
    void readDeclaredStaticFieldTest() throws IllegalAccessException {
        //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            fieldUtils.when(() -> FieldUtils.readDeclaredStaticField(Object.class, "fieldName1", false)).thenReturn(object);
            //Act Statement(s)
            Object result = FieldUtils.readDeclaredStaticField(Object.class, "fieldName1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(object));
                fieldUtils.verify(() -> FieldUtils.readDeclaredStaticField(Object.class, "fieldName1", false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${readDeclaredStaticField1Test}, hash: FC694B3B920E662D3742D8197F34543E
    @Test()
    void readDeclaredStaticField1Test() throws IllegalAccessException {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            fieldUtils.when(() -> FieldUtils.getDeclaredField(Object.class, "fieldName1", false)).thenReturn(fieldMock);
            Object object = new Object();
            fieldUtils.when(() -> FieldUtils.readStaticField(fieldMock, false)).thenReturn(object);
            //Act Statement(s)
            Object result = FieldUtils.readDeclaredStaticField(Object.class, "fieldName1", false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(object));
                fieldUtils.verify(() -> FieldUtils.getDeclaredField(Object.class, "fieldName1", false), atLeast(1));
                fieldUtils.verify(() -> FieldUtils.readStaticField(fieldMock, false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${readFieldTest}, hash: DEA9F7B41BA03ADD4E34AAF7BAD47AB2
    @Test()
    void readFieldTest() throws IllegalAccessException {
        //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            fieldUtils.when(() -> FieldUtils.readField(fieldMock, object2, false)).thenReturn(object);
            //Act Statement(s)
            Object result = FieldUtils.readField(fieldMock, object2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(object));
                fieldUtils.verify(() -> FieldUtils.readField(fieldMock, object2, false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${readField1WhenNotForceAccess}, hash: 74706E16294D1AA6534C505722866948
    @Test()
    void readField1WhenNotForceAccess() throws IllegalAccessException {
        /* Branches:
         * (forceAccess) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        
        //Act Statement(s)
        Object result = FieldUtils.readField(fieldMock, object, false);
        
        //Assert statement(s)
        //TODO: Please implement equals method in Object for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${readField1WhenFieldNotIsAccessible}, hash: C62DF6883B355075B8D009DB56A92B13
    @Test()
    void readField1WhenFieldNotIsAccessible() throws IllegalAccessException, IllegalArgumentException {
        /* Branches:
         * (forceAccess) : true
         * (!field.isAccessible()) : true
         */
         //Arrange Statement(s)
        doReturn(false).when(fieldMock).isAccessible();
        doNothing().when(fieldMock).setAccessible(true);
        Object object = new Object();
        Object object2 = new Object();
        doReturn(object).when(fieldMock).get(object2);
        
        //Act Statement(s)
        Object result = FieldUtils.readField(fieldMock, object2, true);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(object));
            verify(fieldMock).isAccessible();
            verify(fieldMock).setAccessible(true);
            verify(fieldMock).get(object2);
        });
    }

    //BaseRock generated method id: ${readField2Test}, hash: E2D881BDA65061B4E15AF970700BF59A
    @Test()
    void readField2Test() throws IllegalAccessException {
        //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            fieldUtils.when(() -> FieldUtils.readField(object2, "fieldName1", false)).thenReturn(object);
            //Act Statement(s)
            Object result = FieldUtils.readField(object2, "fieldName1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(object));
                fieldUtils.verify(() -> FieldUtils.readField(object2, "fieldName1", false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${readField3WhenFieldIsNotNullThrowsIllegalArgumentException}, hash: B4ADEA2512EB4FD6BC90638ACF4250EB
    @Test()
    void readField3WhenFieldIsNotNullThrowsIllegalArgumentException() throws IllegalAccessException {
        /* Branches:
         * (field != null) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            fieldUtils.when(() -> FieldUtils.getField(Object.class, "fieldName1", false)).thenReturn(fieldMock);
            Object object = new Object();
            //Act Statement(s)
            final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
                FieldUtils.readField(object, "fieldName1", false);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                fieldUtils.verify(() -> FieldUtils.getField(Object.class, "fieldName1", false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${readField3WhenFieldIsNullThrowsIllegalArgumentException}, hash: AD94EFC4FEFC0DC86B24FD561E235F8C
    @Test()
    void readField3WhenFieldIsNullThrowsIllegalArgumentException() throws IllegalAccessException {
        /* Branches:
         * (field != null) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            fieldUtils.when(() -> FieldUtils.getField(Object.class, "fieldName1", false)).thenReturn(null);
            Object object = new Object();
            //Act Statement(s)
            final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
                FieldUtils.readField(object, "fieldName1", false);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                fieldUtils.verify(() -> FieldUtils.getField(Object.class, "fieldName1", false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${readStaticFieldTest}, hash: 2E600A3A6BA34691B2125597E1F894B0
    @Test()
    void readStaticFieldTest() throws IllegalAccessException {
        //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            fieldUtils.when(() -> FieldUtils.readStaticField(Object.class, "fieldName1", false)).thenReturn(object);
            //Act Statement(s)
            Object result = FieldUtils.readStaticField(Object.class, "fieldName1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(object));
                fieldUtils.verify(() -> FieldUtils.readStaticField(Object.class, "fieldName1", false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${readStaticField1Test}, hash: 896A7BE649BCC04CEBE2C4BCEDC11ED1
    @Test()
    void readStaticField1Test() throws IllegalAccessException {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            fieldUtils.when(() -> FieldUtils.getField(Object.class, "fieldName1", false)).thenReturn(fieldMock);
            Object object = new Object();
            fieldUtils.when(() -> FieldUtils.readStaticField(fieldMock, false)).thenReturn(object);
            //Act Statement(s)
            Object result = FieldUtils.readStaticField(Object.class, "fieldName1", false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(object));
                fieldUtils.verify(() -> FieldUtils.getField(Object.class, "fieldName1", false), atLeast(1));
                fieldUtils.verify(() -> FieldUtils.readStaticField(fieldMock, false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${readStaticField2Test}, hash: CF8E834DAD093BE034526C1D941D4CEA
    @Test()
    void readStaticField2Test() throws IllegalAccessException {
        //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            fieldUtils.when(() -> FieldUtils.readStaticField(fieldMock, false)).thenReturn(object);
            //Act Statement(s)
            Object result = FieldUtils.readStaticField(fieldMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(object));
                fieldUtils.verify(() -> FieldUtils.readStaticField(fieldMock, false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${readStaticField3Test}, hash: 1D97DB11FD32800CBC27E66C0EBD3873
    @Test()
    void readStaticField3Test() throws IllegalAccessException {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            fieldUtils.when(() -> FieldUtils.readField(fieldMock, (Object) null, false)).thenReturn(object);
            //Act Statement(s)
            Object result = FieldUtils.readStaticField(fieldMock, false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(object));
                fieldUtils.verify(() -> FieldUtils.readField(fieldMock, (Object) null, false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeFinalModifierTest}, hash: A6CEEA04B4C14FDAD701F732C327D059
    @Test()
    void removeFinalModifierTest() {
        //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            fieldUtils.when(() -> FieldUtils.removeFinalModifier(fieldMock, true)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            FieldUtils.removeFinalModifier(fieldMock);
            //Assert statement(s)
            assertAll("result", () -> fieldUtils.verify(() -> FieldUtils.removeFinalModifier(fieldMock, true), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${removeFinalModifier1WhenDoForceAccess}, hash: 8F78724DA1900D0667D57BA9570E5EE4
    @Test()
    void removeFinalModifier1WhenDoForceAccess() {
        /* Branches:
         * (Modifier.isFinal(field.getModifiers())) : true
         * (forceAccess) : true
         * (!modifiersField.isAccessible()) : true
         * (doForceAccess) : true
         * (doForceAccess) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        FieldUtils.removeFinalModifier(fieldMock, false);
    }

    //BaseRock generated method id: ${removeFinalModifier1WhenNotDoForceAccess}, hash: 49B95B6554315EC1AFCD1A26C2C794A8
    @Test()
    void removeFinalModifier1WhenNotDoForceAccess() {
        /* Branches:
         * (Modifier.isFinal(field.getModifiers())) : true
         * (forceAccess) : true
         * (!modifiersField.isAccessible()) : false
         * (doForceAccess) : false
         * (doForceAccess) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        FieldUtils.removeFinalModifier(fieldMock, false);
    }

    //BaseRock generated method id: ${removeFinalModifier1WhenCaughtNoSuchFieldExceptionOrIllegalAccessExceptionAndSystemUThrowsUnsupportedOperationException}, hash: 3F4DE939E41736C02E18291422AF10B6
    @Test()
    void removeFinalModifier1WhenCaughtNoSuchFieldExceptionOrIllegalAccessExceptionAndSystemUThrowsUnsupportedOperationException() {
        /* Branches:
         * (Modifier.isFinal(field.getModifiers())) : true
         * (forceAccess) : true
         * (!modifiersField.isAccessible()) : true
         * (doForceAccess) : true
         * (doForceAccess) : true
         * (catch-exception (NoSuchFieldException | IllegalAccessException)) : true
         * (SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_12)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        ReflectiveOperationException reflectiveOperationException = new ReflectiveOperationException();
        UnsupportedOperationException unsupportedOperationException = new UnsupportedOperationException("In java 12+ final cannot be removed.", reflectiveOperationException);
        //Act Statement(s)
        final UnsupportedOperationException result = assertThrows(UnsupportedOperationException.class, () -> {
            FieldUtils.removeFinalModifier(fieldMock, false);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(unsupportedOperationException.getMessage()));
            assertThat(result.getCause(), is(instanceOf(reflectiveOperationException.getClass())));
        });
    }

    //BaseRock generated method id: ${writeDeclaredFieldTest}, hash: B43E2E78980A3FBF08BA39232DD43D14
    @Test()
    void writeDeclaredFieldTest() throws IllegalAccessException {
        //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            fieldUtils.when(() -> FieldUtils.writeDeclaredField(object, "fieldName1", object2, false)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            FieldUtils.writeDeclaredField(object, "fieldName1", object2);
            //Assert statement(s)
            assertAll("result", () -> fieldUtils.verify(() -> FieldUtils.writeDeclaredField(object, "fieldName1", object2, false), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${writeDeclaredField1WhenFieldIsNotNullThrowsIllegalArgumentException}, hash: CBAAE8BD376DDE46DE75E7A2EB41FA36
    @Test()
    void writeDeclaredField1WhenFieldIsNotNullThrowsIllegalArgumentException() throws IllegalAccessException {
        /* Branches:
         * (field != null) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            fieldUtils.when(() -> FieldUtils.getDeclaredField(Object.class, "fieldName1", false)).thenReturn(fieldMock);
            Object object = new Object();
            Object object2 = new Object();
            //Act Statement(s)
            final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
                FieldUtils.writeDeclaredField(object, "fieldName1", object2, false);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                fieldUtils.verify(() -> FieldUtils.getDeclaredField(Object.class, "fieldName1", false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${writeDeclaredField1WhenFieldIsNullThrowsIllegalArgumentException}, hash: F8E5DADF53EC82B1487A03CEA4301D78
    @Test()
    void writeDeclaredField1WhenFieldIsNullThrowsIllegalArgumentException() throws IllegalAccessException {
        /* Branches:
         * (field != null) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            fieldUtils.when(() -> FieldUtils.getDeclaredField(Object.class, "fieldName1", false)).thenReturn(null);
            Object object = new Object();
            Object object2 = new Object();
            //Act Statement(s)
            final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
                FieldUtils.writeDeclaredField(object, "fieldName1", object2, false);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                fieldUtils.verify(() -> FieldUtils.getDeclaredField(Object.class, "fieldName1", false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${writeDeclaredStaticFieldTest}, hash: A53D5FE491334067F342C444162E33E7
    @Test()
    void writeDeclaredStaticFieldTest() throws IllegalAccessException {
        //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            fieldUtils.when(() -> FieldUtils.writeDeclaredStaticField(Object.class, "fieldName1", object, false)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            FieldUtils.writeDeclaredStaticField(Object.class, "fieldName1", object);
            //Assert statement(s)
            assertAll("result", () -> fieldUtils.verify(() -> FieldUtils.writeDeclaredStaticField(Object.class, "fieldName1", object, false), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${writeDeclaredStaticField1Test}, hash: E84E11CCBB7172CF1381B6710F3F7298
    @Test()
    void writeDeclaredStaticField1Test() throws IllegalAccessException {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            fieldUtils.when(() -> FieldUtils.getDeclaredField(Object.class, "fieldName1", false)).thenReturn(fieldMock);
            Object object = new Object();
            fieldUtils.when(() -> FieldUtils.writeField(fieldMock, (Object) null, object, false)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            FieldUtils.writeDeclaredStaticField(Object.class, "fieldName1", object, false);
            //Assert statement(s)
            assertAll("result", () -> {
                fieldUtils.verify(() -> FieldUtils.getDeclaredField(Object.class, "fieldName1", false), atLeast(1));
                fieldUtils.verify(() -> FieldUtils.writeField(fieldMock, (Object) null, object, false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${writeFieldTest}, hash: CB47781504C5BF5DB3E381BEE4C4DF26
    @Test()
    void writeFieldTest() throws IllegalAccessException {
        //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            fieldUtils.when(() -> FieldUtils.writeField(fieldMock, object, object2, false)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            FieldUtils.writeField(fieldMock, object, object2);
            //Assert statement(s)
            assertAll("result", () -> fieldUtils.verify(() -> FieldUtils.writeField(fieldMock, object, object2, false), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${writeField1WhenFieldNotIsAccessible}, hash: A6BE050BED0A8714E9A5FB791A2A04AF
    @Test()
    void writeField1WhenFieldNotIsAccessible() throws IllegalAccessException, IllegalArgumentException {
        /* Branches:
         * (forceAccess) : true
         * (!field.isAccessible()) : true
         */
         //Arrange Statement(s)
        doReturn(false).when(fieldMock).isAccessible();
        doNothing().when(fieldMock).setAccessible(true);
        Object object = new Object();
        Object object2 = new Object();
        doNothing().when(fieldMock).set(object, object2);
        
        //Act Statement(s)
        FieldUtils.writeField(fieldMock, object, object2, true);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(fieldMock).isAccessible();
            verify(fieldMock).setAccessible(true);
            verify(fieldMock).set(object, object2);
        });
    }

    //BaseRock generated method id: ${writeField1WhenFieldIsAccessible}, hash: B4E36792F21A339655E0E70AEB9D6ECD
    @Test()
    void writeField1WhenFieldIsAccessible() throws IllegalAccessException {
        /* Branches:
         * (forceAccess) : true
         * (!field.isAccessible()) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        
        //Act Statement(s)
        FieldUtils.writeField(fieldMock, object, object2, true);
    }

    //BaseRock generated method id: ${writeField2Test}, hash: E8ACD3ECD7B7C35312DE2E117AD2CB5E
    @Test()
    void writeField2Test() throws IllegalAccessException {
        //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            fieldUtils.when(() -> FieldUtils.writeField(object, "fieldName1", object2, false)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            FieldUtils.writeField(object, "fieldName1", object2);
            //Assert statement(s)
            assertAll("result", () -> fieldUtils.verify(() -> FieldUtils.writeField(object, "fieldName1", object2, false), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${writeField3WhenFieldIsNotNullThrowsIllegalArgumentException}, hash: C6B825A01CAF8F952A5FCB9B7406391C
    @Test()
    void writeField3WhenFieldIsNotNullThrowsIllegalArgumentException() throws IllegalAccessException {
        /* Branches:
         * (field != null) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            fieldUtils.when(() -> FieldUtils.getField(Object.class, "fieldName1", false)).thenReturn(fieldMock);
            Object object = new Object();
            Object object2 = new Object();
            //Act Statement(s)
            final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
                FieldUtils.writeField(object, "fieldName1", object2, false);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                fieldUtils.verify(() -> FieldUtils.getField(Object.class, "fieldName1", false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${writeField3WhenFieldIsNullThrowsIllegalArgumentException}, hash: E6BCFF1D1EF03A467D2C09B7FF654AD8
    @Test()
    void writeField3WhenFieldIsNullThrowsIllegalArgumentException() throws IllegalAccessException {
        /* Branches:
         * (field != null) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            fieldUtils.when(() -> FieldUtils.getField(Object.class, "fieldName1", false)).thenReturn(null);
            Object object = new Object();
            Object object2 = new Object();
            //Act Statement(s)
            final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
                FieldUtils.writeField(object, "fieldName1", object2, false);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                fieldUtils.verify(() -> FieldUtils.getField(Object.class, "fieldName1", false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${writeStaticFieldTest}, hash: 381D74BB00DF6D42F6AB11466CCB86EA
    @Test()
    void writeStaticFieldTest() throws IllegalAccessException {
        //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            fieldUtils.when(() -> FieldUtils.writeStaticField(Object.class, "fieldName1", object, false)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            FieldUtils.writeStaticField(Object.class, "fieldName1", object);
            //Assert statement(s)
            assertAll("result", () -> fieldUtils.verify(() -> FieldUtils.writeStaticField(Object.class, "fieldName1", object, false), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${writeStaticField1Test}, hash: AEB817F33D9B5A61D9C6A4857FBC6144
    @Test()
    void writeStaticField1Test() throws IllegalAccessException {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            fieldUtils.when(() -> FieldUtils.getField(Object.class, "fieldName1", false)).thenReturn(fieldMock);
            Object object = new Object();
            fieldUtils.when(() -> FieldUtils.writeStaticField(fieldMock, object, false)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            FieldUtils.writeStaticField(Object.class, "fieldName1", object, false);
            //Assert statement(s)
            assertAll("result", () -> {
                fieldUtils.verify(() -> FieldUtils.getField(Object.class, "fieldName1", false), atLeast(1));
                fieldUtils.verify(() -> FieldUtils.writeStaticField(fieldMock, object, false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${writeStaticField2Test}, hash: B12F59EEC1C6D3350B8110F9A49AA81B
    @Test()
    void writeStaticField2Test() throws IllegalAccessException {
        //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            fieldUtils.when(() -> FieldUtils.writeStaticField(fieldMock, object, false)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            FieldUtils.writeStaticField(fieldMock, object);
            //Assert statement(s)
            assertAll("result", () -> fieldUtils.verify(() -> FieldUtils.writeStaticField(fieldMock, object, false), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${writeStaticField3Test}, hash: AC4080F4F37DD284FBC2C39391121C09
    @Test()
    void writeStaticField3Test() throws IllegalAccessException {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<FieldUtils> fieldUtils = mockStatic(FieldUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            fieldUtils.when(() -> FieldUtils.writeField(fieldMock, (Object) null, object, false)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            FieldUtils.writeStaticField(fieldMock, object, false);
            //Assert statement(s)
            assertAll("result", () -> fieldUtils.verify(() -> FieldUtils.writeField(fieldMock, (Object) null, object, false), atLeast(1)));
        }
    }
}

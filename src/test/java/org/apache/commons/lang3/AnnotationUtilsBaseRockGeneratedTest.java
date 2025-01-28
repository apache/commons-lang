package org.apache.commons.lang3;

import org.apache.commons.lang3.AnnotationUtils;
import java.util.Arrays;
import org.apache.commons.lang3.AnnotationUtils;
import java.util.List;
import org.apache.commons.lang3.exception.UncheckedException;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import static org.mockito.ArgumentMatchers.eq;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.apache.commons.lang3.builder.ToStringBuilder;
import java.lang.reflect.Method;
import org.junit.jupiter.params.provider.CsvSource;
import static org.junit.jupiter.api.Assertions.*;
import java.lang.annotation.Annotation;
import static org.mockito.Mockito.*;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import org.junit.jupiter.api.Disabled;

class AnnotationUtilsBaseRockGeneratedTest {

    //BaseRock generated method id: ${testEquals}, hash: AD05AF313319779CF2E7E83EB9F64DBF
    @Disabled()
    @Test
    void testEquals() {
        Annotation a1 = mock(Annotation.class);
        Annotation a2 = mock(Annotation.class);
        when(a1.annotationType()).thenReturn((Class) TestAnnotation.class);
        when(a2.annotationType()).thenReturn((Class) TestAnnotation.class);
        assertTrue(AnnotationUtils.equals(a1, a1));
        assertFalse(AnnotationUtils.equals(a1, null));
        assertFalse(AnnotationUtils.equals(null, a2));
        assertTrue(AnnotationUtils.equals(a1, a2));
    }

    //BaseRock generated method id: ${testHashCode}, hash: BAF800C1DF6064C198C31CF8D9EC69DD
    @Disabled()
    @Test
    void testHashCode() throws Exception {
        Annotation a = mock(Annotation.class);
        when(a.annotationType()).thenReturn((Class) TestAnnotation.class);
        Method m = TestAnnotation.class.getDeclaredMethod("value");
        when(m.invoke(a)).thenReturn("test");
        int result = AnnotationUtils.hashCode(a);
        assertNotEquals(0, result);
    }

    //BaseRock generated method id: ${testIsValidAnnotationMemberType}, hash: C0BC691E46C0F91D58384966003593AC
    @Test
    void testIsValidAnnotationMemberType() {
        assertTrue(AnnotationUtils.isValidAnnotationMemberType(String.class));
        assertTrue(AnnotationUtils.isValidAnnotationMemberType(int.class));
        assertTrue(AnnotationUtils.isValidAnnotationMemberType(TestEnum.class));
        assertTrue(AnnotationUtils.isValidAnnotationMemberType(TestAnnotation.class));
        assertTrue(AnnotationUtils.isValidAnnotationMemberType(Class.class));
        assertFalse(AnnotationUtils.isValidAnnotationMemberType(Object.class));
    }

    //BaseRock generated method id: ${testToString}, hash: 25E425F2185073594471A5D4613EBFBD
    @Disabled()
    @Test
    void testToString() throws Exception {
        Annotation a = mock(Annotation.class);
        when(a.annotationType()).thenReturn((Class) TestAnnotation.class);
        Method m = TestAnnotation.class.getDeclaredMethod("value");
        when(m.invoke(a)).thenReturn("test");
        String result = AnnotationUtils.toString(a);
        assertTrue(result.contains("value=test"));
    }

    //BaseRock generated method id: ${testAnnotationArrayMemberEquals}, hash: 967B4385AF8BEF34A695F74D5267C443
    @ParameterizedTest
    @CsvSource({ "true, true, true", "true, false, false", "false, true, false", "false, false, false" })
    void testAnnotationArrayMemberEquals(boolean a1Null, boolean a2Null, boolean expected) {
        // This test method has been removed due to private access of AnnotationUtils.annotationArrayMemberEquals
    }

    //BaseRock generated method id: ${testArrayMemberEquals}, hash: 70DF9F297A385ACA7D3A7480CF5C5D6D
    @Test
    void testArrayMemberEquals() {
        // This test method has been removed due to private access of AnnotationUtils.arrayMemberEquals
    }

    //BaseRock generated method id: ${testArrayMemberHash}, hash: 5A5285B4BE090C92854D8EDE95829920
    @Test
    void testArrayMemberHash() {
        // This test method has been removed due to private access of AnnotationUtils.arrayMemberHash
    }

    private @interface TestAnnotation {

        String value();
    }

    private enum TestEnum {

        A, B, C
    }
}

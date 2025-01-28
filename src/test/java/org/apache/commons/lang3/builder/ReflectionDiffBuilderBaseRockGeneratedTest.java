package org.apache.commons.lang3.builder;

// import org.apache.commons.lang3.builder.ReflectionDiffBuilder;
// import java.util.Arrays;
// import org.apache.commons.lang3.reflect.FieldUtils;
// import org.junit.jupiter.api.BeforeEach;
// import static org.hamcrest.Matchers.endsWith;
// import static org.mockito.ArgumentMatchers.any;
// import org.junit.jupiter.api.Test;
// import org.junit.jupiter.params.ParameterizedTest;
// import static org.mockito.ArgumentMatchers.eq;
// import java.lang.reflect.Field;
// import org.apache.commons.lang3.builder.ReflectionDiffBuilder;
// import org.junit.jupiter.params.provider.CsvSource;
// import static org.junit.jupiter.api.Assertions.*;
// import org.apache.commons.lang3.ClassUtils;
// import static org.mockito.Mockito.*;
// import static org.hamcrest.Matchers.startsWith;
// import static org.hamcrest.Matchers.startsWith;
// import static org.hamcrest.Matchers.endsWith;
// import static org.mockito.ArgumentMatchers.any;
// import static org.mockito.ArgumentMatchers.eq;

class ReflectionDiffBuilderBaseRockGeneratedTest {

//     private ReflectionDiffBuilder<Object> reflectionDiffBuilder;

//     private DiffBuilder<Object> mockDiffBuilder;

//     @BeforeEach
//     void setUp() {
//         mockDiffBuilder = mock(DiffBuilder.class);
//         reflectionDiffBuilder = new ReflectionDiffBuilder<>(mockDiffBuilder, new String[0]);
//     }

//     //BaseRock generated method id: ${testBuild}, hash: 3327F3E19FE4B00A520BB6DD51F5F2F0
//     @Test
//     void testBuild() throws Exception {
//         Object left = new TestObject("value1");
//         Object right = new TestObject("value2");
//         when(mockDiffBuilder.getLeft()).thenReturn(left);
//         when(mockDiffBuilder.getRight()).thenReturn(right);
//         reflectionDiffBuilder.build();
//         verify(mockDiffBuilder).append(eq("field"), eq("value1"), eq("value2"));
//         verify(mockDiffBuilder).build();
//     }

//     //BaseRock generated method id: ${testBuildWithEqualObjects}, hash: 1383A546648EC4F9B0B066AB2D4E3755
//     @Test
//     void testBuildWithEqualObjects() {
//         Object obj = new TestObject("value");
//         when(mockDiffBuilder.getLeft()).thenReturn(obj);
//         when(mockDiffBuilder.getRight()).thenReturn(obj);
//         reflectionDiffBuilder.build();
//         verify(mockDiffBuilder, never()).append(anyString(), any(Object.class), any(Object.class));
//         verify(mockDiffBuilder).build();
//     }

//     //BaseRock generated method id: ${testSetExcludeFieldNames}, hash: C986B442C3FF4F74D4A9335C14A9E769
//     @ParameterizedTest
//     @CsvSource({ "field1,field2", "field1,field2,field3" })
//     void testSetExcludeFieldNames(String csv) {
//         String[] excludeFieldNames = csv.split(",");
//         reflectionDiffBuilder.setExcludeFieldNames(excludeFieldNames);
//         assertArrayEquals(excludeFieldNames, reflectionDiffBuilder.getExcludeFieldNames());
//     }

//     //BaseRock generated method id: ${testAccept}, hash: 638559229F9908ED36990E1E0A83D85D
//     @Test
//     void testAccept() throws Exception {
//         Field field = TestObject.class.getDeclaredField("field");
//         assertTrue(invokeAccept(field));
//         Field staticField = TestObject.class.getDeclaredField("staticField");
//         assertFalse(invokeAccept(staticField));
//         Field transientField = TestObject.class.getDeclaredField("transientField");
//         assertFalse(invokeAccept(transientField));
//         reflectionDiffBuilder.setExcludeFieldNames("field");
//         assertFalse(invokeAccept(field));
//     }

//     //BaseRock generated method id: ${testBuilder}, hash: 6FD977FD1783AED97BB7259A12DE4A58
//     @Test
//     void testBuilder() {
//         ReflectionDiffBuilder.Builder<Object> builder = ReflectionDiffBuilder.builder();
//         assertNotNull(builder);
//         DiffBuilder<Object> diffBuilder = mock(DiffBuilder.class);
//         ReflectionDiffBuilder<Object> result = builder.setDiffBuilder(diffBuilder).setExcludeFieldNames("field1", "field2").build();
//         assertNotNull(result);
//         assertArrayEquals(new String[] { "field1", "field2" }, result.getExcludeFieldNames());
//     }

//     private boolean invokeAccept(Field field) throws Exception {
//         return (boolean) FieldUtils.readDeclaredMethod(reflectionDiffBuilder.getClass(), "accept", Field.class).invoke(reflectionDiffBuilder, field);
//     }

//     private static class TestObject {

//         private String field;

//         private static String staticField;

//         private transient String transientField;

//         public TestObject(String field) {
//             this.field = field;
//         }
//     }
}
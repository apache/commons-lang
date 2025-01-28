package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.concurrent.ThreadLocalRandom;
import org.mockito.stubbing.Answer;
import java.util.BitSet;
import java.util.AbstractMap;
import java.util.function.IntFunction;
import java.util.function.Function;
import java.util.Map;
import java.util.HashMap;
import java.util.function.Supplier;
import org.mockito.MockedStatic;
import java.util.Random;
import java.util.Comparator;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doReturn;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.mockito.Mockito.mockStatic;
import org.junit.jupiter.api.Disabled;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class ArrayUtilsBaseRockGeneratedTest {

    private final Object objectMock = mock(Object.class, "object");

    private final Random randomMock = mock(Random.class);

    //BaseRock generated method id: ${addWhenArrayIsNotNull}, hash: 5AB2BB65F2ABA1A46AD96AA43A8AE2FE
    @Disabled()
    @Test()
    void addWhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true  #  inside copyArrayGrow1 method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        //Act Statement(s)
        boolean[] result = ArrayUtils.add(booleanArray, false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${addWhenArrayIsNull}, hash: F354D7C0134270A42E2B523ED28035CE
    @Test()
    void addWhenArrayIsNull() {
        /* Branches:
         * (array != null) : false  #  inside copyArrayGrow1 method
         */
        //Arrange Statement(s)
        boolean[] _boolean = null;
        //Act Statement(s)
        boolean[] result = ArrayUtils.add(_boolean, false);
        boolean[] booleanResultArray = new boolean[] { false };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${add1WhenIndexNotEquals0ThrowsIndexOutOfBoundsException}, hash: A9AB134EE70C6790292C55C83B7A4D56
    @Test()
    void add1WhenIndexNotEquals0ThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array == null) : true  #  inside add method
         * (index != 0) : true  #  inside add method
         */
        //Arrange Statement(s)
        boolean[] _boolean = null;
        IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("Index: 8, Length: 0");
        //Act Statement(s)
        final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
            ArrayUtils.add(_boolean, 8, false);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${add1WhenIndexEquals0}, hash: 6142F01E762CEF32630C0DF54247DBBF
    @Test()
    void add1WhenIndexEquals0() {
        /* Branches:
         * (array == null) : true  #  inside add method
         * (index != 0) : false  #  inside add method
         */
        //Arrange Statement(s)
        boolean[] _boolean = null;
        //Act Statement(s)
        boolean[] result = ArrayUtils.add(_boolean, 0, false);
        boolean[] booleanResultArray = new boolean[] { false };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${add1WhenIndexLessThan0ThrowsIndexOutOfBoundsException}, hash: B417671FA6CC7C6C48E94770DD6D51CE
    @Disabled()
    @Test()
    void add1WhenIndexLessThan0ThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array == null) : false  #  inside add method
         * (index > length) : false  #  inside add method
         * (index < 0) : true  #  inside add method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("s1");
        //Act Statement(s)
        final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
            ArrayUtils.add(booleanArray, 0, false);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${add1WhenIndexLessThanLength}, hash: 91C761EF3303544460A01FB5DF9FEFF7
    @Disabled()
    @Test()
    void add1WhenIndexLessThanLength() {
        /* Branches:
         * (array == null) : false  #  inside add method
         * (index > length) : false  #  inside add method
         * (index < 0) : false  #  inside add method
         * (index < length) : true  #  inside add method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.arraycopy(eq(booleanArray), eq(0), eq(0), eq(0), (Supplier) any())).thenReturn(null);
            //Act Statement(s)
            boolean[] result = ArrayUtils.add(booleanArray, 0, false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.length, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.arraycopy(eq(booleanArray), eq(0), eq(0), eq(0), (Supplier) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${add2WhenArrayIsNotNull}, hash: 8A7E3AF27EC75DCAD88754A0A5AAFF2D
    @Disabled()
    @Test()
    void add2WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true  #  inside copyArrayGrow1 method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        //Act Statement(s)
        byte[] result = ArrayUtils.add(byteArray, (byte) 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${add2WhenArrayIsNull}, hash: ECB8925772AD35B569B9B0486F1A73C2
    @Test()
    void add2WhenArrayIsNull() {
        /* Branches:
         * (array != null) : false  #  inside copyArrayGrow1 method
         */
        //Arrange Statement(s)
        byte[] _byte = null;
        //Act Statement(s)
        byte[] result = ArrayUtils.add(_byte, (byte) 0);
        byte[] byteResultArray = new byte[] { (byte) 0 };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(byteResultArray)));
    }

    //BaseRock generated method id: ${add3WhenIndexNotEquals0ThrowsIndexOutOfBoundsException}, hash: 733588DC8775C4903A9AE1E791277A37
    @Test()
    void add3WhenIndexNotEquals0ThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array == null) : true  #  inside add method
         * (index != 0) : true  #  inside add method
         */
        //Arrange Statement(s)
        byte[] _byte = null;
        IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("Index: 2, Length: 0");
        //Act Statement(s)
        final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
            ArrayUtils.add(_byte, 2, (byte) 2);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${add3WhenIndexEquals0}, hash: 97552C07C2348DC9AF4590947AA9152B
    @Test()
    void add3WhenIndexEquals0() {
        /* Branches:
         * (array == null) : true  #  inside add method
         * (index != 0) : false  #  inside add method
         */
        //Arrange Statement(s)
        byte[] _byte = null;
        //Act Statement(s)
        byte[] result = ArrayUtils.add(_byte, 0, (byte) 0);
        byte[] byteResultArray = new byte[] { (byte) 0 };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(byteResultArray)));
    }

    //BaseRock generated method id: ${add3WhenIndexLessThan0ThrowsIndexOutOfBoundsException}, hash: 3523FB560846369EE7D2950F22C90ECB
    @Disabled()
    @Test()
    void add3WhenIndexLessThan0ThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array == null) : false  #  inside add method
         * (index > length) : false  #  inside add method
         * (index < 0) : true  #  inside add method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("s1");
        //Act Statement(s)
        final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
            ArrayUtils.add(byteArray, 0, (byte) 0);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${add3WhenIndexLessThanLength}, hash: FA25B18DEDFB6B3538FD8690969B9E5A
    @Disabled()
    @Test()
    void add3WhenIndexLessThanLength() {
        /* Branches:
         * (array == null) : false  #  inside add method
         * (index > length) : false  #  inside add method
         * (index < 0) : false  #  inside add method
         * (index < length) : true  #  inside add method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.arraycopy(eq(byteArray), eq(0), eq(0), eq(0), (Supplier) any())).thenReturn(null);
            //Act Statement(s)
            byte[] result = ArrayUtils.add(byteArray, 0, (byte) 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.length, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.arraycopy(eq(byteArray), eq(0), eq(0), eq(0), (Supplier) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${add4WhenArrayIsNotNull}, hash: B718545B4F1AC623384C1B2E4A1379EA
    @Disabled()
    @Test()
    void add4WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true  #  inside copyArrayGrow1 method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        //Act Statement(s)
        char[] result = ArrayUtils.add(charArray, 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${add4WhenArrayIsNull}, hash: DA63973AD03D8CD9B2E07B26D28F59D8
    @Test()
    void add4WhenArrayIsNull() {
        /* Branches:
         * (array != null) : false  #  inside copyArrayGrow1 method
         */
        //Arrange Statement(s)
        char[] _char = null;
        //Act Statement(s)
        char[] result = ArrayUtils.add(_char, 'A');
        char[] charResultArray = new char[] { 'A' };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(charResultArray)));
    }

    //BaseRock generated method id: ${add5WhenIndexNotEquals0ThrowsIndexOutOfBoundsException}, hash: 47C3CB66AB7AC89C536861A236BC09D6
    @Test()
    void add5WhenIndexNotEquals0ThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array == null) : true  #  inside add method
         * (index != 0) : true  #  inside add method
         */
        //Arrange Statement(s)
        char[] _char = null;
        IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("Index: 2, Length: 0");
        //Act Statement(s)
        final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
            ArrayUtils.add(_char, 2, 'A');
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${add5WhenIndexEquals0}, hash: B1D6D71F04940CF4B5B9819ED1C875DC
    @Disabled()
    @Test()
    void add5WhenIndexEquals0() {
        /* Branches:
         * (array == null) : true  #  inside add method
         * (index != 0) : false  #  inside add method
         */
        //Arrange Statement(s)
        char[] _char = null;
        //Act Statement(s)
        char[] result = ArrayUtils.add(_char, 0, 'A');
        char[] charResultArray = new char[] { '\u0000' };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(charResultArray)));
    }

    //BaseRock generated method id: ${add5WhenIndexLessThan0ThrowsIndexOutOfBoundsException}, hash: 0D4EA8A28E3623549E907AB7E58517C6
    @Disabled()
    @Test()
    void add5WhenIndexLessThan0ThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array == null) : false  #  inside add method
         * (index > length) : false  #  inside add method
         * (index < 0) : true  #  inside add method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("s1");
        //Act Statement(s)
        final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
            ArrayUtils.add(charArray, 0, 'A');
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${add5WhenIndexLessThanLength}, hash: 08E1234B286CF45400A660AB4068BAF7
    @Disabled()
    @Test()
    void add5WhenIndexLessThanLength() {
        /* Branches:
         * (array == null) : false  #  inside add method
         * (index > length) : false  #  inside add method
         * (index < 0) : false  #  inside add method
         * (index < length) : true  #  inside add method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.arraycopy(eq(charArray), eq(0), eq(0), eq(0), (Supplier) any())).thenReturn(null);
            //Act Statement(s)
            char[] result = ArrayUtils.add(charArray, 0, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.length, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.arraycopy(eq(charArray), eq(0), eq(0), eq(0), (Supplier) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${add6WhenArrayIsNotNull}, hash: 4E3447547670AA6A53697D26D45C47B6
    @Disabled()
    @Test()
    void add6WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true  #  inside copyArrayGrow1 method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        double[] doubleArray = new double[] {};
        //Act Statement(s)
        double[] result = ArrayUtils.add(doubleArray, Double.parseDouble("0.0"));
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${add6WhenArrayIsNull}, hash: 63C8E1B4F65660894D70C8DFF8A749F1
    @Test()
    void add6WhenArrayIsNull() {
        /* Branches:
         * (array != null) : false  #  inside copyArrayGrow1 method
         */
        //Arrange Statement(s)
        double[] _double = null;
        //Act Statement(s)
        double[] result = ArrayUtils.add(_double, Double.parseDouble("0.0"));
        double[] doubleResultArray = new double[] { Double.parseDouble("0.0") };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(doubleResultArray)));
    }

    //BaseRock generated method id: ${add7WhenIndexNotEquals0ThrowsIndexOutOfBoundsException}, hash: 6C174BED62DCACC04116D7BDA2E07277
    @Test()
    void add7WhenIndexNotEquals0ThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array == null) : true  #  inside add method
         * (index != 0) : true  #  inside add method
         */
        //Arrange Statement(s)
        double[] _double = null;
        IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("Index: 2, Length: 0");
        //Act Statement(s)
        final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
            ArrayUtils.add(_double, 2, Double.parseDouble("1.0"));
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${add7WhenIndexEquals0}, hash: 5AB3A4AD796FF3E4E4DCF0C54953D47C
    @Disabled()
    @Test()
    void add7WhenIndexEquals0() {
        /* Branches:
         * (array == null) : true  #  inside add method
         * (index != 0) : false  #  inside add method
         */
        //Arrange Statement(s)
        double[] _double = null;
        //Act Statement(s)
        double[] result = ArrayUtils.add(_double, 0, Double.parseDouble("1.0"));
        double[] doubleResultArray = new double[] { Double.parseDouble("0.0") };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(doubleResultArray)));
    }

    //BaseRock generated method id: ${add7WhenIndexLessThan0ThrowsIndexOutOfBoundsException}, hash: 1F377BF54AA53EA362AA74D0A61E9B23
    @Disabled()
    @Test()
    void add7WhenIndexLessThan0ThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array == null) : false  #  inside add method
         * (index > length) : false  #  inside add method
         * (index < 0) : true  #  inside add method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        double[] doubleArray = new double[] {};
        IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("s1");
        //Act Statement(s)
        final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
            ArrayUtils.add(doubleArray, 0, Double.parseDouble("0.0"));
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${add7WhenIndexLessThanLength}, hash: 78BD81D0817C5DF028BB261E2CD1ECFB
    @Disabled()
    @Test()
    void add7WhenIndexLessThanLength() {
        /* Branches:
         * (array == null) : false  #  inside add method
         * (index > length) : false  #  inside add method
         * (index < 0) : false  #  inside add method
         * (index < length) : true  #  inside add method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.arraycopy(eq(doubleArray), eq(0), eq(0), eq(0), (Supplier) any())).thenReturn(null);
            //Act Statement(s)
            double[] result = ArrayUtils.add(doubleArray, 0, Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.length, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.arraycopy(eq(doubleArray), eq(0), eq(0), eq(0), (Supplier) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${add8WhenArrayIsNotNull}, hash: 53B083B5D5D86F01417507C902EA747C
    @Disabled()
    @Test()
    void add8WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true  #  inside copyArrayGrow1 method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        float[] floatArray = new float[] {};
        //Act Statement(s)
        float[] result = ArrayUtils.add(floatArray, Float.parseFloat("0.0"));
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${add8WhenArrayIsNull}, hash: B5DFAB57C5FA8A297590660437613864
    @Test()
    void add8WhenArrayIsNull() {
        /* Branches:
         * (array != null) : false  #  inside copyArrayGrow1 method
         */
        //Arrange Statement(s)
        float[] _float = null;
        //Act Statement(s)
        float[] result = ArrayUtils.add(_float, Float.parseFloat("0.0"));
        float[] floatResultArray = new float[] { Float.parseFloat("0.0") };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(floatResultArray)));
    }

    //BaseRock generated method id: ${add9WhenIndexNotEquals0ThrowsIndexOutOfBoundsException}, hash: B3F3497FFA50D79718BEDC014347B846
    @Test()
    void add9WhenIndexNotEquals0ThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array == null) : true  #  inside add method
         * (index != 0) : true  #  inside add method
         */
        //Arrange Statement(s)
        float[] _float = null;
        IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("Index: 2, Length: 0");
        //Act Statement(s)
        final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
            ArrayUtils.add(_float, 2, Float.parseFloat("1.0"));
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${add9WhenIndexEquals0}, hash: 869AADDFEDBBC26A26738EDD6F8DAD6D
    @Disabled()
    @Test()
    void add9WhenIndexEquals0() {
        /* Branches:
         * (array == null) : true  #  inside add method
         * (index != 0) : false  #  inside add method
         */
        //Arrange Statement(s)
        float[] _float = null;
        //Act Statement(s)
        float[] result = ArrayUtils.add(_float, 0, Float.parseFloat("1.0"));
        float[] floatResultArray = new float[] { Float.parseFloat("0.0") };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(floatResultArray)));
    }

    //BaseRock generated method id: ${add9WhenIndexLessThan0ThrowsIndexOutOfBoundsException}, hash: D9AE0BA853C8679924D2180A969B4F82
    @Disabled()
    @Test()
    void add9WhenIndexLessThan0ThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array == null) : false  #  inside add method
         * (index > length) : false  #  inside add method
         * (index < 0) : true  #  inside add method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        float[] floatArray = new float[] {};
        IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("s1");
        //Act Statement(s)
        final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
            ArrayUtils.add(floatArray, 0, Float.parseFloat("0.0"));
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${add9WhenIndexLessThanLength}, hash: 751F47CF0D610C37D9183C072CA81991
    @Disabled()
    @Test()
    void add9WhenIndexLessThanLength() {
        /* Branches:
         * (array == null) : false  #  inside add method
         * (index > length) : false  #  inside add method
         * (index < 0) : false  #  inside add method
         * (index < length) : true  #  inside add method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.arraycopy(eq(floatArray), eq(0), eq(0), eq(0), (Supplier) any())).thenReturn(null);
            //Act Statement(s)
            float[] result = ArrayUtils.add(floatArray, 0, Float.parseFloat("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.length, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.arraycopy(eq(floatArray), eq(0), eq(0), eq(0), (Supplier) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${add10WhenArrayIsNotNull}, hash: 820226FBCF067B95B7C2FE571E141EEC
    @Disabled()
    @Test()
    void add10WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true  #  inside copyArrayGrow1 method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        int[] intArray = new int[] {};
        //Act Statement(s)
        int[] result = ArrayUtils.add(intArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${add10WhenArrayIsNull}, hash: AD96135FA212F4A02708DFEE2FE89918
    @Test()
    void add10WhenArrayIsNull() {
        /* Branches:
         * (array != null) : false  #  inside copyArrayGrow1 method
         */
        //Arrange Statement(s)
        int[] _int = null;
        //Act Statement(s)
        int[] result = ArrayUtils.add(_int, 0);
        int[] intResultArray = new int[] { 0 };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(intResultArray)));
    }

    //BaseRock generated method id: ${add11WhenIndexNotEquals0ThrowsIndexOutOfBoundsException}, hash: 5B095136137A56A125BB1813CB9A6850
    @Test()
    void add11WhenIndexNotEquals0ThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array == null) : true  #  inside add method
         * (index != 0) : true  #  inside add method
         */
        //Arrange Statement(s)
        int[] _int = null;
        IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("Index: 2, Length: 0");
        //Act Statement(s)
        final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
            ArrayUtils.add(_int, 2, 2);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${add11WhenIndexEquals0}, hash: B21E62B4A6986D88C8190233B25BA48E
    @Test()
    void add11WhenIndexEquals0() {
        /* Branches:
         * (array == null) : true  #  inside add method
         * (index != 0) : false  #  inside add method
         */
        //Arrange Statement(s)
        int[] _int = null;
        //Act Statement(s)
        int[] result = ArrayUtils.add(_int, 0, 0);
        int[] intResultArray = new int[] { 0 };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(intResultArray)));
    }

    //BaseRock generated method id: ${add11WhenIndexLessThan0ThrowsIndexOutOfBoundsException}, hash: 95080CFB3750D0833D57E47474D30D4A
    @Disabled()
    @Test()
    void add11WhenIndexLessThan0ThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array == null) : false  #  inside add method
         * (index > length) : false  #  inside add method
         * (index < 0) : true  #  inside add method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        int[] intArray = new int[] {};
        IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("s1");
        //Act Statement(s)
        final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
            ArrayUtils.add(intArray, 0, 0);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${add11WhenIndexLessThanLength}, hash: DD9D855C8BB61FF2875C8ECC8C19DE0E
    @Disabled()
    @Test()
    void add11WhenIndexLessThanLength() {
        /* Branches:
         * (array == null) : false  #  inside add method
         * (index > length) : false  #  inside add method
         * (index < 0) : false  #  inside add method
         * (index < length) : true  #  inside add method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.arraycopy(eq(intArray), eq(0), eq(0), eq(0), (Supplier) any())).thenReturn(null);
            //Act Statement(s)
            int[] result = ArrayUtils.add(intArray, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.length, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.arraycopy(eq(intArray), eq(0), eq(0), eq(0), (Supplier) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${add12WhenIndexNotEquals0ThrowsIndexOutOfBoundsException}, hash: A9FF07F8788F1BA9376110CF3E6E170F
    @Test()
    void add12WhenIndexNotEquals0ThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array == null) : true  #  inside add method
         * (index != 0) : true  #  inside add method
         */
        //Arrange Statement(s)
        long[] _long = null;
        IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("Index: 2, Length: 0");
        //Act Statement(s)
        final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
            ArrayUtils.add(_long, 2, 2L);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${add12WhenIndexEquals0}, hash: E66E9E1CD628A4FC306E33806CFD989A
    @Test()
    void add12WhenIndexEquals0() {
        /* Branches:
         * (array == null) : true  #  inside add method
         * (index != 0) : false  #  inside add method
         */
        //Arrange Statement(s)
        long[] _long = null;
        //Act Statement(s)
        long[] result = ArrayUtils.add(_long, 0, 0L);
        long[] longResultArray = new long[] { 0L };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(longResultArray)));
    }

    //BaseRock generated method id: ${add12WhenIndexLessThan0ThrowsIndexOutOfBoundsException}, hash: 97BF5FA3F66FEE8F07FAC81273FF560D
    @Disabled()
    @Test()
    void add12WhenIndexLessThan0ThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array == null) : false  #  inside add method
         * (index > length) : false  #  inside add method
         * (index < 0) : true  #  inside add method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        long[] longArray = new long[] {};
        IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("s1");
        //Act Statement(s)
        final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
            ArrayUtils.add(longArray, 0, 0L);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${add12WhenIndexLessThanLength}, hash: 5BB442D98EC3C23A0B9F1083ECBE4CF1
    @Disabled()
    @Test()
    void add12WhenIndexLessThanLength() {
        /* Branches:
         * (array == null) : false  #  inside add method
         * (index > length) : false  #  inside add method
         * (index < 0) : false  #  inside add method
         * (index < length) : true  #  inside add method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.arraycopy(eq(longArray), eq(0), eq(0), eq(0), (Supplier) any())).thenReturn(null);
            //Act Statement(s)
            long[] result = ArrayUtils.add(longArray, 0, 0L);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.length, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.arraycopy(eq(longArray), eq(0), eq(0), eq(0), (Supplier) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${add13WhenArrayIsNotNull}, hash: 310A0B3911F17B79C30A8E55A7ADECBE
    @Disabled()
    @Test()
    void add13WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true  #  inside copyArrayGrow1 method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        long[] longArray = new long[] {};
        //Act Statement(s)
        long[] result = ArrayUtils.add(longArray, 0L);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${add13WhenArrayIsNull}, hash: 00DF3D08422362BC2BF7824580990FBE
    @Test()
    void add13WhenArrayIsNull() {
        /* Branches:
         * (array != null) : false  #  inside copyArrayGrow1 method
         */
        //Arrange Statement(s)
        long[] _long = null;
        //Act Statement(s)
        long[] result = ArrayUtils.add(_long, 0L);
        long[] longResultArray = new long[] { 0L };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(longResultArray)));
    }

    //BaseRock generated method id: ${add15WhenIndexNotEquals0ThrowsIndexOutOfBoundsException}, hash: 5C07ED7D6D33BBD2E5A8439961136AAA
    @Test()
    void add15WhenIndexNotEquals0ThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array == null) : true  #  inside add method
         * (index != 0) : true  #  inside add method
         */
        //Arrange Statement(s)
        short[] _short = null;
        IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("Index: 2, Length: 0");
        //Act Statement(s)
        final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
            ArrayUtils.add(_short, 2, (short) 2);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${add15WhenIndexEquals0}, hash: 771CE2730EC69CA55FFAF7A391D104A6
    @Test()
    void add15WhenIndexEquals0() {
        /* Branches:
         * (array == null) : true  #  inside add method
         * (index != 0) : false  #  inside add method
         */
        //Arrange Statement(s)
        short[] _short = null;
        //Act Statement(s)
        short[] result = ArrayUtils.add(_short, 0, (short) 0);
        short[] shortResultArray = new short[] { (short) 0 };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(shortResultArray)));
    }

    //BaseRock generated method id: ${add15WhenIndexLessThan0ThrowsIndexOutOfBoundsException}, hash: A13D98CA19B2CFD8B3255F4490A14EE6
    @Disabled()
    @Test()
    void add15WhenIndexLessThan0ThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array == null) : false  #  inside add method
         * (index > length) : false  #  inside add method
         * (index < 0) : true  #  inside add method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] {};
        IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("s1");
        //Act Statement(s)
        final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
            ArrayUtils.add(shortArray, 0, (short) 0);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${add15WhenIndexLessThanLength}, hash: 048F98A827F1A4E81D76242C6BAEEA78
    @Disabled()
    @Test()
    void add15WhenIndexLessThanLength() {
        /* Branches:
         * (array == null) : false  #  inside add method
         * (index > length) : false  #  inside add method
         * (index < 0) : false  #  inside add method
         * (index < length) : true  #  inside add method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.arraycopy(eq(shortArray), eq(0), eq(0), eq(0), (Supplier) any())).thenReturn(null);
            //Act Statement(s)
            short[] result = ArrayUtils.add(shortArray, 0, (short) 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.length, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.arraycopy(eq(shortArray), eq(0), eq(0), eq(0), (Supplier) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${add16WhenArrayIsNotNull}, hash: 74FDC5EFC92B3ADC9CC3E472C732018B
    @Disabled()
    @Test()
    void add16WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true  #  inside copyArrayGrow1 method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] {};
        //Act Statement(s)
        short[] result = ArrayUtils.add(shortArray, (short) 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${add16WhenArrayIsNull}, hash: 069A5E88759BF6F5A8E9133A0FA77C1A
    @Test()
    void add16WhenArrayIsNull() {
        /* Branches:
         * (array != null) : false  #  inside copyArrayGrow1 method
         */
        //Arrange Statement(s)
        short[] _short = null;
        //Act Statement(s)
        short[] result = ArrayUtils.add(_short, (short) 0);
        short[] shortResultArray = new short[] { (short) 0 };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(shortResultArray)));
    }

    //BaseRock generated method id: ${add17WhenElementIsNullThrowsIllegalArgumentException}, hash: E92DBE8574EB0358332C67CCB0C08C87
    @Test()
    void add17WhenElementIsNullThrowsIllegalArgumentException() {
        /* Branches:
         * (array != null) : false
         * (element != null) : false
         */
        //Arrange Statement(s)
        Object[] object = null;
        Object object2 = null;
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Array and element cannot both be null");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            ArrayUtils.add(object, 0, object2);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${add17WhenIndexLessThan0ThrowsIndexOutOfBoundsException}, hash: E391D033FBA1D52810006C4FB4BA7666
    @Disabled()
    @Test()
    void add17WhenIndexLessThan0ThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array != null) : true
         * (array == null) : false  #  inside add method
         * (index > length) : false  #  inside add method
         * (index < 0) : true  #  inside add method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        Object object = new Object();
        IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("s1");
        //Act Statement(s)
        final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
            ArrayUtils.add(objectArray, 0, object);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${add17WhenIndexNotEquals0ThrowsIndexOutOfBoundsException}, hash: 95A85F448683CBAA6C679A707F8B8430
    @Test()
    void add17WhenIndexNotEquals0ThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array != null) : false
         * (element != null) : true
         * (array == null) : true  #  inside add method
         * (index != 0) : true  #  inside add method
         */
        //Arrange Statement(s)
        Object[] object = null;
        Object object2 = new Object();
        IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("Index: 2, Length: 0");
        //Act Statement(s)
        final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
            ArrayUtils.add(object, 2, object2);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
        });
    }

    //BaseRock generated method id: ${add17WhenIndexEquals0}, hash: DD1FA90C49293664C975A4999B451295
    @Test()
    void add17WhenIndexEquals0() {
        /* Branches:
         * (array != null) : false
         * (element != null) : true
         * (array == null) : true  #  inside add method
         * (index != 0) : false  #  inside add method
         */
        //Arrange Statement(s)
        Object[] object = null;
        Object object2 = new Object();
        //Act Statement(s)
        Object[] result = ArrayUtils.add(object, 0, object2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${add17WhenIndexLessThanLength}, hash: 42013D5889CFCE2CFCF05D5D9831EADA
    @Disabled()
    @Test()
    void add17WhenIndexLessThanLength() {
        /* Branches:
         * (array != null) : true
         * (array == null) : false  #  inside add method
         * (index > length) : false  #  inside add method
         * (index < 0) : false  #  inside add method
         * (index < length) : true  #  inside add method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.arraycopy(eq(objectArray), eq(0), eq(0), eq(0), (Supplier) any())).thenReturn(null);
            Object object = new Object();
            //Act Statement(s)
            Object[] result = ArrayUtils.add(objectArray, 0, object);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.length, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.arraycopy(eq(objectArray), eq(0), eq(0), eq(0), (Supplier) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${add18WhenArrayIsNotNull}, hash: A0D74BBEEC8BAC716FF4A58BA301991C
    @Test()
    void add18WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         * (array != null) : true  #  inside copyArrayGrow1 method
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        Object object = new Object();
        //Act Statement(s)
        Object[] result = ArrayUtils.add(objectArray, object);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${add18WhenElementIsNullThrowsIllegalArgumentException}, hash: 02BE55146E575FE64EE7E44FA408C41C
    @Test()
    void add18WhenElementIsNullThrowsIllegalArgumentException() {
        /* Branches:
         * (array != null) : false
         * (element != null) : false
         */
        //Arrange Statement(s)
        Object[] object = null;
        Object object2 = null;
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Arguments cannot both be null");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            ArrayUtils.add(object, object2);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${add18WhenArrayIsNull}, hash: 4A969082D86BF102AB757AA5BC117E9B
    @Test()
    void add18WhenArrayIsNull() {
        /* Branches:
         * (array != null) : false
         * (element != null) : true
         * (array != null) : false  #  inside copyArrayGrow1 method
         */
        //Arrange Statement(s)
        Object[] object = null;
        Object object2 = new Object();
        //Act Statement(s)
        Object[] result = ArrayUtils.add(object, object2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${addAllWhenArray1IsNull}, hash: BD8F18AF4571E41BC8E25F339DC6AE4B
    @Test()
    void addAllWhenArray1IsNull() {
        /* Branches:
         * (array1 == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            boolean[] booleanArray2 = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.clone(booleanArray2)).thenReturn(booleanArray);
            boolean[] _boolean = null;
            //Act Statement(s)
            boolean[] result = ArrayUtils.addAll(_boolean, booleanArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(booleanArray));
                arrayUtils.verify(() -> ArrayUtils.clone(booleanArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addAllWhenArray2IsNull}, hash: 2D0F898F37CAD6494C83319CAF8DAC50
    @Test()
    void addAllWhenArray2IsNull() {
        /* Branches:
         * (array1 == null) : false
         * (array2 == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            boolean[] booleanArray2 = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.clone(booleanArray2)).thenReturn(booleanArray);
            boolean[] _boolean = null;
            //Act Statement(s)
            boolean[] result = ArrayUtils.addAll(booleanArray2, _boolean);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(booleanArray));
                arrayUtils.verify(() -> ArrayUtils.clone(booleanArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addAllWhenArray2IsNotNull}, hash: EBA98C7B3CBCD2891B7628406FC243E5
    @Test()
    void addAllWhenArray2IsNotNull() {
        /* Branches:
         * (array1 == null) : false
         * (array2 == null) : false
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        boolean[] booleanArray2 = new boolean[] {};
        //Act Statement(s)
        boolean[] result = ArrayUtils.addAll(booleanArray, booleanArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${addAll1WhenArray1IsNull}, hash: 60EB63A473FC1A292DE16FAE16436121
    @Test()
    void addAll1WhenArray1IsNull() {
        /* Branches:
         * (array1 == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            byte[] byteArray2 = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.clone(byteArray2)).thenReturn(byteArray);
            byte[] _byte = null;
            //Act Statement(s)
            byte[] result = ArrayUtils.addAll(_byte, byteArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(byteArray));
                arrayUtils.verify(() -> ArrayUtils.clone(byteArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addAll1WhenArray2IsNull}, hash: B1AADA4E4CD0DDBA85CE9F706FE13A72
    @Test()
    void addAll1WhenArray2IsNull() {
        /* Branches:
         * (array1 == null) : false
         * (array2 == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            byte[] byteArray2 = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.clone(byteArray2)).thenReturn(byteArray);
            byte[] _byte = null;
            //Act Statement(s)
            byte[] result = ArrayUtils.addAll(byteArray2, _byte);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(byteArray));
                arrayUtils.verify(() -> ArrayUtils.clone(byteArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addAll1WhenArray2IsNotNull}, hash: 11C6F0FAE5B06A8C9C9DACEB85AA04BE
    @Test()
    void addAll1WhenArray2IsNotNull() {
        /* Branches:
         * (array1 == null) : false
         * (array2 == null) : false
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        byte[] byteArray2 = new byte[] {};
        //Act Statement(s)
        byte[] result = ArrayUtils.addAll(byteArray, byteArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${addAll2WhenArray1IsNull}, hash: 9AB71785718F9FC2B64472F9DD8396D9
    @Test()
    void addAll2WhenArray1IsNull() {
        /* Branches:
         * (array1 == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            char[] charArray2 = new char[] {};
            arrayUtils.when(() -> ArrayUtils.clone(charArray2)).thenReturn(charArray);
            char[] _char = null;
            //Act Statement(s)
            char[] result = ArrayUtils.addAll(_char, charArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(charArray));
                arrayUtils.verify(() -> ArrayUtils.clone(charArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addAll2WhenArray2IsNull}, hash: DF69745443BE2C2CE9E99178C51F3760
    @Test()
    void addAll2WhenArray2IsNull() {
        /* Branches:
         * (array1 == null) : false
         * (array2 == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            char[] charArray2 = new char[] {};
            arrayUtils.when(() -> ArrayUtils.clone(charArray2)).thenReturn(charArray);
            char[] _char = null;
            //Act Statement(s)
            char[] result = ArrayUtils.addAll(charArray2, _char);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(charArray));
                arrayUtils.verify(() -> ArrayUtils.clone(charArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addAll2WhenArray2IsNotNull}, hash: 427BFC358F019D01B04C6666D3B80642
    @Test()
    void addAll2WhenArray2IsNotNull() {
        /* Branches:
         * (array1 == null) : false
         * (array2 == null) : false
         */
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        char[] charArray2 = new char[] {};
        //Act Statement(s)
        char[] result = ArrayUtils.addAll(charArray, charArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${addAll3WhenArray1IsNull}, hash: CD34A79A45D487AE505641A16EDD508F
    @Test()
    void addAll3WhenArray1IsNull() {
        /* Branches:
         * (array1 == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            double[] doubleArray2 = new double[] {};
            arrayUtils.when(() -> ArrayUtils.clone(doubleArray2)).thenReturn(doubleArray);
            double[] _double = null;
            //Act Statement(s)
            double[] result = ArrayUtils.addAll(_double, doubleArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(doubleArray));
                arrayUtils.verify(() -> ArrayUtils.clone(doubleArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addAll3WhenArray2IsNull}, hash: AE9131F11065FDB120E28C5E51D168FE
    @Test()
    void addAll3WhenArray2IsNull() {
        /* Branches:
         * (array1 == null) : false
         * (array2 == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            double[] doubleArray2 = new double[] {};
            arrayUtils.when(() -> ArrayUtils.clone(doubleArray2)).thenReturn(doubleArray);
            double[] _double = null;
            //Act Statement(s)
            double[] result = ArrayUtils.addAll(doubleArray2, _double);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(doubleArray));
                arrayUtils.verify(() -> ArrayUtils.clone(doubleArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addAll3WhenArray2IsNotNull}, hash: D52394F8D001EFADEA1231C28FB13861
    @Test()
    void addAll3WhenArray2IsNotNull() {
        /* Branches:
         * (array1 == null) : false
         * (array2 == null) : false
         */
        //Arrange Statement(s)
        double[] doubleArray = new double[] {};
        double[] doubleArray2 = new double[] {};
        //Act Statement(s)
        double[] result = ArrayUtils.addAll(doubleArray, doubleArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${addAll4WhenArray1IsNull}, hash: 2D51BF220F2FB8C1EBB4DEEAE257D577
    @Test()
    void addAll4WhenArray1IsNull() {
        /* Branches:
         * (array1 == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            float[] floatArray2 = new float[] {};
            arrayUtils.when(() -> ArrayUtils.clone(floatArray2)).thenReturn(floatArray);
            float[] _float = null;
            //Act Statement(s)
            float[] result = ArrayUtils.addAll(_float, floatArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(floatArray));
                arrayUtils.verify(() -> ArrayUtils.clone(floatArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addAll4WhenArray2IsNull}, hash: 33D44EAD6B4F8644D71E72B0E5F41C90
    @Test()
    void addAll4WhenArray2IsNull() {
        /* Branches:
         * (array1 == null) : false
         * (array2 == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            float[] floatArray2 = new float[] {};
            arrayUtils.when(() -> ArrayUtils.clone(floatArray2)).thenReturn(floatArray);
            float[] _float = null;
            //Act Statement(s)
            float[] result = ArrayUtils.addAll(floatArray2, _float);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(floatArray));
                arrayUtils.verify(() -> ArrayUtils.clone(floatArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addAll4WhenArray2IsNotNull}, hash: 33518504AE396D5E47EC93E74ED91793
    @Test()
    void addAll4WhenArray2IsNotNull() {
        /* Branches:
         * (array1 == null) : false
         * (array2 == null) : false
         */
        //Arrange Statement(s)
        float[] floatArray = new float[] {};
        float[] floatArray2 = new float[] {};
        //Act Statement(s)
        float[] result = ArrayUtils.addAll(floatArray, floatArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${addAll5WhenArray1IsNull}, hash: 80A6F2A24B79A3B07433C10CCB6F71A4
    @Test()
    void addAll5WhenArray1IsNull() {
        /* Branches:
         * (array1 == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            int[] intArray2 = new int[] {};
            arrayUtils.when(() -> ArrayUtils.clone(intArray2)).thenReturn(intArray);
            int[] _int = null;
            //Act Statement(s)
            int[] result = ArrayUtils.addAll(_int, intArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(intArray));
                arrayUtils.verify(() -> ArrayUtils.clone(intArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addAll5WhenArray2IsNull}, hash: 61894B5DE295B2539F4E8CA289F56138
    @Test()
    void addAll5WhenArray2IsNull() {
        /* Branches:
         * (array1 == null) : false
         * (array2 == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            int[] intArray2 = new int[] {};
            arrayUtils.when(() -> ArrayUtils.clone(intArray2)).thenReturn(intArray);
            int[] _int = null;
            //Act Statement(s)
            int[] result = ArrayUtils.addAll(intArray2, _int);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(intArray));
                arrayUtils.verify(() -> ArrayUtils.clone(intArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addAll5WhenArray2IsNotNull}, hash: 89DB2E06B7D44D47E75102871C7AE0D3
    @Test()
    void addAll5WhenArray2IsNotNull() {
        /* Branches:
         * (array1 == null) : false
         * (array2 == null) : false
         */
        //Arrange Statement(s)
        int[] intArray = new int[] {};
        int[] intArray2 = new int[] {};
        //Act Statement(s)
        int[] result = ArrayUtils.addAll(intArray, intArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${addAll6WhenArray1IsNull}, hash: 48E4FF6EF7DC4A247B1F80305CD08F8F
    @Test()
    void addAll6WhenArray1IsNull() {
        /* Branches:
         * (array1 == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            long[] longArray2 = new long[] {};
            arrayUtils.when(() -> ArrayUtils.clone(longArray2)).thenReturn(longArray);
            long[] _long = null;
            //Act Statement(s)
            long[] result = ArrayUtils.addAll(_long, longArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(longArray));
                arrayUtils.verify(() -> ArrayUtils.clone(longArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addAll6WhenArray2IsNull}, hash: F6C38017F1647678BEB9230BF361BA59
    @Test()
    void addAll6WhenArray2IsNull() {
        /* Branches:
         * (array1 == null) : false
         * (array2 == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            long[] longArray2 = new long[] {};
            arrayUtils.when(() -> ArrayUtils.clone(longArray2)).thenReturn(longArray);
            long[] _long = null;
            //Act Statement(s)
            long[] result = ArrayUtils.addAll(longArray2, _long);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(longArray));
                arrayUtils.verify(() -> ArrayUtils.clone(longArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addAll6WhenArray2IsNotNull}, hash: 1C7EA3DB5352FD0BF4555CEA5E9F481C
    @Test()
    void addAll6WhenArray2IsNotNull() {
        /* Branches:
         * (array1 == null) : false
         * (array2 == null) : false
         */
        //Arrange Statement(s)
        long[] longArray = new long[] {};
        long[] longArray2 = new long[] {};
        //Act Statement(s)
        long[] result = ArrayUtils.addAll(longArray, longArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${addAll7WhenArray1IsNull}, hash: A64C0B44A5319766101A8EF33B31D159
    @Test()
    void addAll7WhenArray1IsNull() {
        /* Branches:
         * (array1 == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            short[] shortArray2 = new short[] {};
            arrayUtils.when(() -> ArrayUtils.clone(shortArray2)).thenReturn(shortArray);
            short[] _short = null;
            //Act Statement(s)
            short[] result = ArrayUtils.addAll(_short, shortArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(shortArray));
                arrayUtils.verify(() -> ArrayUtils.clone(shortArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addAll7WhenArray2IsNull}, hash: B0D43CFB30E5AC3661CB3965E60A356E
    @Test()
    void addAll7WhenArray2IsNull() {
        /* Branches:
         * (array1 == null) : false
         * (array2 == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            short[] shortArray2 = new short[] {};
            arrayUtils.when(() -> ArrayUtils.clone(shortArray2)).thenReturn(shortArray);
            short[] _short = null;
            //Act Statement(s)
            short[] result = ArrayUtils.addAll(shortArray2, _short);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(shortArray));
                arrayUtils.verify(() -> ArrayUtils.clone(shortArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addAll7WhenArray2IsNotNull}, hash: 7687AF480E6FC1BA4A60AB6A850F7B0C
    @Test()
    void addAll7WhenArray2IsNotNull() {
        /* Branches:
         * (array1 == null) : false
         * (array2 == null) : false
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] {};
        short[] shortArray2 = new short[] {};
        //Act Statement(s)
        short[] result = ArrayUtils.addAll(shortArray, shortArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${addAll8WhenArray1IsNull}, hash: 0997D5E26C6721F747DC7EBE19A678C6
    @Test()
    void addAll8WhenArray1IsNull() {
        /* Branches:
         * (array1 == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            Object[] objectArray2 = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.clone(objectArray2)).thenReturn(objectArray);
            Object[] object = null;
            //Act Statement(s)
            Object[] result = ArrayUtils.addAll(object, objectArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(objectArray));
                arrayUtils.verify(() -> ArrayUtils.clone(objectArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addAll8WhenArray2IsNull}, hash: EDDB1BE957CED8D5D942058EBDB9509B
    @Test()
    void addAll8WhenArray2IsNull() {
        /* Branches:
         * (array1 == null) : false
         * (array2 == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            Object[] objectArray2 = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.clone(objectArray2)).thenReturn(objectArray);
            Object[] object = null;
            //Act Statement(s)
            Object[] result = ArrayUtils.addAll(objectArray2, object);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(objectArray));
                arrayUtils.verify(() -> ArrayUtils.clone(objectArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addAll8WhenArray2IsNotNull}, hash: EA507C16417BBADDC5B751616EA68F7E
    @Test()
    void addAll8WhenArray2IsNotNull() {
        /* Branches:
         * (array1 == null) : false
         * (array2 == null) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            Object[] objectArray2 = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.arraycopy(eq(objectArray2), eq(0), eq(0), eq(0), (Supplier) any())).thenReturn(objectArray);
            Object[] objectArray3 = new Object[] {};
            //Act Statement(s)
            Object[] result = ArrayUtils.addAll(objectArray2, objectArray3);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(objectArray));
                arrayUtils.verify(() -> ArrayUtils.arraycopy(eq(objectArray2), eq(0), eq(0), eq(0), (Supplier) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addAll8WhenType1NotIsAssignableFromType2ThrowsIllegalArgumentException}, hash: E5768F2F0344E937407CCA617779B973
    @Disabled()
    @Test()
    void addAll8WhenType1NotIsAssignableFromType2ThrowsIllegalArgumentException() {
        /* Branches:
         * (array1 == null) : false
         * (array2 == null) : false
         * (catch-exception (ArrayStoreException)) : true
         * (!type1.isAssignableFrom(type2)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            Object[] objectArray2 = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.arraycopy(eq(objectArray2), eq(0), eq(0), eq(0), (Supplier) any())).thenReturn(objectArray);
            Object[] objectArray3 = new Object[] {};
            //Act Statement(s)
            final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
                ArrayUtils.addAll(objectArray2, objectArray3);
            });
            ArrayStoreException arrayStoreException = new ArrayStoreException();
            IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Cannot store java.lang.Object in an array of java.lang.Object", arrayStoreException);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
                assertThat(result.getCause(), is(instanceOf(arrayStoreException.getClass())));
                arrayUtils.verify(() -> ArrayUtils.arraycopy(eq(objectArray2), eq(0), eq(0), eq(0), (Supplier) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addAll8WhenType1IsAssignableFromType2ThrowsArrayStoreException}, hash: 4D428E5212E4A908361AC386185CCAE5
    @Disabled()
    @Test()
    void addAll8WhenType1IsAssignableFromType2ThrowsArrayStoreException() {
        /* Branches:
         * (array1 == null) : false
         * (array2 == null) : false
         * (catch-exception (ArrayStoreException)) : true
         * (!type1.isAssignableFrom(type2)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            Object[] objectArray2 = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.arraycopy(eq(objectArray2), eq(0), eq(0), eq(0), (Supplier) any())).thenReturn(objectArray);
            Object[] objectArray3 = new Object[] {};
            //Act Statement(s)
            final ArrayStoreException result = assertThrows(ArrayStoreException.class, () -> {
                ArrayUtils.addAll(objectArray2, objectArray3);
            });
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                arrayUtils.verify(() -> ArrayUtils.arraycopy(eq(objectArray2), eq(0), eq(0), eq(0), (Supplier) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addFirstWhenArrayIsNull}, hash: 30E4F0CCABA6759BD6285C5FB409D849
    @Test()
    void addFirstWhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.add((boolean[]) null, false)).thenReturn(booleanArray);
            boolean[] _boolean = null;
            //Act Statement(s)
            boolean[] result = ArrayUtils.addFirst(_boolean, false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(booleanArray));
                arrayUtils.verify(() -> ArrayUtils.add((boolean[]) null, false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addFirstWhenArrayIsNotNull}, hash: 6CCB4C87CE747EB1259BA4A1242695F7
    @Test()
    void addFirstWhenArrayIsNotNull() {
        /* Branches:
         * (array == null) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            boolean[] booleanArray2 = new boolean[] {};
            boolean[] booleanArray3 = new boolean[] { false };
            arrayUtils.when(() -> ArrayUtils.insert(0, booleanArray2, booleanArray3)).thenReturn(booleanArray);
            //Act Statement(s)
            boolean[] result = ArrayUtils.addFirst(booleanArray2, false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(booleanArray));
                arrayUtils.verify(() -> ArrayUtils.insert(0, booleanArray2, booleanArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addFirst1WhenArrayIsNull}, hash: E191231E1AC6F522E3CC3D82CFC3A064
    @Test()
    void addFirst1WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.add((byte[]) null, (byte) 0)).thenReturn(byteArray);
            byte[] _byte = null;
            //Act Statement(s)
            byte[] result = ArrayUtils.addFirst(_byte, (byte) 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(byteArray));
                arrayUtils.verify(() -> ArrayUtils.add((byte[]) null, (byte) 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addFirst1WhenArrayIsNotNull}, hash: ED0452CCCD250041650259D15E9D84A9
    @Test()
    void addFirst1WhenArrayIsNotNull() {
        /* Branches:
         * (array == null) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            byte[] byteArray2 = new byte[] {};
            byte[] byteArray3 = new byte[] { (byte) 0 };
            arrayUtils.when(() -> ArrayUtils.insert(0, byteArray2, byteArray3)).thenReturn(byteArray);
            //Act Statement(s)
            byte[] result = ArrayUtils.addFirst(byteArray2, (byte) 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(byteArray));
                arrayUtils.verify(() -> ArrayUtils.insert(0, byteArray2, byteArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addFirst2WhenArrayIsNull}, hash: 75DB03555AE0748206982E5912DA634E
    @Test()
    void addFirst2WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.add((char[]) null, 'A')).thenReturn(charArray);
            char[] _char = null;
            //Act Statement(s)
            char[] result = ArrayUtils.addFirst(_char, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(charArray));
                arrayUtils.verify(() -> ArrayUtils.add((char[]) null, 'A'), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addFirst2WhenArrayIsNotNull}, hash: 8BAEC9A952CD816C706050D12665D494
    @Test()
    void addFirst2WhenArrayIsNotNull() {
        /* Branches:
         * (array == null) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            char[] charArray2 = new char[] {};
            char[] charArray3 = new char[] { 'A' };
            arrayUtils.when(() -> ArrayUtils.insert(0, charArray2, charArray3)).thenReturn(charArray);
            //Act Statement(s)
            char[] result = ArrayUtils.addFirst(charArray2, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(charArray));
                arrayUtils.verify(() -> ArrayUtils.insert(0, charArray2, charArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addFirst3WhenArrayIsNull}, hash: 0A600C6CD9DE0B0610B4197AEDC5E03A
    @Test()
    void addFirst3WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.add((double[]) null, Double.parseDouble("0.0"))).thenReturn(doubleArray);
            double[] _double = null;
            //Act Statement(s)
            double[] result = ArrayUtils.addFirst(_double, Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(doubleArray));
                arrayUtils.verify(() -> ArrayUtils.add((double[]) null, Double.parseDouble("0.0")), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addFirst3WhenArrayIsNotNull}, hash: C7BA39FB2CD5995D6C097F21BC87A8F9
    @Test()
    void addFirst3WhenArrayIsNotNull() {
        /* Branches:
         * (array == null) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            double[] doubleArray2 = new double[] {};
            double[] doubleArray3 = new double[] { Double.parseDouble("0.0") };
            arrayUtils.when(() -> ArrayUtils.insert(0, doubleArray2, doubleArray3)).thenReturn(doubleArray);
            //Act Statement(s)
            double[] result = ArrayUtils.addFirst(doubleArray2, Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(doubleArray));
                arrayUtils.verify(() -> ArrayUtils.insert(0, doubleArray2, doubleArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addFirst4WhenArrayIsNull}, hash: 9466DBA7BE2DEEC484298678A7B7A05E
    @Test()
    void addFirst4WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.add((float[]) null, Float.parseFloat("0.0"))).thenReturn(floatArray);
            float[] _float = null;
            //Act Statement(s)
            float[] result = ArrayUtils.addFirst(_float, Float.parseFloat("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(floatArray));
                arrayUtils.verify(() -> ArrayUtils.add((float[]) null, Float.parseFloat("0.0")), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addFirst4WhenArrayIsNotNull}, hash: 9A8BC04ADE5CA30CAB21E2D338493F6F
    @Test()
    void addFirst4WhenArrayIsNotNull() {
        /* Branches:
         * (array == null) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            float[] floatArray2 = new float[] {};
            float[] floatArray3 = new float[] { Float.parseFloat("0.0") };
            arrayUtils.when(() -> ArrayUtils.insert(0, floatArray2, floatArray3)).thenReturn(floatArray);
            //Act Statement(s)
            float[] result = ArrayUtils.addFirst(floatArray2, Float.parseFloat("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(floatArray));
                arrayUtils.verify(() -> ArrayUtils.insert(0, floatArray2, floatArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addFirst5WhenArrayIsNull}, hash: A7B2BDCA593EDD20EF77D151D47D211B
    @Test()
    void addFirst5WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.add((int[]) null, 0)).thenReturn(intArray);
            int[] _int = null;
            //Act Statement(s)
            int[] result = ArrayUtils.addFirst(_int, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(intArray));
                arrayUtils.verify(() -> ArrayUtils.add((int[]) null, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addFirst5WhenArrayIsNotNull}, hash: 89D945B64D8AB4354DD379B1611E4F8C
    @Test()
    void addFirst5WhenArrayIsNotNull() {
        /* Branches:
         * (array == null) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            int[] intArray2 = new int[] {};
            int[] intArray3 = new int[] { 0 };
            arrayUtils.when(() -> ArrayUtils.insert(0, intArray2, intArray3)).thenReturn(intArray);
            //Act Statement(s)
            int[] result = ArrayUtils.addFirst(intArray2, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(intArray));
                arrayUtils.verify(() -> ArrayUtils.insert(0, intArray2, intArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addFirst6WhenArrayIsNull}, hash: 6B92C0BA61E66AE9E105EBD5487BA2D0
    @Test()
    void addFirst6WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.add((long[]) null, 0L)).thenReturn(longArray);
            long[] _long = null;
            //Act Statement(s)
            long[] result = ArrayUtils.addFirst(_long, 0L);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(longArray));
                arrayUtils.verify(() -> ArrayUtils.add((long[]) null, 0L), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addFirst6WhenArrayIsNotNull}, hash: 7DF0C194349B32EF6816C92EA7A1F1C9
    @Test()
    void addFirst6WhenArrayIsNotNull() {
        /* Branches:
         * (array == null) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            long[] longArray2 = new long[] {};
            long[] longArray3 = new long[] { 0L };
            arrayUtils.when(() -> ArrayUtils.insert(0, longArray2, longArray3)).thenReturn(longArray);
            //Act Statement(s)
            long[] result = ArrayUtils.addFirst(longArray2, 0L);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(longArray));
                arrayUtils.verify(() -> ArrayUtils.insert(0, longArray2, longArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addFirst7WhenArrayIsNull}, hash: 8859194F426A4513A3FA7F9EC73BEC52
    @Test()
    void addFirst7WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.add((short[]) null, (short) 0)).thenReturn(shortArray);
            short[] _short = null;
            //Act Statement(s)
            short[] result = ArrayUtils.addFirst(_short, (short) 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(shortArray));
                arrayUtils.verify(() -> ArrayUtils.add((short[]) null, (short) 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addFirst7WhenArrayIsNotNull}, hash: 81975002E6862DB754231DFCDE9CD18D
    @Test()
    void addFirst7WhenArrayIsNotNull() {
        /* Branches:
         * (array == null) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            short[] shortArray2 = new short[] {};
            short[] shortArray3 = new short[] { (short) 0 };
            arrayUtils.when(() -> ArrayUtils.insert(0, shortArray2, shortArray3)).thenReturn(shortArray);
            //Act Statement(s)
            short[] result = ArrayUtils.addFirst(shortArray2, (short) 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(shortArray));
                arrayUtils.verify(() -> ArrayUtils.insert(0, shortArray2, shortArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addFirst8WhenArrayIsNull}, hash: 6661C4D3C5D7AF34DA30463BCAE3D224
    @Test()
    void addFirst8WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            Object object = new Object();
            arrayUtils.when(() -> ArrayUtils.add((Object[]) null, object)).thenReturn(objectArray);
            Object[] object2 = null;
            //Act Statement(s)
            Object[] result = ArrayUtils.addFirst(object2, object);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(objectArray));
                arrayUtils.verify(() -> ArrayUtils.add((Object[]) null, object), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${addFirst8WhenArrayIsNotNull}, hash: 179540A002AFEF4AA010A2D8390CA674
    @Test()
    void addFirst8WhenArrayIsNotNull() {
        /* Branches:
         * (array == null) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            Object[] objectArray2 = new Object[] {};
            Object object = new Object();
            Object[] objectArray3 = new Object[] { object };
            arrayUtils.when(() -> ArrayUtils.insert(0, objectArray2, objectArray3)).thenReturn(objectArray);
            //Act Statement(s)
            Object[] result = ArrayUtils.addFirst(objectArray2, object);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(objectArray));
                arrayUtils.verify(() -> ArrayUtils.insert(0, objectArray2, objectArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${arraycopyTest}, hash: B42E7AF103046E91E6B19C37AA050686
    @Disabled()
    @Test()
    void arraycopyTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object object = new Object();
        Function<Integer, Object> functionMock = mock(Function.class);
        //Act Statement(s)
        Object result = ArrayUtils.arraycopy(object, 0, 0, 1, functionMock);
        //Assert statement(s)
        //TODO: Please implement equals method in Object for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${arraycopy1Test}, hash: 1B4D452F18FE145DBFF12B56E6A779E8
    @Disabled()
    @Test()
    void arraycopy1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object object = new Object();
        Supplier<Object> supplierMock = mock(Supplier.class);
        //Act Statement(s)
        Object result = ArrayUtils.arraycopy(object, 0, 0, 0, supplierMock);
        //Assert statement(s)
        //TODO: Please implement equals method in Object for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${arraycopy2Test}, hash: 962809E3B35810E965AB5889009E61C5
    @Disabled()
    @Test()
    void arraycopy2Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        //Act Statement(s)
        Object result = ArrayUtils.arraycopy(object, 0, object2, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(object2)));
    }

    //BaseRock generated method id: ${cloneWhenArrayIsNotNull}, hash: D46CB89C67627A749E331063F00387F6
    @Test()
    void cloneWhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        //Act Statement(s)
        boolean[] result = ArrayUtils.clone(booleanArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${cloneWhenArrayIsNull}, hash: 7321BC48ADC86320CB3436DC18617AC9
    @Test()
    void cloneWhenArrayIsNull() {
        /* Branches:
         * (array != null) : false
         */
        //Arrange Statement(s)
        boolean[] _boolean = null;
        //Act Statement(s)
        boolean[] result = ArrayUtils.clone(_boolean);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${clone1WhenArrayIsNotNull}, hash: A971C6DB1E0CCED72158510349FBB54D
    @Test()
    void clone1WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        //Act Statement(s)
        byte[] result = ArrayUtils.clone(byteArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${clone1WhenArrayIsNull}, hash: 27309E3DD77446C2CD7E3E0CA0F55583
    @Test()
    void clone1WhenArrayIsNull() {
        /* Branches:
         * (array != null) : false
         */
        //Arrange Statement(s)
        byte[] _byte = null;
        //Act Statement(s)
        byte[] result = ArrayUtils.clone(_byte);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${clone2WhenArrayIsNotNull}, hash: D9A5F6E4081DE37781464AE31F22420F
    @Test()
    void clone2WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        //Act Statement(s)
        char[] result = ArrayUtils.clone(charArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${clone2WhenArrayIsNull}, hash: 1D18E3FA85808DDEA01E60332E0A6558
    @Test()
    void clone2WhenArrayIsNull() {
        /* Branches:
         * (array != null) : false
         */
        //Arrange Statement(s)
        char[] _char = null;
        //Act Statement(s)
        char[] result = ArrayUtils.clone(_char);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${clone3WhenArrayIsNotNull}, hash: D8BDC3769FD861C92B62CC0425FC79CF
    @Test()
    void clone3WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        double[] doubleArray = new double[] {};
        //Act Statement(s)
        double[] result = ArrayUtils.clone(doubleArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${clone3WhenArrayIsNull}, hash: 317D8471EDB62D4791406A89DA63D602
    @Test()
    void clone3WhenArrayIsNull() {
        /* Branches:
         * (array != null) : false
         */
        //Arrange Statement(s)
        double[] _double = null;
        //Act Statement(s)
        double[] result = ArrayUtils.clone(_double);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${clone4WhenArrayIsNotNull}, hash: 2042F74903F35EE8721F0FECBF929DD9
    @Test()
    void clone4WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        float[] floatArray = new float[] {};
        //Act Statement(s)
        float[] result = ArrayUtils.clone(floatArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${clone4WhenArrayIsNull}, hash: 875DC2140510A08C3CFE14553B5E8ECA
    @Test()
    void clone4WhenArrayIsNull() {
        /* Branches:
         * (array != null) : false
         */
        //Arrange Statement(s)
        float[] _float = null;
        //Act Statement(s)
        float[] result = ArrayUtils.clone(_float);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${clone5WhenArrayIsNotNull}, hash: AFAE51C0E3767DC70D91C09EB0AC7AE2
    @Test()
    void clone5WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        int[] intArray = new int[] {};
        //Act Statement(s)
        int[] result = ArrayUtils.clone(intArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${clone5WhenArrayIsNull}, hash: CD89F78A962D57EAF39C10FBAFECD024
    @Test()
    void clone5WhenArrayIsNull() {
        /* Branches:
         * (array != null) : false
         */
        //Arrange Statement(s)
        int[] _int = null;
        //Act Statement(s)
        int[] result = ArrayUtils.clone(_int);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${clone6WhenArrayIsNotNull}, hash: 02D033D18353D335732B0B5B94FCB80D
    @Test()
    void clone6WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        long[] longArray = new long[] {};
        //Act Statement(s)
        long[] result = ArrayUtils.clone(longArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${clone6WhenArrayIsNull}, hash: 3344B9946FC83B3FD8DE5484A6A65D07
    @Test()
    void clone6WhenArrayIsNull() {
        /* Branches:
         * (array != null) : false
         */
        //Arrange Statement(s)
        long[] _long = null;
        //Act Statement(s)
        long[] result = ArrayUtils.clone(_long);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${clone7WhenArrayIsNotNull}, hash: D8137195A162294B5234663E8433F293
    @Test()
    void clone7WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] {};
        //Act Statement(s)
        short[] result = ArrayUtils.clone(shortArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${clone7WhenArrayIsNull}, hash: DF953138892E5D20EA668616D282621A
    @Test()
    void clone7WhenArrayIsNull() {
        /* Branches:
         * (array != null) : false
         */
        //Arrange Statement(s)
        short[] _short = null;
        //Act Statement(s)
        short[] result = ArrayUtils.clone(_short);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${clone8WhenArrayIsNotNull}, hash: 884C4437273E1ACF305C988A075887FD
    @Test()
    void clone8WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        //Act Statement(s)
        Object[] result = ArrayUtils.clone(objectArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${clone8WhenArrayIsNull}, hash: EDF5B4E6AE1AFA37F2B4B5110DE4AE36
    @Test()
    void clone8WhenArrayIsNull() {
        /* Branches:
         * (array != null) : false
         */
        //Arrange Statement(s)
        Object[] object = null;
        //Act Statement(s)
        Object[] result = ArrayUtils.clone(object);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${containsWhenIndexOfArrayValueToFindNotEqualsINDEX_NOT_FOUND}, hash: 359F81DB41D8713532993C10ABA60ABF
    @Test()
    void containsWhenIndexOfArrayValueToFindNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (indexOf(array, valueToFind) != INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(booleanArray, false)).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.contains(booleanArray, false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.indexOf(booleanArray, false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${containsWhenIndexOfArrayValueToFindEqualsINDEX_NOT_FOUND}, hash: D9210893182D4ADDC6F7388C35AEB6AC
    @Test()
    void containsWhenIndexOfArrayValueToFindEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (indexOf(array, valueToFind) != INDEX_NOT_FOUND) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(booleanArray, false)).thenReturn(-1);
            //Act Statement(s)
            boolean result = ArrayUtils.contains(booleanArray, false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.indexOf(booleanArray, false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${contains1WhenIndexOfArrayValueToFindNotEqualsINDEX_NOT_FOUND}, hash: 1EC3F63B136AB1990DE0098BC8EE2A05
    @Test()
    void contains1WhenIndexOfArrayValueToFindNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (indexOf(array, valueToFind) != INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(byteArray, (byte) 0)).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.contains(byteArray, (byte) 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.indexOf(byteArray, (byte) 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${contains1WhenIndexOfArrayValueToFindEqualsINDEX_NOT_FOUND}, hash: 63932546A909DAEFA2B4DE95AE033864
    @Test()
    void contains1WhenIndexOfArrayValueToFindEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (indexOf(array, valueToFind) != INDEX_NOT_FOUND) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(byteArray, (byte) 0)).thenReturn(-1);
            //Act Statement(s)
            boolean result = ArrayUtils.contains(byteArray, (byte) 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.indexOf(byteArray, (byte) 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${contains2WhenIndexOfArrayValueToFindNotEqualsINDEX_NOT_FOUND}, hash: 56F2700BD391F87128433A0FB4FE2E6F
    @Test()
    void contains2WhenIndexOfArrayValueToFindNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (indexOf(array, valueToFind) != INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(charArray, 'A')).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.contains(charArray, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.indexOf(charArray, 'A'), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${contains2WhenIndexOfArrayValueToFindEqualsINDEX_NOT_FOUND}, hash: 4DB9D1638708D39962DE666A04FDFE0D
    @Test()
    void contains2WhenIndexOfArrayValueToFindEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (indexOf(array, valueToFind) != INDEX_NOT_FOUND) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(charArray, 'A')).thenReturn(-1);
            //Act Statement(s)
            boolean result = ArrayUtils.contains(charArray, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.indexOf(charArray, 'A'), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${contains3WhenIndexOfArrayValueToFindNotEqualsINDEX_NOT_FOUND}, hash: 47986BE0A72B2F52C018AF26F0872C8D
    @Test()
    void contains3WhenIndexOfArrayValueToFindNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (indexOf(array, valueToFind) != INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"))).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.contains(doubleArray, Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0")), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${contains3WhenIndexOfArrayValueToFindEqualsINDEX_NOT_FOUND}, hash: B84E169C88D4256F5D4BB0DD809F102F
    @Test()
    void contains3WhenIndexOfArrayValueToFindEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (indexOf(array, valueToFind) != INDEX_NOT_FOUND) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"))).thenReturn(-1);
            //Act Statement(s)
            boolean result = ArrayUtils.contains(doubleArray, Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0")), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${contains4WhenIndexOfArrayValueToFind0ToleranceNotEqualsINDEX_NOT_FOUND}, hash: 66E12DC63E7EF084DD21382FA4A58565
    @Test()
    void contains4WhenIndexOfArrayValueToFind0ToleranceNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (indexOf(array, valueToFind, 0, tolerance) != INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"), 0, Double.parseDouble("0.0"))).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.contains(doubleArray, Double.parseDouble("0.0"), Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"), 0, Double.parseDouble("0.0")), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${contains4WhenIndexOfArrayValueToFind0ToleranceEqualsINDEX_NOT_FOUND}, hash: 1718824BA91769380BA8DC6E9B91E793
    @Test()
    void contains4WhenIndexOfArrayValueToFind0ToleranceEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (indexOf(array, valueToFind, 0, tolerance) != INDEX_NOT_FOUND) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"), 0, Double.parseDouble("0.0"))).thenReturn(-1);
            //Act Statement(s)
            boolean result = ArrayUtils.contains(doubleArray, Double.parseDouble("0.0"), Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"), 0, Double.parseDouble("0.0")), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${contains5WhenIndexOfArrayValueToFindNotEqualsINDEX_NOT_FOUND}, hash: 0FA5CC1DD9EB038A8359A40F598F24AC
    @Test()
    void contains5WhenIndexOfArrayValueToFindNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (indexOf(array, valueToFind) != INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(floatArray, Float.parseFloat("0.0"))).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.contains(floatArray, Float.parseFloat("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.indexOf(floatArray, Float.parseFloat("0.0")), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${contains5WhenIndexOfArrayValueToFindEqualsINDEX_NOT_FOUND}, hash: 87869B814BC9DE11898D7E98BFEC0B57
    @Test()
    void contains5WhenIndexOfArrayValueToFindEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (indexOf(array, valueToFind) != INDEX_NOT_FOUND) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(floatArray, Float.parseFloat("0.0"))).thenReturn(-1);
            //Act Statement(s)
            boolean result = ArrayUtils.contains(floatArray, Float.parseFloat("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.indexOf(floatArray, Float.parseFloat("0.0")), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${contains6WhenIndexOfArrayValueToFindNotEqualsINDEX_NOT_FOUND}, hash: 14822EF63F16AC65CFDE807D8C0E4BA1
    @Test()
    void contains6WhenIndexOfArrayValueToFindNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (indexOf(array, valueToFind) != INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(intArray, 0)).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.contains(intArray, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.indexOf(intArray, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${contains6WhenIndexOfArrayValueToFindEqualsINDEX_NOT_FOUND}, hash: BABCAEA21C3E58A2B742B9EAC1E15CB0
    @Test()
    void contains6WhenIndexOfArrayValueToFindEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (indexOf(array, valueToFind) != INDEX_NOT_FOUND) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(intArray, 0)).thenReturn(-1);
            //Act Statement(s)
            boolean result = ArrayUtils.contains(intArray, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.indexOf(intArray, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${contains7WhenIndexOfArrayValueToFindNotEqualsINDEX_NOT_FOUND}, hash: EDE0FAA3D11267573D3A1C490B00FE11
    @Test()
    void contains7WhenIndexOfArrayValueToFindNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (indexOf(array, valueToFind) != INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(longArray, 0L)).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.contains(longArray, 0L);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.indexOf(longArray, 0L), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${contains7WhenIndexOfArrayValueToFindEqualsINDEX_NOT_FOUND}, hash: D471A017E10CE7901FA1F37D374A3082
    @Test()
    void contains7WhenIndexOfArrayValueToFindEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (indexOf(array, valueToFind) != INDEX_NOT_FOUND) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(longArray, 0L)).thenReturn(-1);
            //Act Statement(s)
            boolean result = ArrayUtils.contains(longArray, 0L);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.indexOf(longArray, 0L), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${contains8WhenIndexOfArrayObjectToFindNotEqualsINDEX_NOT_FOUND}, hash: CE81492609E7D7170C4AEF6EC408D8B8
    @Test()
    void contains8WhenIndexOfArrayObjectToFindNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (indexOf(array, objectToFind) != INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            Object object = new Object();
            arrayUtils.when(() -> ArrayUtils.indexOf(objectArray, object)).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.contains(objectArray, object);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.indexOf(objectArray, object), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${contains8WhenIndexOfArrayObjectToFindEqualsINDEX_NOT_FOUND}, hash: 012252EA443C53E870DC7FF12E931542
    @Test()
    void contains8WhenIndexOfArrayObjectToFindEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (indexOf(array, objectToFind) != INDEX_NOT_FOUND) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            Object object = new Object();
            arrayUtils.when(() -> ArrayUtils.indexOf(objectArray, object)).thenReturn(-1);
            //Act Statement(s)
            boolean result = ArrayUtils.contains(objectArray, object);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.indexOf(objectArray, object), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${contains9WhenIndexOfArrayValueToFindNotEqualsINDEX_NOT_FOUND}, hash: FB173DBE87CA5DA7766DEC3A99DDDD12
    @Test()
    void contains9WhenIndexOfArrayValueToFindNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (indexOf(array, valueToFind) != INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(shortArray, (short) 0)).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.contains(shortArray, (short) 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.indexOf(shortArray, (short) 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${contains9WhenIndexOfArrayValueToFindEqualsINDEX_NOT_FOUND}, hash: 29133CB6EBADFB0199630A8438920F31
    @Test()
    void contains9WhenIndexOfArrayValueToFindEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (indexOf(array, valueToFind) != INDEX_NOT_FOUND) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(shortArray, (short) 0)).thenReturn(-1);
            //Act Statement(s)
            boolean result = ArrayUtils.contains(shortArray, (short) 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.indexOf(shortArray, (short) 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${containsAnyWhenIntStreamsOfObjectsToFindAnyMatchContainsArrayE}, hash: E09F79FD686B452BD8DDAACD88E6FAE0
    @Disabled()
    @Test()
    void containsAnyWhenIntStreamsOfObjectsToFindAnyMatchContainsArrayE() {
        /* Branches:
         * (IntStreams.of(objectsToFind).anyMatch(e -> contains(array, e))) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        int[] intArray = new int[] {};
        int[] intArray2 = new int[] {};
        //Act Statement(s)
        boolean result = ArrayUtils.containsAny(intArray, intArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${containsAnyWhenIntStreamsOfObjectsToFindNotAnyMatchContainsArrayE}, hash: 73783203726CE0146E9A0845F01D4892
    @Test()
    void containsAnyWhenIntStreamsOfObjectsToFindNotAnyMatchContainsArrayE() {
        /* Branches:
         * (IntStreams.of(objectsToFind).anyMatch(e -> contains(array, e))) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        int[] intArray = new int[] {};
        int[] intArray2 = new int[] {};
        //Act Statement(s)
        boolean result = ArrayUtils.containsAny(intArray, intArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${containsAny1WhenStreamsOfObjectsToFindAnyMatchContainsArrayE}, hash: B3A2995592BB1406777FDADC2828C6C2
    @Disabled()
    @Test()
    void containsAny1WhenStreamsOfObjectsToFindAnyMatchContainsArrayE() {
        /* Branches:
         * (Streams.of(objectsToFind).anyMatch(e -> contains(array, e))) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        Object[] objectArray2 = new Object[] {};
        //Act Statement(s)
        boolean result = ArrayUtils.containsAny(objectArray, objectArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${containsAny1WhenStreamsOfObjectsToFindNotAnyMatchContainsArrayE}, hash: 0D21CB19CCB624E54FABAD4AB1EF0017
    @Test()
    void containsAny1WhenStreamsOfObjectsToFindNotAnyMatchContainsArrayE() {
        /* Branches:
         * (Streams.of(objectsToFind).anyMatch(e -> contains(array, e))) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        Object[] objectArray2 = new Object[] {};
        //Act Statement(s)
        boolean result = ArrayUtils.containsAny(objectArray, objectArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${getTest}, hash: 1069379714B62B30F9DEA0CF94E127D0
    @Test()
    void getTest() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.get(objectArray, 0, (Object) null)).thenReturn(object);
            //Act Statement(s)
            Object result = ArrayUtils.get(objectArray, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(object));
                arrayUtils.verify(() -> ArrayUtils.get(objectArray, 0, (Object) null), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${get1WhenIsArrayIndexValidArrayIndex}, hash: F5D3BEBA4D7ACC1008908CE745128E30
    @Test()
    void get1WhenIsArrayIndexValidArrayIndex() {
        /* Branches:
         * (isArrayIndexValid(array, index)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object[] objectArray = new Object[] { object };
            arrayUtils.when(() -> ArrayUtils.isArrayIndexValid(objectArray, 0)).thenReturn(true);
            Object object2 = new Object();
            //Act Statement(s)
            Object result = ArrayUtils.get(objectArray, 0, object2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(object));
                arrayUtils.verify(() -> ArrayUtils.isArrayIndexValid(objectArray, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${get1WhenIsArrayIndexValidNotArrayIndex}, hash: 483AD24D0F9E253EC6B32791565D7CD5
    @Test()
    void get1WhenIsArrayIndexValidNotArrayIndex() {
        /* Branches:
         * (isArrayIndexValid(array, index)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.isArrayIndexValid(objectArray, 0)).thenReturn(false);
            Object object = new Object();
            //Act Statement(s)
            Object result = ArrayUtils.get(objectArray, 0, object);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(object));
                arrayUtils.verify(() -> ArrayUtils.isArrayIndexValid(objectArray, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getComponentTypeTest}, hash: 73CA192DE42A9CB7E9ED67ABBBA10B0C
    @Test()
    void getComponentTypeTest() {
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        //Act Statement(s)
        Class result = ArrayUtils.getComponentType(objectArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Object.class)));
    }

    //BaseRock generated method id: ${getLengthWhenArrayIsNotNull}, hash: F51E5DB298B9D342D3214D169CE9405F
    @Disabled()
    @Test()
    void getLengthWhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object object = new Object();
        //Act Statement(s)
        int result = ArrayUtils.getLength(object);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${getLengthWhenArrayIsNull}, hash: 1C645FBC78FDD07157EDF280F63BDA73
    @Test()
    void getLengthWhenArrayIsNull() {
        /* Branches:
         * (array != null) : false
         */
        //Arrange Statement(s)
        Object object = null;
        //Act Statement(s)
        int result = ArrayUtils.getLength(object);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${hashCodeTest}, hash: 2E6CFF48EF7C93F6F4A16741AED33065
    @Disabled()
    @Test()
    void hashCodeTest() {
        //Arrange Statement(s)
        Object object = new Object();
        //Act Statement(s)
        int result = ArrayUtils.hashCode(object);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1892733741)));
    }

    //BaseRock generated method id: ${indexesOfTest}, hash: B631158B1081A84B12E2EC141D0E5183
    @Test()
    void indexesOfTest() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.indexesOf(booleanArray, false, 0)).thenReturn(bitSet);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(booleanArray, false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(booleanArray, false, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf1WhenArrayIsNull}, hash: 71F9EB9EEAAA92CA0F21C3F53024749D
    @Test()
    void indexesOf1WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        boolean[] _boolean = null;
        //Act Statement(s)
        BitSet result = ArrayUtils.indexesOf(_boolean, false, 0);
        BitSet bitSet = new BitSet();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(bitSet)));
    }

    //BaseRock generated method id: ${indexesOf1WhenStartIndexEqualsINDEX_NOT_FOUND}, hash: 992853943741D5ACDC1E5F554D25C38A
    @Test()
    void indexesOf1WhenStartIndexEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (array == null) : false
         * (startIndex < array.length) : true
         * (startIndex == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(booleanArray, false, -1)).thenReturn(-1);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(booleanArray, false, -1);
            BitSet bitSet = new BitSet();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexOf(booleanArray, false, -1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf1WhenStartIndexNotEqualsINDEX_NOT_FOUND}, hash: BFAEBAD75FF35A5615015D2EE8F2507D
    @Disabled()
    @Test()
    void indexesOf1WhenStartIndexNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (array == null) : false
         * (startIndex < array.length) : true
         * (startIndex == INDEX_NOT_FOUND) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(booleanArray, false, -1)).thenReturn(0);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(booleanArray, false, -1);
            BitSet bitSet = new BitSet();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexOf(booleanArray, false, -1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf2Test}, hash: 69DA150EF76C7F1B8F23CEA2808DD4B5
    @Test()
    void indexesOf2Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.indexesOf(byteArray, (byte) 0, 0)).thenReturn(bitSet);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(byteArray, (byte) 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(byteArray, (byte) 0, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf3WhenArrayIsNull}, hash: 5038BC3A632338DC9C40456734F9D276
    @Test()
    void indexesOf3WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        byte[] _byte = null;
        //Act Statement(s)
        BitSet result = ArrayUtils.indexesOf(_byte, (byte) 0, 0);
        BitSet bitSet = new BitSet();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(bitSet)));
    }

    //BaseRock generated method id: ${indexesOf3WhenStartIndexEqualsINDEX_NOT_FOUND}, hash: 22F3CC2A7BF321FC96EF1E410335965D
    @Test()
    void indexesOf3WhenStartIndexEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (array == null) : false
         * (startIndex < array.length) : true
         * (startIndex == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(byteArray, (byte) 0, -1)).thenReturn(-1);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(byteArray, (byte) 0, -1);
            BitSet bitSet = new BitSet();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexOf(byteArray, (byte) 0, -1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf3WhenStartIndexNotEqualsINDEX_NOT_FOUND}, hash: EB9D7EE3C64C6DF0A4DD9C9C3B576ECD
    @Disabled()
    @Test()
    void indexesOf3WhenStartIndexNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (array == null) : false
         * (startIndex < array.length) : true
         * (startIndex == INDEX_NOT_FOUND) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(byteArray, (byte) 0, -1)).thenReturn(0);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(byteArray, (byte) 0, -1);
            BitSet bitSet = new BitSet();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexOf(byteArray, (byte) 0, -1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf4Test}, hash: 4536C1363D13302FB2F2AC27D5166F94
    @Test()
    void indexesOf4Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.indexesOf(charArray, 'A', 0)).thenReturn(bitSet);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(charArray, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(charArray, 'A', 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf5WhenArrayIsNull}, hash: E3E957FB079137A768127C9D5D747E64
    @Test()
    void indexesOf5WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        char[] _char = null;
        //Act Statement(s)
        BitSet result = ArrayUtils.indexesOf(_char, 'A', 0);
        BitSet bitSet = new BitSet();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(bitSet)));
    }

    //BaseRock generated method id: ${indexesOf5WhenStartIndexEqualsINDEX_NOT_FOUND}, hash: 864BC49AFA0C085D809514BD4CDC36F4
    @Test()
    void indexesOf5WhenStartIndexEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (array == null) : false
         * (startIndex < array.length) : true
         * (startIndex == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(charArray, 'A', -1)).thenReturn(-1);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(charArray, 'A', -1);
            BitSet bitSet = new BitSet();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexOf(charArray, 'A', -1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf5WhenStartIndexNotEqualsINDEX_NOT_FOUND}, hash: EDD2029A0681CD5E48D5CDEB1F8C93EE
    @Disabled()
    @Test()
    void indexesOf5WhenStartIndexNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (array == null) : false
         * (startIndex < array.length) : true
         * (startIndex == INDEX_NOT_FOUND) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(charArray, 'A', -1)).thenReturn(0);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(charArray, 'A', -1);
            BitSet bitSet = new BitSet();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexOf(charArray, 'A', -1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf6Test}, hash: AAECBFA1C8A9A647C0188AB0F968EC13
    @Test()
    void indexesOf6Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.indexesOf(doubleArray, Double.parseDouble("0.0"), 0)).thenReturn(bitSet);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(doubleArray, Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(doubleArray, Double.parseDouble("0.0"), 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf7Test}, hash: DCD6D6C2B9A5BD614B55B849F0404BB1
    @Test()
    void indexesOf7Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.indexesOf(doubleArray, Double.parseDouble("0.0"), 0, Double.parseDouble("0.0"))).thenReturn(bitSet);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(doubleArray, Double.parseDouble("0.0"), Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(doubleArray, Double.parseDouble("0.0"), 0, Double.parseDouble("0.0")), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf8WhenArrayIsNull}, hash: 8141D708E4D53B1DB06F4B5B75807EED
    @Test()
    void indexesOf8WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        double[] _double = null;
        //Act Statement(s)
        BitSet result = ArrayUtils.indexesOf(_double, Double.parseDouble("0.0"), 0);
        BitSet bitSet = new BitSet();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(bitSet)));
    }

    //BaseRock generated method id: ${indexesOf8WhenStartIndexEqualsINDEX_NOT_FOUND}, hash: 7DABAA460CBB9422F32FE365A60E0E35
    @Test()
    void indexesOf8WhenStartIndexEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (array == null) : false
         * (startIndex < array.length) : true
         * (startIndex == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"), -1)).thenReturn(-1);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(doubleArray, Double.parseDouble("0.0"), -1);
            BitSet bitSet = new BitSet();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"), -1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf8WhenStartIndexNotEqualsINDEX_NOT_FOUND}, hash: E1D9BC862A7B3FA0D0197DCF847132ED
    @Disabled()
    @Test()
    void indexesOf8WhenStartIndexNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (array == null) : false
         * (startIndex < array.length) : true
         * (startIndex == INDEX_NOT_FOUND) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"), -1)).thenReturn(0);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(doubleArray, Double.parseDouble("0.0"), -1);
            BitSet bitSet = new BitSet();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"), -1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf9WhenArrayIsNull}, hash: 1AAA9D20F1708551C4280F5127FC3210
    @Test()
    void indexesOf9WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        double[] _double = null;
        //Act Statement(s)
        BitSet result = ArrayUtils.indexesOf(_double, Double.parseDouble("0.0"), 0, Double.parseDouble("0.0"));
        BitSet bitSet = new BitSet();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(bitSet)));
    }

    //BaseRock generated method id: ${indexesOf9WhenStartIndexEqualsINDEX_NOT_FOUND}, hash: 5D19325CEB82B9D2E1E25D81884CEDDF
    @Test()
    void indexesOf9WhenStartIndexEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (array == null) : false
         * (startIndex < array.length) : true
         * (startIndex == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"), -1, Double.parseDouble("0.0"))).thenReturn(-1);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(doubleArray, Double.parseDouble("0.0"), -1, Double.parseDouble("0.0"));
            BitSet bitSet = new BitSet();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"), -1, Double.parseDouble("0.0")), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf9WhenStartIndexNotEqualsINDEX_NOT_FOUND}, hash: E4776789F1094389970029BC8A60586E
    @Disabled()
    @Test()
    void indexesOf9WhenStartIndexNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (array == null) : false
         * (startIndex < array.length) : true
         * (startIndex == INDEX_NOT_FOUND) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"), -1, Double.parseDouble("0.0"))).thenReturn(0);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(doubleArray, Double.parseDouble("0.0"), -1, Double.parseDouble("0.0"));
            BitSet bitSet = new BitSet();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"), -1, Double.parseDouble("0.0")), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf10Test}, hash: DD4A8168D614C111B59B1DF0CDC9DF26
    @Test()
    void indexesOf10Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.indexesOf(floatArray, Float.parseFloat("0.0"), 0)).thenReturn(bitSet);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(floatArray, Float.parseFloat("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(floatArray, Float.parseFloat("0.0"), 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf11WhenArrayIsNull}, hash: 09EA21766EBB7A04E70FED21B3DC5CB4
    @Test()
    void indexesOf11WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        float[] _float = null;
        //Act Statement(s)
        BitSet result = ArrayUtils.indexesOf(_float, Float.parseFloat("0.0"), 0);
        BitSet bitSet = new BitSet();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(bitSet)));
    }

    //BaseRock generated method id: ${indexesOf11WhenStartIndexEqualsINDEX_NOT_FOUND}, hash: 8A84489098C6565BC0C81BA7F2FEFACD
    @Test()
    void indexesOf11WhenStartIndexEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (array == null) : false
         * (startIndex < array.length) : true
         * (startIndex == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(floatArray, Float.parseFloat("0.0"), -1)).thenReturn(-1);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(floatArray, Float.parseFloat("0.0"), -1);
            BitSet bitSet = new BitSet();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexOf(floatArray, Float.parseFloat("0.0"), -1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf11WhenStartIndexNotEqualsINDEX_NOT_FOUND}, hash: 049D614DCFBEDEE9492EBAEACB9FE39B
    @Disabled()
    @Test()
    void indexesOf11WhenStartIndexNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (array == null) : false
         * (startIndex < array.length) : true
         * (startIndex == INDEX_NOT_FOUND) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(floatArray, Float.parseFloat("0.0"), -1)).thenReturn(0);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(floatArray, Float.parseFloat("0.0"), -1);
            BitSet bitSet = new BitSet();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexOf(floatArray, Float.parseFloat("0.0"), -1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf12Test}, hash: F2BE2CC9E2855746DED5369FD1ADACFF
    @Test()
    void indexesOf12Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.indexesOf(intArray, 0, 0)).thenReturn(bitSet);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(intArray, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(intArray, 0, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf13WhenArrayIsNull}, hash: B6720792788CDD3F455B484013A4F35E
    @Test()
    void indexesOf13WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        int[] _int = null;
        //Act Statement(s)
        BitSet result = ArrayUtils.indexesOf(_int, 0, 0);
        BitSet bitSet = new BitSet();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(bitSet)));
    }

    //BaseRock generated method id: ${indexesOf13WhenStartIndexEqualsINDEX_NOT_FOUND}, hash: 007C3A377ABBA24A41A2650B978202FB
    @Test()
    void indexesOf13WhenStartIndexEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (array == null) : false
         * (startIndex < array.length) : true
         * (startIndex == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(intArray, 0, -1)).thenReturn(-1);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(intArray, 0, -1);
            BitSet bitSet = new BitSet();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexOf(intArray, 0, -1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf13WhenStartIndexNotEqualsINDEX_NOT_FOUND}, hash: BED11D8E7A9FEB0DD8A85A9BF1733A92
    @Disabled()
    @Test()
    void indexesOf13WhenStartIndexNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (array == null) : false
         * (startIndex < array.length) : true
         * (startIndex == INDEX_NOT_FOUND) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(intArray, 0, -1)).thenReturn(0);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(intArray, 0, -1);
            BitSet bitSet = new BitSet();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexOf(intArray, 0, -1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf14Test}, hash: D7255F0ABFE0A8DE1348506E5766B1D7
    @Test()
    void indexesOf14Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.indexesOf(longArray, 0L, 0)).thenReturn(bitSet);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(longArray, 0L);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(longArray, 0L, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf15WhenArrayIsNull}, hash: 349711ADD77650192947B4E391DBC8F3
    @Test()
    void indexesOf15WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        long[] _long = null;
        //Act Statement(s)
        BitSet result = ArrayUtils.indexesOf(_long, 0L, 0);
        BitSet bitSet = new BitSet();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(bitSet)));
    }

    //BaseRock generated method id: ${indexesOf15WhenStartIndexEqualsINDEX_NOT_FOUND}, hash: 5AAD52E0CC60DD94239994C904A9109D
    @Test()
    void indexesOf15WhenStartIndexEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (array == null) : false
         * (startIndex < array.length) : true
         * (startIndex == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(longArray, 0L, -1)).thenReturn(-1);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(longArray, 0L, -1);
            BitSet bitSet = new BitSet();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexOf(longArray, 0L, -1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf15WhenStartIndexNotEqualsINDEX_NOT_FOUND}, hash: 2F9DCCE552264225EE838F955E379762
    @Disabled()
    @Test()
    void indexesOf15WhenStartIndexNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (array == null) : false
         * (startIndex < array.length) : true
         * (startIndex == INDEX_NOT_FOUND) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(longArray, 0L, -1)).thenReturn(0);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(longArray, 0L, -1);
            BitSet bitSet = new BitSet();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexOf(longArray, 0L, -1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf16Test}, hash: 65799737AC54BFA6A003B39F3714B488
    @Test()
    void indexesOf16Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            Object[] objectArray = new Object[] {};
            Object object = new Object();
            arrayUtils.when(() -> ArrayUtils.indexesOf(objectArray, object, 0)).thenReturn(bitSet);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(objectArray, object);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(objectArray, object, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf17WhenArrayIsNull}, hash: DF8664EC6C9369F7C7D9E5F6902A1DCD
    @Test()
    void indexesOf17WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Object[] object = null;
        Object object2 = new Object();
        //Act Statement(s)
        BitSet result = ArrayUtils.indexesOf(object, object2, 0);
        BitSet bitSet = new BitSet();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(bitSet)));
    }

    //BaseRock generated method id: ${indexesOf17WhenStartIndexEqualsINDEX_NOT_FOUND}, hash: F016D2154635C5BA56041D9D9A0FEF3B
    @Test()
    void indexesOf17WhenStartIndexEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (array == null) : false
         * (startIndex < array.length) : true
         * (startIndex == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            Object object = new Object();
            arrayUtils.when(() -> ArrayUtils.indexOf(objectArray, object, -1)).thenReturn(-1);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(objectArray, object, -1);
            BitSet bitSet = new BitSet();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexOf(objectArray, object, -1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf17WhenStartIndexNotEqualsINDEX_NOT_FOUND}, hash: E6F715F5D3C2A914BCBF9F8D1A925A19
    @Disabled()
    @Test()
    void indexesOf17WhenStartIndexNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (array == null) : false
         * (startIndex < array.length) : true
         * (startIndex == INDEX_NOT_FOUND) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            Object object = new Object();
            arrayUtils.when(() -> ArrayUtils.indexOf(objectArray, object, -1)).thenReturn(0);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(objectArray, object, -1);
            BitSet bitSet = new BitSet();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexOf(objectArray, object, -1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf18Test}, hash: 57D93D52F77DEFB821E93A85324AD9DD
    @Test()
    void indexesOf18Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.indexesOf(shortArray, (short) 0, 0)).thenReturn(bitSet);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(shortArray, (short) 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(shortArray, (short) 0, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf19WhenArrayIsNull}, hash: 4164D2FFFBF976B2B494D9EFAB08B4B0
    @Test()
    void indexesOf19WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        short[] _short = null;
        //Act Statement(s)
        BitSet result = ArrayUtils.indexesOf(_short, (short) 0, 0);
        BitSet bitSet = new BitSet();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(bitSet)));
    }

    //BaseRock generated method id: ${indexesOf19WhenStartIndexEqualsINDEX_NOT_FOUND}, hash: 9B2DD6EA222F99AE3BBCD61AF3D8B2E8
    @Test()
    void indexesOf19WhenStartIndexEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (array == null) : false
         * (startIndex < array.length) : true
         * (startIndex == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(shortArray, (short) 0, -1)).thenReturn(-1);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(shortArray, (short) 0, -1);
            BitSet bitSet = new BitSet();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexOf(shortArray, (short) 0, -1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexesOf19WhenStartIndexNotEqualsINDEX_NOT_FOUND}, hash: DCE40136244C0AD901416CFDD755B190
    @Disabled()
    @Test()
    void indexesOf19WhenStartIndexNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (array == null) : false
         * (startIndex < array.length) : true
         * (startIndex == INDEX_NOT_FOUND) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(shortArray, (short) 0, -1)).thenReturn(0);
            //Act Statement(s)
            BitSet result = ArrayUtils.indexesOf(shortArray, (short) 0, -1);
            BitSet bitSet = new BitSet();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(bitSet));
                arrayUtils.verify(() -> ArrayUtils.indexOf(shortArray, (short) 0, -1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOfTest}, hash: D0511154C75FED73276570CEFD17DD5B
    @Test()
    void indexOfTest() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(booleanArray, false, 0)).thenReturn(0);
            //Act Statement(s)
            int result = ArrayUtils.indexOf(booleanArray, false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.indexOf(booleanArray, false, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOf1WhenIsEmptyArray}, hash: 42AA814E38E4CAB4DC4628E483C578C7
    @Test()
    void indexOf1WhenIsEmptyArray() {
        /* Branches:
         * (isEmpty(array)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(booleanArray)).thenReturn(true);
            //Act Statement(s)
            int result = ArrayUtils.indexOf(booleanArray, false, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(-1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(booleanArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOf1WhenValueToFindEqualsIIndexOfArray}, hash: 84260AD2F23B20F031179738DE8B9761
    @Test()
    void indexOf1WhenValueToFindEqualsIIndexOfArray() {
        /* Branches:
         * (isEmpty(array)) : false
         * (i < array.length) : true
         * (valueToFind == array[i]) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] { false };
            arrayUtils.when(() -> ArrayUtils.isEmpty(booleanArray)).thenReturn(false);
            //Act Statement(s)
            int result = ArrayUtils.indexOf(booleanArray, false, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(booleanArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOf1WhenValueToFindNotEqualsIIndexOfArray}, hash: 3A07E679A653F0F10292B825E9AF7BF1
    @Test()
    void indexOf1WhenValueToFindNotEqualsIIndexOfArray() {
        /* Branches:
         * (isEmpty(array)) : false
         * (i < array.length) : true
         * (valueToFind == array[i]) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] { false };
            arrayUtils.when(() -> ArrayUtils.isEmpty(booleanArray)).thenReturn(false);
            //Act Statement(s)
            int result = ArrayUtils.indexOf(booleanArray, true, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(-1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(booleanArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOf2Test}, hash: EADCACC1CD8CC1F3F95F0E30A2CF0DC9
    @Test()
    void indexOf2Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(byteArray, (byte) 0, 0)).thenReturn(0);
            //Act Statement(s)
            int result = ArrayUtils.indexOf(byteArray, (byte) 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.indexOf(byteArray, (byte) 0, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOf3WhenArrayIsNull}, hash: 1AF84350E8AD9FED3B928458F4C01DEB
    @Test()
    void indexOf3WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        byte[] _byte = null;
        //Act Statement(s)
        int result = ArrayUtils.indexOf(_byte, (byte) 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOf3WhenValueToFindEqualsIIndexOfArray}, hash: 48A67DEDC1EF7DFD00BC66F426499353
    @Test()
    void indexOf3WhenValueToFindEqualsIIndexOfArray() {
        /* Branches:
         * (array == null) : false
         * (i < array.length) : true
         * (valueToFind == array[i]) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] { (byte) 0 };
        //Act Statement(s)
        int result = ArrayUtils.indexOf(byteArray, (byte) 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${indexOf3WhenValueToFindNotEqualsIIndexOfArray}, hash: C49E70470068DDBC03C155D3C9CB729E
    @Test()
    void indexOf3WhenValueToFindNotEqualsIIndexOfArray() {
        /* Branches:
         * (array == null) : false
         * (i < array.length) : true
         * (valueToFind == array[i]) : false
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] { (byte) 1 };
        //Act Statement(s)
        int result = ArrayUtils.indexOf(byteArray, (byte) 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOf4Test}, hash: 092C6F79B678084C75CDD090CF0C6E5E
    @Test()
    void indexOf4Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(charArray, 'A', 0)).thenReturn(0);
            //Act Statement(s)
            int result = ArrayUtils.indexOf(charArray, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.indexOf(charArray, 'A', 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOf5WhenArrayIsNull}, hash: 179421517C2518783ADCB799142F8963
    @Test()
    void indexOf5WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        char[] _char = null;
        //Act Statement(s)
        int result = ArrayUtils.indexOf(_char, 'A', 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOf5WhenValueToFindEqualsIIndexOfArray}, hash: C5997AB09A4CEC9F7F807EAA5356BD00
    @Test()
    void indexOf5WhenValueToFindEqualsIIndexOfArray() {
        /* Branches:
         * (array == null) : false
         * (i < array.length) : true
         * (valueToFind == array[i]) : true
         */
        //Arrange Statement(s)
        char[] charArray = new char[] { 'A' };
        //Act Statement(s)
        int result = ArrayUtils.indexOf(charArray, 'A', 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${indexOf5WhenValueToFindNotEqualsIIndexOfArray}, hash: EB1CD3C7FDA5B04AFE7F21FC69A615BB
    @Test()
    void indexOf5WhenValueToFindNotEqualsIIndexOfArray() {
        /* Branches:
         * (array == null) : false
         * (i < array.length) : true
         * (valueToFind == array[i]) : false
         */
        //Arrange Statement(s)
        char[] charArray = new char[] { 'B' };
        //Act Statement(s)
        int result = ArrayUtils.indexOf(charArray, 'A', 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOf6Test}, hash: 304E3985D6562028A2778E12DB2D6F8C
    @Test()
    void indexOf6Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"), 0)).thenReturn(0);
            //Act Statement(s)
            int result = ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"), 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOf7Test}, hash: 2017C3FB68F932D4DA2EB3E9186FF9E8
    @Test()
    void indexOf7Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"), 0, Double.parseDouble("0.0"))).thenReturn(0);
            //Act Statement(s)
            int result = ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"), Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"), 0, Double.parseDouble("0.0")), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOf8WhenIsEmptyArray}, hash: 15D0A4FD4A334443806E2562511CA26F
    @Test()
    void indexOf8WhenIsEmptyArray() {
        /* Branches:
         * (isEmpty(array)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(doubleArray)).thenReturn(true);
            //Act Statement(s)
            int result = ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"), 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(-1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(doubleArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOf8WhenDoubleIsNaNElement}, hash: DB997BA32E511B26D99C8015775C4C59
    @Disabled()
    @Test()
    void indexOf8WhenDoubleIsNaNElement() {
        /* Branches:
         * (isEmpty(array)) : false
         * (i < array.length) : true
         * (valueToFind == element) : false
         * (searchNaN) : true
         * (Double.isNaN(element)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(doubleArray)).thenReturn(false);
            //Act Statement(s)
            int result = ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"), 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(doubleArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOf8WhenDoubleNotIsNaNElement}, hash: 76D4E2BE2BC71890312BB74BD5758CFF
    @Test()
    void indexOf8WhenDoubleNotIsNaNElement() {
        /* Branches:
         * (isEmpty(array)) : false
         * (i < array.length) : true
         * (valueToFind == element) : false
         * (searchNaN) : true
         * (Double.isNaN(element)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(doubleArray)).thenReturn(false);
            //Act Statement(s)
            int result = ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"), 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(-1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(doubleArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOf9WhenIsEmptyArray}, hash: 4427693329385C0F0704150AC6FC32D5
    @Test()
    void indexOf9WhenIsEmptyArray() {
        /* Branches:
         * (isEmpty(array)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(doubleArray)).thenReturn(true);
            //Act Statement(s)
            int result = ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"), 0, Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(-1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(doubleArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOf9WhenIIndexOfArrayLessThanOrEqualsToMax}, hash: 95A2D6F99588DF47BCBCF35EB6C1C3E9
    @Test()
    void indexOf9WhenIIndexOfArrayLessThanOrEqualsToMax() {
        /* Branches:
         * (isEmpty(array)) : false
         * (i < array.length) : true
         * (array[i] >= min) : true
         * (array[i] <= max) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] { Double.parseDouble("0.0") };
            arrayUtils.when(() -> ArrayUtils.isEmpty(doubleArray)).thenReturn(false);
            //Act Statement(s)
            int result = ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"), 0, Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(doubleArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOf9WhenIIndexOfArrayGreaterThanMax}, hash: E9CE2D03C4E0CDE935AE25C34366E276
    @Test()
    void indexOf9WhenIIndexOfArrayGreaterThanMax() {
        /* Branches:
         * (isEmpty(array)) : false
         * (i < array.length) : true
         * (array[i] >= min) : true
         * (array[i] <= max) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] { Double.parseDouble("0.0") };
            arrayUtils.when(() -> ArrayUtils.isEmpty(doubleArray)).thenReturn(false);
            //Act Statement(s)
            int result = ArrayUtils.indexOf(doubleArray, Double.parseDouble("-0.25"), 0, Double.parseDouble("-0.25"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(-1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(doubleArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOf10Test}, hash: 9054DEAC2D502626955F88DA115CFED6
    @Test()
    void indexOf10Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(floatArray, Float.parseFloat("0.0"), 0)).thenReturn(0);
            //Act Statement(s)
            int result = ArrayUtils.indexOf(floatArray, Float.parseFloat("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.indexOf(floatArray, Float.parseFloat("0.0"), 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOf11WhenIsEmptyArray}, hash: 1E8EF334F9B09FBA77087BB5A7233BC5
    @Test()
    void indexOf11WhenIsEmptyArray() {
        /* Branches:
         * (isEmpty(array)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(floatArray)).thenReturn(true);
            //Act Statement(s)
            int result = ArrayUtils.indexOf(floatArray, Float.parseFloat("0.0"), 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(-1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(floatArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOf11WhenFloatIsNaNElement}, hash: B5C4ACDAB892FDC44082D358ED5D6123
    @Disabled()
    @Test()
    void indexOf11WhenFloatIsNaNElement() {
        /* Branches:
         * (isEmpty(array)) : false
         * (i < array.length) : true
         * (valueToFind == element) : false
         * (searchNaN) : true
         * (Float.isNaN(element)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(floatArray)).thenReturn(false);
            //Act Statement(s)
            int result = ArrayUtils.indexOf(floatArray, Float.parseFloat("0.0"), 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(floatArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOf11WhenFloatNotIsNaNElement}, hash: 213046F1C44C924038CC35C93A941309
    @Test()
    void indexOf11WhenFloatNotIsNaNElement() {
        /* Branches:
         * (isEmpty(array)) : false
         * (i < array.length) : true
         * (valueToFind == element) : false
         * (searchNaN) : true
         * (Float.isNaN(element)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(floatArray)).thenReturn(false);
            //Act Statement(s)
            int result = ArrayUtils.indexOf(floatArray, Float.parseFloat("0.0"), 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(-1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(floatArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOf12Test}, hash: AD864BE430E201193F00E8240088F979
    @Test()
    void indexOf12Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(intArray, 0, 0)).thenReturn(0);
            //Act Statement(s)
            int result = ArrayUtils.indexOf(intArray, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.indexOf(intArray, 0, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOf13WhenArrayIsNull}, hash: 53E8DD830A07968ACABD2BEC8822C421
    @Test()
    void indexOf13WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        int[] _int = null;
        //Act Statement(s)
        int result = ArrayUtils.indexOf(_int, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOf13WhenValueToFindEqualsIIndexOfArray}, hash: 5DDBF28A4559188876BC21FD00997B8F
    @Test()
    void indexOf13WhenValueToFindEqualsIIndexOfArray() {
        /* Branches:
         * (array == null) : false
         * (i < array.length) : true
         * (valueToFind == array[i]) : true
         */
        //Arrange Statement(s)
        int[] intArray = new int[] { 0 };
        //Act Statement(s)
        int result = ArrayUtils.indexOf(intArray, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${indexOf13WhenValueToFindNotEqualsIIndexOfArray}, hash: D7A4FAFDAF614216CC3BABCFA5426F2E
    @Test()
    void indexOf13WhenValueToFindNotEqualsIIndexOfArray() {
        /* Branches:
         * (array == null) : false
         * (i < array.length) : true
         * (valueToFind == array[i]) : false
         */
        //Arrange Statement(s)
        int[] intArray = new int[] { 1 };
        //Act Statement(s)
        int result = ArrayUtils.indexOf(intArray, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOf14Test}, hash: B3474F08677F1ECF7E8FA810C56DD675
    @Test()
    void indexOf14Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(longArray, 0L, 0)).thenReturn(0);
            //Act Statement(s)
            int result = ArrayUtils.indexOf(longArray, 0L);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.indexOf(longArray, 0L, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOf15WhenArrayIsNull}, hash: 5506826EA36D825FE0E78E87D7FD0722
    @Test()
    void indexOf15WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        long[] _long = null;
        //Act Statement(s)
        int result = ArrayUtils.indexOf(_long, 0L, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOf15WhenValueToFindEqualsIIndexOfArray}, hash: CE9AFF7A85FC8C87F86CCA83616651DC
    @Test()
    void indexOf15WhenValueToFindEqualsIIndexOfArray() {
        /* Branches:
         * (array == null) : false
         * (i < array.length) : true
         * (valueToFind == array[i]) : true
         */
        //Arrange Statement(s)
        long[] longArray = new long[] { 0L };
        //Act Statement(s)
        int result = ArrayUtils.indexOf(longArray, 0L, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${indexOf15WhenValueToFindNotEqualsIIndexOfArray}, hash: 629B56FE2CFD03B528B186939DA5C3B9
    @Test()
    void indexOf15WhenValueToFindNotEqualsIIndexOfArray() {
        /* Branches:
         * (array == null) : false
         * (i < array.length) : true
         * (valueToFind == array[i]) : false
         */
        //Arrange Statement(s)
        long[] longArray = new long[] { 1L };
        //Act Statement(s)
        int result = ArrayUtils.indexOf(longArray, 0L, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOf16Test}, hash: 376B0D215E73D59B31FAAD8F16840F79
    @Test()
    void indexOf16Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            Object object = new Object();
            arrayUtils.when(() -> ArrayUtils.indexOf(objectArray, object, 0)).thenReturn(0);
            //Act Statement(s)
            int result = ArrayUtils.indexOf(objectArray, object);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.indexOf(objectArray, object, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOf17WhenArrayIsNull}, hash: A4CF6913F757739FC6B5647CB948FDB8
    @Test()
    void indexOf17WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Object[] object = null;
        Object object2 = new Object();
        //Act Statement(s)
        int result = ArrayUtils.indexOf(object, object2, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOf17WhenIIndexOfArrayIsNull}, hash: BF150D2B33487F770824578E8B822777
    @Test()
    void indexOf17WhenIIndexOfArrayIsNull() {
        /* Branches:
         * (array == null) : false
         * (objectToFind == null) : true
         * (i < array.length) : true
         * (array[i] == null) : true
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] { (Object) null };
        Object object = null;
        //Act Statement(s)
        int result = ArrayUtils.indexOf(objectArray, object, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${indexOf17WhenIIndexOfArrayIsNotNull}, hash: 3B114BF1E476B4571D9220A7D8EA8793
    @Test()
    void indexOf17WhenIIndexOfArrayIsNotNull() {
        /* Branches:
         * (array == null) : false
         * (objectToFind == null) : true
         * (i < array.length) : true
         * (array[i] == null) : false
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object[] objectArray = new Object[] { object };
        Object object2 = null;
        //Act Statement(s)
        int result = ArrayUtils.indexOf(objectArray, object2, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOf17WhenObjectToFindEqualsIIndexOfArray}, hash: 54E8945445B6CAFF2ED9D3DE70A90687
    @Test()
    void indexOf17WhenObjectToFindEqualsIIndexOfArray() {
        /* Branches:
         * (array == null) : false
         * (objectToFind == null) : false
         * (i < array.length) : true
         * (objectToFind.equals(array[i])) : true
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object[] objectArray = new Object[] { object };
        //Act Statement(s)
        int result = ArrayUtils.indexOf(objectArray, object, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${indexOf17WhenObjectToFindNotEqualsIIndexOfArray}, hash: E6504767C41F739162A05D78A746EF17
    @Test()
    void indexOf17WhenObjectToFindNotEqualsIIndexOfArray() {
        /* Branches:
         * (array == null) : false
         * (objectToFind == null) : false
         * (i < array.length) : true
         * (objectToFind.equals(array[i])) : false
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object[] objectArray = new Object[] { object };
        Object object2 = new Object();
        //Act Statement(s)
        int result = ArrayUtils.indexOf(objectArray, object2, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOf18Test}, hash: FD4E13E89E99D885D5C05ABA9F86E1A0
    @Test()
    void indexOf18Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(shortArray, (short) 0, 0)).thenReturn(0);
            //Act Statement(s)
            int result = ArrayUtils.indexOf(shortArray, (short) 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.indexOf(shortArray, (short) 0, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${indexOf19WhenArrayIsNull}, hash: 55757FC95070F8B3D93E7415FCC007DA
    @Test()
    void indexOf19WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        short[] _short = null;
        //Act Statement(s)
        int result = ArrayUtils.indexOf(_short, (short) 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${indexOf19WhenValueToFindEqualsIIndexOfArray}, hash: 069C1C598729D7A1782D2A1F9A8C6F1C
    @Test()
    void indexOf19WhenValueToFindEqualsIIndexOfArray() {
        /* Branches:
         * (array == null) : false
         * (i < array.length) : true
         * (valueToFind == array[i]) : true
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] { (short) 0 };
        //Act Statement(s)
        int result = ArrayUtils.indexOf(shortArray, (short) 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${indexOf19WhenValueToFindNotEqualsIIndexOfArray}, hash: DB08E9A41E728350E3DD30CC4F2F7D67
    @Test()
    void indexOf19WhenValueToFindNotEqualsIIndexOfArray() {
        /* Branches:
         * (array == null) : false
         * (i < array.length) : true
         * (valueToFind == array[i]) : false
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] { (short) 1 };
        //Act Statement(s)
        int result = ArrayUtils.indexOf(shortArray, (short) 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${insertWhenArrayIsNull}, hash: 198C48666960C6A283A404B1031710E7
    @Test()
    void insertWhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        boolean[] _boolean = null;
        boolean[] booleanArray = new boolean[] {};
        //Act Statement(s)
        boolean[] result = ArrayUtils.insert(0, _boolean, booleanArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${insertWhenIsEmptyValues}, hash: 7AFA78C2E64D9EA4266EF6FC7DC1F7EA
    @Test()
    void insertWhenIsEmptyValues() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(booleanArray)).thenReturn(true);
            boolean[] booleanArray2 = new boolean[] {};
            boolean[] booleanArray3 = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.clone(booleanArray3)).thenReturn(booleanArray2);
            //Act Statement(s)
            boolean[] result = ArrayUtils.insert(0, booleanArray3, booleanArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(booleanArray2));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(booleanArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(booleanArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${insertWhenIndexGreaterThanArrayLengthThrowsIndexOutOfBoundsException}, hash: 6EB236C46D263439287D2C2FD8FAA7FC
    @Test()
    void insertWhenIndexGreaterThanArrayLengthThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : false
         * (index < 0) : false
         * (index > array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(booleanArray)).thenReturn(false);
            boolean[] booleanArray2 = new boolean[] {};
            //Act Statement(s)
            final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
                ArrayUtils.insert(2, booleanArray2, booleanArray);
            });
            IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("Index: 2, Length: 0");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(booleanArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${insertWhenIndexLessThanArrayLength}, hash: F0F73B046AADF619D6DD9BB9368B3CF6
    @Test()
    void insertWhenIndexLessThanArrayLength() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : false
         * (index < 0) : false
         * (index > array.length) : false
         * (index > 0) : true
         * (index < array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(booleanArray)).thenReturn(false);
            boolean[] booleanArray2 = new boolean[] { false, false };
            //Act Statement(s)
            boolean[] result = ArrayUtils.insert(1, booleanArray2, booleanArray);
            boolean[] booleanResultArray = new boolean[] { false, false };
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(booleanResultArray));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(booleanArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${insert1WhenArrayIsNull}, hash: 54671EA804CEDEE4D7EBB8A95B4D53D7
    @Test()
    void insert1WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        byte[] _byte = null;
        byte[] byteArray = new byte[] {};
        //Act Statement(s)
        byte[] result = ArrayUtils.insert(0, _byte, byteArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${insert1WhenIsEmptyValues}, hash: 0B4498F0C99AD7E939D0558416FE912F
    @Test()
    void insert1WhenIsEmptyValues() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(byteArray)).thenReturn(true);
            byte[] byteArray2 = new byte[] {};
            byte[] byteArray3 = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.clone(byteArray3)).thenReturn(byteArray2);
            //Act Statement(s)
            byte[] result = ArrayUtils.insert(0, byteArray3, byteArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(byteArray2));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(byteArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(byteArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${insert1WhenIndexGreaterThanArrayLengthThrowsIndexOutOfBoundsException}, hash: 15F7D8908B85357F03196F164EDF5A58
    @Test()
    void insert1WhenIndexGreaterThanArrayLengthThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : false
         * (index < 0) : false
         * (index > array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(byteArray)).thenReturn(false);
            byte[] byteArray2 = new byte[] {};
            //Act Statement(s)
            final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
                ArrayUtils.insert(2, byteArray2, byteArray);
            });
            IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("Index: 2, Length: 0");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(byteArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${insert1WhenIndexLessThanArrayLength}, hash: F846780A0E4CE409818EFA409CFBF347
    @Disabled()
    @Test()
    void insert1WhenIndexLessThanArrayLength() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : false
         * (index < 0) : false
         * (index > array.length) : false
         * (index > 0) : true
         * (index < array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(byteArray)).thenReturn(false);
            byte[] byteArray2 = new byte[] { (byte) 0, (byte) 1 };
            //Act Statement(s)
            byte[] result = ArrayUtils.insert(1, byteArray2, byteArray);
            byte[] byteResultArray = new byte[] { (byte) 0, (byte) 0 };
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(byteResultArray));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(byteArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${insert2WhenArrayIsNull}, hash: 1545F78FF8D904B53839FD1A571FC1B4
    @Test()
    void insert2WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        char[] _char = null;
        char[] charArray = new char[] {};
        //Act Statement(s)
        char[] result = ArrayUtils.insert(0, _char, charArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${insert2WhenIsEmptyValues}, hash: FF81584D5FE27D0131BE10D3BEA7B4D6
    @Test()
    void insert2WhenIsEmptyValues() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(charArray)).thenReturn(true);
            char[] charArray2 = new char[] {};
            char[] charArray3 = new char[] {};
            arrayUtils.when(() -> ArrayUtils.clone(charArray3)).thenReturn(charArray2);
            //Act Statement(s)
            char[] result = ArrayUtils.insert(0, charArray3, charArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(charArray2));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(charArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(charArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${insert2WhenIndexGreaterThanArrayLengthThrowsIndexOutOfBoundsException}, hash: 91235673D4CA8519810499B46411A931
    @Test()
    void insert2WhenIndexGreaterThanArrayLengthThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : false
         * (index < 0) : false
         * (index > array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(charArray)).thenReturn(false);
            char[] charArray2 = new char[] {};
            //Act Statement(s)
            final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
                ArrayUtils.insert(2, charArray2, charArray);
            });
            IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("Index: 2, Length: 0");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(charArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${insert2WhenIndexLessThanArrayLength}, hash: C9F7EAACCB8167548C60B1AE5270ED00
    @Test()
    void insert2WhenIndexLessThanArrayLength() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : false
         * (index < 0) : false
         * (index > array.length) : false
         * (index > 0) : true
         * (index < array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(charArray)).thenReturn(false);
            char[] charArray2 = new char[] { 'A', 'A' };
            //Act Statement(s)
            char[] result = ArrayUtils.insert(1, charArray2, charArray);
            char[] charResultArray = new char[] { 'A', 'A' };
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(charResultArray));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(charArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${insert3WhenArrayIsNull}, hash: 7892A4B52A00F5C360829BC87C1A23F2
    @Test()
    void insert3WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        double[] _double = null;
        double[] doubleArray = new double[] {};
        //Act Statement(s)
        double[] result = ArrayUtils.insert(0, _double, doubleArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${insert3WhenIsEmptyValues}, hash: 055CF7AA57803FE1696101488B07A260
    @Test()
    void insert3WhenIsEmptyValues() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(doubleArray)).thenReturn(true);
            double[] doubleArray2 = new double[] {};
            double[] doubleArray3 = new double[] {};
            arrayUtils.when(() -> ArrayUtils.clone(doubleArray3)).thenReturn(doubleArray2);
            //Act Statement(s)
            double[] result = ArrayUtils.insert(0, doubleArray3, doubleArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(doubleArray2));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(doubleArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(doubleArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${insert3WhenIndexGreaterThanArrayLengthThrowsIndexOutOfBoundsException}, hash: 25A7E7AE0AEBF113B14D53125A627A0D
    @Test()
    void insert3WhenIndexGreaterThanArrayLengthThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : false
         * (index < 0) : false
         * (index > array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(doubleArray)).thenReturn(false);
            double[] doubleArray2 = new double[] {};
            //Act Statement(s)
            final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
                ArrayUtils.insert(2, doubleArray2, doubleArray);
            });
            IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("Index: 2, Length: 0");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(doubleArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${insert3WhenIndexLessThanArrayLength}, hash: 3586A6D1ED82603789E9A0E5B3882C98
    @Disabled()
    @Test()
    void insert3WhenIndexLessThanArrayLength() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : false
         * (index < 0) : false
         * (index > array.length) : false
         * (index > 0) : true
         * (index < array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(doubleArray)).thenReturn(false);
            double[] doubleArray2 = new double[] { Double.parseDouble("0"), Double.parseDouble("1") };
            //Act Statement(s)
            double[] result = ArrayUtils.insert(1, doubleArray2, doubleArray);
            double[] doubleResultArray = new double[] { Double.parseDouble("0.0"), Double.parseDouble("0.0") };
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(doubleResultArray));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(doubleArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${insert4WhenArrayIsNull}, hash: 93334CD1B1909E314334B5887B7C2B6A
    @Test()
    void insert4WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        float[] _float = null;
        float[] floatArray = new float[] {};
        //Act Statement(s)
        float[] result = ArrayUtils.insert(0, _float, floatArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${insert4WhenIsEmptyValues}, hash: FDE0AB5D3D323B4BD2A592802DED04D0
    @Test()
    void insert4WhenIsEmptyValues() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(floatArray)).thenReturn(true);
            float[] floatArray2 = new float[] {};
            float[] floatArray3 = new float[] {};
            arrayUtils.when(() -> ArrayUtils.clone(floatArray3)).thenReturn(floatArray2);
            //Act Statement(s)
            float[] result = ArrayUtils.insert(0, floatArray3, floatArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(floatArray2));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(floatArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(floatArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${insert4WhenIndexGreaterThanArrayLengthThrowsIndexOutOfBoundsException}, hash: D1D440D5D7BD38E3C0B9216849497372
    @Test()
    void insert4WhenIndexGreaterThanArrayLengthThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : false
         * (index < 0) : false
         * (index > array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(floatArray)).thenReturn(false);
            float[] floatArray2 = new float[] {};
            //Act Statement(s)
            final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
                ArrayUtils.insert(2, floatArray2, floatArray);
            });
            IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("Index: 2, Length: 0");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(floatArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${insert4WhenIndexLessThanArrayLength}, hash: E01E8B37698E2D9EE54CD108616103E1
    @Disabled()
    @Test()
    void insert4WhenIndexLessThanArrayLength() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : false
         * (index < 0) : false
         * (index > array.length) : false
         * (index > 0) : true
         * (index < array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(floatArray)).thenReturn(false);
            float[] floatArray2 = new float[] { Float.parseFloat("0"), Float.parseFloat("1") };
            //Act Statement(s)
            float[] result = ArrayUtils.insert(1, floatArray2, floatArray);
            float[] floatResultArray = new float[] { Float.parseFloat("0.0"), Float.parseFloat("0.0") };
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(floatResultArray));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(floatArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${insert5WhenArrayIsNull}, hash: ED990E35CB06BAE4F27161575982C2DF
    @Test()
    void insert5WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        int[] _int = null;
        int[] intArray = new int[] {};
        //Act Statement(s)
        int[] result = ArrayUtils.insert(0, _int, intArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${insert5WhenIsEmptyValues}, hash: FD7E479BBC88DC279C6396C6B04CFF1A
    @Test()
    void insert5WhenIsEmptyValues() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(intArray)).thenReturn(true);
            int[] intArray2 = new int[] {};
            int[] intArray3 = new int[] {};
            arrayUtils.when(() -> ArrayUtils.clone(intArray3)).thenReturn(intArray2);
            //Act Statement(s)
            int[] result = ArrayUtils.insert(0, intArray3, intArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(intArray2));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(intArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(intArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${insert5WhenIndexGreaterThanArrayLengthThrowsIndexOutOfBoundsException}, hash: CE8A4F3723ACEFC756EEAD340ACE4A9C
    @Test()
    void insert5WhenIndexGreaterThanArrayLengthThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : false
         * (index < 0) : false
         * (index > array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(intArray)).thenReturn(false);
            int[] intArray2 = new int[] {};
            //Act Statement(s)
            final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
                ArrayUtils.insert(2, intArray2, intArray);
            });
            IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("Index: 2, Length: 0");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(intArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${insert5WhenIndexLessThanArrayLength}, hash: B707D04457AC9BC5E9E85F7524F39475
    @Disabled()
    @Test()
    void insert5WhenIndexLessThanArrayLength() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : false
         * (index < 0) : false
         * (index > array.length) : false
         * (index > 0) : true
         * (index < array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(intArray)).thenReturn(false);
            int[] intArray2 = new int[] { 0, 1 };
            //Act Statement(s)
            int[] result = ArrayUtils.insert(1, intArray2, intArray);
            int[] intResultArray = new int[] { 0, 0 };
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(intResultArray));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(intArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${insert6WhenArrayIsNull}, hash: 4C57D0CE019B6B0AFDB3596F00650CA3
    @Test()
    void insert6WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        long[] _long = null;
        long[] longArray = new long[] {};
        //Act Statement(s)
        long[] result = ArrayUtils.insert(0, _long, longArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${insert6WhenIsEmptyValues}, hash: 4A97AADB1258C5A6837A00A74E28C029
    @Test()
    void insert6WhenIsEmptyValues() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(longArray)).thenReturn(true);
            long[] longArray2 = new long[] {};
            long[] longArray3 = new long[] {};
            arrayUtils.when(() -> ArrayUtils.clone(longArray3)).thenReturn(longArray2);
            //Act Statement(s)
            long[] result = ArrayUtils.insert(0, longArray3, longArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(longArray2));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(longArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(longArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${insert6WhenIndexGreaterThanArrayLengthThrowsIndexOutOfBoundsException}, hash: 9D30433C2F7D37BAC910D7065ECC84CA
    @Test()
    void insert6WhenIndexGreaterThanArrayLengthThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : false
         * (index < 0) : false
         * (index > array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(longArray)).thenReturn(false);
            long[] longArray2 = new long[] {};
            //Act Statement(s)
            final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
                ArrayUtils.insert(2, longArray2, longArray);
            });
            IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("Index: 2, Length: 0");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(longArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${insert6WhenIndexLessThanArrayLength}, hash: 4F595C514729BFEED52B2BED462DE590
    @Disabled()
    @Test()
    void insert6WhenIndexLessThanArrayLength() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : false
         * (index < 0) : false
         * (index > array.length) : false
         * (index > 0) : true
         * (index < array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(longArray)).thenReturn(false);
            long[] longArray2 = new long[] { 0L, 1L };
            //Act Statement(s)
            long[] result = ArrayUtils.insert(1, longArray2, longArray);
            long[] longResultArray = new long[] { 0L, 0L };
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(longResultArray));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(longArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${insert7WhenArrayIsNull}, hash: A63CB94FD11E89DE41D10E167526C822
    @Test()
    void insert7WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        short[] _short = null;
        short[] shortArray = new short[] {};
        //Act Statement(s)
        short[] result = ArrayUtils.insert(0, _short, shortArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${insert7WhenIsEmptyValues}, hash: 90392574A31864995318F2AF96827EEA
    @Test()
    void insert7WhenIsEmptyValues() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(shortArray)).thenReturn(true);
            short[] shortArray2 = new short[] {};
            short[] shortArray3 = new short[] {};
            arrayUtils.when(() -> ArrayUtils.clone(shortArray3)).thenReturn(shortArray2);
            //Act Statement(s)
            short[] result = ArrayUtils.insert(0, shortArray3, shortArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(shortArray2));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(shortArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(shortArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${insert7WhenIndexGreaterThanArrayLengthThrowsIndexOutOfBoundsException}, hash: 4837AEC93F4C4B1DF10A3FCB711E0C6F
    @Test()
    void insert7WhenIndexGreaterThanArrayLengthThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : false
         * (index < 0) : false
         * (index > array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(shortArray)).thenReturn(false);
            short[] shortArray2 = new short[] {};
            //Act Statement(s)
            final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
                ArrayUtils.insert(2, shortArray2, shortArray);
            });
            IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("Index: 2, Length: 0");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(shortArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${insert7WhenIndexLessThanArrayLength}, hash: ED4EB5BF8EAEF7552FF3CF3E5F840795
    @Disabled()
    @Test()
    void insert7WhenIndexLessThanArrayLength() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : false
         * (index < 0) : false
         * (index > array.length) : false
         * (index > 0) : true
         * (index < array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(shortArray)).thenReturn(false);
            short[] shortArray2 = new short[] { (short) 0, (short) 1 };
            //Act Statement(s)
            short[] result = ArrayUtils.insert(1, shortArray2, shortArray);
            short[] shortResultArray = new short[] { (short) 0, (short) 0 };
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(shortResultArray));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(shortArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${insert8WhenArrayIsNull}, hash: D6ABCCACA0F257B2534E52431C3DF28F
    @Test()
    void insert8WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Object[] object = null;
        Object[] objectArray = new Object[] {};
        //Act Statement(s)
        Object[] result = ArrayUtils.insert(0, object, objectArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${insert8WhenIsEmptyValues}, hash: 3E13D266F6069807F60789557D9C0C8B
    @Test()
    void insert8WhenIsEmptyValues() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(objectArray)).thenReturn(true);
            Object[] objectArray2 = new Object[] {};
            Object[] objectArray3 = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.clone(objectArray3)).thenReturn(objectArray2);
            //Act Statement(s)
            Object[] result = ArrayUtils.insert(0, objectArray3, objectArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(objectArray2));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(objectArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(objectArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${insert8WhenIndexGreaterThanArrayLengthThrowsIndexOutOfBoundsException}, hash: A1D8007702C55A5E900776AD16187ED5
    @Test()
    void insert8WhenIndexGreaterThanArrayLengthThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : false
         * (index < 0) : false
         * (index > array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(objectArray)).thenReturn(false);
            Object[] objectArray2 = new Object[] {};
            //Act Statement(s)
            final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
                ArrayUtils.insert(2, objectArray2, objectArray);
            });
            IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("Index: 2, Length: 0");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(objectArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${insert8WhenIndexLessThanArrayLength}, hash: 7140D8FF0C11D22AFE54BBF13186A856
    @Test()
    void insert8WhenIndexLessThanArrayLength() {
        /* Branches:
         * (array == null) : false
         * (isEmpty(values)) : false
         * (index < 0) : false
         * (index > array.length) : false
         * (index > 0) : true
         * (index < array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(objectArray)).thenReturn(false);
            Object object = new Object();
            Object object2 = new Object();
            Object[] objectArray2 = new Object[] { object, object2 };
            //Act Statement(s)
            Object[] result = ArrayUtils.insert(1, objectArray2, objectArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(objectArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isArrayIndexValidWhenGetLengthArrayGreaterThanIndex}, hash: E6C66D3E86B62B56F1712009E0D229B4
    @Test()
    void isArrayIndexValidWhenGetLengthArrayGreaterThanIndex() {
        /* Branches:
         * (index >= 0) : true
         * (getLength(array) > index) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(objectArray)).thenReturn(2);
            //Act Statement(s)
            boolean result = ArrayUtils.isArrayIndexValid(objectArray, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(objectArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isArrayIndexValidWhenGetLengthArrayNotGreaterThanIndex}, hash: 4D1A6810094A9E2893399759675A19D3
    @Test()
    void isArrayIndexValidWhenGetLengthArrayNotGreaterThanIndex() {
        /* Branches:
         * (index >= 0) : true
         * (getLength(array) > index) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(objectArray)).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.isArrayIndexValid(objectArray, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(objectArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isEmptyWhenGetLengthArrayEquals0}, hash: FE4D86C319C1C6294A124E7AF1B6356A
    @Test()
    void isEmptyWhenGetLengthArrayEquals0() {
        /* Branches:
         * (getLength(array) == 0) : true  #  inside isArrayEmpty method
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(booleanArray)).thenReturn(0);
            //Act Statement(s)
            boolean result = ArrayUtils.isEmpty(booleanArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(booleanArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isEmptyWhenGetLengthArrayNotEquals0}, hash: 43A4CD5C4D2342373B9C957B6057F013
    @Test()
    void isEmptyWhenGetLengthArrayNotEquals0() {
        /* Branches:
         * (getLength(array) == 0) : false  #  inside isArrayEmpty method
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(booleanArray)).thenReturn(-1);
            //Act Statement(s)
            boolean result = ArrayUtils.isEmpty(booleanArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(booleanArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isEmpty1WhenGetLengthArrayEquals0}, hash: 32A0018C0D7A9AAA0959D6A2CF615F32
    @Test()
    void isEmpty1WhenGetLengthArrayEquals0() {
        /* Branches:
         * (getLength(array) == 0) : true  #  inside isArrayEmpty method
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(byteArray)).thenReturn(0);
            //Act Statement(s)
            boolean result = ArrayUtils.isEmpty(byteArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(byteArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isEmpty1WhenGetLengthArrayNotEquals0}, hash: 26F70B79B6A8B87B737D6C31802AC373
    @Test()
    void isEmpty1WhenGetLengthArrayNotEquals0() {
        /* Branches:
         * (getLength(array) == 0) : false  #  inside isArrayEmpty method
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(byteArray)).thenReturn(-1);
            //Act Statement(s)
            boolean result = ArrayUtils.isEmpty(byteArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(byteArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isEmpty2WhenGetLengthArrayEquals0}, hash: 307983149D9AB79F34A4CFC305A5022D
    @Test()
    void isEmpty2WhenGetLengthArrayEquals0() {
        /* Branches:
         * (getLength(array) == 0) : true  #  inside isArrayEmpty method
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(charArray)).thenReturn(0);
            //Act Statement(s)
            boolean result = ArrayUtils.isEmpty(charArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(charArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isEmpty2WhenGetLengthArrayNotEquals0}, hash: 546839765A5AD738200EA97A8502F69E
    @Test()
    void isEmpty2WhenGetLengthArrayNotEquals0() {
        /* Branches:
         * (getLength(array) == 0) : false  #  inside isArrayEmpty method
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(charArray)).thenReturn(-1);
            //Act Statement(s)
            boolean result = ArrayUtils.isEmpty(charArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(charArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isEmpty3WhenGetLengthArrayEquals0}, hash: 047F75FFB7144D2B73CDC898A99B64FA
    @Test()
    void isEmpty3WhenGetLengthArrayEquals0() {
        /* Branches:
         * (getLength(array) == 0) : true  #  inside isArrayEmpty method
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(doubleArray)).thenReturn(0);
            //Act Statement(s)
            boolean result = ArrayUtils.isEmpty(doubleArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(doubleArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isEmpty3WhenGetLengthArrayNotEquals0}, hash: D1C61863923DD46230BD1B655AC6AD64
    @Test()
    void isEmpty3WhenGetLengthArrayNotEquals0() {
        /* Branches:
         * (getLength(array) == 0) : false  #  inside isArrayEmpty method
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(doubleArray)).thenReturn(-1);
            //Act Statement(s)
            boolean result = ArrayUtils.isEmpty(doubleArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(doubleArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isEmpty4WhenGetLengthArrayEquals0}, hash: 5EDD8695E01CE02C779845F7E38A144F
    @Test()
    void isEmpty4WhenGetLengthArrayEquals0() {
        /* Branches:
         * (getLength(array) == 0) : true  #  inside isArrayEmpty method
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(floatArray)).thenReturn(0);
            //Act Statement(s)
            boolean result = ArrayUtils.isEmpty(floatArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(floatArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isEmpty4WhenGetLengthArrayNotEquals0}, hash: 0B58D4483B24A6EF6B027D37E36678A1
    @Test()
    void isEmpty4WhenGetLengthArrayNotEquals0() {
        /* Branches:
         * (getLength(array) == 0) : false  #  inside isArrayEmpty method
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(floatArray)).thenReturn(-1);
            //Act Statement(s)
            boolean result = ArrayUtils.isEmpty(floatArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(floatArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isEmpty5WhenGetLengthArrayEquals0}, hash: 4089B2CBADE20C6063FE7784CF34E362
    @Test()
    void isEmpty5WhenGetLengthArrayEquals0() {
        /* Branches:
         * (getLength(array) == 0) : true  #  inside isArrayEmpty method
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(intArray)).thenReturn(0);
            //Act Statement(s)
            boolean result = ArrayUtils.isEmpty(intArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(intArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isEmpty5WhenGetLengthArrayNotEquals0}, hash: 06BA5F17E4D7857005549BBA7DEBEE13
    @Test()
    void isEmpty5WhenGetLengthArrayNotEquals0() {
        /* Branches:
         * (getLength(array) == 0) : false  #  inside isArrayEmpty method
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(intArray)).thenReturn(-1);
            //Act Statement(s)
            boolean result = ArrayUtils.isEmpty(intArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(intArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isEmpty6WhenGetLengthArrayEquals0}, hash: F9DC28718EEC3B8031E1A889BB2ADA14
    @Test()
    void isEmpty6WhenGetLengthArrayEquals0() {
        /* Branches:
         * (getLength(array) == 0) : true  #  inside isArrayEmpty method
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(longArray)).thenReturn(0);
            //Act Statement(s)
            boolean result = ArrayUtils.isEmpty(longArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(longArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isEmpty6WhenGetLengthArrayNotEquals0}, hash: C521BBF3B7D346604F4AC53C0129BAC1
    @Test()
    void isEmpty6WhenGetLengthArrayNotEquals0() {
        /* Branches:
         * (getLength(array) == 0) : false  #  inside isArrayEmpty method
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(longArray)).thenReturn(-1);
            //Act Statement(s)
            boolean result = ArrayUtils.isEmpty(longArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(longArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isEmpty7WhenGetLengthArrayEquals0}, hash: 037107C6BAE34CDD11297BEDC3FE8423
    @Test()
    void isEmpty7WhenGetLengthArrayEquals0() {
        /* Branches:
         * (getLength(array) == 0) : true  #  inside isArrayEmpty method
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(objectArray)).thenReturn(0);
            //Act Statement(s)
            boolean result = ArrayUtils.isEmpty(objectArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(objectArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isEmpty7WhenGetLengthArrayNotEquals0}, hash: 2222B44547D2FE9990C780055D45FD0C
    @Test()
    void isEmpty7WhenGetLengthArrayNotEquals0() {
        /* Branches:
         * (getLength(array) == 0) : false  #  inside isArrayEmpty method
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(objectArray)).thenReturn(-1);
            //Act Statement(s)
            boolean result = ArrayUtils.isEmpty(objectArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(objectArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isEmpty8WhenGetLengthArrayEquals0}, hash: 3FD1A5BFD65D601A5CB522A4221E2850
    @Test()
    void isEmpty8WhenGetLengthArrayEquals0() {
        /* Branches:
         * (getLength(array) == 0) : true  #  inside isArrayEmpty method
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(shortArray)).thenReturn(0);
            //Act Statement(s)
            boolean result = ArrayUtils.isEmpty(shortArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(shortArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isEmpty8WhenGetLengthArrayNotEquals0}, hash: CF786C8E4888C7C151F0C2145BDA0341
    @Test()
    void isEmpty8WhenGetLengthArrayNotEquals0() {
        /* Branches:
         * (getLength(array) == 0) : false  #  inside isArrayEmpty method
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(shortArray)).thenReturn(-1);
            //Act Statement(s)
            boolean result = ArrayUtils.isEmpty(shortArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(shortArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isEqualsWhenNewEqualsBuilderAppendArray1Array2IsEquals}, hash: 41EA86A66319969D1C3D67590484BA63
    @Disabled()
    @Test()
    void isEqualsWhenNewEqualsBuilderAppendArray1Array2IsEquals() {
        /* Branches:
         * (new EqualsBuilder().append(array1, array2).isEquals()) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: object of type EqualsBuilder - Method: append
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        //Act Statement(s)
        boolean result = ArrayUtils.isEquals(object, object2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isEqualsWhenNewEqualsBuilderAppendArray1Array2NotIsEquals}, hash: 6ADBD6DE3576C2930F8D35A7A882CAED
    @Test()
    void isEqualsWhenNewEqualsBuilderAppendArray1Array2NotIsEquals() {
        /* Branches:
         * (new EqualsBuilder().append(array1, array2).isEquals()) : false
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        //Act Statement(s)
        boolean result = ArrayUtils.isEquals(object, object2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isNotEmptyWhenIsEmptyNotArray}, hash: FB418E2CD9D2F9F32DAC3D93435E1CBE
    @Test()
    void isNotEmptyWhenIsEmptyNotArray() {
        /* Branches:
         * (!isEmpty(array)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(booleanArray)).thenReturn(false);
            //Act Statement(s)
            boolean result = ArrayUtils.isNotEmpty(booleanArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(booleanArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNotEmptyWhenIsEmptyArray}, hash: B97AF1D48D5B2632B28256F8D6D90918
    @Test()
    void isNotEmptyWhenIsEmptyArray() {
        /* Branches:
         * (!isEmpty(array)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(booleanArray)).thenReturn(true);
            //Act Statement(s)
            boolean result = ArrayUtils.isNotEmpty(booleanArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(booleanArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNotEmpty1WhenIsEmptyNotArray}, hash: 74B83611A58DF9008137DFF0F9D5B564
    @Test()
    void isNotEmpty1WhenIsEmptyNotArray() {
        /* Branches:
         * (!isEmpty(array)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(byteArray)).thenReturn(false);
            //Act Statement(s)
            boolean result = ArrayUtils.isNotEmpty(byteArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(byteArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNotEmpty1WhenIsEmptyArray}, hash: 4631E396E41FDAECAD052ACDBFB3AA88
    @Test()
    void isNotEmpty1WhenIsEmptyArray() {
        /* Branches:
         * (!isEmpty(array)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(byteArray)).thenReturn(true);
            //Act Statement(s)
            boolean result = ArrayUtils.isNotEmpty(byteArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(byteArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNotEmpty2WhenIsEmptyNotArray}, hash: B504CD6B1C41557AFDD1190588BC8039
    @Test()
    void isNotEmpty2WhenIsEmptyNotArray() {
        /* Branches:
         * (!isEmpty(array)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(charArray)).thenReturn(false);
            //Act Statement(s)
            boolean result = ArrayUtils.isNotEmpty(charArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(charArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNotEmpty2WhenIsEmptyArray}, hash: A25D7A604397BE8FB2F2D630BBE00247
    @Test()
    void isNotEmpty2WhenIsEmptyArray() {
        /* Branches:
         * (!isEmpty(array)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(charArray)).thenReturn(true);
            //Act Statement(s)
            boolean result = ArrayUtils.isNotEmpty(charArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(charArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNotEmpty3WhenIsEmptyNotArray}, hash: 3016A2AB928B818301F9EC419F7B0EF5
    @Test()
    void isNotEmpty3WhenIsEmptyNotArray() {
        /* Branches:
         * (!isEmpty(array)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(doubleArray)).thenReturn(false);
            //Act Statement(s)
            boolean result = ArrayUtils.isNotEmpty(doubleArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(doubleArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNotEmpty3WhenIsEmptyArray}, hash: 99DBF0AAC088CB5328C0F1A4A185E973
    @Test()
    void isNotEmpty3WhenIsEmptyArray() {
        /* Branches:
         * (!isEmpty(array)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(doubleArray)).thenReturn(true);
            //Act Statement(s)
            boolean result = ArrayUtils.isNotEmpty(doubleArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(doubleArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNotEmpty4WhenIsEmptyNotArray}, hash: A75FF4E8E18BFAB24912534209E181C2
    @Test()
    void isNotEmpty4WhenIsEmptyNotArray() {
        /* Branches:
         * (!isEmpty(array)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(floatArray)).thenReturn(false);
            //Act Statement(s)
            boolean result = ArrayUtils.isNotEmpty(floatArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(floatArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNotEmpty4WhenIsEmptyArray}, hash: 031E88D2A7040187A670CDB589A5BF7E
    @Test()
    void isNotEmpty4WhenIsEmptyArray() {
        /* Branches:
         * (!isEmpty(array)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(floatArray)).thenReturn(true);
            //Act Statement(s)
            boolean result = ArrayUtils.isNotEmpty(floatArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(floatArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNotEmpty5WhenIsEmptyNotArray}, hash: C5A45B7952613C5EF73456EC1CBC1D9B
    @Test()
    void isNotEmpty5WhenIsEmptyNotArray() {
        /* Branches:
         * (!isEmpty(array)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(intArray)).thenReturn(false);
            //Act Statement(s)
            boolean result = ArrayUtils.isNotEmpty(intArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(intArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNotEmpty5WhenIsEmptyArray}, hash: C11DAD0387CC0406D8A793FAD2DD9559
    @Test()
    void isNotEmpty5WhenIsEmptyArray() {
        /* Branches:
         * (!isEmpty(array)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(intArray)).thenReturn(true);
            //Act Statement(s)
            boolean result = ArrayUtils.isNotEmpty(intArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(intArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNotEmpty6WhenIsEmptyNotArray}, hash: C2B380F5E85C04950CFB74745BFED0E8
    @Test()
    void isNotEmpty6WhenIsEmptyNotArray() {
        /* Branches:
         * (!isEmpty(array)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(longArray)).thenReturn(false);
            //Act Statement(s)
            boolean result = ArrayUtils.isNotEmpty(longArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(longArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNotEmpty6WhenIsEmptyArray}, hash: EC6BF4E0AA1DEBC528EAF0355A695959
    @Test()
    void isNotEmpty6WhenIsEmptyArray() {
        /* Branches:
         * (!isEmpty(array)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(longArray)).thenReturn(true);
            //Act Statement(s)
            boolean result = ArrayUtils.isNotEmpty(longArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(longArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNotEmpty7WhenIsEmptyNotArray}, hash: 577468DFEBB47AAD619E3BF6A6B9737C
    @Test()
    void isNotEmpty7WhenIsEmptyNotArray() {
        /* Branches:
         * (!isEmpty(array)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(shortArray)).thenReturn(false);
            //Act Statement(s)
            boolean result = ArrayUtils.isNotEmpty(shortArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(shortArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNotEmpty7WhenIsEmptyArray}, hash: E41145DA92F551D20AA473EF96F06B69
    @Test()
    void isNotEmpty7WhenIsEmptyArray() {
        /* Branches:
         * (!isEmpty(array)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(shortArray)).thenReturn(true);
            //Act Statement(s)
            boolean result = ArrayUtils.isNotEmpty(shortArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(shortArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNotEmpty8WhenIsEmptyNotArray}, hash: 9F9F10E692C851AA46859CB35C6F58E7
    @Test()
    void isNotEmpty8WhenIsEmptyNotArray() {
        /* Branches:
         * (!isEmpty(array)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(objectArray)).thenReturn(false);
            //Act Statement(s)
            boolean result = ArrayUtils.isNotEmpty(objectArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(objectArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isNotEmpty8WhenIsEmptyArray}, hash: 033B8AF5F8E6CEF39C73C6EA15CDD6DD
    @Test()
    void isNotEmpty8WhenIsEmptyArray() {
        /* Branches:
         * (!isEmpty(array)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(objectArray)).thenReturn(true);
            //Act Statement(s)
            boolean result = ArrayUtils.isNotEmpty(objectArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(objectArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSameLengthWhenGetLengthArray1EqualsGetLengthArray2}, hash: E4D29609AA82DA2A08140238903578DF
    @Test()
    void isSameLengthWhenGetLengthArray1EqualsGetLengthArray2() {
        /* Branches:
         * (getLength(array1) == getLength(array2)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(booleanArray)).thenReturn(1);
            boolean[] booleanArray2 = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(booleanArray2)).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.isSameLength(booleanArray, booleanArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(booleanArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.getLength(booleanArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSameLengthWhenGetLengthArray1NotEqualsGetLengthArray2}, hash: 95C7913D3F365837C739B5046D87A303
    @Disabled()
    @Test()
    void isSameLengthWhenGetLengthArray1NotEqualsGetLengthArray2() {
        /* Branches:
         * (getLength(array1) == getLength(array2)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(booleanArray)).thenReturn(1);
            boolean[] booleanArray2 = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(booleanArray2)).thenReturn(2);
            //Act Statement(s)
            boolean result = ArrayUtils.isSameLength(booleanArray, booleanArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(booleanArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.getLength(booleanArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSameLength1WhenGetLengthArray1EqualsGetLengthArray2}, hash: 063E549CAC1EB2B6E93A8111ED354A26
    @Test()
    void isSameLength1WhenGetLengthArray1EqualsGetLengthArray2() {
        /* Branches:
         * (getLength(array1) == getLength(array2)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(byteArray)).thenReturn(1);
            byte[] byteArray2 = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(byteArray2)).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.isSameLength(byteArray, byteArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(byteArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.getLength(byteArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSameLength1WhenGetLengthArray1NotEqualsGetLengthArray2}, hash: AD96041A57E8B107E01A38801C036A50
    @Disabled()
    @Test()
    void isSameLength1WhenGetLengthArray1NotEqualsGetLengthArray2() {
        /* Branches:
         * (getLength(array1) == getLength(array2)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(byteArray)).thenReturn(1);
            byte[] byteArray2 = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(byteArray2)).thenReturn(2);
            //Act Statement(s)
            boolean result = ArrayUtils.isSameLength(byteArray, byteArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(byteArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.getLength(byteArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSameLength2WhenGetLengthArray1EqualsGetLengthArray2}, hash: B4146D59B1BD0DA25470FEE8A0FD4E09
    @Test()
    void isSameLength2WhenGetLengthArray1EqualsGetLengthArray2() {
        /* Branches:
         * (getLength(array1) == getLength(array2)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(charArray)).thenReturn(1);
            char[] charArray2 = new char[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(charArray2)).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.isSameLength(charArray, charArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(charArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.getLength(charArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSameLength2WhenGetLengthArray1NotEqualsGetLengthArray2}, hash: 5B78668B200DA487F29FDE402AD2D196
    @Disabled()
    @Test()
    void isSameLength2WhenGetLengthArray1NotEqualsGetLengthArray2() {
        /* Branches:
         * (getLength(array1) == getLength(array2)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(charArray)).thenReturn(1);
            char[] charArray2 = new char[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(charArray2)).thenReturn(2);
            //Act Statement(s)
            boolean result = ArrayUtils.isSameLength(charArray, charArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(charArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.getLength(charArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSameLength3WhenGetLengthArray1EqualsGetLengthArray2}, hash: 58B23F44336B31730487A4594852E927
    @Test()
    void isSameLength3WhenGetLengthArray1EqualsGetLengthArray2() {
        /* Branches:
         * (getLength(array1) == getLength(array2)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(doubleArray)).thenReturn(1);
            double[] doubleArray2 = new double[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(doubleArray2)).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.isSameLength(doubleArray, doubleArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(doubleArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.getLength(doubleArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSameLength3WhenGetLengthArray1NotEqualsGetLengthArray2}, hash: 875BB50D50136CA011D9D53194A99351
    @Disabled()
    @Test()
    void isSameLength3WhenGetLengthArray1NotEqualsGetLengthArray2() {
        /* Branches:
         * (getLength(array1) == getLength(array2)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(doubleArray)).thenReturn(1);
            double[] doubleArray2 = new double[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(doubleArray2)).thenReturn(2);
            //Act Statement(s)
            boolean result = ArrayUtils.isSameLength(doubleArray, doubleArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(doubleArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.getLength(doubleArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSameLength4WhenGetLengthArray1EqualsGetLengthArray2}, hash: 504C24F6FACE3C3E8C042E6EE2FC8881
    @Test()
    void isSameLength4WhenGetLengthArray1EqualsGetLengthArray2() {
        /* Branches:
         * (getLength(array1) == getLength(array2)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(floatArray)).thenReturn(1);
            float[] floatArray2 = new float[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(floatArray2)).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.isSameLength(floatArray, floatArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(floatArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.getLength(floatArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSameLength4WhenGetLengthArray1NotEqualsGetLengthArray2}, hash: E2B5326F74261092C34E2A895D59B148
    @Disabled()
    @Test()
    void isSameLength4WhenGetLengthArray1NotEqualsGetLengthArray2() {
        /* Branches:
         * (getLength(array1) == getLength(array2)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(floatArray)).thenReturn(1);
            float[] floatArray2 = new float[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(floatArray2)).thenReturn(2);
            //Act Statement(s)
            boolean result = ArrayUtils.isSameLength(floatArray, floatArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(floatArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.getLength(floatArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSameLength5WhenGetLengthArray1EqualsGetLengthArray2}, hash: 6A2C5F9326C7E502A15F1AD3C1A959C5
    @Test()
    void isSameLength5WhenGetLengthArray1EqualsGetLengthArray2() {
        /* Branches:
         * (getLength(array1) == getLength(array2)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(intArray)).thenReturn(1);
            int[] intArray2 = new int[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(intArray2)).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.isSameLength(intArray, intArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(intArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.getLength(intArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSameLength5WhenGetLengthArray1NotEqualsGetLengthArray2}, hash: 8864524635499A53C2848FAC3AFB86A2
    @Disabled()
    @Test()
    void isSameLength5WhenGetLengthArray1NotEqualsGetLengthArray2() {
        /* Branches:
         * (getLength(array1) == getLength(array2)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(intArray)).thenReturn(1);
            int[] intArray2 = new int[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(intArray2)).thenReturn(2);
            //Act Statement(s)
            boolean result = ArrayUtils.isSameLength(intArray, intArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(intArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.getLength(intArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSameLength6WhenGetLengthArray1EqualsGetLengthArray2}, hash: 995FD17ADB64FBD65982BBEC16DDF1FB
    @Test()
    void isSameLength6WhenGetLengthArray1EqualsGetLengthArray2() {
        /* Branches:
         * (getLength(array1) == getLength(array2)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(longArray)).thenReturn(1);
            long[] longArray2 = new long[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(longArray2)).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.isSameLength(longArray, longArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(longArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.getLength(longArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSameLength6WhenGetLengthArray1NotEqualsGetLengthArray2}, hash: FC6D5D26EDCD67ED1DA86985A1CF1A54
    @Disabled()
    @Test()
    void isSameLength6WhenGetLengthArray1NotEqualsGetLengthArray2() {
        /* Branches:
         * (getLength(array1) == getLength(array2)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(longArray)).thenReturn(1);
            long[] longArray2 = new long[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(longArray2)).thenReturn(2);
            //Act Statement(s)
            boolean result = ArrayUtils.isSameLength(longArray, longArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(longArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.getLength(longArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSameLength7WhenGetLengthArray1EqualsGetLengthArray2}, hash: 2447354279E380C12B2BA1B431CC7ADB
    @Test()
    void isSameLength7WhenGetLengthArray1EqualsGetLengthArray2() {
        /* Branches:
         * (getLength(array1) == getLength(array2)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            arrayUtils.when(() -> ArrayUtils.getLength(object)).thenReturn(1);
            Object object2 = new Object();
            arrayUtils.when(() -> ArrayUtils.getLength(object2)).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.isSameLength(object, object2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(object), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.getLength(object2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSameLength7WhenGetLengthArray1NotEqualsGetLengthArray2}, hash: 8264B02C09942E2914574A8D6E973427
    @Test()
    void isSameLength7WhenGetLengthArray1NotEqualsGetLengthArray2() {
        /* Branches:
         * (getLength(array1) == getLength(array2)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            arrayUtils.when(() -> ArrayUtils.getLength(object)).thenReturn(1);
            Object object2 = new Object();
            arrayUtils.when(() -> ArrayUtils.getLength(object2)).thenReturn(2);
            //Act Statement(s)
            boolean result = ArrayUtils.isSameLength(object, object2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(object), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.getLength(object2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSameLength8WhenGetLengthArray1EqualsGetLengthArray2}, hash: A715D0B50B8F246A4949953F9EE4BB2B
    @Test()
    void isSameLength8WhenGetLengthArray1EqualsGetLengthArray2() {
        /* Branches:
         * (getLength(array1) == getLength(array2)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(objectArray)).thenReturn(1);
            Object[] objectArray2 = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(objectArray2)).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.isSameLength(objectArray, objectArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(objectArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.getLength(objectArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSameLength8WhenGetLengthArray1NotEqualsGetLengthArray2}, hash: 8EF143DC51B611F8E2C7DB411DA3BDC1
    @Disabled()
    @Test()
    void isSameLength8WhenGetLengthArray1NotEqualsGetLengthArray2() {
        /* Branches:
         * (getLength(array1) == getLength(array2)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(objectArray)).thenReturn(1);
            Object[] objectArray2 = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(objectArray2)).thenReturn(2);
            //Act Statement(s)
            boolean result = ArrayUtils.isSameLength(objectArray, objectArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(objectArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.getLength(objectArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSameLength9WhenGetLengthArray1EqualsGetLengthArray2}, hash: C77E1891EC248953A53911B946488FB9
    @Test()
    void isSameLength9WhenGetLengthArray1EqualsGetLengthArray2() {
        /* Branches:
         * (getLength(array1) == getLength(array2)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(shortArray)).thenReturn(1);
            short[] shortArray2 = new short[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(shortArray2)).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.isSameLength(shortArray, shortArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(shortArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.getLength(shortArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSameLength9WhenGetLengthArray1NotEqualsGetLengthArray2}, hash: 6A54924466E3149C82D643E66CA9B65D
    @Disabled()
    @Test()
    void isSameLength9WhenGetLengthArray1NotEqualsGetLengthArray2() {
        /* Branches:
         * (getLength(array1) == getLength(array2)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(shortArray)).thenReturn(1);
            short[] shortArray2 = new short[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(shortArray2)).thenReturn(2);
            //Act Statement(s)
            boolean result = ArrayUtils.isSameLength(shortArray, shortArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(shortArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.getLength(shortArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSameTypeWhenArray2IsNullThrowsIllegalArgumentException}, hash: B0B387E2B21353FE474B3E73D9E2F7BA
    @Test()
    void isSameTypeWhenArray2IsNullThrowsIllegalArgumentException() {
        /* Branches:
         * (array1 == null) : false
         * (array2 == null) : true
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = null;
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The Array must not be null");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            ArrayUtils.isSameType(object, object2);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${isSameTypeWhenArray1GetClassGetNameEqualsArray2GetClassGetName}, hash: 011157D565E1EA643A27B1468424E353
    @Test()
    void isSameTypeWhenArray1GetClassGetNameEqualsArray2GetClassGetName() {
        /* Branches:
         * (array1 == null) : false
         * (array2 == null) : false
         * (array1.getClass().getName().equals(array2.getClass().getName())) : true
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        //Act Statement(s)
        boolean result = ArrayUtils.isSameType(object, object2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isSameTypeWhenArray1GetClassGetNameNotEqualsArray2GetClassGetName}, hash: 740CF6B493C791FFAC988B2FDD7F4ABC
    @Disabled()
    @Test()
    void isSameTypeWhenArray1GetClassGetNameNotEqualsArray2GetClassGetName() {
        /* Branches:
         * (array1 == null) : false
         * (array2 == null) : false
         * (array1.getClass().getName().equals(array2.getClass().getName())) : false
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        //Act Statement(s)
        boolean result = ArrayUtils.isSameType(object, object2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isSortedWhenGetLengthArrayLessThan2}, hash: B6532EBB76C5CA6E18FD30DBFF2CB199
    @Test()
    void isSortedWhenGetLengthArrayLessThan2() {
        /* Branches:
         * (getLength(array) < 2) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(booleanArray)).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(booleanArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(booleanArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSortedWhenBooleanUtilsComparePreviousCurrentGreaterThan0}, hash: CA703C3DFED64D4B5BF49E0DEEB9FCEB
    @Disabled()
    @Test()
    void isSortedWhenBooleanUtilsComparePreviousCurrentGreaterThan0() {
        /* Branches:
         * (getLength(array) < 2) : false
         * (i < n) : true
         * (BooleanUtils.compare(previous, current) > 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(booleanArray)).thenReturn(0);
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(booleanArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(booleanArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSortedWhenBooleanUtilsComparePreviousCurrentNotGreaterThan0}, hash: 3C99328628622E1EE99FB705123CA323
    @Test()
    void isSortedWhenBooleanUtilsComparePreviousCurrentNotGreaterThan0() {
        /* Branches:
         * (getLength(array) < 2) : false
         * (i < n) : true
         * (BooleanUtils.compare(previous, current) > 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] { false, false };
            arrayUtils.when(() -> ArrayUtils.getLength(booleanArray)).thenReturn(2);
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(booleanArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(booleanArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSorted1WhenGetLengthArrayLessThan2}, hash: F8C107AD02320292ABA531F0EB905FDA
    @Test()
    void isSorted1WhenGetLengthArrayLessThan2() {
        /* Branches:
         * (getLength(array) < 2) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(byteArray)).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(byteArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(byteArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSorted1WhenNumberUtilsComparePreviousCurrentGreaterThan0}, hash: CCF0C9F0D8801DD500E59E79125CC08F
    @Disabled()
    @Test()
    void isSorted1WhenNumberUtilsComparePreviousCurrentGreaterThan0() {
        /* Branches:
         * (getLength(array) < 2) : false
         * (i < n) : true
         * (NumberUtils.compare(previous, current) > 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] { (byte) 1, (byte) 1 };
            arrayUtils.when(() -> ArrayUtils.getLength(byteArray)).thenReturn(2);
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(byteArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(byteArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSorted1WhenNumberUtilsComparePreviousCurrentNotGreaterThan0}, hash: 0A0A4A3D35CCFB6638DC80521B0E2C19
    @Test()
    void isSorted1WhenNumberUtilsComparePreviousCurrentNotGreaterThan0() {
        /* Branches:
         * (getLength(array) < 2) : false
         * (i < n) : true
         * (NumberUtils.compare(previous, current) > 0) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] { (byte) 0, (byte) 0 };
            arrayUtils.when(() -> ArrayUtils.getLength(byteArray)).thenReturn(2);
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(byteArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(byteArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSorted2WhenGetLengthArrayLessThan2}, hash: 97AA2A595FD19DBA9C891E199B0EB489
    @Test()
    void isSorted2WhenGetLengthArrayLessThan2() {
        /* Branches:
         * (getLength(array) < 2) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(charArray)).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(charArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(charArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSorted2WhenCharUtilsComparePreviousCurrentGreaterThan0}, hash: 2B89BB2601518ADAA06177059080E9B9
    @Disabled()
    @Test()
    void isSorted2WhenCharUtilsComparePreviousCurrentGreaterThan0() {
        /* Branches:
         * (getLength(array) < 2) : false
         * (i < n) : true
         * (CharUtils.compare(previous, current) > 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(charArray)).thenReturn(0);
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(charArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(charArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSorted2WhenCharUtilsComparePreviousCurrentNotGreaterThan0}, hash: 51A732BC6AC8542BE2F1AB39DFEE335E
    @Test()
    void isSorted2WhenCharUtilsComparePreviousCurrentNotGreaterThan0() {
        /* Branches:
         * (getLength(array) < 2) : false
         * (i < n) : true
         * (CharUtils.compare(previous, current) > 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] { 'A', 'A' };
            arrayUtils.when(() -> ArrayUtils.getLength(charArray)).thenReturn(2);
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(charArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(charArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSorted3WhenGetLengthArrayLessThan2}, hash: B8332A5B15F27F970316AD91E52B593E
    @Test()
    void isSorted3WhenGetLengthArrayLessThan2() {
        /* Branches:
         * (getLength(array) < 2) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(doubleArray)).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(doubleArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(doubleArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSorted3WhenDoubleComparePreviousCurrentGreaterThan0}, hash: 95DCFD2217C7A64806A9440BA1718A0D
    @Test()
    void isSorted3WhenDoubleComparePreviousCurrentGreaterThan0() {
        /* Branches:
         * (getLength(array) < 2) : false
         * (i < n) : true
         * (Double.compare(previous, current) > 0) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] { Double.parseDouble("0.0"), Double.parseDouble("-0.5") };
            arrayUtils.when(() -> ArrayUtils.getLength(doubleArray)).thenReturn(2);
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(doubleArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(doubleArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSorted3WhenDoubleComparePreviousCurrentNotGreaterThan0}, hash: D4445942EA376F92FAD19DAB349A7216
    @Test()
    void isSorted3WhenDoubleComparePreviousCurrentNotGreaterThan0() {
        /* Branches:
         * (getLength(array) < 2) : false
         * (i < n) : true
         * (Double.compare(previous, current) > 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] { Double.parseDouble("0.0"), Double.parseDouble("0.0") };
            arrayUtils.when(() -> ArrayUtils.getLength(doubleArray)).thenReturn(2);
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(doubleArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(doubleArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSorted4WhenGetLengthArrayLessThan2}, hash: 760783D88DE9DD80B8FC2547AF39E834
    @Test()
    void isSorted4WhenGetLengthArrayLessThan2() {
        /* Branches:
         * (getLength(array) < 2) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(floatArray)).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(floatArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(floatArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSorted4WhenFloatComparePreviousCurrentGreaterThan0}, hash: 4CEBBB6481C698B7DF6218C436439CC4
    @Test()
    void isSorted4WhenFloatComparePreviousCurrentGreaterThan0() {
        /* Branches:
         * (getLength(array) < 2) : false
         * (i < n) : true
         * (Float.compare(previous, current) > 0) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] { Float.parseFloat("0.0"), Float.parseFloat("-0.5") };
            arrayUtils.when(() -> ArrayUtils.getLength(floatArray)).thenReturn(2);
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(floatArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(floatArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSorted4WhenFloatComparePreviousCurrentNotGreaterThan0}, hash: C88A275115DEA48DE6641DA18585C0D3
    @Test()
    void isSorted4WhenFloatComparePreviousCurrentNotGreaterThan0() {
        /* Branches:
         * (getLength(array) < 2) : false
         * (i < n) : true
         * (Float.compare(previous, current) > 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] { Float.parseFloat("0.0"), Float.parseFloat("0.0") };
            arrayUtils.when(() -> ArrayUtils.getLength(floatArray)).thenReturn(2);
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(floatArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(floatArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSorted5WhenGetLengthArrayLessThan2}, hash: 13B1E0B56BAB9011BF3736F6D682336B
    @Test()
    void isSorted5WhenGetLengthArrayLessThan2() {
        /* Branches:
         * (getLength(array) < 2) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(intArray)).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(intArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(intArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSorted5WhenNumberUtilsComparePreviousCurrentGreaterThan0}, hash: AEDB26E9FD67469BBC7D6069BD4C98D3
    @Disabled()
    @Test()
    void isSorted5WhenNumberUtilsComparePreviousCurrentGreaterThan0() {
        /* Branches:
         * (getLength(array) < 2) : false
         * (i < n) : true
         * (NumberUtils.compare(previous, current) > 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(intArray)).thenReturn(0);
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(intArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(intArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSorted5WhenNumberUtilsComparePreviousCurrentNotGreaterThan0}, hash: 83E4C1A38F4B10E83BE3931370763C70
    @Test()
    void isSorted5WhenNumberUtilsComparePreviousCurrentNotGreaterThan0() {
        /* Branches:
         * (getLength(array) < 2) : false
         * (i < n) : true
         * (NumberUtils.compare(previous, current) > 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] { 0, 0 };
            arrayUtils.when(() -> ArrayUtils.getLength(intArray)).thenReturn(2);
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(intArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(intArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSorted6WhenGetLengthArrayLessThan2}, hash: 9B31AF9A08B3D318FEE6FFDECC354AA3
    @Test()
    void isSorted6WhenGetLengthArrayLessThan2() {
        /* Branches:
         * (getLength(array) < 2) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(longArray)).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(longArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(longArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSorted6WhenNumberUtilsComparePreviousCurrentGreaterThan0}, hash: A666E2130D4BCFBABF7035A16C276A1E
    @Disabled()
    @Test()
    void isSorted6WhenNumberUtilsComparePreviousCurrentGreaterThan0() {
        /* Branches:
         * (getLength(array) < 2) : false
         * (i < n) : true
         * (NumberUtils.compare(previous, current) > 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(longArray)).thenReturn(0);
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(longArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(longArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSorted6WhenNumberUtilsComparePreviousCurrentNotGreaterThan0}, hash: D0F4B776D19F399F24ECB4B5D459B3AC
    @Test()
    void isSorted6WhenNumberUtilsComparePreviousCurrentNotGreaterThan0() {
        /* Branches:
         * (getLength(array) < 2) : false
         * (i < n) : true
         * (NumberUtils.compare(previous, current) > 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] { 0L, 0L };
            arrayUtils.when(() -> ArrayUtils.getLength(longArray)).thenReturn(2);
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(longArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(longArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSorted7WhenGetLengthArrayLessThan2}, hash: 3DAD81779BAEAE3D533186A7DBCBB235
    @Test()
    void isSorted7WhenGetLengthArrayLessThan2() {
        /* Branches:
         * (getLength(array) < 2) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(shortArray)).thenReturn(1);
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(shortArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(shortArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSorted7WhenNumberUtilsComparePreviousCurrentGreaterThan0}, hash: 8D050951349214810E311B7B779A71D8
    @Disabled()
    @Test()
    void isSorted7WhenNumberUtilsComparePreviousCurrentGreaterThan0() {
        /* Branches:
         * (getLength(array) < 2) : false
         * (i < n) : true
         * (NumberUtils.compare(previous, current) > 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] { (short) 1, (short) 1 };
            arrayUtils.when(() -> ArrayUtils.getLength(shortArray)).thenReturn(2);
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(shortArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(shortArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSorted7WhenNumberUtilsComparePreviousCurrentNotGreaterThan0}, hash: 90F8E208FBD42548496BE60E450936A0
    @Test()
    void isSorted7WhenNumberUtilsComparePreviousCurrentNotGreaterThan0() {
        /* Branches:
         * (getLength(array) < 2) : false
         * (i < n) : true
         * (NumberUtils.compare(previous, current) > 0) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] { (short) 0, (short) 0 };
            arrayUtils.when(() -> ArrayUtils.getLength(shortArray)).thenReturn(2);
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(shortArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(shortArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSorted9WhenGetLengthArrayLessThan2}, hash: ED10CE183C7554FBD93DBA5F118DB086
    @Test()
    void isSorted9WhenGetLengthArrayLessThan2() {
        /* Branches:
         * (getLength(array) < 2) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(objectArray)).thenReturn(0);
            Comparator comparator = Comparator.reverseOrder();
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(objectArray, comparator);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(objectArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSorted9WhenComparatorComparePreviousCurrentGreaterThan0}, hash: 0AF04B5F9A2F527DD804449A75B0F789
    @Disabled()
    @Test()
    void isSorted9WhenComparatorComparePreviousCurrentGreaterThan0() {
        /* Branches:
         * (getLength(array) < 2) : false
         * (i < n) : true
         * (comparator.compare(previous, current) > 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            Object[] objectArray = new Object[] { object, object2 };
            arrayUtils.when(() -> ArrayUtils.getLength(objectArray)).thenReturn(2);
            Comparator comparator = Comparator.reverseOrder();
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(objectArray, comparator);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                arrayUtils.verify(() -> ArrayUtils.getLength(objectArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSorted9WhenComparatorComparePreviousCurrentNotGreaterThan0}, hash: BF960259A21D6E4A2A10FAF7B649002A
    @Disabled()
    @Test()
    void isSorted9WhenComparatorComparePreviousCurrentNotGreaterThan0() {
        /* Branches:
         * (getLength(array) < 2) : false
         * (i < n) : true
         * (comparator.compare(previous, current) > 0) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            Object[] objectArray = new Object[] { object, object2 };
            arrayUtils.when(() -> ArrayUtils.getLength(objectArray)).thenReturn(2);
            Comparator comparator = Comparator.reverseOrder();
            //Act Statement(s)
            boolean result = ArrayUtils.isSorted(objectArray, comparator);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                arrayUtils.verify(() -> ArrayUtils.getLength(objectArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOfTest}, hash: 6CA9928077A7F04A5C2C05F086D06F94
    @Test()
    void lastIndexOfTest() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.lastIndexOf(booleanArray, false, 2147483647)).thenReturn(0);
            //Act Statement(s)
            int result = ArrayUtils.lastIndexOf(booleanArray, false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.lastIndexOf(booleanArray, false, 2147483647), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOf1WhenStartIndexLessThan0}, hash: 91472A65E2926B563634A15D11E05A35
    @Test()
    void lastIndexOf1WhenStartIndexLessThan0() {
        /* Branches:
         * (isEmpty(array)) : false
         * (startIndex < 0) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(booleanArray)).thenReturn(false);
            //Act Statement(s)
            int result = ArrayUtils.lastIndexOf(booleanArray, false, -2147483648);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(-1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(booleanArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOf1WhenValueToFindEqualsIIndexOfArray}, hash: 4B92BED3B5131792B27BBEB29D5C2721
    @Test()
    void lastIndexOf1WhenValueToFindEqualsIIndexOfArray() {
        /* Branches:
         * (isEmpty(array)) : false
         * (startIndex < 0) : false
         * (startIndex >= array.length) : true
         * (i >= 0) : true
         * (valueToFind == array[i]) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] { false };
            arrayUtils.when(() -> ArrayUtils.isEmpty(booleanArray)).thenReturn(false);
            //Act Statement(s)
            int result = ArrayUtils.lastIndexOf(booleanArray, false, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(booleanArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOf1WhenValueToFindNotEqualsIIndexOfArray}, hash: 1B8802AE03FDBEA2729003409EE4505F
    @Test()
    void lastIndexOf1WhenValueToFindNotEqualsIIndexOfArray() {
        /* Branches:
         * (isEmpty(array)) : false
         * (startIndex < 0) : false
         * (startIndex >= array.length) : true
         * (i >= 0) : true
         * (valueToFind == array[i]) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] { false };
            arrayUtils.when(() -> ArrayUtils.isEmpty(booleanArray)).thenReturn(false);
            //Act Statement(s)
            int result = ArrayUtils.lastIndexOf(booleanArray, true, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(-1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(booleanArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOf2Test}, hash: AEC566886795BFBAFF2186F1BC31E34D
    @Test()
    void lastIndexOf2Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.lastIndexOf(byteArray, (byte) 0, 2147483647)).thenReturn(0);
            //Act Statement(s)
            int result = ArrayUtils.lastIndexOf(byteArray, (byte) 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.lastIndexOf(byteArray, (byte) 0, 2147483647), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOf3WhenStartIndexLessThan0}, hash: BB5BEFC21579B19CF6F68205DF195C3C
    @Test()
    void lastIndexOf3WhenStartIndexLessThan0() {
        /* Branches:
         * (array == null) : false
         * (startIndex < 0) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        //Act Statement(s)
        int result = ArrayUtils.lastIndexOf(byteArray, (byte) 0, -2147483648);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${lastIndexOf3WhenValueToFindEqualsIIndexOfArray}, hash: D32F61045A2E98B2180657D047546AE4
    @Test()
    void lastIndexOf3WhenValueToFindEqualsIIndexOfArray() {
        /* Branches:
         * (array == null) : false
         * (startIndex < 0) : false
         * (startIndex >= array.length) : true
         * (i >= 0) : true
         * (valueToFind == array[i]) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] { (byte) 1 };
        //Act Statement(s)
        int result = ArrayUtils.lastIndexOf(byteArray, (byte) 1, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${lastIndexOf3WhenValueToFindNotEqualsIIndexOfArray}, hash: E2D94B535AB33C87EA1AAADD16384F64
    @Test()
    void lastIndexOf3WhenValueToFindNotEqualsIIndexOfArray() {
        /* Branches:
         * (array == null) : false
         * (startIndex < 0) : false
         * (startIndex >= array.length) : true
         * (i >= 0) : true
         * (valueToFind == array[i]) : false
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] { (byte) 2 };
        //Act Statement(s)
        int result = ArrayUtils.lastIndexOf(byteArray, (byte) 1, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${lastIndexOf4Test}, hash: 0BE3864CD5F8A9F2B3CAF110A2BC021C
    @Test()
    void lastIndexOf4Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.lastIndexOf(charArray, 'A', 2147483647)).thenReturn(0);
            //Act Statement(s)
            int result = ArrayUtils.lastIndexOf(charArray, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.lastIndexOf(charArray, 'A', 2147483647), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOf5WhenStartIndexLessThan0}, hash: 388450A6D1DFDC7DF649421B0767DE5D
    @Test()
    void lastIndexOf5WhenStartIndexLessThan0() {
        /* Branches:
         * (array == null) : false
         * (startIndex < 0) : true
         */
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        //Act Statement(s)
        int result = ArrayUtils.lastIndexOf(charArray, 'A', -2147483648);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${lastIndexOf5WhenValueToFindEqualsIIndexOfArray}, hash: 4BAF2A5C79EAEB4BA87246ABABBD7F87
    @Test()
    void lastIndexOf5WhenValueToFindEqualsIIndexOfArray() {
        /* Branches:
         * (array == null) : false
         * (startIndex < 0) : false
         * (startIndex >= array.length) : true
         * (i >= 0) : true
         * (valueToFind == array[i]) : true
         */
        //Arrange Statement(s)
        char[] charArray = new char[] { 'A' };
        //Act Statement(s)
        int result = ArrayUtils.lastIndexOf(charArray, 'A', 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${lastIndexOf5WhenValueToFindNotEqualsIIndexOfArray}, hash: 28FF132AF2E0BACF489F229F3AF78650
    @Test()
    void lastIndexOf5WhenValueToFindNotEqualsIIndexOfArray() {
        /* Branches:
         * (array == null) : false
         * (startIndex < 0) : false
         * (startIndex >= array.length) : true
         * (i >= 0) : true
         * (valueToFind == array[i]) : false
         */
        //Arrange Statement(s)
        char[] charArray = new char[] { 'B' };
        //Act Statement(s)
        int result = ArrayUtils.lastIndexOf(charArray, 'A', 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${lastIndexOf6Test}, hash: 9619E77191CF70A9171214BA5B9A58C3
    @Test()
    void lastIndexOf6Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.lastIndexOf(doubleArray, Double.parseDouble("0.0"), 2147483647)).thenReturn(0);
            //Act Statement(s)
            int result = ArrayUtils.lastIndexOf(doubleArray, Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.lastIndexOf(doubleArray, Double.parseDouble("0.0"), 2147483647), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOf7Test}, hash: E938A4EDD997AA0960131D6EE2D7FBED
    @Test()
    void lastIndexOf7Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.lastIndexOf(doubleArray, Double.parseDouble("0.0"), 2147483647, Double.parseDouble("0.0"))).thenReturn(0);
            //Act Statement(s)
            int result = ArrayUtils.lastIndexOf(doubleArray, Double.parseDouble("0.0"), Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.lastIndexOf(doubleArray, Double.parseDouble("0.0"), 2147483647, Double.parseDouble("0.0")), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOf8WhenStartIndexLessThan0}, hash: 2F19655FFBE977288980ECF34F687CCC
    @Test()
    void lastIndexOf8WhenStartIndexLessThan0() {
        /* Branches:
         * (isEmpty(array)) : false
         * (startIndex < 0) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(doubleArray)).thenReturn(false);
            //Act Statement(s)
            int result = ArrayUtils.lastIndexOf(doubleArray, Double.parseDouble("0.0"), -2147483648);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(-1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(doubleArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOf8WhenValueToFindEqualsIIndexOfArray}, hash: 256568E4E44920A34D11DD494DA3C398
    @Test()
    void lastIndexOf8WhenValueToFindEqualsIIndexOfArray() {
        /* Branches:
         * (isEmpty(array)) : false
         * (startIndex < 0) : false
         * (startIndex >= array.length) : true
         * (i >= 0) : true
         * (valueToFind == array[i]) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] { Double.parseDouble("0.5") };
            arrayUtils.when(() -> ArrayUtils.isEmpty(doubleArray)).thenReturn(false);
            //Act Statement(s)
            int result = ArrayUtils.lastIndexOf(doubleArray, Double.parseDouble("0.5"), 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(doubleArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOf8WhenValueToFindNotEqualsIIndexOfArray}, hash: DBF4078A18BB2427C17F245EAA0BCDB9
    @Test()
    void lastIndexOf8WhenValueToFindNotEqualsIIndexOfArray() {
        /* Branches:
         * (isEmpty(array)) : false
         * (startIndex < 0) : false
         * (startIndex >= array.length) : true
         * (i >= 0) : true
         * (valueToFind == array[i]) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] { Double.parseDouble("0.25") };
            arrayUtils.when(() -> ArrayUtils.isEmpty(doubleArray)).thenReturn(false);
            //Act Statement(s)
            int result = ArrayUtils.lastIndexOf(doubleArray, Double.parseDouble("0.5"), 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(-1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(doubleArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOf9WhenStartIndexLessThan0}, hash: 45306EF8EAC4A19259E875CF1C419053
    @Test()
    void lastIndexOf9WhenStartIndexLessThan0() {
        /* Branches:
         * (isEmpty(array)) : false
         * (startIndex < 0) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(doubleArray)).thenReturn(false);
            //Act Statement(s)
            int result = ArrayUtils.lastIndexOf(doubleArray, Double.parseDouble("0.0"), -2147483648, Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(-1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(doubleArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOf9WhenIIndexOfArrayLessThanOrEqualsToMax}, hash: 21A6D869FB4B3B05015EB9D1497E5BAE
    @Test()
    void lastIndexOf9WhenIIndexOfArrayLessThanOrEqualsToMax() {
        /* Branches:
         * (isEmpty(array)) : false
         * (startIndex < 0) : false
         * (startIndex >= array.length) : true
         * (i >= 0) : true
         * (array[i] >= min) : true
         * (array[i] <= max) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] { Double.parseDouble("0.25") };
            arrayUtils.when(() -> ArrayUtils.isEmpty(doubleArray)).thenReturn(false);
            //Act Statement(s)
            int result = ArrayUtils.lastIndexOf(doubleArray, Double.parseDouble("0.25"), 1, Double.parseDouble("0.25"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(doubleArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOf9WhenIIndexOfArrayGreaterThanMax}, hash: E8A8E47792AA765D4644F5D07E1B98DD
    @Test()
    void lastIndexOf9WhenIIndexOfArrayGreaterThanMax() {
        /* Branches:
         * (isEmpty(array)) : false
         * (startIndex < 0) : false
         * (startIndex >= array.length) : true
         * (i >= 0) : true
         * (array[i] >= min) : true
         * (array[i] <= max) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] { Double.parseDouble("0.25") };
            arrayUtils.when(() -> ArrayUtils.isEmpty(doubleArray)).thenReturn(false);
            //Act Statement(s)
            int result = ArrayUtils.lastIndexOf(doubleArray, Double.parseDouble("0.0"), 1, Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(-1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(doubleArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOf10Test}, hash: 9A19FD65DB8D38097A36780484E938CA
    @Test()
    void lastIndexOf10Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.lastIndexOf(floatArray, Float.parseFloat("0.0"), 2147483647)).thenReturn(0);
            //Act Statement(s)
            int result = ArrayUtils.lastIndexOf(floatArray, Float.parseFloat("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.lastIndexOf(floatArray, Float.parseFloat("0.0"), 2147483647), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOf11WhenStartIndexLessThan0}, hash: 60BD3929D367248C8AC0CD86004F6F58
    @Test()
    void lastIndexOf11WhenStartIndexLessThan0() {
        /* Branches:
         * (isEmpty(array)) : false
         * (startIndex < 0) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(floatArray)).thenReturn(false);
            //Act Statement(s)
            int result = ArrayUtils.lastIndexOf(floatArray, Float.parseFloat("0.0"), -2147483648);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(-1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(floatArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOf11WhenValueToFindEqualsIIndexOfArray}, hash: 00746E35E76DA2E155D70B0FD468F114
    @Test()
    void lastIndexOf11WhenValueToFindEqualsIIndexOfArray() {
        /* Branches:
         * (isEmpty(array)) : false
         * (startIndex < 0) : false
         * (startIndex >= array.length) : true
         * (i >= 0) : true
         * (valueToFind == array[i]) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] { Float.parseFloat("0.5") };
            arrayUtils.when(() -> ArrayUtils.isEmpty(floatArray)).thenReturn(false);
            //Act Statement(s)
            int result = ArrayUtils.lastIndexOf(floatArray, Float.parseFloat("0.5"), 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(floatArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOf11WhenValueToFindNotEqualsIIndexOfArray}, hash: 64AD9A248FC98B22077F04BC9D62FB41
    @Test()
    void lastIndexOf11WhenValueToFindNotEqualsIIndexOfArray() {
        /* Branches:
         * (isEmpty(array)) : false
         * (startIndex < 0) : false
         * (startIndex >= array.length) : true
         * (i >= 0) : true
         * (valueToFind == array[i]) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] { Float.parseFloat("0.25") };
            arrayUtils.when(() -> ArrayUtils.isEmpty(floatArray)).thenReturn(false);
            //Act Statement(s)
            int result = ArrayUtils.lastIndexOf(floatArray, Float.parseFloat("0.5"), 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(-1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(floatArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOf12Test}, hash: A91F9BBBE40DCA16E1480EFD6989E2BF
    @Test()
    void lastIndexOf12Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.lastIndexOf(intArray, 0, 2147483647)).thenReturn(0);
            //Act Statement(s)
            int result = ArrayUtils.lastIndexOf(intArray, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.lastIndexOf(intArray, 0, 2147483647), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOf13WhenStartIndexLessThan0}, hash: 77D88EA81DCB2D5B13D707DC210C0AC2
    @Test()
    void lastIndexOf13WhenStartIndexLessThan0() {
        /* Branches:
         * (array == null) : false
         * (startIndex < 0) : true
         */
        //Arrange Statement(s)
        int[] intArray = new int[] {};
        //Act Statement(s)
        int result = ArrayUtils.lastIndexOf(intArray, 0, -2147483648);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${lastIndexOf13WhenValueToFindEqualsIIndexOfArray}, hash: 3A083C743AAE8039AE339E40105A2F58
    @Test()
    void lastIndexOf13WhenValueToFindEqualsIIndexOfArray() {
        /* Branches:
         * (array == null) : false
         * (startIndex < 0) : false
         * (startIndex >= array.length) : true
         * (i >= 0) : true
         * (valueToFind == array[i]) : true
         */
        //Arrange Statement(s)
        int[] intArray = new int[] { 1 };
        //Act Statement(s)
        int result = ArrayUtils.lastIndexOf(intArray, 1, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${lastIndexOf13WhenValueToFindNotEqualsIIndexOfArray}, hash: 3525D5393846407181D1E0B040D1BA33
    @Test()
    void lastIndexOf13WhenValueToFindNotEqualsIIndexOfArray() {
        /* Branches:
         * (array == null) : false
         * (startIndex < 0) : false
         * (startIndex >= array.length) : true
         * (i >= 0) : true
         * (valueToFind == array[i]) : false
         */
        //Arrange Statement(s)
        int[] intArray = new int[] { 2 };
        //Act Statement(s)
        int result = ArrayUtils.lastIndexOf(intArray, 1, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${lastIndexOf14Test}, hash: AFEB7DEE9D97BAE3EF96A962F4E85452
    @Test()
    void lastIndexOf14Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.lastIndexOf(longArray, 0L, 2147483647)).thenReturn(0);
            //Act Statement(s)
            int result = ArrayUtils.lastIndexOf(longArray, 0L);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.lastIndexOf(longArray, 0L, 2147483647), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOf15WhenStartIndexLessThan0}, hash: 16D8D19403CC9305EEC40138763ACA33
    @Test()
    void lastIndexOf15WhenStartIndexLessThan0() {
        /* Branches:
         * (array == null) : false
         * (startIndex < 0) : true
         */
        //Arrange Statement(s)
        long[] longArray = new long[] {};
        //Act Statement(s)
        int result = ArrayUtils.lastIndexOf(longArray, 0L, -2147483648);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${lastIndexOf15WhenValueToFindEqualsIIndexOfArray}, hash: 2574A2005516E8486ABEA08F2915AE88
    @Test()
    void lastIndexOf15WhenValueToFindEqualsIIndexOfArray() {
        /* Branches:
         * (array == null) : false
         * (startIndex < 0) : false
         * (startIndex >= array.length) : true
         * (i >= 0) : true
         * (valueToFind == array[i]) : true
         */
        //Arrange Statement(s)
        long[] longArray = new long[] { 1L };
        //Act Statement(s)
        int result = ArrayUtils.lastIndexOf(longArray, 1L, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${lastIndexOf15WhenValueToFindNotEqualsIIndexOfArray}, hash: 73675ADF06FB295E9836C88097CF457E
    @Test()
    void lastIndexOf15WhenValueToFindNotEqualsIIndexOfArray() {
        /* Branches:
         * (array == null) : false
         * (startIndex < 0) : false
         * (startIndex >= array.length) : true
         * (i >= 0) : true
         * (valueToFind == array[i]) : false
         */
        //Arrange Statement(s)
        long[] longArray = new long[] { 1L };
        //Act Statement(s)
        int result = ArrayUtils.lastIndexOf(longArray, 2L, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${lastIndexOf16Test}, hash: EFDEEA07F14BDADA04CFDBD0BCE706D5
    @Test()
    void lastIndexOf16Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            Object object = new Object();
            arrayUtils.when(() -> ArrayUtils.lastIndexOf(objectArray, object, 2147483647)).thenReturn(0);
            //Act Statement(s)
            int result = ArrayUtils.lastIndexOf(objectArray, object);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.lastIndexOf(objectArray, object, 2147483647), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOf17WhenStartIndexLessThan0}, hash: 187DC52776DF2102F5C783DD3840516C
    @Test()
    void lastIndexOf17WhenStartIndexLessThan0() {
        /* Branches:
         * (array == null) : false
         * (startIndex < 0) : true
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        Object object = new Object();
        //Act Statement(s)
        int result = ArrayUtils.lastIndexOf(objectArray, object, -2147483648);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${lastIndexOf17WhenIIndexOfArrayIsNull}, hash: 9F90B0254083FD66C7C7FB23E6872F6F
    @Test()
    void lastIndexOf17WhenIIndexOfArrayIsNull() {
        /* Branches:
         * (array == null) : false
         * (startIndex < 0) : false
         * (startIndex >= array.length) : true
         * (objectToFind == null) : true
         * (i >= 0) : true
         * (array[i] == null) : true
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] { (Object) null };
        Object object = null;
        //Act Statement(s)
        int result = ArrayUtils.lastIndexOf(objectArray, object, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${lastIndexOf17WhenIIndexOfArrayIsNotNull}, hash: 95BEAD624C49567F3D9CDB8409EE9EBB
    @Test()
    void lastIndexOf17WhenIIndexOfArrayIsNotNull() {
        /* Branches:
         * (array == null) : false
         * (startIndex < 0) : false
         * (startIndex >= array.length) : true
         * (objectToFind == null) : true
         * (i >= 0) : true
         * (array[i] == null) : false
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object[] objectArray = new Object[] { object };
        Object object2 = null;
        //Act Statement(s)
        int result = ArrayUtils.lastIndexOf(objectArray, object2, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${lastIndexOf17WhenObjectToFindEqualsIIndexOfArray}, hash: 675402E31428F3E816E06FD097BE9A52
    @Test()
    void lastIndexOf17WhenObjectToFindEqualsIIndexOfArray() {
        /* Branches:
         * (array == null) : false
         * (startIndex < 0) : false
         * (startIndex >= array.length) : true
         * (objectToFind == null) : false
         * (array.getClass().getComponentType().isInstance(objectToFind)) : true
         * (i >= 0) : true
         * (objectToFind.equals(array[i])) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object[] objectArray = new Object[] { object };
        //Act Statement(s)
        int result = ArrayUtils.lastIndexOf(objectArray, object, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${lastIndexOf17WhenObjectToFindNotEqualsIIndexOfArray}, hash: E01D19605DC9566CBCFEC894A73D9BF9
    @Test()
    void lastIndexOf17WhenObjectToFindNotEqualsIIndexOfArray() {
        /* Branches:
         * (array == null) : false
         * (startIndex < 0) : false
         * (startIndex >= array.length) : true
         * (objectToFind == null) : false
         * (array.getClass().getComponentType().isInstance(objectToFind)) : true
         * (i >= 0) : true
         * (objectToFind.equals(array[i])) : false
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object[] objectArray = new Object[] { object };
        Object object2 = new Object();
        //Act Statement(s)
        int result = ArrayUtils.lastIndexOf(objectArray, object2, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${lastIndexOf18Test}, hash: 045349A185E7EBFD18C636C3BDA762CE
    @Test()
    void lastIndexOf18Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.lastIndexOf(shortArray, (short) 0, 2147483647)).thenReturn(0);
            //Act Statement(s)
            int result = ArrayUtils.lastIndexOf(shortArray, (short) 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.lastIndexOf(shortArray, (short) 0, 2147483647), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${lastIndexOf19WhenStartIndexLessThan0}, hash: 37FE56F912DD21DB6E7C17F4BBFC4192
    @Test()
    void lastIndexOf19WhenStartIndexLessThan0() {
        /* Branches:
         * (array == null) : false
         * (startIndex < 0) : true
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] {};
        //Act Statement(s)
        int result = ArrayUtils.lastIndexOf(shortArray, (short) 0, -2147483648);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${lastIndexOf19WhenValueToFindEqualsIIndexOfArray}, hash: 8B3FF6F95CBB9341DF318791043A8920
    @Test()
    void lastIndexOf19WhenValueToFindEqualsIIndexOfArray() {
        /* Branches:
         * (array == null) : false
         * (startIndex < 0) : false
         * (startIndex >= array.length) : true
         * (i >= 0) : true
         * (valueToFind == array[i]) : true
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] { (short) 1 };
        //Act Statement(s)
        int result = ArrayUtils.lastIndexOf(shortArray, (short) 1, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${lastIndexOf19WhenValueToFindNotEqualsIIndexOfArray}, hash: FB32535C3792DE293BDE2CDB186B8612
    @Test()
    void lastIndexOf19WhenValueToFindNotEqualsIIndexOfArray() {
        /* Branches:
         * (array == null) : false
         * (startIndex < 0) : false
         * (startIndex >= array.length) : true
         * (i >= 0) : true
         * (valueToFind == array[i]) : false
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] { (short) 2 };
        //Act Statement(s)
        int result = ArrayUtils.lastIndexOf(shortArray, (short) 1, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${newInstanceTest}, hash: 8115C271A0555C11D34426CCCAE06EA0
    @Test()
    void newInstanceTest() {
        //Act Statement(s)
        Object[] result = ArrayUtils.newInstance(Object.class, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${nullToWhenIsEmptyArray}, hash: 5CB0D580817C98E7573CDAC1CB41B6A9
    @Test()
    void nullToWhenIsEmptyArray() {
        /* Branches:
         * (isEmpty(array)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(objectArray)).thenReturn(true);
            Object[] objectArray2 = new Object[] {};
            //Act Statement(s)
            Object[] result = ArrayUtils.nullTo(objectArray, objectArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(objectArray2));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(objectArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToWhenIsEmptyNotArray}, hash: CD86317181D7736A82B33A0EFB52D49C
    @Test()
    void nullToWhenIsEmptyNotArray() {
        /* Branches:
         * (isEmpty(array)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(objectArray)).thenReturn(false);
            Object[] objectArray2 = new Object[] {};
            //Act Statement(s)
            Object[] result = ArrayUtils.nullTo(objectArray, objectArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(objectArray));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(objectArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmptyWhenIsEmptyArray}, hash: 98CE6001CE19FB05002BC2699197A801
    @Test()
    void nullToEmptyWhenIsEmptyArray() {
        /* Branches:
         * (isEmpty(array)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(booleanArray)).thenReturn(true);
            //Act Statement(s)
            boolean[] result = ArrayUtils.nullToEmpty(booleanArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.length, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(booleanArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmptyWhenIsEmptyNotArray}, hash: B71AACC6908F8C8D643B97D703A79EBF
    @Test()
    void nullToEmptyWhenIsEmptyNotArray() {
        /* Branches:
         * (isEmpty(array)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(booleanArray)).thenReturn(false);
            //Act Statement(s)
            boolean[] result = ArrayUtils.nullToEmpty(booleanArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(booleanArray));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(booleanArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmpty1Test}, hash: 505DAB42304EEDD35200D718D9B5AA28
    @Test()
    void nullToEmpty1Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Boolean[] booleanArray = new Boolean[] {};
            Boolean[] booleanArray2 = new Boolean[] {};
            Boolean[] booleanArray3 = new Boolean[] {};
            arrayUtils.when(() -> ArrayUtils.nullTo(booleanArray2, booleanArray3)).thenReturn(booleanArray);
            //Act Statement(s)
            Boolean[] result = ArrayUtils.nullToEmpty(booleanArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(booleanArray));
                arrayUtils.verify(() -> ArrayUtils.nullTo(booleanArray2, booleanArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmpty2WhenIsEmptyArray}, hash: 97943EEA5930B942F35884EC23465A6D
    @Test()
    void nullToEmpty2WhenIsEmptyArray() {
        /* Branches:
         * (isEmpty(array)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(byteArray)).thenReturn(true);
            //Act Statement(s)
            byte[] result = ArrayUtils.nullToEmpty(byteArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.length, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(byteArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmpty2WhenIsEmptyNotArray}, hash: 5A6DB297FC13BE4754CB47BB24C10BFA
    @Test()
    void nullToEmpty2WhenIsEmptyNotArray() {
        /* Branches:
         * (isEmpty(array)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(byteArray)).thenReturn(false);
            //Act Statement(s)
            byte[] result = ArrayUtils.nullToEmpty(byteArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(byteArray));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(byteArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmpty3Test}, hash: 7E624D878BEFC4F44B12B336D08FB1B8
    @Test()
    void nullToEmpty3Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Byte[] byteArray = new Byte[] {};
            Byte[] byteArray2 = new Byte[] {};
            Byte[] byteArray3 = new Byte[] {};
            arrayUtils.when(() -> ArrayUtils.nullTo(byteArray2, byteArray3)).thenReturn(byteArray);
            //Act Statement(s)
            Byte[] result = ArrayUtils.nullToEmpty(byteArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(byteArray));
                arrayUtils.verify(() -> ArrayUtils.nullTo(byteArray2, byteArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmpty4WhenIsEmptyArray}, hash: C69B95A6591B1F3B4BB6E5F6F4FD9DA3
    @Test()
    void nullToEmpty4WhenIsEmptyArray() {
        /* Branches:
         * (isEmpty(array)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(charArray)).thenReturn(true);
            //Act Statement(s)
            char[] result = ArrayUtils.nullToEmpty(charArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.length, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(charArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmpty4WhenIsEmptyNotArray}, hash: 112E6AB3B60BA00D008F88D1B6166485
    @Test()
    void nullToEmpty4WhenIsEmptyNotArray() {
        /* Branches:
         * (isEmpty(array)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(charArray)).thenReturn(false);
            //Act Statement(s)
            char[] result = ArrayUtils.nullToEmpty(charArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(charArray));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(charArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmpty5Test}, hash: D2CE8D061C0CE1152C2FD73292CD6C41
    @Test()
    void nullToEmpty5Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Character[] characterArray = new Character[] {};
            Character[] characterArray2 = new Character[] {};
            Character[] characterArray3 = new Character[] {};
            arrayUtils.when(() -> ArrayUtils.nullTo(characterArray2, characterArray3)).thenReturn(characterArray);
            //Act Statement(s)
            Character[] result = ArrayUtils.nullToEmpty(characterArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(characterArray));
                arrayUtils.verify(() -> ArrayUtils.nullTo(characterArray2, characterArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmpty6Test}, hash: 1377C25D269C5125EC16FC8474AF9106
    @Test()
    void nullToEmpty6Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Class<?>[] classArray = new Class[] {};
            Class<?>[] classArray2 = new Class[] {};
            Class<?>[] classArray3 = new Class[] {};
            arrayUtils.when(() -> ArrayUtils.nullTo(classArray2, classArray3)).thenReturn(classArray);
            //Act Statement(s)
            Class<?>[] result = ArrayUtils.nullToEmpty(classArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(classArray));
                arrayUtils.verify(() -> ArrayUtils.nullTo(classArray2, classArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmpty7WhenIsEmptyArray}, hash: A24E4FC7345DA76764F66E5B81C77FF0
    @Test()
    void nullToEmpty7WhenIsEmptyArray() {
        /* Branches:
         * (isEmpty(array)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(doubleArray)).thenReturn(true);
            //Act Statement(s)
            double[] result = ArrayUtils.nullToEmpty(doubleArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.length, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(doubleArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmpty7WhenIsEmptyNotArray}, hash: 3E5DA3C11A54640161C31FBC29725728
    @Test()
    void nullToEmpty7WhenIsEmptyNotArray() {
        /* Branches:
         * (isEmpty(array)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(doubleArray)).thenReturn(false);
            //Act Statement(s)
            double[] result = ArrayUtils.nullToEmpty(doubleArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(doubleArray));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(doubleArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmpty8Test}, hash: 63139B48FA2BAAFFBC9E32AD33E9979E
    @Test()
    void nullToEmpty8Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Double[] doubleArray = new Double[] {};
            Double[] doubleArray2 = new Double[] {};
            Double[] doubleArray3 = new Double[] {};
            arrayUtils.when(() -> ArrayUtils.nullTo(doubleArray2, doubleArray3)).thenReturn(doubleArray);
            //Act Statement(s)
            Double[] result = ArrayUtils.nullToEmpty(doubleArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(doubleArray));
                arrayUtils.verify(() -> ArrayUtils.nullTo(doubleArray2, doubleArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmpty9WhenIsEmptyArray}, hash: CB84B4CBC11D847F2DD405C612EBC106
    @Test()
    void nullToEmpty9WhenIsEmptyArray() {
        /* Branches:
         * (isEmpty(array)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(floatArray)).thenReturn(true);
            //Act Statement(s)
            float[] result = ArrayUtils.nullToEmpty(floatArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.length, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(floatArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmpty9WhenIsEmptyNotArray}, hash: 92CC0BB0226B3A6BE2F8C00E73EE5D42
    @Test()
    void nullToEmpty9WhenIsEmptyNotArray() {
        /* Branches:
         * (isEmpty(array)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(floatArray)).thenReturn(false);
            //Act Statement(s)
            float[] result = ArrayUtils.nullToEmpty(floatArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(floatArray));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(floatArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmpty10Test}, hash: 1831AA445399A9417C79BDA1EE3CE1D6
    @Test()
    void nullToEmpty10Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Float[] floatArray = new Float[] {};
            Float[] floatArray2 = new Float[] {};
            Float[] floatArray3 = new Float[] {};
            arrayUtils.when(() -> ArrayUtils.nullTo(floatArray2, floatArray3)).thenReturn(floatArray);
            //Act Statement(s)
            Float[] result = ArrayUtils.nullToEmpty(floatArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(floatArray));
                arrayUtils.verify(() -> ArrayUtils.nullTo(floatArray2, floatArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmpty11WhenIsEmptyArray}, hash: 56731E4A47C4D4801D0C362ADBBB991C
    @Test()
    void nullToEmpty11WhenIsEmptyArray() {
        /* Branches:
         * (isEmpty(array)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(intArray)).thenReturn(true);
            //Act Statement(s)
            int[] result = ArrayUtils.nullToEmpty(intArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.length, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(intArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmpty11WhenIsEmptyNotArray}, hash: 2CEEBFAB3A154E5A916773AE9EFB2E27
    @Test()
    void nullToEmpty11WhenIsEmptyNotArray() {
        /* Branches:
         * (isEmpty(array)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(intArray)).thenReturn(false);
            //Act Statement(s)
            int[] result = ArrayUtils.nullToEmpty(intArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(intArray));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(intArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmpty12Test}, hash: 40B45877B0699B5FB3221BB6B4238F04
    @Test()
    void nullToEmpty12Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Integer[] integerArray = new Integer[] {};
            Integer[] integerArray2 = new Integer[] {};
            Integer[] integerArray3 = new Integer[] {};
            arrayUtils.when(() -> ArrayUtils.nullTo(integerArray2, integerArray3)).thenReturn(integerArray);
            //Act Statement(s)
            Integer[] result = ArrayUtils.nullToEmpty(integerArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(integerArray));
                arrayUtils.verify(() -> ArrayUtils.nullTo(integerArray2, integerArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmpty13WhenIsEmptyArray}, hash: D8620E77669DE7C72C3F712B02B15BA5
    @Test()
    void nullToEmpty13WhenIsEmptyArray() {
        /* Branches:
         * (isEmpty(array)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(longArray)).thenReturn(true);
            //Act Statement(s)
            long[] result = ArrayUtils.nullToEmpty(longArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.length, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(longArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmpty13WhenIsEmptyNotArray}, hash: BE249188A43156CE634A5AA1EB719D1D
    @Test()
    void nullToEmpty13WhenIsEmptyNotArray() {
        /* Branches:
         * (isEmpty(array)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(longArray)).thenReturn(false);
            //Act Statement(s)
            long[] result = ArrayUtils.nullToEmpty(longArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(longArray));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(longArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmpty14Test}, hash: E4992A06E5E365D8F47E803CA014DFAE
    @Test()
    void nullToEmpty14Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Long[] longArray = new Long[] {};
            Long[] longArray2 = new Long[] {};
            Long[] longArray3 = new Long[] {};
            arrayUtils.when(() -> ArrayUtils.nullTo(longArray2, longArray3)).thenReturn(longArray);
            //Act Statement(s)
            Long[] result = ArrayUtils.nullToEmpty(longArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(longArray));
                arrayUtils.verify(() -> ArrayUtils.nullTo(longArray2, longArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmpty15Test}, hash: DCA19C4187E681BC89709438127203BE
    @Test()
    void nullToEmpty15Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            Object[] objectArray2 = new Object[] {};
            Object[] objectArray3 = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.nullTo(objectArray2, objectArray3)).thenReturn(objectArray);
            //Act Statement(s)
            Object[] result = ArrayUtils.nullToEmpty(objectArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(objectArray));
                arrayUtils.verify(() -> ArrayUtils.nullTo(objectArray2, objectArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmpty16WhenIsEmptyArray}, hash: D927DB8593C2C68699749103F0CAB022
    @Test()
    void nullToEmpty16WhenIsEmptyArray() {
        /* Branches:
         * (isEmpty(array)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(shortArray)).thenReturn(true);
            //Act Statement(s)
            short[] result = ArrayUtils.nullToEmpty(shortArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.length, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(shortArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmpty16WhenIsEmptyNotArray}, hash: 8DECEFFA58DB43C406D9011974831B01
    @Test()
    void nullToEmpty16WhenIsEmptyNotArray() {
        /* Branches:
         * (isEmpty(array)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(shortArray)).thenReturn(false);
            //Act Statement(s)
            short[] result = ArrayUtils.nullToEmpty(shortArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(shortArray));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(shortArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmpty17Test}, hash: 86FBC8DC4B89C95F2EEF959E7F8D6AB2
    @Test()
    void nullToEmpty17Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Short[] shortArray = new Short[] {};
            Short[] shortArray2 = new Short[] {};
            Short[] shortArray3 = new Short[] {};
            arrayUtils.when(() -> ArrayUtils.nullTo(shortArray2, shortArray3)).thenReturn(shortArray);
            //Act Statement(s)
            Short[] result = ArrayUtils.nullToEmpty(shortArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(shortArray));
                arrayUtils.verify(() -> ArrayUtils.nullTo(shortArray2, shortArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmpty18Test}, hash: 6EF7720E87C96A82C04AE6EA859D8322
    @Test()
    void nullToEmpty18Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            String[] stringArray = new String[] {};
            String[] stringArray2 = new String[] {};
            String[] stringArray3 = new String[] {};
            arrayUtils.when(() -> ArrayUtils.nullTo(stringArray2, stringArray3)).thenReturn(stringArray);
            //Act Statement(s)
            String[] result = ArrayUtils.nullToEmpty(stringArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(stringArray));
                arrayUtils.verify(() -> ArrayUtils.nullTo(stringArray2, stringArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullToEmpty19WhenTypeIsNullThrowsIllegalArgumentException}, hash: F0402098BDA96F3AAABFF93CB1745F4A
    @Test()
    void nullToEmpty19WhenTypeIsNullThrowsIllegalArgumentException() {
        /* Branches:
         * (type == null) : true
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        Class<Object[]> _class = null;
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The type must not be null");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            ArrayUtils.nullToEmpty(objectArray, _class);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${nullToEmpty19WhenArrayIsNull}, hash: D57C16AE62EDACC110E5356CBDA7131E
    @Test()
    void nullToEmpty19WhenArrayIsNull() {
        /* Branches:
         * (type == null) : false
         * (array == null) : true
         */
        //Arrange Statement(s)
        Object[] object = null;
        //Act Statement(s)
        Object[] result = ArrayUtils.nullToEmpty(object, Object[].class);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${nullToEmpty19WhenArrayIsNotNull}, hash: 35011E5F89EA9D6B28F0E117FFAFB324
    @Test()
    void nullToEmpty19WhenArrayIsNotNull() {
        /* Branches:
         * (type == null) : false
         * (array == null) : false
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        //Act Statement(s)
        Object[] result = ArrayUtils.nullToEmpty(objectArray, Object[].class);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(objectArray)));
    }

    //BaseRock generated method id: ${removeTest}, hash: 4AA83B930D3E4E204D6E0E687F097710
    @Disabled()
    @Test()
    void removeTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        //Act Statement(s)
        boolean[] result = ArrayUtils.remove(booleanArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${remove1Test}, hash: 74BD9F9A71AEFE676FDEB286DCC59576
    @Disabled()
    @Test()
    void remove1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        //Act Statement(s)
        byte[] result = ArrayUtils.remove(byteArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${remove2Test}, hash: F1AE110DA59DA3725436887FE8D87021
    @Disabled()
    @Test()
    void remove2Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        //Act Statement(s)
        char[] result = ArrayUtils.remove(charArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${remove3Test}, hash: 198F8BB27E00ED1917BE34E03FA86309
    @Disabled()
    @Test()
    void remove3Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        double[] doubleArray = new double[] {};
        //Act Statement(s)
        double[] result = ArrayUtils.remove(doubleArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${remove4Test}, hash: 97DA965B6C0B7B81A88F74F0C2FD7438
    @Disabled()
    @Test()
    void remove4Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        float[] floatArray = new float[] {};
        //Act Statement(s)
        float[] result = ArrayUtils.remove(floatArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${remove5Test}, hash: B7E2A7D6AC7971F27D7BE63501957599
    @Disabled()
    @Test()
    void remove5Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        int[] intArray = new int[] {};
        //Act Statement(s)
        int[] result = ArrayUtils.remove(intArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${remove6Test}, hash: 7223C046BDB32A30582FF1E029EBE656
    @Disabled()
    @Test()
    void remove6Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        long[] longArray = new long[] {};
        //Act Statement(s)
        long[] result = ArrayUtils.remove(longArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${remove8WhenIndexGreaterThanOrEqualsToLengthThrowsIndexOutOfBoundsException}, hash: 9FCCF45EC2C16ACEF6B9CFB4EF527EEE
    @Test()
    void remove8WhenIndexGreaterThanOrEqualsToLengthThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (index < 0) : false  #  inside remove method
         * (index >= length) : true  #  inside remove method
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(shortArray)).thenReturn(2);
            //Act Statement(s)
            final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
                ArrayUtils.remove(shortArray, 2);
            });
            IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("Index: 2, Length: 2");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
                arrayUtils.verify(() -> ArrayUtils.getLength(shortArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${remove8WhenIndexLessThanLengthMinus1}, hash: E2B603C627FA690479EB4A5319BC4502
    @Disabled()
    @Test()
    void remove8WhenIndexLessThanLengthMinus1() {
        /* Branches:
         * (index < 0) : false  #  inside remove method
         * (index >= length) : false  #  inside remove method
         * (index < length - 1) : true  #  inside remove method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(shortArray)).thenReturn(3);
            //Act Statement(s)
            short[] result = ArrayUtils.remove(shortArray, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.length, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.getLength(shortArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${remove9WhenIndexGreaterThanOrEqualsToLengthThrowsIndexOutOfBoundsException}, hash: 9FA2B98A5BC53E04565D2C5E1EBDAFCB
    @Test()
    void remove9WhenIndexGreaterThanOrEqualsToLengthThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (index < 0) : false  #  inside remove method
         * (index >= length) : true  #  inside remove method
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(objectArray)).thenReturn(2);
            //Act Statement(s)
            final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
                ArrayUtils.remove(objectArray, 2);
            });
            IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("Index: 2, Length: 2");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
                arrayUtils.verify(() -> ArrayUtils.getLength(objectArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${remove9WhenIndexLessThanLengthMinus1}, hash: 1AE591943F81F3A60E5B0B5155724562
    @Disabled()
    @Test()
    void remove9WhenIndexLessThanLengthMinus1() {
        /* Branches:
         * (index < 0) : false  #  inside remove method
         * (index >= length) : false  #  inside remove method
         * (index < length - 1) : true  #  inside remove method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.getLength(objectArray)).thenReturn(3);
            //Act Statement(s)
            Object[] result = ArrayUtils.remove(objectArray, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.length, equalTo(0));
                arrayUtils.verify(() -> ArrayUtils.getLength(objectArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeAllTest}, hash: 6E70E229C5CD4F140035C2B76AE2F636
    @Test()
    void removeAllTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        int[] intArray = new int[] {};
        //Act Statement(s)
        boolean[] result = ArrayUtils.removeAll(booleanArray, intArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${removeAll1Test}, hash: BAF92A2BE81C91FED2806DAAF0268F73
    @Test()
    void removeAll1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        int[] intArray = new int[] {};
        //Act Statement(s)
        byte[] result = ArrayUtils.removeAll(byteArray, intArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${removeAll2Test}, hash: 236BB658A25664348217651A1C0AF0D5
    @Test()
    void removeAll2Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        int[] intArray = new int[] {};
        //Act Statement(s)
        char[] result = ArrayUtils.removeAll(charArray, intArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${removeAll3Test}, hash: 2EC40931821B89F31AEBCB4985FABE50
    @Test()
    void removeAll3Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        double[] doubleArray = new double[] {};
        int[] intArray = new int[] {};
        //Act Statement(s)
        double[] result = ArrayUtils.removeAll(doubleArray, intArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${removeAll4Test}, hash: 2C61BD86DC51C5FB5148F3E0CE855AE5
    @Test()
    void removeAll4Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        float[] floatArray = new float[] {};
        int[] intArray = new int[] {};
        //Act Statement(s)
        float[] result = ArrayUtils.removeAll(floatArray, intArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${removeAll5Test}, hash: 392C7FE83FDEBBA3925913D87ABBDBE4
    @Test()
    void removeAll5Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        int[] intArray = new int[] {};
        int[] intArray2 = new int[] {};
        //Act Statement(s)
        int[] result = ArrayUtils.removeAll(intArray, intArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${removeAll6Test}, hash: B80575196E106BCAE4E66E4A7BEB1C0E
    @Test()
    void removeAll6Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        long[] longArray = new long[] {};
        int[] intArray = new int[] {};
        //Act Statement(s)
        long[] result = ArrayUtils.removeAll(longArray, intArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${removeAll7WhenArrayIsNull}, hash: B06CCED6D4DA60DF086606463B1F5F7F
    @Test()
    void removeAll7WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Object object = null;
        BitSet bitSet = new BitSet();
        //Act Statement(s)
        Object result = ArrayUtils.removeAll(object, bitSet);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${removeAll7WhenCountGreaterThan0AndCountGreaterThan0}, hash: 2E463B807D59B9D9FB82F43B7274B386
    @Disabled()
    @Test()
    void removeAll7WhenCountGreaterThan0AndCountGreaterThan0() {
        /* Branches:
         * (array == null) : false
         * ((set = indices.nextSetBit(srcIndex)) != -1) : true
         * (count > 0) : true
         * (count > 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            arrayUtils.when(() -> ArrayUtils.getLength(object)).thenReturn(0);
            BitSet bitSet = new BitSet();
            //Act Statement(s)
            Object result = ArrayUtils.removeAll(object, bitSet);
            //Assert statement(s)
            //TODO: Please implement equals method in Object for verification of the entire object or you need to adjust respective assertion statements
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                arrayUtils.verify(() -> ArrayUtils.getLength(object), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeAll8WhenArrayIsNull}, hash: FD4E90E506BF2C0BBAAFDF5987FE0302
    @Test()
    void removeAll8WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Object object = null;
        int[] intArray = new int[] {};
        //Act Statement(s)
        Object result = ArrayUtils.removeAll(object, intArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${removeAll8WhenIndexGreaterThanOrEqualsToLengthThrowsIndexOutOfBoundsException}, hash: 0B4973D07D3B49AFC61E104FEF7312FF
    @Disabled()
    @Test()
    void removeAll8WhenIndexGreaterThanOrEqualsToLengthThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (array == null) : false
         * (isNotEmpty(clonedIndices)) : true
         * (--i >= 0) : true
         * (index < 0) : false
         * (index >= length) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            arrayUtils.when(() -> ArrayUtils.getLength(object)).thenReturn(0);
            int[] intArray = new int[] {};
            int[] intArray2 = new int[] {};
            arrayUtils.when(() -> ArrayUtils.clone(intArray2)).thenReturn(intArray);
            int[] intArray3 = new int[] {};
            arrayUtils.when(() -> ArrayUtils.isNotEmpty(intArray3)).thenReturn(false);
            //Act Statement(s)
            final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
                ArrayUtils.removeAll(object, intArray2);
            });
            IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("s1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
                arrayUtils.verify(() -> ArrayUtils.getLength(object), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(intArray2), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.isNotEmpty(intArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeAll8WhenEndMinusIndexGreaterThan1AndEndGreaterThan0}, hash: 5397AAF0AF0BE3C226B99D3478B16811
    @Disabled()
    @Test()
    void removeAll8WhenEndMinusIndexGreaterThan1AndEndGreaterThan0() {
        /* Branches:
         * (array == null) : false
         * (isNotEmpty(clonedIndices)) : true
         * (--i >= 0) : true
         * (index < 0) : false
         * (index >= length) : false
         * (index >= prevIndex) : false
         * (diff < length) : true
         * (clonedIndices != null) : true
         * (i >= 0) : true
         * (end - index > 1) : true
         * (end > 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            arrayUtils.when(() -> ArrayUtils.getLength(object)).thenReturn(0);
            int[] intArray = new int[] {};
            int[] intArray2 = new int[] {};
            arrayUtils.when(() -> ArrayUtils.clone(intArray2)).thenReturn(intArray);
            int[] intArray3 = new int[] {};
            arrayUtils.when(() -> ArrayUtils.isNotEmpty(intArray3)).thenReturn(false);
            //Act Statement(s)
            Object result = ArrayUtils.removeAll(object, intArray2);
            //Assert statement(s)
            //TODO: Please implement equals method in Object for verification of the entire object or you need to adjust respective assertion statements
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                arrayUtils.verify(() -> ArrayUtils.getLength(object), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(intArray2), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.isNotEmpty(intArray3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeAll9Test}, hash: 908411901BA6B18C4956F259635194BB
    @Test()
    void removeAll9Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            short[] shortArray2 = new short[] {};
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(shortArray2, intArray)).thenReturn(shortArray);
            //Act Statement(s)
            short[] result = ArrayUtils.removeAll(shortArray2, intArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(shortArray));
                arrayUtils.verify(() -> ArrayUtils.removeAll(shortArray2, intArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeAll10Test}, hash: 83CE940DE630E23A88925518E903B217
    @Test()
    void removeAll10Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            Object[] objectArray2 = new Object[] {};
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(objectArray2, intArray)).thenReturn(objectArray);
            //Act Statement(s)
            Object[] result = ArrayUtils.removeAll(objectArray2, intArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(objectArray));
                arrayUtils.verify(() -> ArrayUtils.removeAll(objectArray2, intArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeAllOccurencesTest}, hash: 4C1CE965B098F0CB09E06B2C874C18B7
    @Test()
    void removeAllOccurencesTest() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.indexesOf(booleanArray, false)).thenReturn(bitSet);
            boolean[] booleanArray2 = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(booleanArray, bitSet)).thenReturn(booleanArray2);
            //Act Statement(s)
            boolean[] result = ArrayUtils.removeAllOccurences(booleanArray, false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(booleanArray2));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(booleanArray, false), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(booleanArray, bitSet), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeAllOccurences1Test}, hash: FCF66345D7F2C395AEBAB593E80DB953
    @Test()
    void removeAllOccurences1Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.indexesOf(byteArray, (byte) 0)).thenReturn(bitSet);
            byte[] byteArray2 = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(byteArray, bitSet)).thenReturn(byteArray2);
            //Act Statement(s)
            byte[] result = ArrayUtils.removeAllOccurences(byteArray, (byte) 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(byteArray2));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(byteArray, (byte) 0), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(byteArray, bitSet), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeAllOccurences2Test}, hash: E1BBF587EF1C3E86790970C74FAC2752
    @Test()
    void removeAllOccurences2Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.indexesOf(charArray, 'A')).thenReturn(bitSet);
            char[] charArray2 = new char[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(charArray, bitSet)).thenReturn(charArray2);
            //Act Statement(s)
            char[] result = ArrayUtils.removeAllOccurences(charArray, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(charArray2));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(charArray, 'A'), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(charArray, bitSet), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeAllOccurences3Test}, hash: CEFB34B61742B4E86D9F3D43DACF9134
    @Test()
    void removeAllOccurences3Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.indexesOf(doubleArray, Double.parseDouble("0.0"))).thenReturn(bitSet);
            double[] doubleArray2 = new double[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(doubleArray, bitSet)).thenReturn(doubleArray2);
            //Act Statement(s)
            double[] result = ArrayUtils.removeAllOccurences(doubleArray, Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(doubleArray2));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(doubleArray, Double.parseDouble("0.0")), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(doubleArray, bitSet), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeAllOccurences4Test}, hash: 1BE65E6CC4C21C63896BBEE857238F8C
    @Test()
    void removeAllOccurences4Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.indexesOf(floatArray, Float.parseFloat("0.0"))).thenReturn(bitSet);
            float[] floatArray2 = new float[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(floatArray, bitSet)).thenReturn(floatArray2);
            //Act Statement(s)
            float[] result = ArrayUtils.removeAllOccurences(floatArray, Float.parseFloat("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(floatArray2));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(floatArray, Float.parseFloat("0.0")), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(floatArray, bitSet), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeAllOccurences5Test}, hash: 7ED12711F278509E083DDBEBDEB8584C
    @Test()
    void removeAllOccurences5Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.indexesOf(intArray, 0)).thenReturn(bitSet);
            int[] intArray2 = new int[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(intArray, bitSet)).thenReturn(intArray2);
            //Act Statement(s)
            int[] result = ArrayUtils.removeAllOccurences(intArray, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(intArray2));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(intArray, 0), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(intArray, bitSet), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeAllOccurences6Test}, hash: 593D1138BBD9DF88ADD064975ED240B3
    @Test()
    void removeAllOccurences6Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.indexesOf(longArray, 0L)).thenReturn(bitSet);
            long[] longArray2 = new long[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(longArray, bitSet)).thenReturn(longArray2);
            //Act Statement(s)
            long[] result = ArrayUtils.removeAllOccurences(longArray, 0L);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(longArray2));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(longArray, 0L), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(longArray, bitSet), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeAllOccurences7Test}, hash: 32E01386F09549BEEF45B8AC13AD880B
    @Test()
    void removeAllOccurences7Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.indexesOf(shortArray, (short) 0)).thenReturn(bitSet);
            short[] shortArray2 = new short[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(shortArray, bitSet)).thenReturn(shortArray2);
            //Act Statement(s)
            short[] result = ArrayUtils.removeAllOccurences(shortArray, (short) 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(shortArray2));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(shortArray, (short) 0), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(shortArray, bitSet), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeAllOccurences8Test}, hash: E488D9F3C175F7592C97A948FC984E66
    @Test()
    void removeAllOccurences8Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            Object[] objectArray = new Object[] {};
            Object object = new Object();
            arrayUtils.when(() -> ArrayUtils.indexesOf(objectArray, object)).thenReturn(bitSet);
            Object[] objectArray2 = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(objectArray, bitSet)).thenReturn(objectArray2);
            //Act Statement(s)
            Object[] result = ArrayUtils.removeAllOccurences(objectArray, object);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(objectArray2));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(objectArray, object), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(objectArray, bitSet), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeAllOccurrencesTest}, hash: B007DCF32D9FDDD449A4A7197AA80E2E
    @Test()
    void removeAllOccurrencesTest() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.indexesOf(booleanArray, false)).thenReturn(bitSet);
            boolean[] booleanArray2 = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(booleanArray, bitSet)).thenReturn(booleanArray2);
            //Act Statement(s)
            boolean[] result = ArrayUtils.removeAllOccurrences(booleanArray, false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(booleanArray2));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(booleanArray, false), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(booleanArray, bitSet), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeAllOccurrences1Test}, hash: EC30FB81139AD6786C8386E05A19C357
    @Test()
    void removeAllOccurrences1Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.indexesOf(byteArray, (byte) 0)).thenReturn(bitSet);
            byte[] byteArray2 = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(byteArray, bitSet)).thenReturn(byteArray2);
            //Act Statement(s)
            byte[] result = ArrayUtils.removeAllOccurrences(byteArray, (byte) 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(byteArray2));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(byteArray, (byte) 0), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(byteArray, bitSet), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeAllOccurrences2Test}, hash: 103C94BE089A7D6F9AC6529BA361752D
    @Test()
    void removeAllOccurrences2Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.indexesOf(charArray, 'A')).thenReturn(bitSet);
            char[] charArray2 = new char[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(charArray, bitSet)).thenReturn(charArray2);
            //Act Statement(s)
            char[] result = ArrayUtils.removeAllOccurrences(charArray, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(charArray2));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(charArray, 'A'), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(charArray, bitSet), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeAllOccurrences3Test}, hash: 1E417717C1A3E8A518DCD3DAD6D21F1C
    @Test()
    void removeAllOccurrences3Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.indexesOf(doubleArray, Double.parseDouble("0.0"))).thenReturn(bitSet);
            double[] doubleArray2 = new double[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(doubleArray, bitSet)).thenReturn(doubleArray2);
            //Act Statement(s)
            double[] result = ArrayUtils.removeAllOccurrences(doubleArray, Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(doubleArray2));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(doubleArray, Double.parseDouble("0.0")), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(doubleArray, bitSet), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeAllOccurrences4Test}, hash: 8BADCDDA6EC0FB174C6043E0861300A0
    @Test()
    void removeAllOccurrences4Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.indexesOf(floatArray, Float.parseFloat("0.0"))).thenReturn(bitSet);
            float[] floatArray2 = new float[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(floatArray, bitSet)).thenReturn(floatArray2);
            //Act Statement(s)
            float[] result = ArrayUtils.removeAllOccurrences(floatArray, Float.parseFloat("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(floatArray2));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(floatArray, Float.parseFloat("0.0")), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(floatArray, bitSet), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeAllOccurrences5Test}, hash: B6DBF46D11DAB631E8E293D941F7829E
    @Test()
    void removeAllOccurrences5Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.indexesOf(intArray, 0)).thenReturn(bitSet);
            int[] intArray2 = new int[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(intArray, bitSet)).thenReturn(intArray2);
            //Act Statement(s)
            int[] result = ArrayUtils.removeAllOccurrences(intArray, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(intArray2));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(intArray, 0), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(intArray, bitSet), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeAllOccurrences6Test}, hash: 1BCA7CEF35CDC67E4976E42914C15FD0
    @Test()
    void removeAllOccurrences6Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.indexesOf(longArray, 0L)).thenReturn(bitSet);
            long[] longArray2 = new long[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(longArray, bitSet)).thenReturn(longArray2);
            //Act Statement(s)
            long[] result = ArrayUtils.removeAllOccurrences(longArray, 0L);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(longArray2));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(longArray, 0L), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(longArray, bitSet), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeAllOccurrences7Test}, hash: 66ACC8EFADB159F2E5C5145F4CD5A92B
    @Test()
    void removeAllOccurrences7Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.indexesOf(shortArray, (short) 0)).thenReturn(bitSet);
            short[] shortArray2 = new short[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(shortArray, bitSet)).thenReturn(shortArray2);
            //Act Statement(s)
            short[] result = ArrayUtils.removeAllOccurrences(shortArray, (short) 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(shortArray2));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(shortArray, (short) 0), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(shortArray, bitSet), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeAllOccurrences8Test}, hash: 65DC446B5492F0A0623DE508DE7001D4
    @Test()
    void removeAllOccurrences8Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            BitSet bitSet = new BitSet();
            Object[] objectArray = new Object[] {};
            Object object = new Object();
            arrayUtils.when(() -> ArrayUtils.indexesOf(objectArray, object)).thenReturn(bitSet);
            Object[] objectArray2 = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(objectArray, bitSet)).thenReturn(objectArray2);
            //Act Statement(s)
            Object[] result = ArrayUtils.removeAllOccurrences(objectArray, object);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(objectArray2));
                arrayUtils.verify(() -> ArrayUtils.indexesOf(objectArray, object), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(objectArray, bitSet), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElementWhenIndexEqualsINDEX_NOT_FOUND}, hash: 81426D4196FD1C9A84ECA742C621C30F
    @Test()
    void removeElementWhenIndexEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (index == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(booleanArray, false)).thenReturn(-1);
            boolean[] booleanArray2 = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.clone(booleanArray)).thenReturn(booleanArray2);
            //Act Statement(s)
            boolean[] result = ArrayUtils.removeElement(booleanArray, false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(booleanArray2));
                arrayUtils.verify(() -> ArrayUtils.indexOf(booleanArray, false), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(booleanArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElementWhenIndexNotEqualsINDEX_NOT_FOUND}, hash: B4C105AFA8779EDC0CF1828B4B72C97F
    @Test()
    void removeElementWhenIndexNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (index == INDEX_NOT_FOUND) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(booleanArray, false)).thenReturn(1);
            boolean[] booleanArray2 = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.remove(booleanArray, 1)).thenReturn(booleanArray2);
            //Act Statement(s)
            boolean[] result = ArrayUtils.removeElement(booleanArray, false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(booleanArray2));
                arrayUtils.verify(() -> ArrayUtils.indexOf(booleanArray, false), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.remove(booleanArray, 1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElement1WhenIndexEqualsINDEX_NOT_FOUND}, hash: FD67949496D869637D5BFCC294E6F607
    @Test()
    void removeElement1WhenIndexEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (index == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(byteArray, (byte) 0)).thenReturn(-1);
            byte[] byteArray2 = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.clone(byteArray)).thenReturn(byteArray2);
            //Act Statement(s)
            byte[] result = ArrayUtils.removeElement(byteArray, (byte) 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(byteArray2));
                arrayUtils.verify(() -> ArrayUtils.indexOf(byteArray, (byte) 0), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(byteArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElement1WhenIndexNotEqualsINDEX_NOT_FOUND}, hash: 0C465FB6858C90764105D8DB8836018C
    @Test()
    void removeElement1WhenIndexNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (index == INDEX_NOT_FOUND) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(byteArray, (byte) 0)).thenReturn(1);
            byte[] byteArray2 = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.remove(byteArray, 1)).thenReturn(byteArray2);
            //Act Statement(s)
            byte[] result = ArrayUtils.removeElement(byteArray, (byte) 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(byteArray2));
                arrayUtils.verify(() -> ArrayUtils.indexOf(byteArray, (byte) 0), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.remove(byteArray, 1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElement2WhenIndexEqualsINDEX_NOT_FOUND}, hash: CDB7677A882DB6499E0969B055D54826
    @Test()
    void removeElement2WhenIndexEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (index == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(charArray, 'A')).thenReturn(-1);
            char[] charArray2 = new char[] {};
            arrayUtils.when(() -> ArrayUtils.clone(charArray)).thenReturn(charArray2);
            //Act Statement(s)
            char[] result = ArrayUtils.removeElement(charArray, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(charArray2));
                arrayUtils.verify(() -> ArrayUtils.indexOf(charArray, 'A'), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(charArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElement2WhenIndexNotEqualsINDEX_NOT_FOUND}, hash: B129C8E054A78F483EF09D1FBA6E8898
    @Test()
    void removeElement2WhenIndexNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (index == INDEX_NOT_FOUND) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(charArray, 'A')).thenReturn(1);
            char[] charArray2 = new char[] {};
            arrayUtils.when(() -> ArrayUtils.remove(charArray, 1)).thenReturn(charArray2);
            //Act Statement(s)
            char[] result = ArrayUtils.removeElement(charArray, 'A');
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(charArray2));
                arrayUtils.verify(() -> ArrayUtils.indexOf(charArray, 'A'), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.remove(charArray, 1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElement3WhenIndexEqualsINDEX_NOT_FOUND}, hash: AB6B398D741D138FB6CD7E6336D80BC1
    @Test()
    void removeElement3WhenIndexEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (index == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"))).thenReturn(-1);
            double[] doubleArray2 = new double[] {};
            arrayUtils.when(() -> ArrayUtils.clone(doubleArray)).thenReturn(doubleArray2);
            //Act Statement(s)
            double[] result = ArrayUtils.removeElement(doubleArray, Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(doubleArray2));
                arrayUtils.verify(() -> ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0")), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(doubleArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElement3WhenIndexNotEqualsINDEX_NOT_FOUND}, hash: 0AD27BA922F2B1223BA02DC8D281728A
    @Test()
    void removeElement3WhenIndexNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (index == INDEX_NOT_FOUND) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0"))).thenReturn(1);
            double[] doubleArray2 = new double[] {};
            arrayUtils.when(() -> ArrayUtils.remove(doubleArray, 1)).thenReturn(doubleArray2);
            //Act Statement(s)
            double[] result = ArrayUtils.removeElement(doubleArray, Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(doubleArray2));
                arrayUtils.verify(() -> ArrayUtils.indexOf(doubleArray, Double.parseDouble("0.0")), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.remove(doubleArray, 1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElement4WhenIndexEqualsINDEX_NOT_FOUND}, hash: E7D74B066E84BEFA7F89CED4204021C2
    @Test()
    void removeElement4WhenIndexEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (index == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(floatArray, Float.parseFloat("0.0"))).thenReturn(-1);
            float[] floatArray2 = new float[] {};
            arrayUtils.when(() -> ArrayUtils.clone(floatArray)).thenReturn(floatArray2);
            //Act Statement(s)
            float[] result = ArrayUtils.removeElement(floatArray, Float.parseFloat("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(floatArray2));
                arrayUtils.verify(() -> ArrayUtils.indexOf(floatArray, Float.parseFloat("0.0")), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(floatArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElement4WhenIndexNotEqualsINDEX_NOT_FOUND}, hash: C796E225697BD2E2CDF1CCEBD5BB69FF
    @Test()
    void removeElement4WhenIndexNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (index == INDEX_NOT_FOUND) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(floatArray, Float.parseFloat("0.0"))).thenReturn(1);
            float[] floatArray2 = new float[] {};
            arrayUtils.when(() -> ArrayUtils.remove(floatArray, 1)).thenReturn(floatArray2);
            //Act Statement(s)
            float[] result = ArrayUtils.removeElement(floatArray, Float.parseFloat("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(floatArray2));
                arrayUtils.verify(() -> ArrayUtils.indexOf(floatArray, Float.parseFloat("0.0")), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.remove(floatArray, 1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElement5WhenIndexEqualsINDEX_NOT_FOUND}, hash: 08AC1669EA7CEF9073480914F64C56F7
    @Test()
    void removeElement5WhenIndexEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (index == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(intArray, 0)).thenReturn(-1);
            int[] intArray2 = new int[] {};
            arrayUtils.when(() -> ArrayUtils.clone(intArray)).thenReturn(intArray2);
            //Act Statement(s)
            int[] result = ArrayUtils.removeElement(intArray, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(intArray2));
                arrayUtils.verify(() -> ArrayUtils.indexOf(intArray, 0), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(intArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElement5WhenIndexNotEqualsINDEX_NOT_FOUND}, hash: ABD8CDA14FE0CF4EEFA7A50B94B92150
    @Test()
    void removeElement5WhenIndexNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (index == INDEX_NOT_FOUND) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(intArray, 0)).thenReturn(1);
            int[] intArray2 = new int[] {};
            arrayUtils.when(() -> ArrayUtils.remove(intArray, 1)).thenReturn(intArray2);
            //Act Statement(s)
            int[] result = ArrayUtils.removeElement(intArray, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(intArray2));
                arrayUtils.verify(() -> ArrayUtils.indexOf(intArray, 0), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.remove(intArray, 1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElement6WhenIndexEqualsINDEX_NOT_FOUND}, hash: ED408089F01EAE0D8EA26FC8DFAC64ED
    @Test()
    void removeElement6WhenIndexEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (index == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(longArray, 0L)).thenReturn(-1);
            long[] longArray2 = new long[] {};
            arrayUtils.when(() -> ArrayUtils.clone(longArray)).thenReturn(longArray2);
            //Act Statement(s)
            long[] result = ArrayUtils.removeElement(longArray, 0L);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(longArray2));
                arrayUtils.verify(() -> ArrayUtils.indexOf(longArray, 0L), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(longArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElement6WhenIndexNotEqualsINDEX_NOT_FOUND}, hash: E10A55868B72BC744960CCEB74E88375
    @Test()
    void removeElement6WhenIndexNotEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (index == INDEX_NOT_FOUND) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(longArray, 0L)).thenReturn(1);
            long[] longArray2 = new long[] {};
            arrayUtils.when(() -> ArrayUtils.remove(longArray, 1)).thenReturn(longArray2);
            //Act Statement(s)
            long[] result = ArrayUtils.removeElement(longArray, 0L);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(longArray2));
                arrayUtils.verify(() -> ArrayUtils.indexOf(longArray, 0L), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.remove(longArray, 1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElement7WhenIndexEqualsINDEX_NOT_FOUND}, hash: 00027889EF507AEA2BAF27818986C192
    @Test()
    void removeElement7WhenIndexEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (index == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(shortArray, (short) 0)).thenReturn(-1);
            short[] shortArray2 = new short[] {};
            arrayUtils.when(() -> ArrayUtils.clone(shortArray)).thenReturn(shortArray2);
            //Act Statement(s)
            short[] result = ArrayUtils.removeElement(shortArray, (short) 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(shortArray2));
                arrayUtils.verify(() -> ArrayUtils.indexOf(shortArray, (short) 0), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(shortArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElement7WhenIndexGreaterThanOrEqualsToLengthThrowsIndexOutOfBoundsException}, hash: ABE0C3041AF514FC78EB44C197327B6C
    @Test()
    void removeElement7WhenIndexGreaterThanOrEqualsToLengthThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (index == INDEX_NOT_FOUND) : false
         * (index < 0) : false  #  inside remove method
         * (index >= length) : true  #  inside remove method
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(shortArray, (short) 0)).thenReturn(2);
            arrayUtils.when(() -> ArrayUtils.getLength(shortArray)).thenReturn(2);
            //Act Statement(s)
            final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
                ArrayUtils.removeElement(shortArray, (short) 0);
            });
            IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("Index: 2, Length: 2");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
                arrayUtils.verify(() -> ArrayUtils.indexOf(shortArray, (short) 0), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.getLength(shortArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElement7WhenIndexLessThanLengthMinus1}, hash: 43A5507554AB8132467ED7EA305F3E3A
    @Disabled()
    @Test()
    void removeElement7WhenIndexLessThanLengthMinus1() {
        /* Branches:
         * (index == INDEX_NOT_FOUND) : false
         * (index < 0) : false  #  inside remove method
         * (index >= length) : false  #  inside remove method
         * (index < length - 1) : true  #  inside remove method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.indexOf(shortArray, (short) 0)).thenReturn(0);
            arrayUtils.when(() -> ArrayUtils.getLength(shortArray)).thenReturn(2);
            //Act Statement(s)
            short[] result = ArrayUtils.removeElement(shortArray, (short) 0);
            //Assert statement(s)
            //TODO: Please implement equals method in Object for verification of the entire object or you need to adjust respective assertion statements
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                arrayUtils.verify(() -> ArrayUtils.indexOf(shortArray, (short) 0), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.getLength(shortArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElement8WhenIndexEqualsINDEX_NOT_FOUND}, hash: 90963AB9604118BFC34BACF3144188A4
    @Test()
    void removeElement8WhenIndexEqualsINDEX_NOT_FOUND() {
        /* Branches:
         * (index == INDEX_NOT_FOUND) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            Object object = new Object();
            arrayUtils.when(() -> ArrayUtils.indexOf(objectArray, object)).thenReturn(-1);
            Object[] objectArray2 = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.clone(objectArray)).thenReturn(objectArray2);
            //Act Statement(s)
            Object[] result = ArrayUtils.removeElement(objectArray, object);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(objectArray2));
                arrayUtils.verify(() -> ArrayUtils.indexOf(objectArray, object), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(objectArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElement8WhenIndexGreaterThanOrEqualsToLengthThrowsIndexOutOfBoundsException}, hash: 49DC10D4D1503538B050B5DC0612FF58
    @Test()
    void removeElement8WhenIndexGreaterThanOrEqualsToLengthThrowsIndexOutOfBoundsException() {
        /* Branches:
         * (index == INDEX_NOT_FOUND) : false
         * (index < 0) : false  #  inside remove method
         * (index >= length) : true  #  inside remove method
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            Object object = new Object();
            arrayUtils.when(() -> ArrayUtils.indexOf(objectArray, object)).thenReturn(2);
            arrayUtils.when(() -> ArrayUtils.getLength(objectArray)).thenReturn(2);
            //Act Statement(s)
            final IndexOutOfBoundsException result = assertThrows(IndexOutOfBoundsException.class, () -> {
                ArrayUtils.removeElement(objectArray, object);
            });
            IndexOutOfBoundsException indexOutOfBoundsException = new IndexOutOfBoundsException("Index: 2, Length: 2");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                assertThat(result.getMessage(), equalTo(indexOutOfBoundsException.getMessage()));
                arrayUtils.verify(() -> ArrayUtils.indexOf(objectArray, object), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.getLength(objectArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElement8WhenIndexLessThanLengthMinus1}, hash: BCC0387CA569571480FE49808046ACAC
    @Disabled()
    @Test()
    void removeElement8WhenIndexLessThanLengthMinus1() {
        /* Branches:
         * (index == INDEX_NOT_FOUND) : false
         * (index < 0) : false  #  inside remove method
         * (index >= length) : false  #  inside remove method
         * (index < length - 1) : true  #  inside remove method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            Object object = new Object();
            arrayUtils.when(() -> ArrayUtils.indexOf(objectArray, object)).thenReturn(0);
            arrayUtils.when(() -> ArrayUtils.getLength(objectArray)).thenReturn(2);
            //Act Statement(s)
            Object[] result = ArrayUtils.removeElement(objectArray, object);
            //Assert statement(s)
            //TODO: Please implement equals method in Object for verification of the entire object or you need to adjust respective assertion statements
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                arrayUtils.verify(() -> ArrayUtils.indexOf(objectArray, object), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.getLength(objectArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElementsWhenIsEmptyValues}, hash: 224A56726FCE61F0853FCB77EC9B1E97
    @Test()
    void removeElementsWhenIsEmptyValues() {
        /* Branches:
         * (isEmpty(array)) : false
         * (isEmpty(values)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(booleanArray)).thenReturn(false);
            boolean[] booleanArray2 = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(booleanArray2)).thenReturn(true);
            boolean[] booleanArray3 = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.clone(booleanArray)).thenReturn(booleanArray3);
            //Act Statement(s)
            boolean[] result = ArrayUtils.removeElements(booleanArray, booleanArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(booleanArray3));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(booleanArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(booleanArray2), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(booleanArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElementsWhenCountDecrementAndGetEquals0}, hash: 7E60D3476B90A26AB2B48539886C2E76
    @Test()
    void removeElementsWhenCountDecrementAndGetEquals0() {
        /* Branches:
         * (isEmpty(array)) : false
         * (isEmpty(values)) : false
         * (for-each(values)) : true
         * (count == null) : true
         * (i < array.length) : true
         * (count != null) : true
         * (count.decrementAndGet() == 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(booleanArray)).thenReturn(false);
            boolean[] booleanArray2 = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(booleanArray2)).thenReturn(false);
            boolean[] booleanArray3 = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(eq(booleanArray), (BitSet) any())).thenReturn(booleanArray3);
            //Act Statement(s)
            boolean[] result = ArrayUtils.removeElements(booleanArray, booleanArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(booleanArray3));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(booleanArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(booleanArray2), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(eq(booleanArray), (BitSet) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElements1WhenIsEmptyValues}, hash: F93887B514206FBAAE17071FDD5E9534
    @Test()
    void removeElements1WhenIsEmptyValues() {
        /* Branches:
         * (isEmpty(array)) : false
         * (isEmpty(values)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(byteArray)).thenReturn(false);
            byte[] byteArray2 = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(byteArray2)).thenReturn(true);
            byte[] byteArray3 = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.clone(byteArray)).thenReturn(byteArray3);
            //Act Statement(s)
            byte[] result = ArrayUtils.removeElements(byteArray, byteArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(byteArray3));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(byteArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(byteArray2), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(byteArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElements1WhenCountDecrementAndGetEquals0}, hash: BE69E24AF7393B4630987CFFEC16690F
    @Test()
    void removeElements1WhenCountDecrementAndGetEquals0() {
        /* Branches:
         * (isEmpty(array)) : false
         * (isEmpty(values)) : false
         * (for-each(values)) : true
         * (count == null) : true
         * (i < array.length) : true
         * (count != null) : true
         * (count.decrementAndGet() == 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(byteArray)).thenReturn(false);
            byte[] byteArray2 = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(byteArray2)).thenReturn(false);
            byte[] byteArray3 = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(eq(byteArray), (BitSet) any())).thenReturn(byteArray3);
            //Act Statement(s)
            byte[] result = ArrayUtils.removeElements(byteArray, byteArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(byteArray3));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(byteArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(byteArray2), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(eq(byteArray), (BitSet) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElements2WhenIsEmptyValues}, hash: 155217FAA78509A9F5944635BD26495C
    @Test()
    void removeElements2WhenIsEmptyValues() {
        /* Branches:
         * (isEmpty(array)) : false
         * (isEmpty(values)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(charArray)).thenReturn(false);
            char[] charArray2 = new char[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(charArray2)).thenReturn(true);
            char[] charArray3 = new char[] {};
            arrayUtils.when(() -> ArrayUtils.clone(charArray)).thenReturn(charArray3);
            //Act Statement(s)
            char[] result = ArrayUtils.removeElements(charArray, charArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(charArray3));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(charArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(charArray2), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(charArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElements2WhenCountDecrementAndGetEquals0}, hash: B69AB52F05EF24E5245C760B0CFF443D
    @Test()
    void removeElements2WhenCountDecrementAndGetEquals0() {
        /* Branches:
         * (isEmpty(array)) : false
         * (isEmpty(values)) : false
         * (for-each(values)) : true
         * (count == null) : true
         * (i < array.length) : true
         * (count != null) : true
         * (count.decrementAndGet() == 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(charArray)).thenReturn(false);
            char[] charArray2 = new char[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(charArray2)).thenReturn(false);
            char[] charArray3 = new char[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(eq(charArray), (BitSet) any())).thenReturn(charArray3);
            //Act Statement(s)
            char[] result = ArrayUtils.removeElements(charArray, charArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(charArray3));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(charArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(charArray2), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(eq(charArray), (BitSet) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElements3WhenIsEmptyValues}, hash: D3812585AF3595011D338A77C322AD98
    @Test()
    void removeElements3WhenIsEmptyValues() {
        /* Branches:
         * (isEmpty(array)) : false
         * (isEmpty(values)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(doubleArray)).thenReturn(false);
            double[] doubleArray2 = new double[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(doubleArray2)).thenReturn(true);
            double[] doubleArray3 = new double[] {};
            arrayUtils.when(() -> ArrayUtils.clone(doubleArray)).thenReturn(doubleArray3);
            //Act Statement(s)
            double[] result = ArrayUtils.removeElements(doubleArray, doubleArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(doubleArray3));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(doubleArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(doubleArray2), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(doubleArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElements3WhenCountDecrementAndGetEquals0}, hash: B47477790FD66D46CE685CCF09172AAA
    @Test()
    void removeElements3WhenCountDecrementAndGetEquals0() {
        /* Branches:
         * (isEmpty(array)) : false
         * (isEmpty(values)) : false
         * (for-each(values)) : true
         * (count == null) : true
         * (i < array.length) : true
         * (count != null) : true
         * (count.decrementAndGet() == 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(doubleArray)).thenReturn(false);
            double[] doubleArray2 = new double[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(doubleArray2)).thenReturn(false);
            double[] doubleArray3 = new double[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(eq(doubleArray), (BitSet) any())).thenReturn(doubleArray3);
            //Act Statement(s)
            double[] result = ArrayUtils.removeElements(doubleArray, doubleArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(doubleArray3));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(doubleArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(doubleArray2), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(eq(doubleArray), (BitSet) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElements4WhenIsEmptyValues}, hash: 82EC8EF245D7FAD69CB9E65577630E19
    @Test()
    void removeElements4WhenIsEmptyValues() {
        /* Branches:
         * (isEmpty(array)) : false
         * (isEmpty(values)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(floatArray)).thenReturn(false);
            float[] floatArray2 = new float[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(floatArray2)).thenReturn(true);
            float[] floatArray3 = new float[] {};
            arrayUtils.when(() -> ArrayUtils.clone(floatArray)).thenReturn(floatArray3);
            //Act Statement(s)
            float[] result = ArrayUtils.removeElements(floatArray, floatArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(floatArray3));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(floatArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(floatArray2), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(floatArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElements4WhenCountDecrementAndGetEquals0}, hash: 71EC7CED8B459BE4E1F12333479EA3D8
    @Test()
    void removeElements4WhenCountDecrementAndGetEquals0() {
        /* Branches:
         * (isEmpty(array)) : false
         * (isEmpty(values)) : false
         * (for-each(values)) : true
         * (count == null) : true
         * (i < array.length) : true
         * (count != null) : true
         * (count.decrementAndGet() == 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(floatArray)).thenReturn(false);
            float[] floatArray2 = new float[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(floatArray2)).thenReturn(false);
            float[] floatArray3 = new float[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(eq(floatArray), (BitSet) any())).thenReturn(floatArray3);
            //Act Statement(s)
            float[] result = ArrayUtils.removeElements(floatArray, floatArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(floatArray3));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(floatArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(floatArray2), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(eq(floatArray), (BitSet) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElements5WhenIsEmptyValues}, hash: 6A35A697750C0C86A9E5B6E584ECE46C
    @Test()
    void removeElements5WhenIsEmptyValues() {
        /* Branches:
         * (isEmpty(array)) : false
         * (isEmpty(values)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(intArray)).thenReturn(false);
            int[] intArray2 = new int[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(intArray2)).thenReturn(true);
            int[] intArray3 = new int[] {};
            arrayUtils.when(() -> ArrayUtils.clone(intArray)).thenReturn(intArray3);
            //Act Statement(s)
            int[] result = ArrayUtils.removeElements(intArray, intArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(intArray3));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(intArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(intArray2), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(intArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElements5WhenCountDecrementAndGetEquals0}, hash: 400E5F80492BB8C5F8BE61E224A05A4B
    @Test()
    void removeElements5WhenCountDecrementAndGetEquals0() {
        /* Branches:
         * (isEmpty(array)) : false
         * (isEmpty(values)) : false
         * (for-each(values)) : true
         * (count == null) : true
         * (i < array.length) : true
         * (count != null) : true
         * (count.decrementAndGet() == 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(intArray)).thenReturn(false);
            int[] intArray2 = new int[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(intArray2)).thenReturn(false);
            int[] intArray3 = new int[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(eq(intArray), (BitSet) any())).thenReturn(intArray3);
            //Act Statement(s)
            int[] result = ArrayUtils.removeElements(intArray, intArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(intArray3));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(intArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(intArray2), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(eq(intArray), (BitSet) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElements6WhenIsEmptyValues}, hash: 9DCBA0595E4B1EF7CC0029AF5CE053F7
    @Test()
    void removeElements6WhenIsEmptyValues() {
        /* Branches:
         * (isEmpty(array)) : false
         * (isEmpty(values)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(longArray)).thenReturn(false);
            long[] longArray2 = new long[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(longArray2)).thenReturn(true);
            long[] longArray3 = new long[] {};
            arrayUtils.when(() -> ArrayUtils.clone(longArray)).thenReturn(longArray3);
            //Act Statement(s)
            long[] result = ArrayUtils.removeElements(longArray, longArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(longArray3));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(longArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(longArray2), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(longArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElements6WhenCountDecrementAndGetEquals0}, hash: A7F77EA4EF59B39FA1DC22FB4716B5AC
    @Test()
    void removeElements6WhenCountDecrementAndGetEquals0() {
        /* Branches:
         * (isEmpty(array)) : false
         * (isEmpty(values)) : false
         * (for-each(values)) : true
         * (count == null) : true
         * (i < array.length) : true
         * (count != null) : true
         * (count.decrementAndGet() == 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(longArray)).thenReturn(false);
            long[] longArray2 = new long[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(longArray2)).thenReturn(false);
            long[] longArray3 = new long[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(eq(longArray), (BitSet) any())).thenReturn(longArray3);
            //Act Statement(s)
            long[] result = ArrayUtils.removeElements(longArray, longArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(longArray3));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(longArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(longArray2), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(eq(longArray), (BitSet) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElements7WhenIsEmptyValues}, hash: AFC9743A1370D540A800D3C12728CADB
    @Test()
    void removeElements7WhenIsEmptyValues() {
        /* Branches:
         * (isEmpty(array)) : false
         * (isEmpty(values)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(shortArray)).thenReturn(false);
            short[] shortArray2 = new short[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(shortArray2)).thenReturn(true);
            short[] shortArray3 = new short[] {};
            arrayUtils.when(() -> ArrayUtils.clone(shortArray)).thenReturn(shortArray3);
            //Act Statement(s)
            short[] result = ArrayUtils.removeElements(shortArray, shortArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(shortArray3));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(shortArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(shortArray2), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(shortArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElements7WhenCountDecrementAndGetEquals0}, hash: 422968EFFAEC7F4F0F7F87C9550FAAE6
    @Test()
    void removeElements7WhenCountDecrementAndGetEquals0() {
        /* Branches:
         * (isEmpty(array)) : false
         * (isEmpty(values)) : false
         * (for-each(values)) : true
         * (count == null) : true
         * (i < array.length) : true
         * (count != null) : true
         * (count.decrementAndGet() == 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(shortArray)).thenReturn(false);
            short[] shortArray2 = new short[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(shortArray2)).thenReturn(false);
            short[] shortArray3 = new short[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(eq(shortArray), (BitSet) any())).thenReturn(shortArray3);
            //Act Statement(s)
            short[] result = ArrayUtils.removeElements(shortArray, shortArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(shortArray3));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(shortArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(shortArray2), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(eq(shortArray), (BitSet) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElements8WhenIsEmptyValues}, hash: DE7F6138741FCD2E61ADA363F21D2674
    @Test()
    void removeElements8WhenIsEmptyValues() {
        /* Branches:
         * (isEmpty(array)) : false
         * (isEmpty(values)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(objectArray)).thenReturn(false);
            Object[] objectArray2 = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(objectArray2)).thenReturn(true);
            Object[] objectArray3 = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.clone(objectArray)).thenReturn(objectArray3);
            //Act Statement(s)
            Object[] result = ArrayUtils.removeElements(objectArray, objectArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(objectArray3));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(objectArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(objectArray2), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.clone(objectArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${removeElements8WhenCountDecrementAndGetEquals0}, hash: DAD28030B3D04B3DB8B2C5455CC5CCAF
    @Test()
    void removeElements8WhenCountDecrementAndGetEquals0() {
        /* Branches:
         * (isEmpty(array)) : false
         * (isEmpty(values)) : false
         * (for-each(values)) : true
         * (count == null) : true
         * (i < array.length) : true
         * (count != null) : true
         * (count.decrementAndGet() == 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object[] objectArray = new Object[] { object };
            arrayUtils.when(() -> ArrayUtils.isEmpty(objectArray)).thenReturn(false);
            Object[] objectArray2 = new Object[] { object };
            arrayUtils.when(() -> ArrayUtils.isEmpty(objectArray2)).thenReturn(false);
            Object[] objectArray3 = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.removeAll(eq(objectArray), (BitSet) any())).thenReturn(objectArray3);
            //Act Statement(s)
            Object[] result = ArrayUtils.removeElements(objectArray, objectArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(objectArray3));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(objectArray), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(objectArray2), atLeast(1));
                arrayUtils.verify(() -> ArrayUtils.removeAll(eq(objectArray), (BitSet) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${reverseWhenArrayIsNull}, hash: D80B56EF478ED5D697C96D174E4ED522
    @Test()
    void reverseWhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        boolean[] _boolean = null;
        //Act Statement(s)
        ArrayUtils.reverse(_boolean);
    }

    //BaseRock generated method id: ${reverseWhenArrayIsNotNull}, hash: 4326C4AB12423E2948268ACF96E4E253
    @Test()
    void reverseWhenArrayIsNotNull() {
        /* Branches:
         * (array == null) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.reverse(booleanArray, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.reverse(booleanArray);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.reverse(booleanArray, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${reverse1WhenArrayIsNull}, hash: D8BBB8215F9F446D58DA0A65F163C133
    @Test()
    void reverse1WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        boolean[] _boolean = null;
        //Act Statement(s)
        ArrayUtils.reverse(_boolean, 0, 0);
    }

    //BaseRock generated method id: ${reverse1WhenJGreaterThanI}, hash: B775CD495669C146D80DD051AB78E901
    @Test()
    void reverse1WhenJGreaterThanI() {
        /* Branches:
         * (array == null) : false
         * (j > i) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false, false };
        //Act Statement(s)
        ArrayUtils.reverse(booleanArray, -1, 2);
    }

    //BaseRock generated method id: ${reverse2WhenArrayIsNotNull}, hash: 6140FC101BB2257904C2F05A84D1FFEB
    @Test()
    void reverse2WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.reverse(byteArray, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.reverse(byteArray);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.reverse(byteArray, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${reverse3WhenArrayIsNull}, hash: F247CA28526A2C9D4755928237B8E0D0
    @Test()
    void reverse3WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        byte[] _byte = null;
        //Act Statement(s)
        ArrayUtils.reverse(_byte, 0, 0);
    }

    //BaseRock generated method id: ${reverse3WhenJGreaterThanI}, hash: F841539204CDDE75EBEB39906DF21221
    @Test()
    void reverse3WhenJGreaterThanI() {
        /* Branches:
         * (array == null) : false
         * (j > i) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] { (byte) 1, (byte) 0 };
        //Act Statement(s)
        ArrayUtils.reverse(byteArray, -1, 2);
        byte[] byteByteArrayArray = new byte[] { (byte) 0, (byte) 1 };
        //Assert statement(s)
        assertAll("result", () -> assertThat(byteArray, equalTo(byteByteArrayArray)));
    }

    //BaseRock generated method id: ${reverse4WhenArrayIsNotNull}, hash: 4E0892700ACA7CA8991414E3F5FF3B6D
    @Test()
    void reverse4WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.reverse(charArray, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.reverse(charArray);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.reverse(charArray, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${reverse5WhenArrayIsNull}, hash: 2B96C35547AC16FF96D16E1FB23C860A
    @Test()
    void reverse5WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        char[] _char = null;
        //Act Statement(s)
        ArrayUtils.reverse(_char, 0, 0);
    }

    //BaseRock generated method id: ${reverse5WhenJGreaterThanI}, hash: 550F52873D623E35F529405A533A75A2
    @Test()
    void reverse5WhenJGreaterThanI() {
        /* Branches:
         * (array == null) : false
         * (j > i) : true
         */
        //Arrange Statement(s)
        char[] charArray = new char[] { 'A', 'A' };
        //Act Statement(s)
        ArrayUtils.reverse(charArray, -1, 2);
    }

    //BaseRock generated method id: ${reverse6WhenArrayIsNotNull}, hash: DDE6B4F12BF97BFF504C8323C9830380
    @Test()
    void reverse6WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.reverse(doubleArray, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.reverse(doubleArray);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.reverse(doubleArray, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${reverse7WhenArrayIsNull}, hash: B77DD5F801041388CAD8150E94820592
    @Test()
    void reverse7WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        double[] _double = null;
        //Act Statement(s)
        ArrayUtils.reverse(_double, 0, 0);
    }

    //BaseRock generated method id: ${reverse7WhenJGreaterThanI}, hash: EB711844C0BF2B220374C6CF1EB6F13F
    @Test()
    void reverse7WhenJGreaterThanI() {
        /* Branches:
         * (array == null) : false
         * (j > i) : true
         */
        //Arrange Statement(s)
        double[] doubleArray = new double[] { Double.parseDouble("1"), Double.parseDouble("0") };
        //Act Statement(s)
        ArrayUtils.reverse(doubleArray, -1, 2);
        double[] doubleDoubleArrayArray = new double[] { Double.parseDouble("0.0"), Double.parseDouble("1.0") };
        //Assert statement(s)
        assertAll("result", () -> assertThat(doubleArray, equalTo(doubleDoubleArrayArray)));
    }

    //BaseRock generated method id: ${reverse8WhenArrayIsNotNull}, hash: E9B7EF4DDE90DA7F0DFAF429A1F02583
    @Test()
    void reverse8WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.reverse(floatArray, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.reverse(floatArray);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.reverse(floatArray, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${reverse9WhenArrayIsNull}, hash: 445E9CE14606E791D75C9C474B35869C
    @Test()
    void reverse9WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        float[] _float = null;
        //Act Statement(s)
        ArrayUtils.reverse(_float, 0, 0);
    }

    //BaseRock generated method id: ${reverse9WhenJGreaterThanI}, hash: 96F9F2A2DC3AB1D74EE22383BEA2C982
    @Test()
    void reverse9WhenJGreaterThanI() {
        /* Branches:
         * (array == null) : false
         * (j > i) : true
         */
        //Arrange Statement(s)
        float[] floatArray = new float[] { Float.parseFloat("1"), Float.parseFloat("0") };
        //Act Statement(s)
        ArrayUtils.reverse(floatArray, -1, 2);
        float[] floatFloatArrayArray = new float[] { Float.parseFloat("0.0"), Float.parseFloat("1.0") };
        //Assert statement(s)
        assertAll("result", () -> assertThat(floatArray, equalTo(floatFloatArrayArray)));
    }

    //BaseRock generated method id: ${reverse10WhenArrayIsNotNull}, hash: D9C1B0573AB59DEB316B8275776D8C5C
    @Test()
    void reverse10WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.reverse(intArray, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.reverse(intArray);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.reverse(intArray, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${reverse11WhenArrayIsNull}, hash: E887931093A02CBE4D7BDF919F93D3EE
    @Test()
    void reverse11WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        int[] _int = null;
        //Act Statement(s)
        ArrayUtils.reverse(_int, 0, 0);
    }

    //BaseRock generated method id: ${reverse11WhenJGreaterThanI}, hash: 10167D4EECD8DE5C660BE9AD037D03B6
    @Test()
    void reverse11WhenJGreaterThanI() {
        /* Branches:
         * (array == null) : false
         * (j > i) : true
         */
        //Arrange Statement(s)
        int[] intArray = new int[] { 1, 0 };
        //Act Statement(s)
        ArrayUtils.reverse(intArray, -1, 2);
        int[] intIntArrayArray = new int[] { 0, 1 };
        //Assert statement(s)
        assertAll("result", () -> assertThat(intArray, equalTo(intIntArrayArray)));
    }

    //BaseRock generated method id: ${reverse12WhenArrayIsNotNull}, hash: AFCE1707CD24E2F7170A3460C392E81E
    @Test()
    void reverse12WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.reverse(longArray, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.reverse(longArray);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.reverse(longArray, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${reverse13WhenArrayIsNull}, hash: F7D9F3B927D6E10AAF08C99C8DC95426
    @Test()
    void reverse13WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        long[] _long = null;
        //Act Statement(s)
        ArrayUtils.reverse(_long, 0, 0);
    }

    //BaseRock generated method id: ${reverse13WhenJGreaterThanI}, hash: D1E78FDC54A5E2EB5301D80149DB151D
    @Test()
    void reverse13WhenJGreaterThanI() {
        /* Branches:
         * (array == null) : false
         * (j > i) : true
         */
        //Arrange Statement(s)
        long[] longArray = new long[] { 1L, 0L };
        //Act Statement(s)
        ArrayUtils.reverse(longArray, -1, 2);
        long[] longLongArrayArray = new long[] { 0L, 1L };
        //Assert statement(s)
        assertAll("result", () -> assertThat(longArray, equalTo(longLongArrayArray)));
    }

    //BaseRock generated method id: ${reverse14WhenArrayIsNotNull}, hash: A46EBBD8EC5E3CF1E42F302BF66FF828
    @Test()
    void reverse14WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.reverse(objectArray, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.reverse(objectArray);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.reverse(objectArray, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${reverse15WhenArrayIsNull}, hash: 724A90FE6BF2A2216CE49784509BFD5F
    @Test()
    void reverse15WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Object[] object = null;
        //Act Statement(s)
        ArrayUtils.reverse(object, 0, 0);
    }

    //BaseRock generated method id: ${reverse15WhenJGreaterThanI}, hash: 33EE584B33C71D323DDF479B08FADA69
    @Test()
    void reverse15WhenJGreaterThanI() {
        /* Branches:
         * (array == null) : false
         * (j > i) : true
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        Object[] objectArray = new Object[] { object, object2 };
        //Act Statement(s)
        ArrayUtils.reverse(objectArray, -1, 2);
    }

    //BaseRock generated method id: ${reverse16WhenArrayIsNotNull}, hash: 7333CE25AB99A9290FC46169CD2D121A
    @Test()
    void reverse16WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.reverse(shortArray, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.reverse(shortArray);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.reverse(shortArray, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${reverse17WhenArrayIsNull}, hash: 8F927E2576DEF86FC864F42949B667E4
    @Test()
    void reverse17WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        short[] _short = null;
        //Act Statement(s)
        ArrayUtils.reverse(_short, 0, 0);
    }

    //BaseRock generated method id: ${reverse17WhenJGreaterThanI}, hash: 6C2F4296D8C5D0710F5329C7B3E2980A
    @Test()
    void reverse17WhenJGreaterThanI() {
        /* Branches:
         * (array == null) : false
         * (j > i) : true
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] { (short) 1, (short) 0 };
        //Act Statement(s)
        ArrayUtils.reverse(shortArray, -1, 2);
        short[] shortShortArrayArray = new short[] { (short) 0, (short) 1 };
        //Assert statement(s)
        assertAll("result", () -> assertThat(shortArray, equalTo(shortShortArrayArray)));
    }

    //BaseRock generated method id: ${setAllWhenGeneratorIsNotNull}, hash: D1C482CEC08B3C445F49A6B33A986A6E
    @Test()
    void setAllWhenGeneratorIsNotNull() {
        /* Branches:
         * (array != null) : true
         * (generator != null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        IntFunction intFunctionMock = mock(IntFunction.class);
        //Act Statement(s)
        Object[] result = ArrayUtils.setAll(objectArray, intFunctionMock);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(objectArray)));
    }

    //BaseRock generated method id: ${setAll1WhenILessThanArrayLength}, hash: 6220A5045C95CA168447330E126BA8CD
    @Test()
    void setAll1WhenILessThanArrayLength() {
        /* Branches:
         * (array != null) : true
         * (generator != null) : true
         * (i < array.length) : true
         */
        //Arrange Statement(s)
        Supplier generatorMock = mock(Supplier.class);
        Object object = new Object();
        doReturn(object).when(generatorMock).get();
        Object object2 = new Object();
        Object[] objectArray = new Object[] { object2 };
        //Act Statement(s)
        Object[] result = ArrayUtils.setAll(objectArray, generatorMock);
        Object[] objectResultArray = new Object[] { object };
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(objectResultArray));
            verify(generatorMock).get();
        });
    }

    //BaseRock generated method id: ${shiftWhenArrayIsNotNull}, hash: 678594F595844698237DB653B3CE78AF
    @Test()
    void shiftWhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.shift(booleanArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(booleanArray, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.shift(booleanArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift1WhenEndIndexExclusiveLessThanOrEqualsTo0}, hash: CAA9DE98CA9EAEDD7C4066567A44E87B
    @Test()
    void shift1WhenEndIndexExclusiveLessThanOrEqualsTo0() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        //Act Statement(s)
        ArrayUtils.shift(booleanArray, -2, 0, 0);
    }

    //BaseRock generated method id: ${shift1WhenNLessThanOrEqualsTo1}, hash: 4E2130777E84CD5767D65915007F810F
    @Test()
    void shift1WhenNLessThanOrEqualsTo1() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] { false };
        //Act Statement(s)
        ArrayUtils.shift(booleanArray, -1, 1, 0);
    }

    //BaseRock generated method id: ${shift1WhenOffsetGreaterThanNOffset}, hash: 903EB6B490608BFE519FC4B93B199614
    @Test()
    void shift1WhenOffsetGreaterThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.swap(booleanArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(booleanArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(booleanArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift1WhenOffsetLessThanNOffset}, hash: 97E0CAAC49C7CEF73AA1B14431C022BA
    @Test()
    void shift1WhenOffsetLessThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : false
         * (offset < nOffset) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.swap(booleanArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(booleanArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(booleanArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift1WhenOffsetNotLessThanNOffset}, hash: C9B56BF8F0AA42BBB1692061D9E42FA4
    @Test()
    void shift1WhenOffsetNotLessThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : false
         * (offset < nOffset) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.swap(booleanArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(booleanArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(booleanArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift2WhenArrayIsNotNull}, hash: EC4659F249C03721BB987EEE3B6C7E41
    @Test()
    void shift2WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.shift(byteArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(byteArray, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.shift(byteArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift3WhenEndIndexExclusiveLessThanOrEqualsTo0}, hash: A96D3F084CB04D695F2BA4D503590A9C
    @Test()
    void shift3WhenEndIndexExclusiveLessThanOrEqualsTo0() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        //Act Statement(s)
        ArrayUtils.shift(byteArray, -2, 0, 0);
    }

    //BaseRock generated method id: ${shift3WhenNLessThanOrEqualsTo1}, hash: 27D2D467E1FEA57318D9549BBC9ECEC0
    @Test()
    void shift3WhenNLessThanOrEqualsTo1() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] { (byte) 0 };
        //Act Statement(s)
        ArrayUtils.shift(byteArray, -1, 1, 0);
    }

    //BaseRock generated method id: ${shift3WhenOffsetGreaterThanNOffset}, hash: 7E7BDB3CC5E61F9B9EC8BCC80B663A7C
    @Test()
    void shift3WhenOffsetGreaterThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.swap(byteArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(byteArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(byteArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift3WhenOffsetLessThanNOffset}, hash: 55F6475AE8DF3F392583FB51AC219864
    @Test()
    void shift3WhenOffsetLessThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : false
         * (offset < nOffset) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.swap(byteArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(byteArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(byteArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift3WhenOffsetNotLessThanNOffset}, hash: 874F1E7D154452EA16890BC3A44B41FF
    @Test()
    void shift3WhenOffsetNotLessThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : false
         * (offset < nOffset) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.swap(byteArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(byteArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(byteArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift4WhenArrayIsNotNull}, hash: 2A4F34242899A6BD75E2D5C7863D509F
    @Test()
    void shift4WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.shift(charArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(charArray, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.shift(charArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift5WhenEndIndexExclusiveLessThanOrEqualsTo0}, hash: 002AF1228B2AF93DB754149B34C972D8
    @Test()
    void shift5WhenEndIndexExclusiveLessThanOrEqualsTo0() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : true
         */
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        //Act Statement(s)
        ArrayUtils.shift(charArray, -2, 0, 0);
    }

    //BaseRock generated method id: ${shift5WhenNLessThanOrEqualsTo1}, hash: D7D08987FC816541D33B1AB7DA8B342B
    @Test()
    void shift5WhenNLessThanOrEqualsTo1() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : true
         */
        //Arrange Statement(s)
        char[] charArray = new char[] { 'A' };
        //Act Statement(s)
        ArrayUtils.shift(charArray, -1, 1, 0);
    }

    //BaseRock generated method id: ${shift5WhenOffsetGreaterThanNOffset}, hash: 3F77B366F97D3C319DDCCE56008EA51B
    @Test()
    void shift5WhenOffsetGreaterThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.swap(charArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(charArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(charArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift5WhenOffsetLessThanNOffset}, hash: 72553F72514141516D5C97BCC35C73AC
    @Test()
    void shift5WhenOffsetLessThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : false
         * (offset < nOffset) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.swap(charArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(charArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(charArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift5WhenOffsetNotLessThanNOffset}, hash: FD06E5F5FEBD0B9F91D3E8CEBC16425E
    @Test()
    void shift5WhenOffsetNotLessThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : false
         * (offset < nOffset) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.swap(charArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(charArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(charArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift6WhenArrayIsNotNull}, hash: 44820037E5DFFCF755D37BB186372B29
    @Test()
    void shift6WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.shift(doubleArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(doubleArray, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.shift(doubleArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift7WhenEndIndexExclusiveLessThanOrEqualsTo0}, hash: 4BF5D54C3317076A0D5E6BD6CF0D6E4C
    @Test()
    void shift7WhenEndIndexExclusiveLessThanOrEqualsTo0() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : true
         */
        //Arrange Statement(s)
        double[] doubleArray = new double[] {};
        //Act Statement(s)
        ArrayUtils.shift(doubleArray, -2, 0, 0);
    }

    //BaseRock generated method id: ${shift7WhenNLessThanOrEqualsTo1}, hash: BF22829E68EA96E44694989D0DED7242
    @Test()
    void shift7WhenNLessThanOrEqualsTo1() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : true
         */
        //Arrange Statement(s)
        double[] doubleArray = new double[] { Double.parseDouble("0") };
        //Act Statement(s)
        ArrayUtils.shift(doubleArray, -1, 1, 0);
    }

    //BaseRock generated method id: ${shift7WhenOffsetGreaterThanNOffset}, hash: A51D4F305B921C650230E76E4EFB0F81
    @Test()
    void shift7WhenOffsetGreaterThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.swap(doubleArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(doubleArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(doubleArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift7WhenOffsetLessThanNOffset}, hash: 937A11E51C8E6BC788C42D17D6170B54
    @Test()
    void shift7WhenOffsetLessThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : false
         * (offset < nOffset) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.swap(doubleArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(doubleArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(doubleArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift7WhenOffsetNotLessThanNOffset}, hash: 8F1317ABFF6AE6A4DC8A8C595AA0BA59
    @Test()
    void shift7WhenOffsetNotLessThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : false
         * (offset < nOffset) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.swap(doubleArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(doubleArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(doubleArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift8WhenArrayIsNotNull}, hash: 7CEE54DD325AF7804AE5455E435DA8D2
    @Test()
    void shift8WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.shift(floatArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(floatArray, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.shift(floatArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift9WhenEndIndexExclusiveLessThanOrEqualsTo0}, hash: 16E96FB369D00AA0E76D067E0C8A3F8A
    @Test()
    void shift9WhenEndIndexExclusiveLessThanOrEqualsTo0() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : true
         */
        //Arrange Statement(s)
        float[] floatArray = new float[] {};
        //Act Statement(s)
        ArrayUtils.shift(floatArray, -2, 0, 0);
    }

    //BaseRock generated method id: ${shift9WhenNLessThanOrEqualsTo1}, hash: 8405BD532B44D18A058139264F1272D6
    @Test()
    void shift9WhenNLessThanOrEqualsTo1() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : true
         */
        //Arrange Statement(s)
        float[] floatArray = new float[] { Float.parseFloat("0") };
        //Act Statement(s)
        ArrayUtils.shift(floatArray, -1, 1, 0);
    }

    //BaseRock generated method id: ${shift9WhenOffsetGreaterThanNOffset}, hash: 39F031F6D8022677851A22EAE444FD8B
    @Test()
    void shift9WhenOffsetGreaterThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.swap(floatArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(floatArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(floatArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift9WhenOffsetLessThanNOffset}, hash: 52546000D70BFFB2A5C97AE7FE7E28E7
    @Test()
    void shift9WhenOffsetLessThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : false
         * (offset < nOffset) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.swap(floatArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(floatArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(floatArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift9WhenOffsetNotLessThanNOffset}, hash: C170829F258371B7F7D35C33274AE886
    @Test()
    void shift9WhenOffsetNotLessThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : false
         * (offset < nOffset) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.swap(floatArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(floatArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(floatArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift10WhenArrayIsNotNull}, hash: 0073C596A2A83DBE03BAE6E5C3F10E3F
    @Test()
    void shift10WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.shift(intArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(intArray, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.shift(intArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift11WhenEndIndexExclusiveLessThanOrEqualsTo0}, hash: E01E18151DE8B69F066868616E214C22
    @Test()
    void shift11WhenEndIndexExclusiveLessThanOrEqualsTo0() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : true
         */
        //Arrange Statement(s)
        int[] intArray = new int[] {};
        //Act Statement(s)
        ArrayUtils.shift(intArray, -2, 0, 0);
    }

    //BaseRock generated method id: ${shift11WhenNLessThanOrEqualsTo1}, hash: 440890288DAC734D0C1982B35040960E
    @Test()
    void shift11WhenNLessThanOrEqualsTo1() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : true
         */
        //Arrange Statement(s)
        int[] intArray = new int[] { 0 };
        //Act Statement(s)
        ArrayUtils.shift(intArray, -1, 1, 0);
    }

    //BaseRock generated method id: ${shift11WhenOffsetGreaterThanNOffset}, hash: ADC7DB5A78E2135B1000DB9804AA05CA
    @Test()
    void shift11WhenOffsetGreaterThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.swap(intArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(intArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(intArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift11WhenOffsetLessThanNOffset}, hash: FA4C9A275E665CCBB51337111D7B9BFD
    @Test()
    void shift11WhenOffsetLessThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : false
         * (offset < nOffset) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.swap(intArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(intArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(intArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift11WhenOffsetNotLessThanNOffset}, hash: A5FE5129FF898A0F197ACFF3863C2153
    @Test()
    void shift11WhenOffsetNotLessThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : false
         * (offset < nOffset) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.swap(intArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(intArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(intArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift12WhenArrayIsNotNull}, hash: 653574019100AD28AC7E9AAE3AA00DB1
    @Test()
    void shift12WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.shift(longArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(longArray, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.shift(longArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift13WhenEndIndexExclusiveLessThanOrEqualsTo0}, hash: 66B20FCFA7CEB8A1A866D8BEB468B9E2
    @Test()
    void shift13WhenEndIndexExclusiveLessThanOrEqualsTo0() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : true
         */
        //Arrange Statement(s)
        long[] longArray = new long[] {};
        //Act Statement(s)
        ArrayUtils.shift(longArray, -2, 0, 0);
    }

    //BaseRock generated method id: ${shift13WhenNLessThanOrEqualsTo1}, hash: CE423DD02947D3D76A7B1AE344F0CA5A
    @Test()
    void shift13WhenNLessThanOrEqualsTo1() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : true
         */
        //Arrange Statement(s)
        long[] longArray = new long[] { 0L };
        //Act Statement(s)
        ArrayUtils.shift(longArray, -1, 1, 0);
    }

    //BaseRock generated method id: ${shift13WhenOffsetGreaterThanNOffset}, hash: 826A5863782B6CDD6C903D7A40E548FB
    @Test()
    void shift13WhenOffsetGreaterThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.swap(longArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(longArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(longArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift13WhenOffsetLessThanNOffset}, hash: 64E9A37AF54249C1488C873BB9E2AB49
    @Test()
    void shift13WhenOffsetLessThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : false
         * (offset < nOffset) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.swap(longArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(longArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(longArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift13WhenOffsetNotLessThanNOffset}, hash: 522BC4A0E24512F0F9BD6AEB452DDA19
    @Test()
    void shift13WhenOffsetNotLessThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : false
         * (offset < nOffset) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.swap(longArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(longArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(longArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift14WhenArrayIsNotNull}, hash: D2EEF6B887DB1E4099FBF202E758D2A4
    @Test()
    void shift14WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.shift(objectArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(objectArray, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.shift(objectArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift15WhenEndIndexExclusiveLessThanOrEqualsTo0}, hash: BDCC81DD8B9937F302030B61A2BD1141
    @Test()
    void shift15WhenEndIndexExclusiveLessThanOrEqualsTo0() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : true
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        //Act Statement(s)
        ArrayUtils.shift(objectArray, -2, 0, 0);
    }

    //BaseRock generated method id: ${shift15WhenNLessThanOrEqualsTo1}, hash: 049FCDF2AEDFD8E8385882740936F61F
    @Test()
    void shift15WhenNLessThanOrEqualsTo1() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : true
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object[] objectArray = new Object[] { object };
        //Act Statement(s)
        ArrayUtils.shift(objectArray, -1, 1, 0);
    }

    //BaseRock generated method id: ${shift15WhenOffsetGreaterThanNOffset}, hash: C3499BAF2B0587ACB16832AD24C10174
    @Test()
    void shift15WhenOffsetGreaterThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.swap(objectArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(objectArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(objectArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift15WhenOffsetLessThanNOffset}, hash: 6D5FD6F4475A2A450F135FCD6F1F5E5E
    @Test()
    void shift15WhenOffsetLessThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : false
         * (offset < nOffset) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.swap(objectArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(objectArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(objectArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift15WhenOffsetNotLessThanNOffset}, hash: 83F573309AA0AF13A4C777F8439AB079
    @Test()
    void shift15WhenOffsetNotLessThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : false
         * (offset < nOffset) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.swap(objectArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(objectArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(objectArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift16WhenArrayIsNotNull}, hash: C9EEF179FD71433D9B17196496ECC278
    @Test()
    void shift16WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.shift(shortArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(shortArray, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.shift(shortArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift17WhenEndIndexExclusiveLessThanOrEqualsTo0}, hash: A621AF163027910FCB93F7B3C2108E55
    @Test()
    void shift17WhenEndIndexExclusiveLessThanOrEqualsTo0() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : true
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] {};
        //Act Statement(s)
        ArrayUtils.shift(shortArray, -2, 0, 0);
    }

    //BaseRock generated method id: ${shift17WhenNLessThanOrEqualsTo1}, hash: FC5C9100CA740DB9783D2F44BA99D6BA
    @Test()
    void shift17WhenNLessThanOrEqualsTo1() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : true
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] { (short) 0 };
        //Act Statement(s)
        ArrayUtils.shift(shortArray, -1, 1, 0);
    }

    //BaseRock generated method id: ${shift17WhenOffsetGreaterThanNOffset}, hash: 7E21CE5805EE35326A780CEAD1E97B23
    @Test()
    void shift17WhenOffsetGreaterThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.swap(shortArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(shortArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(shortArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift17WhenOffsetLessThanNOffset}, hash: 7F26EB9FAD9C7E64E9F6D06687E8D805
    @Test()
    void shift17WhenOffsetLessThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : false
         * (offset < nOffset) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.swap(shortArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(shortArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(shortArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shift17WhenOffsetNotLessThanNOffset}, hash: 47A6DF0049F992DE40F8232B0AE6FC49
    @Test()
    void shift17WhenOffsetNotLessThanNOffset() {
        /* Branches:
         * (array == null) : false
         * (startIndexInclusive >= array.length - 1) : false
         * (endIndexExclusive <= 0) : false
         * (n <= 1) : false
         * (offset < 0) : true
         * (n > 1) : true
         * (offset > 0) : true
         * (offset > nOffset) : false
         * (offset < nOffset) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.swap(shortArray, 0, 0, 0)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shift(shortArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(shortArray, 0, 0, 0), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shuffleTest}, hash: 23E0914677CE88B43B3F885640DE9422
    @Test()
    void shuffleTest() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.shuffle(eq(booleanArray), (ThreadLocalRandom) any())).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shuffle(booleanArray);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.shuffle(eq(booleanArray), (ThreadLocalRandom) any()), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shuffle1WhenIGreaterThan1}, hash: 9FEBACA6A427568B543AA40B9EDB5F9B
    @Test()
    void shuffle1WhenIGreaterThan1() {
        /* Branches:
         * (i > 1) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            doReturn(0).when(randomMock).nextInt(2);
            boolean[] booleanArray = new boolean[] { false, false };
            arrayUtils.when(() -> ArrayUtils.swap(booleanArray, 1, 0, 1)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shuffle(booleanArray, randomMock);
            //Assert statement(s)
            assertAll("result", () -> {
                verify(randomMock, atLeast(1)).nextInt(2);
                arrayUtils.verify(() -> ArrayUtils.swap(booleanArray, 1, 0, 1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${shuffle2Test}, hash: 401870B533E6661A99BD1EB5267431C2
    @Test()
    void shuffle2Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.shuffle(eq(byteArray), (ThreadLocalRandom) any())).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shuffle(byteArray);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.shuffle(eq(byteArray), (ThreadLocalRandom) any()), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shuffle3WhenIGreaterThan1}, hash: 6CAECB8408EA1BBFE2048F7A3E07888D
    @Test()
    void shuffle3WhenIGreaterThan1() {
        /* Branches:
         * (i > 1) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            doReturn(0).when(randomMock).nextInt(2);
            byte[] byteArray = new byte[] { (byte) 0, (byte) 1 };
            arrayUtils.when(() -> ArrayUtils.swap(byteArray, 1, 0, 1)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shuffle(byteArray, randomMock);
            //Assert statement(s)
            assertAll("result", () -> {
                verify(randomMock, atLeast(1)).nextInt(2);
                arrayUtils.verify(() -> ArrayUtils.swap(byteArray, 1, 0, 1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${shuffle4Test}, hash: 2C3303E6D05D016348D5273C6A99FFFB
    @Test()
    void shuffle4Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.shuffle(eq(charArray), (ThreadLocalRandom) any())).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shuffle(charArray);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.shuffle(eq(charArray), (ThreadLocalRandom) any()), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shuffle5WhenIGreaterThan1}, hash: B9C602D0C444BDF1EEFF5E5EE64D3617
    @Test()
    void shuffle5WhenIGreaterThan1() {
        /* Branches:
         * (i > 1) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            doReturn(0).when(randomMock).nextInt(2);
            char[] charArray = new char[] { 'A', 'A' };
            arrayUtils.when(() -> ArrayUtils.swap(charArray, 1, 0, 1)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shuffle(charArray, randomMock);
            //Assert statement(s)
            assertAll("result", () -> {
                verify(randomMock, atLeast(1)).nextInt(2);
                arrayUtils.verify(() -> ArrayUtils.swap(charArray, 1, 0, 1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${shuffle6Test}, hash: 202FB34E07A64B6DE17A587CE1DF34E8
    @Test()
    void shuffle6Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.shuffle(eq(doubleArray), (ThreadLocalRandom) any())).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shuffle(doubleArray);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.shuffle(eq(doubleArray), (ThreadLocalRandom) any()), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shuffle7WhenIGreaterThan1}, hash: D0B31B7E5BC4006176A75F42B2C46B67
    @Test()
    void shuffle7WhenIGreaterThan1() {
        /* Branches:
         * (i > 1) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            doReturn(0).when(randomMock).nextInt(2);
            double[] doubleArray = new double[] { Double.parseDouble("0"), Double.parseDouble("1") };
            arrayUtils.when(() -> ArrayUtils.swap(doubleArray, 1, 0, 1)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shuffle(doubleArray, randomMock);
            //Assert statement(s)
            assertAll("result", () -> {
                verify(randomMock, atLeast(1)).nextInt(2);
                arrayUtils.verify(() -> ArrayUtils.swap(doubleArray, 1, 0, 1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${shuffle8Test}, hash: DB0BBB7010B9EF16B7FCA7A8EDB75896
    @Test()
    void shuffle8Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.shuffle(eq(floatArray), (ThreadLocalRandom) any())).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shuffle(floatArray);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.shuffle(eq(floatArray), (ThreadLocalRandom) any()), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shuffle9WhenIGreaterThan1}, hash: 3C99AFC681F5D9A2B7C50D427DEC02CB
    @Test()
    void shuffle9WhenIGreaterThan1() {
        /* Branches:
         * (i > 1) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            doReturn(0).when(randomMock).nextInt(2);
            float[] floatArray = new float[] { Float.parseFloat("0"), Float.parseFloat("1") };
            arrayUtils.when(() -> ArrayUtils.swap(floatArray, 1, 0, 1)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shuffle(floatArray, randomMock);
            //Assert statement(s)
            assertAll("result", () -> {
                verify(randomMock, atLeast(1)).nextInt(2);
                arrayUtils.verify(() -> ArrayUtils.swap(floatArray, 1, 0, 1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${shuffle10Test}, hash: E1A28288554DD1CD3A24436AC663A487
    @Test()
    void shuffle10Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.shuffle(eq(intArray), (ThreadLocalRandom) any())).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shuffle(intArray);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.shuffle(eq(intArray), (ThreadLocalRandom) any()), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shuffle11WhenIGreaterThan1}, hash: 6F061BDA0DA31C469E19B025F9FCD0A2
    @Test()
    void shuffle11WhenIGreaterThan1() {
        /* Branches:
         * (i > 1) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            doReturn(0).when(randomMock).nextInt(2);
            int[] intArray = new int[] { 0, 1 };
            arrayUtils.when(() -> ArrayUtils.swap(intArray, 1, 0, 1)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shuffle(intArray, randomMock);
            //Assert statement(s)
            assertAll("result", () -> {
                verify(randomMock, atLeast(1)).nextInt(2);
                arrayUtils.verify(() -> ArrayUtils.swap(intArray, 1, 0, 1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${shuffle12Test}, hash: 0AAB63972E8EEB861137E3FD12339342
    @Test()
    void shuffle12Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.shuffle(eq(longArray), (ThreadLocalRandom) any())).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shuffle(longArray);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.shuffle(eq(longArray), (ThreadLocalRandom) any()), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shuffle13WhenIGreaterThan1}, hash: EFE343052BCE85D547C0CD6472896EAC
    @Test()
    void shuffle13WhenIGreaterThan1() {
        /* Branches:
         * (i > 1) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            doReturn(0).when(randomMock).nextInt(2);
            long[] longArray = new long[] { 0L, 1L };
            arrayUtils.when(() -> ArrayUtils.swap(longArray, 1, 0, 1)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shuffle(longArray, randomMock);
            //Assert statement(s)
            assertAll("result", () -> {
                verify(randomMock, atLeast(1)).nextInt(2);
                arrayUtils.verify(() -> ArrayUtils.swap(longArray, 1, 0, 1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${shuffle14Test}, hash: 0D9EFB769FDEE1E775720F0CF64D0A27
    @Test()
    void shuffle14Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.shuffle(eq(objectArray), (ThreadLocalRandom) any())).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shuffle(objectArray);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.shuffle(eq(objectArray), (ThreadLocalRandom) any()), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shuffle15WhenIGreaterThan1}, hash: 8EAB6478F5E42C630DD5AB9069244AE7
    @Test()
    void shuffle15WhenIGreaterThan1() {
        /* Branches:
         * (i > 1) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            doReturn(0).when(randomMock).nextInt(2);
            Object object = new Object();
            Object object2 = new Object();
            Object[] objectArray = new Object[] { object, object2 };
            arrayUtils.when(() -> ArrayUtils.swap(objectArray, 1, 0, 1)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shuffle(objectArray, randomMock);
            //Assert statement(s)
            assertAll("result", () -> {
                verify(randomMock, atLeast(1)).nextInt(2);
                arrayUtils.verify(() -> ArrayUtils.swap(objectArray, 1, 0, 1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${shuffle16Test}, hash: 801CFD40676AF6B73492DE7F6EC5E9C7
    @Test()
    void shuffle16Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.shuffle(eq(shortArray), (ThreadLocalRandom) any())).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shuffle(shortArray);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.shuffle(eq(shortArray), (ThreadLocalRandom) any()), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${shuffle17WhenIGreaterThan1}, hash: CB9F28303E310254BD47398ABDBED38C
    @Test()
    void shuffle17WhenIGreaterThan1() {
        /* Branches:
         * (i > 1) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            doReturn(0).when(randomMock).nextInt(2);
            short[] shortArray = new short[] { (short) 0, (short) 1 };
            arrayUtils.when(() -> ArrayUtils.swap(shortArray, 1, 0, 1)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.shuffle(shortArray, randomMock);
            //Assert statement(s)
            assertAll("result", () -> {
                verify(randomMock, atLeast(1)).nextInt(2);
                arrayUtils.verify(() -> ArrayUtils.swap(shortArray, 1, 0, 1), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${startsWithWhenDataEqualsExpected}, hash: 814EC47F2F75ADF4B37DE58BE879298D
    @Test()
    void startsWithWhenDataEqualsExpected() {
        /* Branches:
         * (data == expected) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        //Act Statement(s)
        boolean result = ArrayUtils.startsWith(byteArray, byteArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${startsWithWhenExpectedIsNull}, hash: 721EB99536560ED9F5F327C895FC475C
    @Test()
    void startsWithWhenExpectedIsNull() {
        /* Branches:
         * (data == expected) : false
         * (data == null) : false
         * (expected == null) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        byte[] _byte = null;
        //Act Statement(s)
        boolean result = ArrayUtils.startsWith(byteArray, _byte);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${startsWithWhenExpectedLengthGreaterThanDataLen}, hash: 75EB4A8A44D4214E390C837603EF1585
    @Test()
    void startsWithWhenExpectedLengthGreaterThanDataLen() {
        /* Branches:
         * (data == expected) : false
         * (data == null) : false
         * (expected == null) : false
         * (expected.length > dataLen) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        byte[] byteArray2 = new byte[] { (byte) 0 };
        //Act Statement(s)
        boolean result = ArrayUtils.startsWith(byteArray, byteArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${startsWithWhenArraysEqualsDataExpected}, hash: 1C6EC117AC95B5D750A1CD3DEEFD2600
    @Test()
    void startsWithWhenArraysEqualsDataExpected() {
        /* Branches:
         * (data == expected) : false
         * (data == null) : false
         * (expected == null) : false
         * (expected.length > dataLen) : false
         * (expected.length == dataLen) : true
         * (Arrays.equals(data, expected)) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        byte[] byteArray2 = new byte[] {};
        //Act Statement(s)
        boolean result = ArrayUtils.startsWith(byteArray, byteArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${startsWithWhenArraysNotEqualsDataExpected}, hash: A5AE679C139E8AD2D277484C3DE7993F
    @Disabled()
    @Test()
    void startsWithWhenArraysNotEqualsDataExpected() {
        /* Branches:
         * (data == expected) : false
         * (data == null) : false
         * (expected == null) : false
         * (expected.length > dataLen) : false
         * (expected.length == dataLen) : true
         * (Arrays.equals(data, expected)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        byte[] byteArray2 = new byte[] {};
        //Act Statement(s)
        boolean result = ArrayUtils.startsWith(byteArray, byteArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${startsWithWhenIIndexOfDataNotEqualsIIndexOfExpected}, hash: 8CCE144B00916D10309C5139CE844F9B
    @Test()
    void startsWithWhenIIndexOfDataNotEqualsIIndexOfExpected() {
        /* Branches:
         * (data == expected) : false
         * (data == null) : false
         * (expected == null) : false
         * (expected.length > dataLen) : false
         * (expected.length == dataLen) : false
         * (i < expected.length) : true
         * (data[i] != expected[i]) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] { (byte) 1, (byte) 1 };
        byte[] byteArray2 = new byte[] { (byte) 2 };
        //Act Statement(s)
        boolean result = ArrayUtils.startsWith(byteArray, byteArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${startsWithWhenIIndexOfDataEqualsIIndexOfExpected}, hash: F6C74851458CF63EBDDFB220CCCF5C53
    @Test()
    void startsWithWhenIIndexOfDataEqualsIIndexOfExpected() {
        /* Branches:
         * (data == expected) : false
         * (data == null) : false
         * (expected == null) : false
         * (expected.length > dataLen) : false
         * (expected.length == dataLen) : false
         * (i < expected.length) : true
         * (data[i] != expected[i]) : false
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] { (byte) 1, (byte) 1 };
        byte[] byteArray2 = new byte[] { (byte) 1 };
        //Act Statement(s)
        boolean result = ArrayUtils.startsWith(byteArray, byteArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${subarrayWhenArrayIsNull}, hash: EFAB02184FBCF620AA0BAFF9318157D9
    @Test()
    void subarrayWhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        boolean[] _boolean = null;
        //Act Statement(s)
        boolean[] result = ArrayUtils.subarray(_boolean, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${subarrayWhenNewSizeLessThanOrEqualsTo0}, hash: F50784D28B81859EA58D990BBCD66679
    @Test()
    void subarrayWhenNewSizeLessThanOrEqualsTo0() {
        /* Branches:
         * (array == null) : false
         * (newSize <= 0) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        //Act Statement(s)
        boolean[] result = ArrayUtils.subarray(booleanArray, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${subarrayWhenNewSizeGreaterThan0}, hash: 7883A86306C79CBBF8AD4D12C641F145
    @Test()
    void subarrayWhenNewSizeGreaterThan0() {
        /* Branches:
         * (array == null) : false
         * (newSize <= 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            boolean[] booleanArray2 = new boolean[] { false };
            arrayUtils.when(() -> ArrayUtils.arraycopy(eq(booleanArray2), eq(0), eq(0), eq(1), (Function) any())).thenReturn(booleanArray);
            //Act Statement(s)
            boolean[] result = ArrayUtils.subarray(booleanArray2, 0, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(booleanArray));
                arrayUtils.verify(() -> ArrayUtils.arraycopy(eq(booleanArray2), eq(0), eq(0), eq(1), (Function) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${subarray1WhenArrayIsNull}, hash: 16ED93E16ABC63131C194C9F34BDFA0C
    @Test()
    void subarray1WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        byte[] _byte = null;
        //Act Statement(s)
        byte[] result = ArrayUtils.subarray(_byte, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${subarray1WhenNewSizeLessThanOrEqualsTo0}, hash: 0EED78FD545AE72BCD7FFD9439119ADF
    @Test()
    void subarray1WhenNewSizeLessThanOrEqualsTo0() {
        /* Branches:
         * (array == null) : false
         * (newSize <= 0) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        //Act Statement(s)
        byte[] result = ArrayUtils.subarray(byteArray, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${subarray1WhenNewSizeGreaterThan0}, hash: 15B4A7C196A7EAE484206F789B078C51
    @Test()
    void subarray1WhenNewSizeGreaterThan0() {
        /* Branches:
         * (array == null) : false
         * (newSize <= 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            byte[] byteArray2 = new byte[] { (byte) 0 };
            arrayUtils.when(() -> ArrayUtils.arraycopy(eq(byteArray2), eq(0), eq(0), eq(1), (Function) any())).thenReturn(byteArray);
            //Act Statement(s)
            byte[] result = ArrayUtils.subarray(byteArray2, 0, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(byteArray));
                arrayUtils.verify(() -> ArrayUtils.arraycopy(eq(byteArray2), eq(0), eq(0), eq(1), (Function) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${subarray2WhenArrayIsNull}, hash: 88239AB34119D7A101AB206894D3D9E2
    @Test()
    void subarray2WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        char[] _char = null;
        //Act Statement(s)
        char[] result = ArrayUtils.subarray(_char, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${subarray2WhenNewSizeLessThanOrEqualsTo0}, hash: 91B7E510A44AC3AB92457096E357ADEA
    @Test()
    void subarray2WhenNewSizeLessThanOrEqualsTo0() {
        /* Branches:
         * (array == null) : false
         * (newSize <= 0) : true
         */
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        //Act Statement(s)
        char[] result = ArrayUtils.subarray(charArray, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${subarray2WhenNewSizeGreaterThan0}, hash: B7FF7E1D11E8CD27B36903CE638835D8
    @Test()
    void subarray2WhenNewSizeGreaterThan0() {
        /* Branches:
         * (array == null) : false
         * (newSize <= 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            char[] charArray2 = new char[] { 'A' };
            arrayUtils.when(() -> ArrayUtils.arraycopy(eq(charArray2), eq(0), eq(0), eq(1), (Function) any())).thenReturn(charArray);
            //Act Statement(s)
            char[] result = ArrayUtils.subarray(charArray2, 0, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(charArray));
                arrayUtils.verify(() -> ArrayUtils.arraycopy(eq(charArray2), eq(0), eq(0), eq(1), (Function) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${subarray3WhenArrayIsNull}, hash: A629741D1169BA1E14477A02CBE6F316
    @Test()
    void subarray3WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        double[] _double = null;
        //Act Statement(s)
        double[] result = ArrayUtils.subarray(_double, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${subarray3WhenNewSizeLessThanOrEqualsTo0}, hash: E252762B5903A40E81CD7F48BABFBA62
    @Test()
    void subarray3WhenNewSizeLessThanOrEqualsTo0() {
        /* Branches:
         * (array == null) : false
         * (newSize <= 0) : true
         */
        //Arrange Statement(s)
        double[] doubleArray = new double[] {};
        //Act Statement(s)
        double[] result = ArrayUtils.subarray(doubleArray, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${subarray3WhenNewSizeGreaterThan0}, hash: BBCA983B61C3BDDB5C6972D291ABF038
    @Test()
    void subarray3WhenNewSizeGreaterThan0() {
        /* Branches:
         * (array == null) : false
         * (newSize <= 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            double[] doubleArray2 = new double[] { Double.parseDouble("0") };
            arrayUtils.when(() -> ArrayUtils.arraycopy(eq(doubleArray2), eq(0), eq(0), eq(1), (Function) any())).thenReturn(doubleArray);
            //Act Statement(s)
            double[] result = ArrayUtils.subarray(doubleArray2, 0, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(doubleArray));
                arrayUtils.verify(() -> ArrayUtils.arraycopy(eq(doubleArray2), eq(0), eq(0), eq(1), (Function) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${subarray4WhenArrayIsNull}, hash: B3B8044A92C29E7AAF8670814AAC2044
    @Test()
    void subarray4WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        float[] _float = null;
        //Act Statement(s)
        float[] result = ArrayUtils.subarray(_float, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${subarray4WhenNewSizeLessThanOrEqualsTo0}, hash: 3139AB242827A9C4951EDD706743A5B0
    @Test()
    void subarray4WhenNewSizeLessThanOrEqualsTo0() {
        /* Branches:
         * (array == null) : false
         * (newSize <= 0) : true
         */
        //Arrange Statement(s)
        float[] floatArray = new float[] {};
        //Act Statement(s)
        float[] result = ArrayUtils.subarray(floatArray, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${subarray4WhenNewSizeGreaterThan0}, hash: 8A24E44C548D8672FAF5094F3978B84F
    @Test()
    void subarray4WhenNewSizeGreaterThan0() {
        /* Branches:
         * (array == null) : false
         * (newSize <= 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            float[] floatArray2 = new float[] { Float.parseFloat("0") };
            arrayUtils.when(() -> ArrayUtils.arraycopy(eq(floatArray2), eq(0), eq(0), eq(1), (Function) any())).thenReturn(floatArray);
            //Act Statement(s)
            float[] result = ArrayUtils.subarray(floatArray2, 0, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(floatArray));
                arrayUtils.verify(() -> ArrayUtils.arraycopy(eq(floatArray2), eq(0), eq(0), eq(1), (Function) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${subarray5WhenArrayIsNull}, hash: A0EE411F4103ED39E4C8EB56A6A50924
    @Test()
    void subarray5WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        int[] _int = null;
        //Act Statement(s)
        int[] result = ArrayUtils.subarray(_int, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${subarray5WhenNewSizeLessThanOrEqualsTo0}, hash: 34F9732C76E53E87491B0D1616EA9677
    @Test()
    void subarray5WhenNewSizeLessThanOrEqualsTo0() {
        /* Branches:
         * (array == null) : false
         * (newSize <= 0) : true
         */
        //Arrange Statement(s)
        int[] intArray = new int[] {};
        //Act Statement(s)
        int[] result = ArrayUtils.subarray(intArray, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${subarray5WhenNewSizeGreaterThan0}, hash: E418A5DEA47DFF8AAFFDD78E037470C1
    @Test()
    void subarray5WhenNewSizeGreaterThan0() {
        /* Branches:
         * (array == null) : false
         * (newSize <= 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            int[] intArray2 = new int[] { 0 };
            arrayUtils.when(() -> ArrayUtils.arraycopy(eq(intArray2), eq(0), eq(0), eq(1), (Function) any())).thenReturn(intArray);
            //Act Statement(s)
            int[] result = ArrayUtils.subarray(intArray2, 0, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(intArray));
                arrayUtils.verify(() -> ArrayUtils.arraycopy(eq(intArray2), eq(0), eq(0), eq(1), (Function) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${subarray6WhenArrayIsNull}, hash: D282BA620D2C74DDDD5A2EF339850B97
    @Test()
    void subarray6WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        long[] _long = null;
        //Act Statement(s)
        long[] result = ArrayUtils.subarray(_long, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${subarray6WhenNewSizeLessThanOrEqualsTo0}, hash: DF6C24223055C2F499CD64E0FD02ACFE
    @Test()
    void subarray6WhenNewSizeLessThanOrEqualsTo0() {
        /* Branches:
         * (array == null) : false
         * (newSize <= 0) : true
         */
        //Arrange Statement(s)
        long[] longArray = new long[] {};
        //Act Statement(s)
        long[] result = ArrayUtils.subarray(longArray, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${subarray6WhenNewSizeGreaterThan0}, hash: 0441810DE9D9B0E2E74059E51FA03FD4
    @Test()
    void subarray6WhenNewSizeGreaterThan0() {
        /* Branches:
         * (array == null) : false
         * (newSize <= 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            long[] longArray2 = new long[] { 0L };
            arrayUtils.when(() -> ArrayUtils.arraycopy(eq(longArray2), eq(0), eq(0), eq(1), (Function) any())).thenReturn(longArray);
            //Act Statement(s)
            long[] result = ArrayUtils.subarray(longArray2, 0, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(longArray));
                arrayUtils.verify(() -> ArrayUtils.arraycopy(eq(longArray2), eq(0), eq(0), eq(1), (Function) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${subarray7WhenArrayIsNull}, hash: 60F940D6748B1102B8717D960D36FE07
    @Test()
    void subarray7WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        short[] _short = null;
        //Act Statement(s)
        short[] result = ArrayUtils.subarray(_short, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${subarray7WhenNewSizeLessThanOrEqualsTo0}, hash: 93AA822221013C311F04FD0B793247B7
    @Test()
    void subarray7WhenNewSizeLessThanOrEqualsTo0() {
        /* Branches:
         * (array == null) : false
         * (newSize <= 0) : true
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] {};
        //Act Statement(s)
        short[] result = ArrayUtils.subarray(shortArray, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${subarray7WhenNewSizeGreaterThan0}, hash: 8923EEEE7F5727A43696F6FC203EBBF3
    @Test()
    void subarray7WhenNewSizeGreaterThan0() {
        /* Branches:
         * (array == null) : false
         * (newSize <= 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            short[] shortArray2 = new short[] { (short) 0 };
            arrayUtils.when(() -> ArrayUtils.arraycopy(eq(shortArray2), eq(0), eq(0), eq(1), (Function) any())).thenReturn(shortArray);
            //Act Statement(s)
            short[] result = ArrayUtils.subarray(shortArray2, 0, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(shortArray));
                arrayUtils.verify(() -> ArrayUtils.arraycopy(eq(shortArray2), eq(0), eq(0), eq(1), (Function) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${subarray8WhenArrayIsNull}, hash: D7919BF4B9937FE13C869D1FE2E4A0CC
    @Test()
    void subarray8WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Object[] object = null;
        //Act Statement(s)
        Object[] result = ArrayUtils.subarray(object, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${subarray8WhenNewSizeLessThanOrEqualsTo0}, hash: 747EABDECCE6C6FF2A778F0E01E55A0F
    @Test()
    void subarray8WhenNewSizeLessThanOrEqualsTo0() {
        /* Branches:
         * (array == null) : false
         * (newSize <= 0) : true
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        //Act Statement(s)
        Object[] result = ArrayUtils.subarray(objectArray, 0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${subarray8WhenNewSizeGreaterThan0}, hash: 5EC8367704280579E7F68D63D98DD872
    @Test()
    void subarray8WhenNewSizeGreaterThan0() {
        /* Branches:
         * (array == null) : false
         * (newSize <= 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            Object object = new Object();
            Object[] objectArray2 = new Object[] { object };
            arrayUtils.when(() -> ArrayUtils.arraycopy(eq(objectArray2), eq(0), eq(0), eq(1), (Supplier) any())).thenReturn(objectArray);
            //Act Statement(s)
            Object[] result = ArrayUtils.subarray(objectArray2, 0, 1);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(objectArray));
                arrayUtils.verify(() -> ArrayUtils.arraycopy(eq(objectArray2), eq(0), eq(0), eq(1), (Supplier) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${swapTest}, hash: A8B4C4FCF7894AC6C7EA306E61EA92EE
    @Test()
    void swapTest() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.swap(booleanArray, 0, 0, 1)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.swap(booleanArray, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(booleanArray, 0, 0, 1), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${swap1WhenOffset2GreaterThanOrEqualsToArrayLength}, hash: D7E0E365A857245BB478DB5F1E0344E9
    @Test()
    void swap1WhenOffset2GreaterThanOrEqualsToArrayLength() {
        /* Branches:
         * (isEmpty(array)) : false
         * (offset1 >= array.length) : false
         * (offset2 >= array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(booleanArray)).thenReturn(false);
            //Act Statement(s)
            ArrayUtils.swap(booleanArray, -1, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.isEmpty(booleanArray), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${swap1WhenILessThanLen}, hash: FF377B8E75B0BAFBCCD95E68B13BBD66
    @Test()
    void swap1WhenILessThanLen() {
        /* Branches:
         * (isEmpty(array)) : false
         * (offset1 >= array.length) : false
         * (offset2 >= array.length) : false
         * (i < len) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] { false };
            arrayUtils.when(() -> ArrayUtils.isEmpty(booleanArray)).thenReturn(false);
            //Act Statement(s)
            ArrayUtils.swap(booleanArray, 0, 0, 1);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.isEmpty(booleanArray), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${swap2Test}, hash: 8950A65B4912C7425CCCE134D4165BE7
    @Test()
    void swap2Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.swap(byteArray, 0, 0, 1)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.swap(byteArray, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(byteArray, 0, 0, 1), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${swap3WhenOffset2GreaterThanOrEqualsToArrayLength}, hash: 6178458935A7FD448B6245E95F8A8960
    @Test()
    void swap3WhenOffset2GreaterThanOrEqualsToArrayLength() {
        /* Branches:
         * (isEmpty(array)) : false
         * (offset1 >= array.length) : false
         * (offset2 >= array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(byteArray)).thenReturn(false);
            //Act Statement(s)
            ArrayUtils.swap(byteArray, -1, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.isEmpty(byteArray), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${swap3WhenILessThanLen}, hash: 4B68B46CF2A97D90B0FC499EA6092BC4
    @Test()
    void swap3WhenILessThanLen() {
        /* Branches:
         * (isEmpty(array)) : false
         * (offset1 >= array.length) : false
         * (offset2 >= array.length) : false
         * (i < len) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] { (byte) 0 };
            arrayUtils.when(() -> ArrayUtils.isEmpty(byteArray)).thenReturn(false);
            //Act Statement(s)
            ArrayUtils.swap(byteArray, 0, 0, 1);
            byte[] byteByteArrayArray = new byte[] { (byte) 0 };
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(byteArray, equalTo(byteByteArrayArray));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(byteArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${swap4Test}, hash: 18128BAD42B7888514C7414844C1EBF9
    @Test()
    void swap4Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.swap(charArray, 0, 0, 1)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.swap(charArray, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(charArray, 0, 0, 1), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${swap5WhenOffset2GreaterThanOrEqualsToArrayLength}, hash: AC8DBA9F3128EFEA4CF7A941220B4AF4
    @Test()
    void swap5WhenOffset2GreaterThanOrEqualsToArrayLength() {
        /* Branches:
         * (isEmpty(array)) : false
         * (offset1 >= array.length) : false
         * (offset2 >= array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(charArray)).thenReturn(false);
            //Act Statement(s)
            ArrayUtils.swap(charArray, -1, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.isEmpty(charArray), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${swap5WhenILessThanLen}, hash: E49B1AC9676A058CD9B0D661681AD1DA
    @Test()
    void swap5WhenILessThanLen() {
        /* Branches:
         * (isEmpty(array)) : false
         * (offset1 >= array.length) : false
         * (offset2 >= array.length) : false
         * (i < len) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] { 'A' };
            arrayUtils.when(() -> ArrayUtils.isEmpty(charArray)).thenReturn(false);
            //Act Statement(s)
            ArrayUtils.swap(charArray, 0, 0, 1);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.isEmpty(charArray), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${swap6Test}, hash: 2A70BD2DD5F07E08214D4F7FC21A0DFC
    @Test()
    void swap6Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.swap(doubleArray, 0, 0, 1)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.swap(doubleArray, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(doubleArray, 0, 0, 1), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${swap7WhenOffset2GreaterThanOrEqualsToArrayLength}, hash: 1708492812EDAEA14676EA53E36C6AF4
    @Test()
    void swap7WhenOffset2GreaterThanOrEqualsToArrayLength() {
        /* Branches:
         * (isEmpty(array)) : false
         * (offset1 >= array.length) : false
         * (offset2 >= array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(doubleArray)).thenReturn(false);
            //Act Statement(s)
            ArrayUtils.swap(doubleArray, -1, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.isEmpty(doubleArray), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${swap7WhenILessThanLen}, hash: 78FD4028C3E5D9E508EFDEBD4262F2C8
    @Test()
    void swap7WhenILessThanLen() {
        /* Branches:
         * (isEmpty(array)) : false
         * (offset1 >= array.length) : false
         * (offset2 >= array.length) : false
         * (i < len) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] { Double.parseDouble("0") };
            arrayUtils.when(() -> ArrayUtils.isEmpty(doubleArray)).thenReturn(false);
            //Act Statement(s)
            ArrayUtils.swap(doubleArray, 0, 0, 1);
            double[] doubleDoubleArrayArray = new double[] { Double.parseDouble("0.0") };
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(doubleArray, equalTo(doubleDoubleArrayArray));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(doubleArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${swap8Test}, hash: 7C211521AFFE95F4F546031DD7A4D113
    @Test()
    void swap8Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.swap(floatArray, 0, 0, 1)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.swap(floatArray, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(floatArray, 0, 0, 1), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${swap9WhenOffset2GreaterThanOrEqualsToArrayLength}, hash: 3BC4EFC4E4E5F250AFA6B5C848986DD7
    @Test()
    void swap9WhenOffset2GreaterThanOrEqualsToArrayLength() {
        /* Branches:
         * (isEmpty(array)) : false
         * (offset1 >= array.length) : false
         * (offset2 >= array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(floatArray)).thenReturn(false);
            //Act Statement(s)
            ArrayUtils.swap(floatArray, -1, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.isEmpty(floatArray), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${swap9WhenILessThanLen}, hash: 35E6B4810D7B7C883BCB3F8285B5F4E4
    @Test()
    void swap9WhenILessThanLen() {
        /* Branches:
         * (isEmpty(array)) : false
         * (offset1 >= array.length) : false
         * (offset2 >= array.length) : false
         * (i < len) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] { Float.parseFloat("0") };
            arrayUtils.when(() -> ArrayUtils.isEmpty(floatArray)).thenReturn(false);
            //Act Statement(s)
            ArrayUtils.swap(floatArray, 0, 0, 1);
            float[] floatFloatArrayArray = new float[] { Float.parseFloat("0.0") };
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(floatArray, equalTo(floatFloatArrayArray));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(floatArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${swap10Test}, hash: D4455F6C83C66B7EB8C97B89EB56907D
    @Test()
    void swap10Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.swap(intArray, 0, 0, 1)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.swap(intArray, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(intArray, 0, 0, 1), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${swap11WhenOffset2GreaterThanOrEqualsToArrayLength}, hash: 132FC01DCEAC2F87DBE794ACEFB80B84
    @Test()
    void swap11WhenOffset2GreaterThanOrEqualsToArrayLength() {
        /* Branches:
         * (isEmpty(array)) : false
         * (offset1 >= array.length) : false
         * (offset2 >= array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(intArray)).thenReturn(false);
            //Act Statement(s)
            ArrayUtils.swap(intArray, -1, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.isEmpty(intArray), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${swap11WhenILessThanLen}, hash: E7AC295F8BF6FE517ED0642240F53C66
    @Test()
    void swap11WhenILessThanLen() {
        /* Branches:
         * (isEmpty(array)) : false
         * (offset1 >= array.length) : false
         * (offset2 >= array.length) : false
         * (i < len) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] { 0 };
            arrayUtils.when(() -> ArrayUtils.isEmpty(intArray)).thenReturn(false);
            //Act Statement(s)
            ArrayUtils.swap(intArray, 0, 0, 1);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.isEmpty(intArray), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${swap12Test}, hash: B904994CE3C69F51420F7F6AC5D0F2E1
    @Test()
    void swap12Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.swap(longArray, 0, 0, 1)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.swap(longArray, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(longArray, 0, 0, 1), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${swap13WhenOffset2GreaterThanOrEqualsToArrayLength}, hash: 97747FB3A5F88FA64820148E32E0B7C1
    @Test()
    void swap13WhenOffset2GreaterThanOrEqualsToArrayLength() {
        /* Branches:
         * (isEmpty(array)) : false
         * (offset1 >= array.length) : false
         * (offset2 >= array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(longArray)).thenReturn(false);
            //Act Statement(s)
            ArrayUtils.swap(longArray, -1, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.isEmpty(longArray), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${swap13WhenILessThanLen}, hash: 019119FF896FCE84DC57F43CBC609FBE
    @Test()
    void swap13WhenILessThanLen() {
        /* Branches:
         * (isEmpty(array)) : false
         * (offset1 >= array.length) : false
         * (offset2 >= array.length) : false
         * (i < len) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] { 0L };
            arrayUtils.when(() -> ArrayUtils.isEmpty(longArray)).thenReturn(false);
            //Act Statement(s)
            ArrayUtils.swap(longArray, 0, 0, 1);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.isEmpty(longArray), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${swap14Test}, hash: 6DC15570A5C4E181F9DD2620671ECCFE
    @Test()
    void swap14Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.swap(objectArray, 0, 0, 1)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.swap(objectArray, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(objectArray, 0, 0, 1), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${swap15WhenOffset2GreaterThanOrEqualsToArrayLength}, hash: CA16803566FBECF7A512531B4BC453C9
    @Test()
    void swap15WhenOffset2GreaterThanOrEqualsToArrayLength() {
        /* Branches:
         * (isEmpty(array)) : false
         * (offset1 >= array.length) : false
         * (offset2 >= array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object[] objectArray = new Object[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(objectArray)).thenReturn(false);
            //Act Statement(s)
            ArrayUtils.swap(objectArray, -1, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.isEmpty(objectArray), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${swap15WhenILessThanLen}, hash: 439B56AFBEBCFC8DBAE31C036536C1E6
    @Test()
    void swap15WhenILessThanLen() {
        /* Branches:
         * (isEmpty(array)) : false
         * (offset1 >= array.length) : false
         * (offset2 >= array.length) : false
         * (i < len) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object[] objectArray = new Object[] { object };
            arrayUtils.when(() -> ArrayUtils.isEmpty(objectArray)).thenReturn(false);
            //Act Statement(s)
            ArrayUtils.swap(objectArray, 0, 0, 1);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.isEmpty(objectArray), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${swap16Test}, hash: 5B1FB41E5B987D414151E1DC5086A46B
    @Test()
    void swap16Test() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.swap(shortArray, 0, 0, 1)).thenAnswer((Answer<Void>) invocation -> null);
            //Act Statement(s)
            ArrayUtils.swap(shortArray, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.swap(shortArray, 0, 0, 1), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${swap17WhenOffset2GreaterThanOrEqualsToArrayLength}, hash: A0F99A397F5D77F0BB521A8282FDFE5C
    @Test()
    void swap17WhenOffset2GreaterThanOrEqualsToArrayLength() {
        /* Branches:
         * (isEmpty(array)) : false
         * (offset1 >= array.length) : false
         * (offset2 >= array.length) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] {};
            arrayUtils.when(() -> ArrayUtils.isEmpty(shortArray)).thenReturn(false);
            //Act Statement(s)
            ArrayUtils.swap(shortArray, -1, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.isEmpty(shortArray), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${swap17WhenOffset1EqualsOffset2}, hash: 2E758BBEE51F4925D52C6A471A6DBF37
    @Test()
    void swap17WhenOffset1EqualsOffset2() {
        /* Branches:
         * (isEmpty(array)) : false
         * (offset1 >= array.length) : false
         * (offset2 >= array.length) : false
         * (offset1 == offset2) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] { (short) 0 };
            arrayUtils.when(() -> ArrayUtils.isEmpty(shortArray)).thenReturn(false);
            //Act Statement(s)
            ArrayUtils.swap(shortArray, 0, 0, 0);
            //Assert statement(s)
            assertAll("result", () -> arrayUtils.verify(() -> ArrayUtils.isEmpty(shortArray), atLeast(1)));
        }
    }

    //BaseRock generated method id: ${swap17WhenILessThanLen}, hash: 4C22FF6B7FDA1406C548E251674CD50C
    @Test()
    void swap17WhenILessThanLen() {
        /* Branches:
         * (isEmpty(array)) : false
         * (offset1 >= array.length) : false
         * (offset2 >= array.length) : false
         * (offset1 == offset2) : false
         * (i < len) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            short[] shortArray = new short[] { (short) 1, (short) 0 };
            arrayUtils.when(() -> ArrayUtils.isEmpty(shortArray)).thenReturn(false);
            //Act Statement(s)
            ArrayUtils.swap(shortArray, 1, 0, 1);
            short[] shortShortArrayArray = new short[] { (short) 0, (short) 1 };
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(shortArray, equalTo(shortShortArrayArray));
                arrayUtils.verify(() -> ArrayUtils.isEmpty(shortArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toArrayTest}, hash: D2FAED1AB2D2097874E0C5B79A3BAD7E
    @Test()
    void toArrayTest() {
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        //Act Statement(s)
        Object[] result = ArrayUtils.toArray(objectArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(objectArray)));
    }

    //BaseRock generated method id: ${toMapWhenArrayIsNull}, hash: F6D3A974C3E60A4D3B41D80C14719436
    @Test()
    void toMapWhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Object[] object = null;
        //Act Statement(s)
        Map<Object, Object> result = ArrayUtils.toMap(object);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toMapWhenObjectInstanceOfMapEntry__}, hash: 6191876D17FA8334135CF17E23EAE095
    @Disabled()
    @Test()
    void toMapWhenObjectInstanceOfMapEntry__() {
        /* Branches:
         * (array == null) : false
         * (i < array.length) : true
         * (object instanceof Map.Entry<?, ?>) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        Object object3 = new Object();
        Object[] objectArray = new Object[] { new AbstractMap.SimpleEntry<>(object, object2), object3 };
        //Act Statement(s)
        Map<Object, Object> result = ArrayUtils.toMap(objectArray);
        Map<Object, Object> objectObjectResultMap = new HashMap<>(3);
        objectObjectResultMap.put(object, object2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(objectObjectResultMap)));
    }

    //BaseRock generated method id: ${toMapWhenObjectNotInstanceOfObjectArrayThrowsIllegalArgumentException}, hash: 24FF1E2E6D066BB8317FEF35B8171C52
    @Test()
    void toMapWhenObjectNotInstanceOfObjectArrayThrowsIllegalArgumentException() {
        /* Branches:
         * (array == null) : false
         * (i < array.length) : true
         * (object instanceof Map.Entry<?, ?>) : false
         * (object instanceof Object[]) : false
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object[] objectArray = new Object[] { objectMock, object };
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Array element 0, 'object', is neither of type Map.Entry nor an Array");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            ArrayUtils.toMap(objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${toMapWhenEntryLengthLessThan2ThrowsIllegalArgumentException}, hash: 5CE891D1C5E43E3A184DB832C817D4F3
    @Disabled()
    @Test()
    void toMapWhenEntryLengthLessThan2ThrowsIllegalArgumentException() {
        /* Branches:
         * (array == null) : false
         * (i < array.length) : true
         * (object instanceof Map.Entry<?, ?>) : false
         * (object instanceof Object[]) : true
         * (entry.length < 2) : true
         */
        //Arrange Statement(s)
        Object[] objectArray2 = new Object[] {};
        Object object = new Object();
        Object[] objectArray = new Object[] { objectArray2, object };
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("Array element 0, '[-]', has a length less than 2");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            ArrayUtils.toMap(objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${toMapWhenEntryLengthNotLessThan2}, hash: 8DDE204A3114E8064E1035AF069C215E
    @Disabled()
    @Test()
    void toMapWhenEntryLengthNotLessThan2() {
        /* Branches:
         * (array == null) : false
         * (i < array.length) : true
         * (object instanceof Map.Entry<?, ?>) : false
         * (object instanceof Object[]) : true
         * (entry.length < 2) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        Object[] objectArray2 = new Object[] { object, object2 };
        Object object3 = new Object();
        Object[] objectArray = new Object[] { objectArray2, object3 };
        //Act Statement(s)
        Map<Object, Object> result = ArrayUtils.toMap(objectArray);
        Map<Object, Object> objectObjectResultMap = new HashMap<>(3);
        objectObjectResultMap.put(object, object2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(objectObjectResultMap)));
    }

    //BaseRock generated method id: ${toObjectWhenArrayIsNull}, hash: AE351B21D99454796BE9D4CB9BA7F461
    @Test()
    void toObjectWhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        boolean[] _boolean = null;
        //Act Statement(s)
        Boolean[] result = ArrayUtils.toObject(_boolean);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toObjectWhenArrayLengthEquals0}, hash: 3BADC4BF00996E366049DD9843C4B298
    @Test()
    void toObjectWhenArrayLengthEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : true
         */
        //Arrange Statement(s)
        boolean[] booleanArray = new boolean[] {};
        //Act Statement(s)
        Boolean[] result = ArrayUtils.toObject(booleanArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toObjectWhenArrayLengthNotEquals0}, hash: 2C19E843F87FD9C5B4E690D2B5208414
    @Test()
    void toObjectWhenArrayLengthNotEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Boolean[] booleanArray = new Boolean[] {};
            Boolean[] booleanArray2 = new Boolean[] { (Boolean) null };
            arrayUtils.when(() -> ArrayUtils.setAll(eq(booleanArray2), (IntFunction) any())).thenReturn(booleanArray);
            boolean[] booleanArray3 = new boolean[] { false };
            //Act Statement(s)
            Boolean[] result = ArrayUtils.toObject(booleanArray3);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(booleanArray));
                arrayUtils.verify(() -> ArrayUtils.setAll(eq(booleanArray2), (IntFunction) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toObject1WhenArrayIsNull}, hash: B397B6D71C299BDA374EDA41213D674E
    @Test()
    void toObject1WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        byte[] _byte = null;
        //Act Statement(s)
        Byte[] result = ArrayUtils.toObject(_byte);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toObject1WhenArrayLengthEquals0}, hash: E597CFFF7B6D47D5F4F62487B4005D83
    @Test()
    void toObject1WhenArrayLengthEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : true
         */
        //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        //Act Statement(s)
        Byte[] result = ArrayUtils.toObject(byteArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toObject1WhenArrayLengthNotEquals0}, hash: 983FF2D8CAD2489CD0F825CB179A2192
    @Test()
    void toObject1WhenArrayLengthNotEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Byte[] byteArray = new Byte[] {};
            Byte[] byteArray2 = new Byte[] { (Byte) null };
            arrayUtils.when(() -> ArrayUtils.setAll(eq(byteArray2), (IntFunction) any())).thenReturn(byteArray);
            byte[] byteArray3 = new byte[] { (byte) 0 };
            //Act Statement(s)
            Byte[] result = ArrayUtils.toObject(byteArray3);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(byteArray));
                arrayUtils.verify(() -> ArrayUtils.setAll(eq(byteArray2), (IntFunction) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toObject2WhenArrayIsNull}, hash: 96FC075B55BE91D9B4AA2D3D48304F63
    @Test()
    void toObject2WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        char[] _char = null;
        //Act Statement(s)
        Character[] result = ArrayUtils.toObject(_char);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toObject2WhenArrayLengthEquals0}, hash: E33776B4F7C10C136B559818CDA7F9C9
    @Test()
    void toObject2WhenArrayLengthEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : true
         */
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        //Act Statement(s)
        Character[] result = ArrayUtils.toObject(charArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toObject2WhenArrayLengthNotEquals0}, hash: 13B64333150121A40F21B51F6399CAAC
    @Test()
    void toObject2WhenArrayLengthNotEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Character[] characterArray = new Character[] {};
            Character[] characterArray2 = new Character[] { (Character) null };
            arrayUtils.when(() -> ArrayUtils.setAll(eq(characterArray2), (IntFunction) any())).thenReturn(characterArray);
            char[] charArray = new char[] { 'A' };
            //Act Statement(s)
            Character[] result = ArrayUtils.toObject(charArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(characterArray));
                arrayUtils.verify(() -> ArrayUtils.setAll(eq(characterArray2), (IntFunction) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toObject3WhenArrayIsNull}, hash: D3B61F2E3D8ED410E910BCFBBA64913E
    @Test()
    void toObject3WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        double[] _double = null;
        //Act Statement(s)
        Double[] result = ArrayUtils.toObject(_double);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toObject3WhenArrayLengthEquals0}, hash: 245302C2F858F68221B3F3659CF53468
    @Test()
    void toObject3WhenArrayLengthEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : true
         */
        //Arrange Statement(s)
        double[] doubleArray = new double[] {};
        //Act Statement(s)
        Double[] result = ArrayUtils.toObject(doubleArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toObject3WhenArrayLengthNotEquals0}, hash: 19BD8A732AEB5CE5A35AA301DF1592C2
    @Test()
    void toObject3WhenArrayLengthNotEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Double[] doubleArray = new Double[] {};
            Double[] doubleArray2 = new Double[] { (Double) null };
            arrayUtils.when(() -> ArrayUtils.setAll(eq(doubleArray2), (IntFunction) any())).thenReturn(doubleArray);
            double[] doubleArray3 = new double[] { Double.parseDouble("0") };
            //Act Statement(s)
            Double[] result = ArrayUtils.toObject(doubleArray3);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(doubleArray));
                arrayUtils.verify(() -> ArrayUtils.setAll(eq(doubleArray2), (IntFunction) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toObject4WhenArrayIsNull}, hash: 88E1BF13B388925714CD7446B7446FDE
    @Test()
    void toObject4WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        float[] _float = null;
        //Act Statement(s)
        Float[] result = ArrayUtils.toObject(_float);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toObject4WhenArrayLengthEquals0}, hash: 540B2B82080A7D0115B519350E7D485D
    @Test()
    void toObject4WhenArrayLengthEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : true
         */
        //Arrange Statement(s)
        float[] floatArray = new float[] {};
        //Act Statement(s)
        Float[] result = ArrayUtils.toObject(floatArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toObject4WhenArrayLengthNotEquals0}, hash: C2BCEC1062944A5A70B1B44AD2951CD5
    @Test()
    void toObject4WhenArrayLengthNotEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Float[] floatArray = new Float[] {};
            Float[] floatArray2 = new Float[] { (Float) null };
            arrayUtils.when(() -> ArrayUtils.setAll(eq(floatArray2), (IntFunction) any())).thenReturn(floatArray);
            float[] floatArray3 = new float[] { Float.parseFloat("0") };
            //Act Statement(s)
            Float[] result = ArrayUtils.toObject(floatArray3);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(floatArray));
                arrayUtils.verify(() -> ArrayUtils.setAll(eq(floatArray2), (IntFunction) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toObject5WhenArrayIsNull}, hash: A054AC8FFA4A9E762CAF6071942F6FC5
    @Test()
    void toObject5WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        int[] _int = null;
        //Act Statement(s)
        Integer[] result = ArrayUtils.toObject(_int);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toObject5WhenArrayLengthEquals0}, hash: E777CEF968C58C750005829453E4D606
    @Test()
    void toObject5WhenArrayLengthEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : true
         */
        //Arrange Statement(s)
        int[] intArray = new int[] {};
        //Act Statement(s)
        Integer[] result = ArrayUtils.toObject(intArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toObject5WhenArrayLengthNotEquals0}, hash: 187475EC2D142EBA935DA53810A7BC8B
    @Test()
    void toObject5WhenArrayLengthNotEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Integer[] integerArray = new Integer[] {};
            Integer[] integerArray2 = new Integer[] { (Integer) null };
            arrayUtils.when(() -> ArrayUtils.setAll(eq(integerArray2), (IntFunction) any())).thenReturn(integerArray);
            int[] intArray = new int[] { 0 };
            //Act Statement(s)
            Integer[] result = ArrayUtils.toObject(intArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(integerArray));
                arrayUtils.verify(() -> ArrayUtils.setAll(eq(integerArray2), (IntFunction) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toObject6WhenArrayIsNull}, hash: 940A5F9097C38774F792FE8D2BBBB1E0
    @Test()
    void toObject6WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        long[] _long = null;
        //Act Statement(s)
        Long[] result = ArrayUtils.toObject(_long);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toObject6WhenArrayLengthEquals0}, hash: FF52834ABF3B3886595711E637850BB2
    @Test()
    void toObject6WhenArrayLengthEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : true
         */
        //Arrange Statement(s)
        long[] longArray = new long[] {};
        //Act Statement(s)
        Long[] result = ArrayUtils.toObject(longArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toObject6WhenArrayLengthNotEquals0}, hash: ED6B9B3A390A90A579A06ECBF174BF2C
    @Test()
    void toObject6WhenArrayLengthNotEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Long[] longArray = new Long[] {};
            Long[] longArray2 = new Long[] { (Long) null };
            arrayUtils.when(() -> ArrayUtils.setAll(eq(longArray2), (IntFunction) any())).thenReturn(longArray);
            long[] longArray3 = new long[] { 0L };
            //Act Statement(s)
            Long[] result = ArrayUtils.toObject(longArray3);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(longArray));
                arrayUtils.verify(() -> ArrayUtils.setAll(eq(longArray2), (IntFunction) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toObject7WhenArrayIsNull}, hash: BC6EFE7A41C339FB38D43B5D10F06525
    @Test()
    void toObject7WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        short[] _short = null;
        //Act Statement(s)
        Short[] result = ArrayUtils.toObject(_short);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toObject7WhenArrayLengthEquals0}, hash: FF3CFD0EE0536A778B3DCC898D538A4E
    @Test()
    void toObject7WhenArrayLengthEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : true
         */
        //Arrange Statement(s)
        short[] shortArray = new short[] {};
        //Act Statement(s)
        Short[] result = ArrayUtils.toObject(shortArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toObject7WhenArrayLengthNotEquals0}, hash: D93F7AB75E4F3FBE46600B2CBE402A61
    @Test()
    void toObject7WhenArrayLengthNotEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Short[] shortArray = new Short[] {};
            Short[] shortArray2 = new Short[] { (Short) null };
            arrayUtils.when(() -> ArrayUtils.setAll(eq(shortArray2), (IntFunction) any())).thenReturn(shortArray);
            short[] shortArray3 = new short[] { (short) 0 };
            //Act Statement(s)
            Short[] result = ArrayUtils.toObject(shortArray3);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(shortArray));
                arrayUtils.verify(() -> ArrayUtils.setAll(eq(shortArray2), (IntFunction) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toPrimitiveTest}, hash: 98F697D1B6A7F54F63A9B2EBE6FD7429
    @Test()
    void toPrimitiveTest() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            Boolean[] booleanArray2 = new Boolean[] {};
            arrayUtils.when(() -> ArrayUtils.toPrimitive(booleanArray2, false)).thenReturn(booleanArray);
            //Act Statement(s)
            boolean[] result = ArrayUtils.toPrimitive(booleanArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(booleanArray));
                arrayUtils.verify(() -> ArrayUtils.toPrimitive(booleanArray2, false), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toPrimitive1WhenArrayIsNull}, hash: 3BCD43F159CA6F13BE068BD065E07ECF
    @Test()
    void toPrimitive1WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Boolean[] _boolean = null;
        //Act Statement(s)
        boolean[] result = ArrayUtils.toPrimitive(_boolean, false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toPrimitive1WhenArrayLengthEquals0}, hash: 7A4E157E708F745EA50E2EFB96FA069E
    @Test()
    void toPrimitive1WhenArrayLengthEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : true
         */
        //Arrange Statement(s)
        Boolean[] booleanArray = new Boolean[] {};
        //Act Statement(s)
        boolean[] result = ArrayUtils.toPrimitive(booleanArray, false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toPrimitive1WhenBIsNull}, hash: B3B65D860668EC2CB2D5A6C8B232B14B
    @Test()
    void toPrimitive1WhenBIsNull() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         * (i < array.length) : true
         * (b == null) : true
         */
        //Arrange Statement(s)
        Boolean[] booleanArray = new Boolean[] { (Boolean) null };
        //Act Statement(s)
        boolean[] result = ArrayUtils.toPrimitive(booleanArray, false);
        boolean[] booleanResultArray = new boolean[] { false };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${toPrimitive1WhenBIsNotNull}, hash: 2371E52626B79E2440349D86ACACE802
    @Test()
    void toPrimitive1WhenBIsNotNull() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         * (i < array.length) : true
         * (b == null) : false
         */
        //Arrange Statement(s)
        Boolean[] booleanArray = new Boolean[] { false };
        //Act Statement(s)
        boolean[] result = ArrayUtils.toPrimitive(booleanArray, false);
        boolean[] booleanResultArray = new boolean[] { false };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(booleanResultArray)));
    }

    //BaseRock generated method id: ${toPrimitive2WhenArrayIsNull}, hash: 2E62A86B07BC9382E3C1DADFAD7C0F76
    @Test()
    void toPrimitive2WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Byte[] _byte = null;
        //Act Statement(s)
        byte[] result = ArrayUtils.toPrimitive(_byte);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toPrimitive2WhenArrayLengthEquals0}, hash: 781ED03C3FB679A840A6142989A210AB
    @Test()
    void toPrimitive2WhenArrayLengthEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : true
         */
        //Arrange Statement(s)
        Byte[] byteArray = new Byte[] {};
        //Act Statement(s)
        byte[] result = ArrayUtils.toPrimitive(byteArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toPrimitive2WhenILessThanArrayLength}, hash: 0DB6189012164C56C1326E3EFA389AB4
    @Test()
    void toPrimitive2WhenILessThanArrayLength() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         * (i < array.length) : true
         */
        //Arrange Statement(s)
        Byte[] byteArray = new Byte[] { (byte) 1 };
        //Act Statement(s)
        byte[] result = ArrayUtils.toPrimitive(byteArray);
        byte[] byteResultArray = new byte[] { (byte) 1 };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(byteResultArray)));
    }

    //BaseRock generated method id: ${toPrimitive3WhenArrayIsNull}, hash: 9BEAD59C934CC855E5876361C4350BEF
    @Test()
    void toPrimitive3WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Byte[] _byte = null;
        //Act Statement(s)
        byte[] result = ArrayUtils.toPrimitive(_byte, (byte) 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toPrimitive3WhenArrayLengthEquals0}, hash: 125229B3F5EEE577AB7D0F5A91564006
    @Test()
    void toPrimitive3WhenArrayLengthEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : true
         */
        //Arrange Statement(s)
        Byte[] byteArray = new Byte[] {};
        //Act Statement(s)
        byte[] result = ArrayUtils.toPrimitive(byteArray, (byte) 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toPrimitive3WhenBIsNull}, hash: 47D2DC38AFBAC05705F8136D53FDACE8
    @Test()
    void toPrimitive3WhenBIsNull() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         * (i < array.length) : true
         * (b == null) : true
         */
        //Arrange Statement(s)
        Byte[] byteArray = new Byte[] { (Byte) null };
        //Act Statement(s)
        byte[] result = ArrayUtils.toPrimitive(byteArray, (byte) 0);
        byte[] byteResultArray = new byte[] { (byte) 0 };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(byteResultArray)));
    }

    //BaseRock generated method id: ${toPrimitive3WhenBIsNotNull}, hash: 3A305B8F51B141CF0EE90679C0CA99AD
    @Test()
    void toPrimitive3WhenBIsNotNull() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         * (i < array.length) : true
         * (b == null) : false
         */
        //Arrange Statement(s)
        Byte[] byteArray = new Byte[] { (byte) 1 };
        //Act Statement(s)
        byte[] result = ArrayUtils.toPrimitive(byteArray, (byte) 0);
        byte[] byteResultArray = new byte[] { (byte) 1 };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(byteResultArray)));
    }

    //BaseRock generated method id: ${toPrimitive4WhenArrayIsNull}, hash: 46504D6CE454A61455AD028F23D86CE1
    @Test()
    void toPrimitive4WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Character[] character = null;
        //Act Statement(s)
        char[] result = ArrayUtils.toPrimitive(character);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toPrimitive4WhenArrayLengthEquals0}, hash: 33B41FDA1D6EDF8A446D1A474FF1FAA1
    @Test()
    void toPrimitive4WhenArrayLengthEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : true
         */
        //Arrange Statement(s)
        Character[] characterArray = new Character[] {};
        //Act Statement(s)
        char[] result = ArrayUtils.toPrimitive(characterArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toPrimitive4WhenILessThanArrayLength}, hash: 5548FC498E1435BBA705FCE0B7673F66
    @Test()
    void toPrimitive4WhenILessThanArrayLength() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         * (i < array.length) : true
         */
        //Arrange Statement(s)
        Character[] characterArray = new Character[] { 'A' };
        //Act Statement(s)
        char[] result = ArrayUtils.toPrimitive(characterArray);
        char[] charResultArray = new char[] { 'A' };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(charResultArray)));
    }

    //BaseRock generated method id: ${toPrimitive5WhenArrayIsNull}, hash: A1D39AE68D178A73C09C308E31B6E371
    @Test()
    void toPrimitive5WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Character[] character = null;
        //Act Statement(s)
        char[] result = ArrayUtils.toPrimitive(character, 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toPrimitive5WhenArrayLengthEquals0}, hash: 5BA01DDBAFDC714FC21D06C260B1B2B7
    @Test()
    void toPrimitive5WhenArrayLengthEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : true
         */
        //Arrange Statement(s)
        Character[] characterArray = new Character[] {};
        //Act Statement(s)
        char[] result = ArrayUtils.toPrimitive(characterArray, 'A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toPrimitive5WhenBIsNull}, hash: 329424CD22EFD070283E0F46EA3C2EAE
    @Test()
    void toPrimitive5WhenBIsNull() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         * (i < array.length) : true
         * (b == null) : true
         */
        //Arrange Statement(s)
        Character[] characterArray = new Character[] { (Character) null };
        //Act Statement(s)
        char[] result = ArrayUtils.toPrimitive(characterArray, 'A');
        char[] charResultArray = new char[] { 'A' };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(charResultArray)));
    }

    //BaseRock generated method id: ${toPrimitive5WhenBIsNotNull}, hash: 45978537D72DDA815591397802FE2DEB
    @Test()
    void toPrimitive5WhenBIsNotNull() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         * (i < array.length) : true
         * (b == null) : false
         */
        //Arrange Statement(s)
        Character[] characterArray = new Character[] { 'A' };
        //Act Statement(s)
        char[] result = ArrayUtils.toPrimitive(characterArray, 'A');
        char[] charResultArray = new char[] { 'A' };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(charResultArray)));
    }

    //BaseRock generated method id: ${toPrimitive6WhenArrayIsNull}, hash: 3A5F4BD5953C8220575933372C58DBE1
    @Test()
    void toPrimitive6WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Double[] _double = null;
        //Act Statement(s)
        double[] result = ArrayUtils.toPrimitive(_double);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toPrimitive6WhenArrayLengthEquals0}, hash: 07CCD5A51B58FBA9CB44D0218EE53553
    @Test()
    void toPrimitive6WhenArrayLengthEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : true
         */
        //Arrange Statement(s)
        Double[] doubleArray = new Double[] {};
        //Act Statement(s)
        double[] result = ArrayUtils.toPrimitive(doubleArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toPrimitive6WhenILessThanArrayLength}, hash: C0D209E7007286E9595F2527725BB7DF
    @Test()
    void toPrimitive6WhenILessThanArrayLength() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         * (i < array.length) : true
         */
        //Arrange Statement(s)
        Double[] doubleArray = new Double[] { Double.parseDouble("1.0") };
        //Act Statement(s)
        double[] result = ArrayUtils.toPrimitive(doubleArray);
        double[] doubleResultArray = new double[] { Double.parseDouble("1.0") };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(doubleResultArray)));
    }

    //BaseRock generated method id: ${toPrimitive7WhenArrayIsNull}, hash: E361DC9D3A0E75499E48934959197098
    @Test()
    void toPrimitive7WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Double[] _double = null;
        //Act Statement(s)
        double[] result = ArrayUtils.toPrimitive(_double, Double.parseDouble("0.0"));
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toPrimitive7WhenArrayLengthEquals0}, hash: E976915D93FA38596E72E524693A1ED3
    @Test()
    void toPrimitive7WhenArrayLengthEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : true
         */
        //Arrange Statement(s)
        Double[] doubleArray = new Double[] {};
        //Act Statement(s)
        double[] result = ArrayUtils.toPrimitive(doubleArray, Double.parseDouble("0.0"));
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toPrimitive7WhenBIsNull}, hash: D801F9A2D349C7FFD8CAB68E8E88FFDE
    @Test()
    void toPrimitive7WhenBIsNull() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         * (i < array.length) : true
         * (b == null) : true
         */
        //Arrange Statement(s)
        Double[] doubleArray = new Double[] { (Double) null };
        //Act Statement(s)
        double[] result = ArrayUtils.toPrimitive(doubleArray, Double.parseDouble("0.0"));
        double[] doubleResultArray = new double[] { Double.parseDouble("0.0") };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(doubleResultArray)));
    }

    //BaseRock generated method id: ${toPrimitive7WhenBIsNotNull}, hash: 755F026FC9AD49DA6C72EA0BBB8B243A
    @Test()
    void toPrimitive7WhenBIsNotNull() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         * (i < array.length) : true
         * (b == null) : false
         */
        //Arrange Statement(s)
        Double[] doubleArray = new Double[] { Double.parseDouble("1.0") };
        //Act Statement(s)
        double[] result = ArrayUtils.toPrimitive(doubleArray, Double.parseDouble("0.0"));
        double[] doubleResultArray = new double[] { Double.parseDouble("1.0") };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(doubleResultArray)));
    }

    //BaseRock generated method id: ${toPrimitive8WhenArrayIsNull}, hash: E3192F048A02F9DE8B225CB4268937BD
    @Test()
    void toPrimitive8WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Float[] _float = null;
        //Act Statement(s)
        float[] result = ArrayUtils.toPrimitive(_float);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toPrimitive8WhenArrayLengthEquals0}, hash: E5DF0EDF692B2BFC1470AA7472E73668
    @Test()
    void toPrimitive8WhenArrayLengthEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : true
         */
        //Arrange Statement(s)
        Float[] floatArray = new Float[] {};
        //Act Statement(s)
        float[] result = ArrayUtils.toPrimitive(floatArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toPrimitive8WhenILessThanArrayLength}, hash: A648B9AFFCF5F71484E44CD97A899659
    @Test()
    void toPrimitive8WhenILessThanArrayLength() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         * (i < array.length) : true
         */
        //Arrange Statement(s)
        Float[] floatArray = new Float[] { Float.parseFloat("1.0") };
        //Act Statement(s)
        float[] result = ArrayUtils.toPrimitive(floatArray);
        float[] floatResultArray = new float[] { Float.parseFloat("1.0") };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(floatResultArray)));
    }

    //BaseRock generated method id: ${toPrimitive9WhenArrayIsNull}, hash: 88C63186BF97608D69B9B4895DB67299
    @Test()
    void toPrimitive9WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Float[] _float = null;
        //Act Statement(s)
        float[] result = ArrayUtils.toPrimitive(_float, Float.parseFloat("0.0"));
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toPrimitive9WhenArrayLengthEquals0}, hash: A7626306FC5BBB9C9FA7D5004FB6231D
    @Test()
    void toPrimitive9WhenArrayLengthEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : true
         */
        //Arrange Statement(s)
        Float[] floatArray = new Float[] {};
        //Act Statement(s)
        float[] result = ArrayUtils.toPrimitive(floatArray, Float.parseFloat("0.0"));
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toPrimitive9WhenBIsNull}, hash: ECBC67579827DA1B640671DC8C575091
    @Test()
    void toPrimitive9WhenBIsNull() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         * (i < array.length) : true
         * (b == null) : true
         */
        //Arrange Statement(s)
        Float[] floatArray = new Float[] { (Float) null };
        //Act Statement(s)
        float[] result = ArrayUtils.toPrimitive(floatArray, Float.parseFloat("0.0"));
        float[] floatResultArray = new float[] { Float.parseFloat("0.0") };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(floatResultArray)));
    }

    //BaseRock generated method id: ${toPrimitive9WhenBIsNotNull}, hash: 86D20B6C0AD8EB0E787B7C6F5FE31C29
    @Test()
    void toPrimitive9WhenBIsNotNull() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         * (i < array.length) : true
         * (b == null) : false
         */
        //Arrange Statement(s)
        Float[] floatArray = new Float[] { Float.parseFloat("1.0") };
        //Act Statement(s)
        float[] result = ArrayUtils.toPrimitive(floatArray, Float.parseFloat("0.0"));
        float[] floatResultArray = new float[] { Float.parseFloat("1.0") };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(floatResultArray)));
    }

    //BaseRock generated method id: ${toPrimitive10WhenArrayIsNull}, hash: 1683BF1119A4D4BC43D4D8F3839D63F6
    @Test()
    void toPrimitive10WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Integer[] integer = null;
        //Act Statement(s)
        int[] result = ArrayUtils.toPrimitive(integer);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toPrimitive10WhenArrayLengthEquals0}, hash: 68D66B2A3F3B7644632807CE8778CB19
    @Test()
    void toPrimitive10WhenArrayLengthEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : true
         */
        //Arrange Statement(s)
        Integer[] integerArray = new Integer[] {};
        //Act Statement(s)
        int[] result = ArrayUtils.toPrimitive(integerArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toPrimitive10WhenILessThanArrayLength}, hash: 2B3295F0B33322868A55CE49FDA8D034
    @Test()
    void toPrimitive10WhenILessThanArrayLength() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         * (i < array.length) : true
         */
        //Arrange Statement(s)
        Integer[] integerArray = new Integer[] { 1 };
        //Act Statement(s)
        int[] result = ArrayUtils.toPrimitive(integerArray);
        int[] intResultArray = new int[] { 1 };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(intResultArray)));
    }

    //BaseRock generated method id: ${toPrimitive11WhenArrayIsNull}, hash: A8CD37A87746230C3707A4AC9532EC16
    @Test()
    void toPrimitive11WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Integer[] integer = null;
        //Act Statement(s)
        int[] result = ArrayUtils.toPrimitive(integer, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toPrimitive11WhenArrayLengthEquals0}, hash: 39E401FD3DE198308D0335CF7768C91B
    @Test()
    void toPrimitive11WhenArrayLengthEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : true
         */
        //Arrange Statement(s)
        Integer[] integerArray = new Integer[] {};
        //Act Statement(s)
        int[] result = ArrayUtils.toPrimitive(integerArray, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toPrimitive11WhenBIsNull}, hash: 1E79E6922FCA67E4D3C59610546296EC
    @Test()
    void toPrimitive11WhenBIsNull() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         * (i < array.length) : true
         * (b == null) : true
         */
        //Arrange Statement(s)
        Integer[] integerArray = new Integer[] { (Integer) null };
        //Act Statement(s)
        int[] result = ArrayUtils.toPrimitive(integerArray, 0);
        int[] intResultArray = new int[] { 0 };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(intResultArray)));
    }

    //BaseRock generated method id: ${toPrimitive11WhenBIsNotNull}, hash: F0ECF6AC3B8B1E2F40157CB520754649
    @Test()
    void toPrimitive11WhenBIsNotNull() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         * (i < array.length) : true
         * (b == null) : false
         */
        //Arrange Statement(s)
        Integer[] integerArray = new Integer[] { 1 };
        //Act Statement(s)
        int[] result = ArrayUtils.toPrimitive(integerArray, 0);
        int[] intResultArray = new int[] { 1 };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(intResultArray)));
    }

    //BaseRock generated method id: ${toPrimitive12WhenArrayIsNull}, hash: 6512FE2E5DDD2809991EB9E9833A73DC
    @Test()
    void toPrimitive12WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Long[] _long = null;
        //Act Statement(s)
        long[] result = ArrayUtils.toPrimitive(_long);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toPrimitive12WhenArrayLengthEquals0}, hash: 833EC80BA3E488FABEB494BB23F21C12
    @Test()
    void toPrimitive12WhenArrayLengthEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : true
         */
        //Arrange Statement(s)
        Long[] longArray = new Long[] {};
        //Act Statement(s)
        long[] result = ArrayUtils.toPrimitive(longArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toPrimitive12WhenILessThanArrayLength}, hash: 69FF540EBEFE4DAC2103B48CA8A20B94
    @Test()
    void toPrimitive12WhenILessThanArrayLength() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         * (i < array.length) : true
         */
        //Arrange Statement(s)
        Long[] longArray = new Long[] { 1L };
        //Act Statement(s)
        long[] result = ArrayUtils.toPrimitive(longArray);
        long[] longResultArray = new long[] { 1L };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(longResultArray)));
    }

    //BaseRock generated method id: ${toPrimitive13WhenArrayIsNull}, hash: 4B1D3D9B12950CDB123DB8B6DE95417F
    @Test()
    void toPrimitive13WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Long[] _long = null;
        //Act Statement(s)
        long[] result = ArrayUtils.toPrimitive(_long, 0L);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toPrimitive13WhenArrayLengthEquals0}, hash: 44D9DEABB6B1F0780112D27C6CAB1B39
    @Test()
    void toPrimitive13WhenArrayLengthEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : true
         */
        //Arrange Statement(s)
        Long[] longArray = new Long[] {};
        //Act Statement(s)
        long[] result = ArrayUtils.toPrimitive(longArray, 0L);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toPrimitive13WhenBIsNull}, hash: 6B2507741EB24F3ABDC09E0AED7543E6
    @Test()
    void toPrimitive13WhenBIsNull() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         * (i < array.length) : true
         * (b == null) : true
         */
        //Arrange Statement(s)
        Long[] longArray = new Long[] { (Long) null };
        //Act Statement(s)
        long[] result = ArrayUtils.toPrimitive(longArray, 0L);
        long[] longResultArray = new long[] { 0L };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(longResultArray)));
    }

    //BaseRock generated method id: ${toPrimitive13WhenBIsNotNull}, hash: 99D5495053B817794C634DE0162E3A35
    @Test()
    void toPrimitive13WhenBIsNotNull() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         * (i < array.length) : true
         * (b == null) : false
         */
        //Arrange Statement(s)
        Long[] longArray = new Long[] { 1L };
        //Act Statement(s)
        long[] result = ArrayUtils.toPrimitive(longArray, 0L);
        long[] longResultArray = new long[] { 1L };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(longResultArray)));
    }

    //BaseRock generated method id: ${toPrimitive14WhenArrayIsNull}, hash: 8C2682AEEBBADA4AAA0D219051CC26CD
    @Test()
    void toPrimitive14WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Object object = null;
        //Act Statement(s)
        Object result = ArrayUtils.toPrimitive(object);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toPrimitive14WhenBooleanTYPEEqualsPt}, hash: E2AB411DD3886600FD663AB2433D1C3B
    @Test()
    void toPrimitive14WhenBooleanTYPEEqualsPt() {
        /* Branches:
         * (array == null) : false
         * (Boolean.TYPE.equals(pt)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            boolean[] booleanArray = new boolean[] {};
            Boolean[] booleanArray2 = new Boolean[] {};
            arrayUtils.when(() -> ArrayUtils.toPrimitive(booleanArray2)).thenReturn(booleanArray);
            //Act Statement(s)
            Object result = ArrayUtils.toPrimitive((Object) booleanArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(booleanArray));
                arrayUtils.verify(() -> ArrayUtils.toPrimitive(booleanArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toPrimitive14WhenCharacterTYPEEqualsPt}, hash: A7D893705B7936606EAECB02C6807489
    @Test()
    void toPrimitive14WhenCharacterTYPEEqualsPt() {
        /* Branches:
         * (array == null) : false
         * (Boolean.TYPE.equals(pt)) : false
         * (Character.TYPE.equals(pt)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            char[] charArray = new char[] {};
            Character[] characterArray = new Character[] {};
            arrayUtils.when(() -> ArrayUtils.toPrimitive(characterArray)).thenReturn(charArray);
            //Act Statement(s)
            Object result = ArrayUtils.toPrimitive((Object) characterArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(charArray));
                arrayUtils.verify(() -> ArrayUtils.toPrimitive(characterArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toPrimitive14WhenByteTYPEEqualsPt}, hash: ED51EB3E145AD8ACFA464FD709849824
    @Test()
    void toPrimitive14WhenByteTYPEEqualsPt() {
        /* Branches:
         * (array == null) : false
         * (Boolean.TYPE.equals(pt)) : false
         * (Character.TYPE.equals(pt)) : false
         * (Byte.TYPE.equals(pt)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            byte[] byteArray = new byte[] {};
            Byte[] byteArray2 = new Byte[] {};
            arrayUtils.when(() -> ArrayUtils.toPrimitive(byteArray2)).thenReturn(byteArray);
            //Act Statement(s)
            Object result = ArrayUtils.toPrimitive((Object) byteArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(byteArray));
                arrayUtils.verify(() -> ArrayUtils.toPrimitive(byteArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toPrimitive14WhenIntegerTYPEEqualsPt}, hash: 6325BE8018F20D3327ECBD07BFC84584
    @Test()
    void toPrimitive14WhenIntegerTYPEEqualsPt() {
        /* Branches:
         * (array == null) : false
         * (Boolean.TYPE.equals(pt)) : false
         * (Character.TYPE.equals(pt)) : false
         * (Byte.TYPE.equals(pt)) : false
         * (Integer.TYPE.equals(pt)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            int[] intArray = new int[] {};
            Integer[] integerArray = new Integer[] {};
            arrayUtils.when(() -> ArrayUtils.toPrimitive(integerArray)).thenReturn(intArray);
            //Act Statement(s)
            Object result = ArrayUtils.toPrimitive((Object) integerArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(intArray));
                arrayUtils.verify(() -> ArrayUtils.toPrimitive(integerArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toPrimitive14WhenLongTYPEEqualsPt}, hash: 64B77B5437E4558FF15E45A385F8C994
    @Test()
    void toPrimitive14WhenLongTYPEEqualsPt() {
        /* Branches:
         * (array == null) : false
         * (Boolean.TYPE.equals(pt)) : false
         * (Character.TYPE.equals(pt)) : false
         * (Byte.TYPE.equals(pt)) : false
         * (Integer.TYPE.equals(pt)) : false
         * (Long.TYPE.equals(pt)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            long[] longArray = new long[] {};
            Long[] longArray2 = new Long[] {};
            arrayUtils.when(() -> ArrayUtils.toPrimitive(longArray2)).thenReturn(longArray);
            //Act Statement(s)
            Object result = ArrayUtils.toPrimitive((Object) longArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(longArray));
                arrayUtils.verify(() -> ArrayUtils.toPrimitive(longArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toPrimitive14WhenShortTYPEEqualsPt}, hash: 08D962C2E4C83E207B0CF21774FCA6C5
    @Test()
    void toPrimitive14WhenShortTYPEEqualsPt() {
        /* Branches:
         * (array == null) : false
         * (Boolean.TYPE.equals(pt)) : false
         * (Character.TYPE.equals(pt)) : false
         * (Byte.TYPE.equals(pt)) : false
         * (Integer.TYPE.equals(pt)) : false
         * (Long.TYPE.equals(pt)) : false
         * (Short.TYPE.equals(pt)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Short[] shortArray = new Short[] {};
        //Act Statement(s)
        Object result = ArrayUtils.toPrimitive((Object) shortArray);
        //Assert statement(s)
        //TODO: Please implement equals method in Object for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${toPrimitive14WhenDoubleTYPEEqualsPt}, hash: 7FB0CECFECC26A414ED78D9C1AD948D3
    @Test()
    void toPrimitive14WhenDoubleTYPEEqualsPt() {
        /* Branches:
         * (array == null) : false
         * (Boolean.TYPE.equals(pt)) : false
         * (Character.TYPE.equals(pt)) : false
         * (Byte.TYPE.equals(pt)) : false
         * (Integer.TYPE.equals(pt)) : false
         * (Long.TYPE.equals(pt)) : false
         * (Short.TYPE.equals(pt)) : false
         * (Double.TYPE.equals(pt)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            double[] doubleArray = new double[] {};
            Double[] doubleArray2 = new Double[] {};
            arrayUtils.when(() -> ArrayUtils.toPrimitive(doubleArray2)).thenReturn(doubleArray);
            //Act Statement(s)
            Object result = ArrayUtils.toPrimitive((Object) doubleArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(doubleArray));
                arrayUtils.verify(() -> ArrayUtils.toPrimitive(doubleArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toPrimitive14WhenFloatTYPEEqualsPt}, hash: 4A945FB7DC878FF1DF22ED8FA58AD087
    @Test()
    void toPrimitive14WhenFloatTYPEEqualsPt() {
        /* Branches:
         * (array == null) : false
         * (Boolean.TYPE.equals(pt)) : false
         * (Character.TYPE.equals(pt)) : false
         * (Byte.TYPE.equals(pt)) : false
         * (Integer.TYPE.equals(pt)) : false
         * (Long.TYPE.equals(pt)) : false
         * (Short.TYPE.equals(pt)) : false
         * (Double.TYPE.equals(pt)) : false
         * (Float.TYPE.equals(pt)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            float[] floatArray = new float[] {};
            Float[] floatArray2 = new Float[] {};
            arrayUtils.when(() -> ArrayUtils.toPrimitive(floatArray2)).thenReturn(floatArray);
            //Act Statement(s)
            Object result = ArrayUtils.toPrimitive((Object) floatArray2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(floatArray));
                arrayUtils.verify(() -> ArrayUtils.toPrimitive(floatArray2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toPrimitive14WhenFloatTYPENotEqualsPt}, hash: DD798A948DBE0CA70B7D8DE7A74E8F63
    @Test()
    void toPrimitive14WhenFloatTYPENotEqualsPt() {
        /* Branches:
         * (array == null) : false
         * (Boolean.TYPE.equals(pt)) : false
         * (Character.TYPE.equals(pt)) : false
         * (Byte.TYPE.equals(pt)) : false
         * (Integer.TYPE.equals(pt)) : false
         * (Long.TYPE.equals(pt)) : false
         * (Short.TYPE.equals(pt)) : false
         * (Double.TYPE.equals(pt)) : false
         * (Float.TYPE.equals(pt)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object object = new Object();
        //Act Statement(s)
        Object result = ArrayUtils.toPrimitive(object);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(object)));
    }

    //BaseRock generated method id: ${toPrimitive15WhenArrayIsNull}, hash: 987FC7CE7FF5C47CB8BDE68C0D1D9193
    @Test()
    void toPrimitive15WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Short[] _short = null;
        //Act Statement(s)
        short[] result = ArrayUtils.toPrimitive(_short);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toPrimitive15WhenArrayLengthEquals0}, hash: 472E9A1DE4EBD708229F23E55C566887
    @Test()
    void toPrimitive15WhenArrayLengthEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : true
         */
        //Arrange Statement(s)
        Short[] shortArray = new Short[] {};
        //Act Statement(s)
        short[] result = ArrayUtils.toPrimitive(shortArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toPrimitive15WhenILessThanArrayLength}, hash: 6D72779B393EB598769886EABFF224BE
    @Test()
    void toPrimitive15WhenILessThanArrayLength() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         * (i < array.length) : true
         */
        //Arrange Statement(s)
        Short[] shortArray = new Short[] { (short) 1 };
        //Act Statement(s)
        short[] result = ArrayUtils.toPrimitive(shortArray);
        short[] shortResultArray = new short[] { (short) 1 };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(shortResultArray)));
    }

    //BaseRock generated method id: ${toPrimitive16WhenArrayIsNull}, hash: 536074BB606EAA110E474E3E617A5005
    @Test()
    void toPrimitive16WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Short[] _short = null;
        //Act Statement(s)
        short[] result = ArrayUtils.toPrimitive(_short, (short) 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toPrimitive16WhenArrayLengthEquals0}, hash: 2B7A8192B0D11C05051EF7CABC55A40A
    @Test()
    void toPrimitive16WhenArrayLengthEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : true
         */
        //Arrange Statement(s)
        Short[] shortArray = new Short[] {};
        //Act Statement(s)
        short[] result = ArrayUtils.toPrimitive(shortArray, (short) 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toPrimitive16WhenBIsNull}, hash: 030C00A10D0218D6704C3C3859AC27FA
    @Test()
    void toPrimitive16WhenBIsNull() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         * (i < array.length) : true
         * (b == null) : true
         */
        //Arrange Statement(s)
        Short[] shortArray = new Short[] { (Short) null };
        //Act Statement(s)
        short[] result = ArrayUtils.toPrimitive(shortArray, (short) 0);
        short[] shortResultArray = new short[] { (short) 0 };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(shortResultArray)));
    }

    //BaseRock generated method id: ${toPrimitive16WhenBIsNotNull}, hash: AD7023147365C75DC0AD9995A8AB8127
    @Test()
    void toPrimitive16WhenBIsNotNull() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         * (i < array.length) : true
         * (b == null) : false
         */
        //Arrange Statement(s)
        Short[] shortArray = new Short[] { (short) 1 };
        //Act Statement(s)
        short[] result = ArrayUtils.toPrimitive(shortArray, (short) 0);
        short[] shortResultArray = new short[] { (short) 1 };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(shortResultArray)));
    }

    //BaseRock generated method id: ${toStringTest}, hash: EF15AB49A77CCC6DE3A6E3487046C728
    @Test()
    void toStringTest() {
        //Arrange Statement(s)
        try (MockedStatic<ArrayUtils> arrayUtils = mockStatic(ArrayUtils.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            arrayUtils.when(() -> ArrayUtils.toString(object, "{}")).thenReturn("return_of_toString1");
            //Act Statement(s)
            String result = ArrayUtils.toString(object);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_toString1"));
                arrayUtils.verify(() -> ArrayUtils.toString(object, "{}"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toString1WhenArrayIsNull}, hash: DA037CA089C6E6B5182A87F8F70C07AE
    @Test()
    void toString1WhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Object object = null;
        //Act Statement(s)
        String result = ArrayUtils.toString(object, "stringIfNull1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("stringIfNull1")));
    }

    //BaseRock generated method id: ${toString1WhenArrayIsNotNull}, hash: 6238AC521BCFE44C2E05C939801D9B68
    @Disabled()
    @Test()
    void toString1WhenArrayIsNotNull() {
        /* Branches:
         * (array == null) : false
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: object of type ToStringBuilder - Method: append
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Object object = new Object();
        //Act Statement(s)
        String result = ArrayUtils.toString(object, "stringIfNull1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${toStringArrayWhenArrayIsNull}, hash: 3647AE6227736477CEF4614E76EFA91E
    @Test()
    void toStringArrayWhenArrayIsNull() {
        /* Branches:
         * (array == null) : true
         */
        //Arrange Statement(s)
        Object[] object = null;
        //Act Statement(s)
        String[] result = ArrayUtils.toStringArray(object);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toStringArrayWhenArrayLengthEquals0}, hash: 61F8B47F350A3F2DE7EF2F0830644224
    @Test()
    void toStringArrayWhenArrayLengthEquals0() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : true
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        //Act Statement(s)
        String[] result = ArrayUtils.toStringArray(objectArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toStringArrayWhenILessThanArrayLength}, hash: F390EED22FBC5F64642FCB7A80FCA6B0
    @Test()
    void toStringArrayWhenILessThanArrayLength() {
        /* Branches:
         * (array == null) : false
         * (array.length == 0) : false
         * (i < array.length) : true
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] { objectMock };
        //Act Statement(s)
        String[] result = ArrayUtils.toStringArray(objectArray);
        String[] stringResultArray = new String[] { "object" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${toStringArray1WhenArrayIsNull}, hash: 4B8E226306DBE709BD8EBE7444A398BF
    @Test()
    void toStringArray1WhenArrayIsNull() {
        /* Branches:
         * (null == array) : true
         */
        //Act Statement(s)
        String[] result = ArrayUtils.toStringArray(null, "valueForNullElements1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${toStringArray1WhenArrayLengthEquals0}, hash: A149A84523252F418EAC1AC60302D1A0
    @Test()
    void toStringArray1WhenArrayLengthEquals0() {
        /* Branches:
         * (null == array) : false
         * (array.length == 0) : true
         */
        //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        //Act Statement(s)
        String[] result = ArrayUtils.toStringArray(objectArray, "valueForNullElements1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toStringArray1WhenILessThanArrayLength}, hash: 09F3BF7AEFB10120F3DCDBA716832FDC
    @Disabled()
    @Test()
    void toStringArray1WhenILessThanArrayLength() {
        /* Branches:
         * (null == array) : false
         * (array.length == 0) : false
         * (i < array.length) : true
         */
        //Arrange Statement(s)
        Object object = new Object();
        Object[] objectArray = new Object[] { object };
        //Act Statement(s)
        String[] result = ArrayUtils.toStringArray(objectArray, "valueForNullElements1");
        String[] stringResultArray = new String[] { "java.lang.Object@3ea49986" };
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }
}

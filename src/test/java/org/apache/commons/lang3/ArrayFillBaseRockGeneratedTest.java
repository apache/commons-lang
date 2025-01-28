package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertAll;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class ArrayFillBaseRockGeneratedTest {

    //BaseRock generated method id: ${fillWhenAIsNotNull}, hash: BB5F2618BCC69075C6E15AB40A931219
    @Test()
    void fillWhenAIsNotNull() {
        /* Branches:
         * (a != null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        
        //Act Statement(s)
        byte[] result = ArrayFill.fill(byteArray, (byte) 1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(byteArray)));
    }

    //BaseRock generated method id: ${fill1WhenAIsNotNull}, hash: 9F62698B87825CAF7BFB4C4B2EBCE338
    @Test()
    void fill1WhenAIsNotNull() {
        /* Branches:
         * (a != null) : true
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        
        //Act Statement(s)
        char[] result = ArrayFill.fill(charArray, 'A');
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(charArray)));
    }

    //BaseRock generated method id: ${fill2WhenAIsNotNull}, hash: FDCF4F4CE8B6F8B52BD3850D91EEB4CD
    @Test()
    void fill2WhenAIsNotNull() {
        /* Branches:
         * (a != null) : true
         */
         //Arrange Statement(s)
        double[] doubleArray = new double[] {};
        
        //Act Statement(s)
        double[] result = ArrayFill.fill(doubleArray, Double.parseDouble("1.0"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(doubleArray)));
    }

    //BaseRock generated method id: ${fill3WhenAIsNotNull}, hash: 25E98143C8A4D7B54B64E48E170383CD
    @Test()
    void fill3WhenAIsNotNull() {
        /* Branches:
         * (a != null) : true
         */
         //Arrange Statement(s)
        float[] floatArray = new float[] {};
        
        //Act Statement(s)
        float[] result = ArrayFill.fill(floatArray, Float.parseFloat("1.0"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(floatArray)));
    }

    //BaseRock generated method id: ${fill4WhenAIsNotNull}, hash: 9814ECCA9B79D9A2FAEFCE2369AB5F4D
    @Test()
    void fill4WhenAIsNotNull() {
        /* Branches:
         * (a != null) : true
         */
         //Arrange Statement(s)
        int[] intArray = new int[] {};
        
        //Act Statement(s)
        int[] result = ArrayFill.fill(intArray, 1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(intArray)));
    }

    //BaseRock generated method id: ${fill5WhenAIsNotNull}, hash: 23CB7F2A769BA673A21284F860D92393
    @Test()
    void fill5WhenAIsNotNull() {
        /* Branches:
         * (a != null) : true
         */
         //Arrange Statement(s)
        long[] longArray = new long[] {};
        
        //Act Statement(s)
        long[] result = ArrayFill.fill(longArray, 1L);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(longArray)));
    }

    //BaseRock generated method id: ${fill6WhenAIsNotNull}, hash: 9EC14D3D6F4980876562770CCFDD9EF6
    @Test()
    void fill6WhenAIsNotNull() {
        /* Branches:
         * (a != null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        short[] shortArray = new short[] {};
        
        //Act Statement(s)
        short[] result = ArrayFill.fill(shortArray, (short) 1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(shortArray)));
    }

    //BaseRock generated method id: ${fill7WhenAIsNotNull}, hash: F551E176D3F1400EF0D4E8B2279FCE9E
    @Test()
    void fill7WhenAIsNotNull() {
        /* Branches:
         * (a != null) : true
         */
         //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        Object object = new Object();
        
        //Act Statement(s)
        Object[] result = ArrayFill.fill(objectArray, object);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(objectArray)));
    }
}

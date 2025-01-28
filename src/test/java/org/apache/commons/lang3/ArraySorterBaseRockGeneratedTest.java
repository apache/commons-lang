package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.Comparator;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertAll;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class ArraySorterBaseRockGeneratedTest {

    //BaseRock generated method id: ${sortWhenArrayIsNotNull}, hash: AC17D10C766D18B9896E0A4184F0D743
    @Test()
    void sortWhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
         //Arrange Statement(s)
        byte[] byteArray = new byte[] {};
        
        //Act Statement(s)
        byte[] result = ArraySorter.sort(byteArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(byteArray)));
    }

    //BaseRock generated method id: ${sort1WhenArrayIsNotNull}, hash: D68FB67862EBDF2565C261B561777D9E
    @Test()
    void sort1WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        
        //Act Statement(s)
        char[] result = ArraySorter.sort(charArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(charArray)));
    }

    //BaseRock generated method id: ${sort2WhenArrayIsNotNull}, hash: EF93EC5D1384E7CD40095BAB63F82CD2
    @Test()
    void sort2WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
         //Arrange Statement(s)
        double[] doubleArray = new double[] {};
        
        //Act Statement(s)
        double[] result = ArraySorter.sort(doubleArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(doubleArray)));
    }

    //BaseRock generated method id: ${sort3WhenArrayIsNotNull}, hash: 49D26800627B8A8ABDC472ABBD246AE4
    @Test()
    void sort3WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
         //Arrange Statement(s)
        float[] floatArray = new float[] {};
        
        //Act Statement(s)
        float[] result = ArraySorter.sort(floatArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(floatArray)));
    }

    //BaseRock generated method id: ${sort4WhenArrayIsNotNull}, hash: 335F784DBFD399170C30444AF026DD4A
    @Test()
    void sort4WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
         //Arrange Statement(s)
        int[] intArray = new int[] {};
        
        //Act Statement(s)
        int[] result = ArraySorter.sort(intArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(intArray)));
    }

    //BaseRock generated method id: ${sort5WhenArrayIsNotNull}, hash: 524EE3601D5E71BD85C6CCF177AB1357
    @Test()
    void sort5WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
         //Arrange Statement(s)
        long[] longArray = new long[] {};
        
        //Act Statement(s)
        long[] result = ArraySorter.sort(longArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(longArray)));
    }

    //BaseRock generated method id: ${sort6WhenArrayIsNotNull}, hash: D57E90F6604412DB3FE790BC5C771372
    @Test()
    void sort6WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
         //Arrange Statement(s)
        short[] shortArray = new short[] {};
        
        //Act Statement(s)
        short[] result = ArraySorter.sort(shortArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(shortArray)));
    }

    //BaseRock generated method id: ${sort7WhenArrayIsNotNull}, hash: 3808B33903B5B0E13650A64FDE0CF56C
    @Test()
    void sort7WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
         //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        
        //Act Statement(s)
        Object[] result = ArraySorter.sort(objectArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(objectArray)));
    }

    //BaseRock generated method id: ${sort8WhenArrayIsNotNull}, hash: B9D9C0B99994499F821DCC3EBAE261D6
    @Test()
    void sort8WhenArrayIsNotNull() {
        /* Branches:
         * (array != null) : true
         */
         //Arrange Statement(s)
        Object[] objectArray = new Object[] {};
        Comparator comparator = Comparator.reverseOrder();
        
        //Act Statement(s)
        Object[] result = ArraySorter.sort(objectArray, comparator);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(objectArray)));
    }
}

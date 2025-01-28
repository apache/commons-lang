package org.apache.commons.lang3.builder;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.mock;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class DiffBuilderBaseRockGeneratedTest {

    //BaseRock generated method id: ${builderTest}, hash: 237681137752D422E13A78BA1E7F7C04
    @Test()
    void builderTest() {
        
        //Act Statement(s)
        DiffBuilder.Builder result = DiffBuilder.builder();
        
        //Assert statement(s)
        //TODO: Please implement equals method in Builder for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${appendWhenEquals}, hash: 54F1779C00EC8A2027D3A796AEE3D564
    @Test()
    void appendWhenEquals() {
        /* Branches:
         * (equals) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffBuilder target = new DiffBuilder(object, object, standardToStringStyle, true);
        
        //Act Statement(s)
        DiffBuilder result = target.append("fieldName1", false, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append1WhenEquals}, hash: A3B02D0E0E62B10A3344BD1A77A72345
    @Test()
    void append1WhenEquals() {
        /* Branches:
         * (equals) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffBuilder target = new DiffBuilder(object, object, standardToStringStyle, true);
        boolean[] booleanArray = new boolean[] {};
        boolean[] booleanArray2 = new boolean[] {};
        
        //Act Statement(s)
        DiffBuilder result = target.append("fieldName1", booleanArray, booleanArray2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append2WhenEquals}, hash: B29EFF51AEBA4AF969899CBF4A748A55
    @Test()
    void append2WhenEquals() {
        /* Branches:
         * (equals) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffBuilder target = new DiffBuilder(object, object, standardToStringStyle, true);
        
        //Act Statement(s)
        DiffBuilder result = target.append("fieldName1", (byte) 0, (byte) 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append3WhenEquals}, hash: 65C969A804BBC0DE9ACA9BDFA7C4B943
    @Test()
    void append3WhenEquals() {
        /* Branches:
         * (equals) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffBuilder target = new DiffBuilder(object, object, standardToStringStyle, true);
        byte[] byteArray = new byte[] {};
        byte[] byteArray2 = new byte[] {};
        
        //Act Statement(s)
        DiffBuilder result = target.append("fieldName1", byteArray, byteArray2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append4WhenEquals}, hash: 61E96C64C307D0BC065075A22DA5C8CD
    @Test()
    void append4WhenEquals() {
        /* Branches:
         * (equals) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffBuilder target = new DiffBuilder(object, object, standardToStringStyle, true);
        
        //Act Statement(s)
        DiffBuilder result = target.append("fieldName1", 'A', 'A');
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append5WhenEquals}, hash: E2F27D566457A905F7762BD6E37174AA
    @Test()
    void append5WhenEquals() {
        /* Branches:
         * (equals) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffBuilder target = new DiffBuilder(object, object, standardToStringStyle, true);
        char[] charArray = new char[] {};
        char[] charArray2 = new char[] {};
        
        //Act Statement(s)
        DiffBuilder result = target.append("fieldName1", charArray, charArray2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append6WhenEquals}, hash: BDC4958C8BBD859F7F42247E2D1116B3
    @Test()
    void append6WhenEquals() {
        /* Branches:
         * (equals) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffBuilder target = new DiffBuilder(object, object, standardToStringStyle, true);
        DiffResult<?> diffResultMock = mock(DiffResult.class);
        
        //Act Statement(s)
        DiffBuilder result = target.append("fieldName1", diffResultMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append7WhenEquals}, hash: E497B8DE9C37ECDC7CE33A8E429760EA
    @Test()
    void append7WhenEquals() {
        /* Branches:
         * (equals) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffBuilder target = new DiffBuilder(object, object, standardToStringStyle, true);
        
        //Act Statement(s)
        DiffBuilder result = target.append("fieldName1", Double.parseDouble("0.0"), Double.parseDouble("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append8WhenEquals}, hash: B252302E4869F30500F9F42E43660D67
    @Test()
    void append8WhenEquals() {
        /* Branches:
         * (equals) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffBuilder target = new DiffBuilder(object, object, standardToStringStyle, true);
        double[] doubleArray = new double[] {};
        double[] doubleArray2 = new double[] {};
        
        //Act Statement(s)
        DiffBuilder result = target.append("fieldName1", doubleArray, doubleArray2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append9WhenEquals}, hash: 1F43EE1551ED39403FB24C4447A383CA
    @Test()
    void append9WhenEquals() {
        /* Branches:
         * (equals) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffBuilder target = new DiffBuilder(object, object, standardToStringStyle, true);
        
        //Act Statement(s)
        DiffBuilder result = target.append("fieldName1", Float.parseFloat("0.0"), Float.parseFloat("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append10WhenEquals}, hash: E13CB78B937A827B30C7F321A4D10C9F
    @Test()
    void append10WhenEquals() {
        /* Branches:
         * (equals) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffBuilder target = new DiffBuilder(object, object, standardToStringStyle, true);
        float[] floatArray = new float[] {};
        float[] floatArray2 = new float[] {};
        
        //Act Statement(s)
        DiffBuilder result = target.append("fieldName1", floatArray, floatArray2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append11WhenEquals}, hash: 63A420D34F95A51F80C6C5ECDC2431E1
    @Test()
    void append11WhenEquals() {
        /* Branches:
         * (equals) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffBuilder target = new DiffBuilder(object, object, standardToStringStyle, true);
        
        //Act Statement(s)
        DiffBuilder result = target.append("fieldName1", 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append12WhenEquals}, hash: B50CFC4A1FB4796E69B72AF2BAF54D69
    @Test()
    void append12WhenEquals() {
        /* Branches:
         * (equals) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffBuilder target = new DiffBuilder(object, object, standardToStringStyle, true);
        int[] intArray = new int[] {};
        int[] intArray2 = new int[] {};
        
        //Act Statement(s)
        DiffBuilder result = target.append("fieldName1", intArray, intArray2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append13WhenEquals}, hash: F198A675165CC6669A0494B7B8764D3D
    @Test()
    void append13WhenEquals() {
        /* Branches:
         * (equals) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffBuilder target = new DiffBuilder(object, object, standardToStringStyle, true);
        
        //Act Statement(s)
        DiffBuilder result = target.append("fieldName1", 0L, 0L);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append14WhenEquals}, hash: 418EE3BF92E92C63AE5033C64CDD4911
    @Test()
    void append14WhenEquals() {
        /* Branches:
         * (equals) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffBuilder target = new DiffBuilder(object, object, standardToStringStyle, true);
        long[] longArray = new long[] {};
        long[] longArray2 = new long[] {};
        
        //Act Statement(s)
        DiffBuilder result = target.append("fieldName1", longArray, longArray2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append15WhenEquals}, hash: F58F9B4CBEF9929951B0B1B16504CAFA
    @Test()
    void append15WhenEquals() {
        /* Branches:
         * (equals) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffBuilder target = new DiffBuilder(object, object, standardToStringStyle, true);
        Object object2 = new Object();
        Object object3 = new Object();
        
        //Act Statement(s)
        DiffBuilder result = target.append("fieldName1", object2, object3);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append16WhenEquals}, hash: 9545EB6E07D0AC985E690C013D16D88A
    @Test()
    void append16WhenEquals() {
        /* Branches:
         * (equals) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffBuilder target = new DiffBuilder(object, object, standardToStringStyle, true);
        Object[] objectArray = new Object[] {};
        Object[] objectArray2 = new Object[] {};
        
        //Act Statement(s)
        DiffBuilder result = target.append("fieldName1", objectArray, objectArray2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append17WhenEquals}, hash: 197A1CC32D06D055FB8389A903ECD4D8
    @Test()
    void append17WhenEquals() {
        /* Branches:
         * (equals) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffBuilder target = new DiffBuilder(object, object, standardToStringStyle, true);
        
        //Act Statement(s)
        DiffBuilder result = target.append("fieldName1", (short) 0, (short) 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append18WhenEquals}, hash: 74AE88521EC899FCB7309A8FFB54ACE0
    @Test()
    void append18WhenEquals() {
        /* Branches:
         * (equals) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffBuilder target = new DiffBuilder(object, object, standardToStringStyle, true);
        short[] shortArray = new short[] {};
        short[] shortArray2 = new short[] {};
        
        //Act Statement(s)
        DiffBuilder result = target.append("fieldName1", shortArray, shortArray2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${buildTest}, hash: 60B8914C6031D1D0C60BBBFAF8901653
    @Test()
    void buildTest() {
        //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffBuilder target = new DiffBuilder(object, object, standardToStringStyle, true);
        
        //Act Statement(s)
        DiffResult result = target.build();
        
        //Assert statement(s)
        //TODO: Please implement equals method in DiffResult for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getLeftTest}, hash: F975444338A5A173133EDB587544F156
    @Test()
    void getLeftTest() {
        //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffBuilder target = new DiffBuilder(object, object, standardToStringStyle, true);
        
        //Act Statement(s)
        Object result = target.getLeft();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(object)));
    }

    //BaseRock generated method id: ${getRightTest}, hash: 73B7FAD0295F63CFA01835EA8ECBA22D
    @Test()
    void getRightTest() {
        //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffBuilder target = new DiffBuilder(object, object, standardToStringStyle, true);
        
        //Act Statement(s)
        Object result = target.getRight();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(object)));
    }
}

package org.apache.commons.lang3.mutable;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.hamcrest.Matchers.closeTo;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class MutableByteBaseRockGeneratedTest {

    //BaseRock generated method id: ${addTest}, hash: 5934B94B699C13B0ECB050353A8AB691
    @Test()
    void addTest() {
        //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 0);
        
        //Act Statement(s)
        target.add((byte) 0);
    }

    //BaseRock generated method id: ${add1Test}, hash: ADBF43F1A4907DCF9F5568F873A3832B
    @Test()
    void add1Test() {
        //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 0);
        
        //Act Statement(s)
        target.add(0);
    }

    //BaseRock generated method id: ${addAndGetTest}, hash: 8338AE005BEF6B306F827327F86E7FBF
    @Test()
    void addAndGetTest() {
        //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 0);
        
        //Act Statement(s)
        byte result = target.addAndGet((byte) 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((byte) 0)));
    }

    //BaseRock generated method id: ${addAndGet1Test}, hash: EB71344ED185E93DAAB405EF05FF9A3E
    @Test()
    void addAndGet1Test() {
        //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 0);
        
        //Act Statement(s)
        byte result = target.addAndGet(0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((byte) 0)));
    }

    //BaseRock generated method id: ${byteValueTest}, hash: 71306DE4A5D1482B6EDDD61F03CE0E3E
    @Test()
    void byteValueTest() {
        //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 0);
        
        //Act Statement(s)
        byte result = target.byteValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((byte) 0)));
    }

    //BaseRock generated method id: ${compareToTest}, hash: CBBB9505294D74A832726556C381D41D
    @Test()
    void compareToTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 1);
        MutableByte mutableByte = new MutableByte((byte) 1);
        
        //Act Statement(s)
        int result = target.compareTo(mutableByte);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${decrementTest}, hash: 6A09187FA042763041901F1E9DEB0892
    @Test()
    void decrementTest() {
        //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 2);
        
        //Act Statement(s)
        target.decrement();
    }

    //BaseRock generated method id: ${decrementAndGetTest}, hash: E0DEC7CB60164DD95094FA7032CB2207
    @Test()
    void decrementAndGetTest() {
        //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 2);
        
        //Act Statement(s)
        byte result = target.decrementAndGet();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((byte) 1)));
    }

    //BaseRock generated method id: ${doubleValueTest}, hash: 2EB897F14EED33C931F7A3CC5DECB191
    @Test()
    void doubleValueTest() {
        //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 0);
        
        //Act Statement(s)
        double result = target.doubleValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001)));
    }

    //BaseRock generated method id: ${equalsWhenObjNotInstanceOfMutableByte}, hash: D0090E8F8DFDCC7DB81FD8C9CC96EC19
    @Test()
    void equalsWhenObjNotInstanceOfMutableByte() {
        /* Branches:
         * (obj instanceof MutableByte) : false
         */
         //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 0);
        Object object = new Object();
        
        //Act Statement(s)
        boolean result = target.equals(object);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equalsWhenValueEqualsObjByteValue}, hash: 54AF4089FBB8DC312E8C9885B6813901
    @Test()
    void equalsWhenValueEqualsObjByteValue() {
        /* Branches:
         * (obj instanceof MutableByte) : true
         * (value == ((MutableByte) obj).byteValue()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 1);
        MutableByte mutableByte = new MutableByte();
        
        //Act Statement(s)
        boolean result = target.equals(mutableByte);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsWhenValueNotEqualsObjByteValue}, hash: 61E3DA3ACA6AEC786EEEA6FD13BB0891
    @Test()
    void equalsWhenValueNotEqualsObjByteValue() {
        /* Branches:
         * (obj instanceof MutableByte) : true
         * (value == ((MutableByte) obj).byteValue()) : false
         */
         //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 1);
        MutableByte mutableByte = new MutableByte();
        
        //Act Statement(s)
        boolean result = target.equals(mutableByte);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${floatValueTest}, hash: 9A782C527DF590BB9674B78F2FE4ED95
    @Test()
    void floatValueTest() {
        //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 0);
        
        //Act Statement(s)
        float result = target.floatValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Float.parseFloat("0.0"))));
    }

    //BaseRock generated method id: ${getAndAddTest}, hash: C4E4FC8AC804A039FDAA56A33A1845ED
    @Test()
    void getAndAddTest() {
        //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 0);
        
        //Act Statement(s)
        byte result = target.getAndAdd((byte) 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((byte) 0)));
    }

    //BaseRock generated method id: ${getAndAdd1Test}, hash: 24916E099B76003834ABE423D00CF1EB
    @Test()
    void getAndAdd1Test() {
        //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 0);
        
        //Act Statement(s)
        byte result = target.getAndAdd(0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((byte) 0)));
    }

    //BaseRock generated method id: ${getAndDecrementTest}, hash: 70C88F3F31B47EF70E6D653D4AB5B4DC
    @Test()
    void getAndDecrementTest() {
        //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 2);
        
        //Act Statement(s)
        byte result = target.getAndDecrement();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((byte) 2)));
    }

    //BaseRock generated method id: ${getAndIncrementTest}, hash: 37196F8361F8C8B0704F85DCB2B3E4B7
    @Test()
    void getAndIncrementTest() {
        //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 1);
        
        //Act Statement(s)
        byte result = target.getAndIncrement();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((byte) 1)));
    }

    //BaseRock generated method id: ${getValueTest}, hash: 8C0E39343FA7ADECD0E2D43F2994E5AA
    @Test()
    void getValueTest() {
        //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 1);
        
        //Act Statement(s)
        Byte result = target.getValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((byte) 1)));
    }

    //BaseRock generated method id: ${incrementTest}, hash: 21E469CC4EAA4920E67AA533C7AE7250
    @Test()
    void incrementTest() {
        //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 1);
        
        //Act Statement(s)
        target.increment();
    }

    //BaseRock generated method id: ${incrementAndGetTest}, hash: FEF932FC90E16E874FCC95AABEB05580
    @Test()
    void incrementAndGetTest() {
        //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 1);
        
        //Act Statement(s)
        byte result = target.incrementAndGet();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((byte) 2)));
    }

    //BaseRock generated method id: ${intValueTest}, hash: 3E66441D9AE4CB0659DDD3942929F2FD
    @Test()
    void intValueTest() {
        //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 0);
        
        //Act Statement(s)
        int result = target.intValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat((byte) result, equalTo((byte) 0)));
    }

    //BaseRock generated method id: ${longValueTest}, hash: 90FF17C7D050F261277338C191E8E5E7
    @Test()
    void longValueTest() {
        //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 1);
        
        //Act Statement(s)
        long result = target.longValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1L)));
    }

    //BaseRock generated method id: ${setValueTest}, hash: 87D144E8F67257783181A663A2D90F72
    @Test()
    void setValueTest() {
        //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 0);
        
        //Act Statement(s)
        target.setValue((byte) 0);
    }

    //BaseRock generated method id: ${setValue1Test}, hash: 10128D5EEF3D061A64433B502609123E
    @Test()
    void setValue1Test() {
        //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 0);
        
        //Act Statement(s)
        target.setValue(1);
    }

    //BaseRock generated method id: ${subtractTest}, hash: 5C30CC83F3A97D1BD05C192DAE2F0B95
    @Test()
    void subtractTest() {
        //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 0);
        
        //Act Statement(s)
        target.subtract((byte) 0);
    }

    //BaseRock generated method id: ${subtract1Test}, hash: BAECD48C27E889DA5F5176F1622C366E
    @Test()
    void subtract1Test() {
        //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 0);
        
        //Act Statement(s)
        target.subtract(0);
    }

    //BaseRock generated method id: ${toByteTest}, hash: F2D9193F745598665963B18E618DE99D
    @Test()
    void toByteTest() {
        //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 1);
        
        //Act Statement(s)
        Byte result = target.toByte();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((byte) 1)));
    }

    //BaseRock generated method id: ${toStringTest}, hash: BFBB0DC6F2376E26BD5E0310B3161D8B
    @Test()
    void toStringTest() {
        //Arrange Statement(s)
        MutableByte target = new MutableByte((byte) 2);
        
        //Act Statement(s)
        String result = target.toString();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("2")));
    }
}

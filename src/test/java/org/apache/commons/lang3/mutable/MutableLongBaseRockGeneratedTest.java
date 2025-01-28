package org.apache.commons.lang3.mutable;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.hamcrest.Matchers.closeTo;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class MutableLongBaseRockGeneratedTest {

    //BaseRock generated method id: ${addTest}, hash: A9A53C8B9F3E3FD185D1BD47A55D1B37
    @Test()
    void addTest() {
        //Arrange Statement(s)
        MutableLong target = new MutableLong(1L);
        
        //Act Statement(s)
        target.add(1L);
    }

    //BaseRock generated method id: ${add1Test}, hash: 9D8941CB5F17C01EF22B5D3B3774891E
    @Test()
    void add1Test() {
        //Arrange Statement(s)
        MutableLong target = new MutableLong(1L);
        
        //Act Statement(s)
        target.add(1);
    }

    //BaseRock generated method id: ${addAndGetTest}, hash: E3F81C7E3073CC723020E72036DD0BF1
    @Test()
    void addAndGetTest() {
        //Arrange Statement(s)
        MutableLong target = new MutableLong(1L);
        
        //Act Statement(s)
        long result = target.addAndGet(1L);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(2L)));
    }

    //BaseRock generated method id: ${addAndGet1Test}, hash: 415A4104D1ECEC39754D1537B3EAFFCA
    @Test()
    void addAndGet1Test() {
        //Arrange Statement(s)
        MutableLong target = new MutableLong(1L);
        
        //Act Statement(s)
        long result = target.addAndGet(1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(2L)));
    }

    //BaseRock generated method id: ${compareToTest}, hash: BE88364D3E11CAC5A06EB6BE5B012183
    @Test()
    void compareToTest() {
        //Arrange Statement(s)
        MutableLong target = new MutableLong(1L);
        MutableLong mutableLong = new MutableLong(1L);
        
        //Act Statement(s)
        int result = target.compareTo(mutableLong);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${decrementTest}, hash: 73C046112FBB764BDA6A0E65DBE45DEA
    @Test()
    void decrementTest() {
        //Arrange Statement(s)
        MutableLong target = new MutableLong(1L);
        
        //Act Statement(s)
        target.decrement();
    }

    //BaseRock generated method id: ${decrementAndGetTest}, hash: 067FDF3847021C18F0D982B09888F08D
    @Test()
    void decrementAndGetTest() {
        //Arrange Statement(s)
        MutableLong target = new MutableLong(1L);
        
        //Act Statement(s)
        long result = target.decrementAndGet();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${doubleValueTest}, hash: A4870F5C00DB7946AADB189F7D3CEAD3
    @Test()
    void doubleValueTest() {
        //Arrange Statement(s)
        MutableLong target = new MutableLong(0L);
        
        //Act Statement(s)
        double result = target.doubleValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001)));
    }

    //BaseRock generated method id: ${equalsWhenObjNotInstanceOfMutableLong}, hash: 61FB34FF35B9F85F81ADCB050FF1C23B
    @Test()
    void equalsWhenObjNotInstanceOfMutableLong() {
        /* Branches:
         * (obj instanceof MutableLong) : false
         */
         //Arrange Statement(s)
        MutableLong target = new MutableLong(0L);
        Object object = new Object();
        
        //Act Statement(s)
        boolean result = target.equals(object);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equalsWhenValueEqualsObjLongValue}, hash: 6B55D29AF1965EE40DE9CF6F323F94CD
    @Test()
    void equalsWhenValueEqualsObjLongValue() {
        /* Branches:
         * (obj instanceof MutableLong) : true
         * (value == ((MutableLong) obj).longValue()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        MutableLong target = new MutableLong(1L);
        MutableLong mutableLong = new MutableLong();
        
        //Act Statement(s)
        boolean result = target.equals(mutableLong);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsWhenValueNotEqualsObjLongValue}, hash: 28B8227E396513165DA5A8BAF07F598E
    @Test()
    void equalsWhenValueNotEqualsObjLongValue() {
        /* Branches:
         * (obj instanceof MutableLong) : true
         * (value == ((MutableLong) obj).longValue()) : false
         */
         //Arrange Statement(s)
        MutableLong target = new MutableLong(1L);
        MutableLong mutableLong = new MutableLong();
        
        //Act Statement(s)
        boolean result = target.equals(mutableLong);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${floatValueTest}, hash: EA338C392400FA0970CE15DF820623F2
    @Test()
    void floatValueTest() {
        //Arrange Statement(s)
        MutableLong target = new MutableLong(0L);
        
        //Act Statement(s)
        float result = target.floatValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Float.parseFloat("0.0"))));
    }

    //BaseRock generated method id: ${getAndAddTest}, hash: D08DBDD205F25E146442CBC4B341DE98
    @Test()
    void getAndAddTest() {
        //Arrange Statement(s)
        MutableLong target = new MutableLong(1L);
        
        //Act Statement(s)
        long result = target.getAndAdd(1L);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1L)));
    }

    //BaseRock generated method id: ${getAndAdd1Test}, hash: AF39F52B278029349721686A1C194371
    @Test()
    void getAndAdd1Test() {
        //Arrange Statement(s)
        MutableLong target = new MutableLong(1L);
        
        //Act Statement(s)
        long result = target.getAndAdd(1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1L)));
    }

    //BaseRock generated method id: ${getAndDecrementTest}, hash: 09747BF0C63A6E168CF5EBFAF7C7E713
    @Test()
    void getAndDecrementTest() {
        //Arrange Statement(s)
        MutableLong target = new MutableLong(1L);
        
        //Act Statement(s)
        long result = target.getAndDecrement();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1L)));
    }

    //BaseRock generated method id: ${getAndIncrementTest}, hash: 93F87A4DCC7F93162019BCCE598003E3
    @Test()
    void getAndIncrementTest() {
        //Arrange Statement(s)
        MutableLong target = new MutableLong(1L);
        
        //Act Statement(s)
        long result = target.getAndIncrement();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1L)));
    }

    //BaseRock generated method id: ${getValueTest}, hash: 76525A8B7F6A0E7900683926097F08C0
    @Test()
    void getValueTest() {
        //Arrange Statement(s)
        MutableLong target = new MutableLong(1L);
        
        //Act Statement(s)
        Long result = target.getValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1L)));
    }

    //BaseRock generated method id: ${incrementTest}, hash: 7F091EC202855D51516DD79FB9EA296B
    @Test()
    void incrementTest() {
        //Arrange Statement(s)
        MutableLong target = new MutableLong(1L);
        
        //Act Statement(s)
        target.increment();
    }

    //BaseRock generated method id: ${incrementAndGetTest}, hash: 0FCD79A43EBD672E354E6A07B544EF01
    @Test()
    void incrementAndGetTest() {
        //Arrange Statement(s)
        MutableLong target = new MutableLong(1L);
        
        //Act Statement(s)
        long result = target.incrementAndGet();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(2L)));
    }

    //BaseRock generated method id: ${intValueTest}, hash: D69AFC7C8AEDA355DD4A3B54643A1786
    @Test()
    void intValueTest() {
        //Arrange Statement(s)
        MutableLong target = new MutableLong(1L);
        
        //Act Statement(s)
        int result = target.intValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${longValueTest}, hash: 4CF58FA424941C095FCB14EBAAA7A1B2
    @Test()
    void longValueTest() {
        //Arrange Statement(s)
        MutableLong target = new MutableLong(0L);
        
        //Act Statement(s)
        long result = target.longValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${setValueTest}, hash: C941017713E7EC983197AF9511F59A17
    @Test()
    void setValueTest() {
        //Arrange Statement(s)
        MutableLong target = new MutableLong(0L);
        
        //Act Statement(s)
        target.setValue(0L);
    }

    //BaseRock generated method id: ${setValue1Test}, hash: 115E3A3A68DABC5FFA71B4357B9D99D9
    @Test()
    void setValue1Test() {
        //Arrange Statement(s)
        MutableLong target = new MutableLong(0L);
        
        //Act Statement(s)
        target.setValue(1);
    }

    //BaseRock generated method id: ${subtractTest}, hash: 9258A35BB8DD0A241928E1AE3C957AA0
    @Test()
    void subtractTest() {
        //Arrange Statement(s)
        MutableLong target = new MutableLong(1L);
        
        //Act Statement(s)
        target.subtract(1L);
    }

    //BaseRock generated method id: ${subtract1Test}, hash: 96C4E59BBAFCBA362F988E880CF921D8
    @Test()
    void subtract1Test() {
        //Arrange Statement(s)
        MutableLong target = new MutableLong(1L);
        
        //Act Statement(s)
        target.subtract(1);
    }

    //BaseRock generated method id: ${toLongTest}, hash: 4C90F3826E98CF641DB98B37F939ABF5
    @Test()
    void toLongTest() {
        //Arrange Statement(s)
        MutableLong target = new MutableLong(1L);
        
        //Act Statement(s)
        Long result = target.toLong();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1L)));
    }

    //BaseRock generated method id: ${toStringTest}, hash: AE759C98FDFF9BCE8CF2B853F0035CE3
    @Test()
    void toStringTest() {
        //Arrange Statement(s)
        MutableLong target = new MutableLong(2L);
        
        //Act Statement(s)
        String result = target.toString();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("2")));
    }
}

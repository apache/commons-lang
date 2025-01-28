package org.apache.commons.lang3.mutable;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.hamcrest.Matchers.closeTo;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class MutableIntBaseRockGeneratedTest {

    //BaseRock generated method id: ${addTest}, hash: 9A8203D62EEE4ACCDA347E48336D10C6
    @Test()
    void addTest() {
        //Arrange Statement(s)
        MutableInt target = new MutableInt(1);
        
        //Act Statement(s)
        target.add(1);
    }

    //BaseRock generated method id: ${add1Test}, hash: CBC1673E8219D6D27FBC9DC4F24F8F6C
    @Test()
    void add1Test() {
        //Arrange Statement(s)
        MutableInt target = new MutableInt(1);
        
        //Act Statement(s)
        target.add(1);
    }

    //BaseRock generated method id: ${addAndGetTest}, hash: ABA0BFF5D39E54960B4AA7CC16D8EFFB
    @Test()
    void addAndGetTest() {
        //Arrange Statement(s)
        MutableInt target = new MutableInt(1);
        
        //Act Statement(s)
        int result = target.addAndGet(1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(2)));
    }

    //BaseRock generated method id: ${addAndGet1Test}, hash: 10A1384691F78098C1945E8966D78D45
    @Test()
    void addAndGet1Test() {
        //Arrange Statement(s)
        MutableInt target = new MutableInt(1);
        
        //Act Statement(s)
        int result = target.addAndGet(1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(2)));
    }

    //BaseRock generated method id: ${compareToTest}, hash: A57ADA2DB60D90E22841ABF382EED53B
    @Test()
    void compareToTest() {
        //Arrange Statement(s)
        MutableInt target = new MutableInt(1);
        MutableInt mutableInt = new MutableInt(1);
        
        //Act Statement(s)
        int result = target.compareTo(mutableInt);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${decrementTest}, hash: 3406E5044AE40119E315F00A18D8BF88
    @Test()
    void decrementTest() {
        //Arrange Statement(s)
        MutableInt target = new MutableInt(1);
        
        //Act Statement(s)
        target.decrement();
    }

    //BaseRock generated method id: ${decrementAndGetTest}, hash: E48E71E72D46735B3E665473945FB312
    @Test()
    void decrementAndGetTest() {
        //Arrange Statement(s)
        MutableInt target = new MutableInt(1);
        
        //Act Statement(s)
        int result = target.decrementAndGet();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${doubleValueTest}, hash: 3CDBA91F54CD1D2798B05DC890C3289D
    @Test()
    void doubleValueTest() {
        //Arrange Statement(s)
        MutableInt target = new MutableInt(0);
        
        //Act Statement(s)
        double result = target.doubleValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001)));
    }

    //BaseRock generated method id: ${equalsWhenObjNotInstanceOfMutableInt}, hash: A6E4E0FF1EE46CDB9E0D7BD9DB17D0A4
    @Test()
    void equalsWhenObjNotInstanceOfMutableInt() {
        /* Branches:
         * (obj instanceof MutableInt) : false
         */
         //Arrange Statement(s)
        MutableInt target = new MutableInt(0);
        Object object = new Object();
        
        //Act Statement(s)
        boolean result = target.equals(object);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equalsWhenValueEqualsObjIntValue}, hash: 8FFABD4DD21B2B11A97F2D73625B3B7B
    @Test()
    void equalsWhenValueEqualsObjIntValue() {
        /* Branches:
         * (obj instanceof MutableInt) : true
         * (value == ((MutableInt) obj).intValue()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        MutableInt target = new MutableInt(1);
        MutableInt mutableInt = new MutableInt();
        
        //Act Statement(s)
        boolean result = target.equals(mutableInt);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsWhenValueNotEqualsObjIntValue}, hash: 890D426F5FFE21B5EBFDDEFCC4745C45
    @Test()
    void equalsWhenValueNotEqualsObjIntValue() {
        /* Branches:
         * (obj instanceof MutableInt) : true
         * (value == ((MutableInt) obj).intValue()) : false
         */
         //Arrange Statement(s)
        MutableInt target = new MutableInt(1);
        MutableInt mutableInt = new MutableInt();
        
        //Act Statement(s)
        boolean result = target.equals(mutableInt);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${floatValueTest}, hash: 503A8629388B7F5F65983B4B6684A342
    @Test()
    void floatValueTest() {
        //Arrange Statement(s)
        MutableInt target = new MutableInt(0);
        
        //Act Statement(s)
        float result = target.floatValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Float.parseFloat("0.0"))));
    }

    //BaseRock generated method id: ${getAndAddTest}, hash: 3B6B7ECDACC3C65916BC3378C6F8F31A
    @Test()
    void getAndAddTest() {
        //Arrange Statement(s)
        MutableInt target = new MutableInt(1);
        
        //Act Statement(s)
        int result = target.getAndAdd(1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${getAndAdd1Test}, hash: AD2DD6C57AB8E0457CB6BC1580790494
    @Test()
    void getAndAdd1Test() {
        //Arrange Statement(s)
        MutableInt target = new MutableInt(1);
        
        //Act Statement(s)
        int result = target.getAndAdd(1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${getAndDecrementTest}, hash: 2F3505A07E36BA7A5EDA6200D9506466
    @Test()
    void getAndDecrementTest() {
        //Arrange Statement(s)
        MutableInt target = new MutableInt(1);
        
        //Act Statement(s)
        int result = target.getAndDecrement();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${getAndIncrementTest}, hash: 2AB2857FC89E08F41D644231110F8088
    @Test()
    void getAndIncrementTest() {
        //Arrange Statement(s)
        MutableInt target = new MutableInt(1);
        
        //Act Statement(s)
        int result = target.getAndIncrement();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${getValueTest}, hash: 131B25EA7FBADBB4BF62255DC487C758
    @Test()
    void getValueTest() {
        //Arrange Statement(s)
        MutableInt target = new MutableInt(1);
        
        //Act Statement(s)
        Integer result = target.getValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${incrementTest}, hash: 7D3D063C2E60E63BEF64EB08493EAF7D
    @Test()
    void incrementTest() {
        //Arrange Statement(s)
        MutableInt target = new MutableInt(1);
        
        //Act Statement(s)
        target.increment();
    }

    //BaseRock generated method id: ${incrementAndGetTest}, hash: 79359FA16D0074FA24EA6AD1E45F857D
    @Test()
    void incrementAndGetTest() {
        //Arrange Statement(s)
        MutableInt target = new MutableInt(1);
        
        //Act Statement(s)
        int result = target.incrementAndGet();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(2)));
    }

    //BaseRock generated method id: ${intValueTest}, hash: 2149E0E7957773B74D871AFABD47C08D
    @Test()
    void intValueTest() {
        //Arrange Statement(s)
        MutableInt target = new MutableInt(0);
        
        //Act Statement(s)
        int result = target.intValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${longValueTest}, hash: CEFA33EEEC046D6481B452432ED8A753
    @Test()
    void longValueTest() {
        //Arrange Statement(s)
        MutableInt target = new MutableInt(1);
        
        //Act Statement(s)
        long result = target.longValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1L)));
    }

    //BaseRock generated method id: ${setValueTest}, hash: CD527C11AC548DB9CB831F78997ADC5A
    @Test()
    void setValueTest() {
        //Arrange Statement(s)
        MutableInt target = new MutableInt(0);
        
        //Act Statement(s)
        target.setValue(0);
    }

    //BaseRock generated method id: ${setValue1Test}, hash: 4D9D888663028A2CC273B32FF4F511F2
    @Test()
    void setValue1Test() {
        //Arrange Statement(s)
        MutableInt target = new MutableInt(0);
        
        //Act Statement(s)
        target.setValue(1);
    }

    //BaseRock generated method id: ${subtractTest}, hash: 9067AE66F9F38FE19EFD537E093E63EF
    @Test()
    void subtractTest() {
        //Arrange Statement(s)
        MutableInt target = new MutableInt(1);
        
        //Act Statement(s)
        target.subtract(1);
    }

    //BaseRock generated method id: ${subtract1Test}, hash: 2F6C1BB559F89C7B61DA65617334AC42
    @Test()
    void subtract1Test() {
        //Arrange Statement(s)
        MutableInt target = new MutableInt(1);
        
        //Act Statement(s)
        target.subtract(1);
    }

    //BaseRock generated method id: ${toIntegerTest}, hash: D8428FDBE13FCB90A04CC0433DE929E7
    @Test()
    void toIntegerTest() {
        //Arrange Statement(s)
        MutableInt target = new MutableInt(1);
        
        //Act Statement(s)
        Integer result = target.toInteger();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1)));
    }

    //BaseRock generated method id: ${toStringTest}, hash: 861F1486C785E842A62970179ADD55E7
    @Test()
    void toStringTest() {
        //Arrange Statement(s)
        MutableInt target = new MutableInt(2);
        
        //Act Statement(s)
        String result = target.toString();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("2")));
    }
}

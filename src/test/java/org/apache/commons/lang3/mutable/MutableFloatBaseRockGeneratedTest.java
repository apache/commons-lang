package org.apache.commons.lang3.mutable;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.hamcrest.Matchers.closeTo;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class MutableFloatBaseRockGeneratedTest {

    //BaseRock generated method id: ${addTest}, hash: F767E16DDAE7913A5EA51C26A5BF2C60
    @Test()
    void addTest() {
        //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("0.0"));
        
        //Act Statement(s)
        target.add(Float.parseFloat("0.0"));
    }

    //BaseRock generated method id: ${add1Test}, hash: 50A84658CA8595EAC35AE5A4DF92BE90
    @Test()
    void add1Test() {
        //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("0.0"));
        
        //Act Statement(s)
        target.add(0);
    }

    //BaseRock generated method id: ${addAndGetTest}, hash: A4D688FB174420226C381F119497A09B
    @Test()
    void addAndGetTest() {
        //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("0.0"));
        
        //Act Statement(s)
        float result = target.addAndGet(Float.parseFloat("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Float.parseFloat("0.0"))));
    }

    //BaseRock generated method id: ${addAndGet1Test}, hash: 3E43411D0874B5CA316F3A07ABD11A4F
    @Test()
    void addAndGet1Test() {
        //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("0.0"));
        
        //Act Statement(s)
        float result = target.addAndGet(0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Float.parseFloat("0.0"))));
    }

    //BaseRock generated method id: ${compareToTest}, hash: 866BEE854DC3DAA31F90E23BA0C5106E
    @Test()
    void compareToTest() {
        //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("1.0"));
        MutableFloat mutableFloat = new MutableFloat(Float.parseFloat("1.0"));
        
        //Act Statement(s)
        int result = target.compareTo(mutableFloat);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${decrementTest}, hash: B01E2F6AB0F09E792EEAF0C78F0262FF
    @Test()
    void decrementTest() {
        //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("0.0"));
        
        //Act Statement(s)
        target.decrement();
    }

    //BaseRock generated method id: ${decrementAndGetTest}, hash: 3D1EE2EDC4FD91C760CF507AFB88BEFD
    @Test()
    void decrementAndGetTest() {
        //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("0.0"));
        
        //Act Statement(s)
        float result = target.decrementAndGet();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Float.parseFloat("-1.0"))));
    }

    //BaseRock generated method id: ${doubleValueTest}, hash: A5651FA32E561AF7A9F9777CF2AEE043
    @Test()
    void doubleValueTest() {
        //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("0.0"));
        
        //Act Statement(s)
        double result = target.doubleValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001)));
    }

    //BaseRock generated method id: ${equalsWhenFloatFloatToIntBitsObjValueEqualsFloatFloatToIntBitsValue}, hash: E4C62FF645E3B2C559955A30D840F24A
    @Test()
    void equalsWhenFloatFloatToIntBitsObjValueEqualsFloatFloatToIntBitsValue() {
        /* Branches:
         * (obj instanceof MutableFloat) : true
         * (Float.floatToIntBits(((MutableFloat) obj).value) == Float.floatToIntBits(value)) : true
         */
         //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("0.0"));
        MutableFloat mutableFloat = new MutableFloat();
        
        //Act Statement(s)
        boolean result = target.equals(mutableFloat);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsWhenFloatFloatToIntBitsObjValueNotEqualsFloatFloatToIntBitsValue}, hash: 25EE56729F1DB9C6A44ADCF5F842521F
    @Test()
    void equalsWhenFloatFloatToIntBitsObjValueNotEqualsFloatFloatToIntBitsValue() {
        /* Branches:
         * (obj instanceof MutableFloat) : true
         * (Float.floatToIntBits(((MutableFloat) obj).value) == Float.floatToIntBits(value)) : false
         */
         //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("1.0"));
        MutableFloat mutableFloat = new MutableFloat();
        
        //Act Statement(s)
        boolean result = target.equals(mutableFloat);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${floatValueTest}, hash: B23EA552A03FBA4B1E2A5FAA58E46B3F
    @Test()
    void floatValueTest() {
        //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("0.0"));
        
        //Act Statement(s)
        float result = target.floatValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Float.parseFloat("0.0"))));
    }

    //BaseRock generated method id: ${getAndAddTest}, hash: 9EDFCAFB9DCF12212C7F5756CDBE831A
    @Test()
    void getAndAddTest() {
        //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("0.0"));
        
        //Act Statement(s)
        float result = target.getAndAdd(Float.parseFloat("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Float.parseFloat("0.0"))));
    }

    //BaseRock generated method id: ${getAndAdd1Test}, hash: 9ABCCE464E306DA76E5F07C9352A686F
    @Test()
    void getAndAdd1Test() {
        //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("0.0"));
        
        //Act Statement(s)
        float result = target.getAndAdd(0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Float.parseFloat("0.0"))));
    }

    //BaseRock generated method id: ${getAndDecrementTest}, hash: 0593AF898C2E105A1756CA39C83FC01D
    @Test()
    void getAndDecrementTest() {
        //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("0.0"));
        
        //Act Statement(s)
        float result = target.getAndDecrement();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Float.parseFloat("0.0"))));
    }

    //BaseRock generated method id: ${getAndIncrementTest}, hash: 40519A5866765E89E9E3E07D15018416
    @Test()
    void getAndIncrementTest() {
        //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("0.0"));
        
        //Act Statement(s)
        float result = target.getAndIncrement();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Float.parseFloat("0.0"))));
    }

    //BaseRock generated method id: ${getValueTest}, hash: 8DBABA1A40EBB67B515ED8BBBCD43AFB
    @Test()
    void getValueTest() {
        //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("0.0"));
        
        //Act Statement(s)
        Float result = target.getValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Float.parseFloat("0.0"))));
    }

    //BaseRock generated method id: ${incrementTest}, hash: 790E263569DF60450A75B6E1533A6774
    @Test()
    void incrementTest() {
        //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("0.0"));
        
        //Act Statement(s)
        target.increment();
    }

    //BaseRock generated method id: ${incrementAndGetTest}, hash: 72BD5E3D0DFFE26D819D6AD894094663
    @Test()
    void incrementAndGetTest() {
        //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("0.0"));
        
        //Act Statement(s)
        float result = target.incrementAndGet();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Float.parseFloat("1.0"))));
    }

    //BaseRock generated method id: ${intValueTest}, hash: EF1CA6ED6A378680DF25615F9EBE6E84
    @Test()
    void intValueTest() {
        //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("0.0"));
        
        //Act Statement(s)
        int result = target.intValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${isInfiniteWhenFloatIsInfiniteValue}, hash: 0339B297A2DEA0770FA61E2740B5CF78
    @Test()
    void isInfiniteWhenFloatIsInfiniteValue() {
        /* Branches:
         * (Float.isInfinite(value)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("0.0"));
        
        //Act Statement(s)
        boolean result = target.isInfinite();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isInfiniteWhenFloatNotIsInfiniteValue}, hash: DA80A08C216E1F1D88D8A0E898D63856
    @Test()
    void isInfiniteWhenFloatNotIsInfiniteValue() {
        /* Branches:
         * (Float.isInfinite(value)) : false
         */
         //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("1.0"));
        
        //Act Statement(s)
        boolean result = target.isInfinite();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isNaNWhenFloatIsNaNValue}, hash: 98D23FE8F38F78B8E8F479F509902F7F
    @Test()
    void isNaNWhenFloatIsNaNValue() {
        /* Branches:
         * (Float.isNaN(value)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("0.0"));
        
        //Act Statement(s)
        boolean result = target.isNaN();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isNaNWhenFloatNotIsNaNValue}, hash: 8BB4F3AD70D19F5DF90C4149F5786C80
    @Test()
    void isNaNWhenFloatNotIsNaNValue() {
        /* Branches:
         * (Float.isNaN(value)) : false
         */
         //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("1.0"));
        
        //Act Statement(s)
        boolean result = target.isNaN();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${longValueTest}, hash: 53F6449417303F1B3BB6AC3E9A5A4050
    @Test()
    void longValueTest() {
        //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("0.0"));
        
        //Act Statement(s)
        long result = target.longValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${setValueTest}, hash: F3263ECB66B4D5933995017A75F6319F
    @Test()
    void setValueTest() {
        //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("0.0"));
        
        //Act Statement(s)
        target.setValue(Float.parseFloat("0.0"));
    }

    //BaseRock generated method id: ${setValue1Test}, hash: 63789C6005A58C3421D615A7D05D857D
    @Test()
    void setValue1Test() {
        //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("0.0"));
        
        //Act Statement(s)
        target.setValue(1);
    }

    //BaseRock generated method id: ${subtractTest}, hash: 9D29041886605EB53957F4AAF796D747
    @Test()
    void subtractTest() {
        //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("0.0"));
        
        //Act Statement(s)
        target.subtract(Float.parseFloat("0.0"));
    }

    //BaseRock generated method id: ${subtract1Test}, hash: A6F94618973D4BD766D59E9A0E45DDF8
    @Test()
    void subtract1Test() {
        //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("0.0"));
        
        //Act Statement(s)
        target.subtract(0);
    }

    //BaseRock generated method id: ${toFloatTest}, hash: CA6EED32EC874824FED96A4BF97EFD01
    @Test()
    void toFloatTest() {
        //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("0.0"));
        
        //Act Statement(s)
        Float result = target.toFloat();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Float.parseFloat("0.0"))));
    }

    //BaseRock generated method id: ${toStringTest}, hash: 339BBBEAF9B4EFD47AF2356E30DE5ED9
    @Test()
    void toStringTest() {
        //Arrange Statement(s)
        MutableFloat target = new MutableFloat(Float.parseFloat("0.0"));
        
        //Act Statement(s)
        String result = target.toString();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("0.0")));
    }
}

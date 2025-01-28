package org.apache.commons.lang3.mutable;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.hamcrest.Matchers.closeTo;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class MutableDoubleBaseRockGeneratedTest {

    //BaseRock generated method id: ${addTest}, hash: F834DC6BE12961C27F20235ECE59EB03
    @Test()
    void addTest() {
        //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        target.add(Double.parseDouble("0.0"));
    }

    //BaseRock generated method id: ${add1Test}, hash: ACCB6CC09708136C93B370C0F1F130D3
    @Test()
    void add1Test() {
        //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        target.add(0);
    }

    //BaseRock generated method id: ${addAndGetTest}, hash: B879A16B26EF0CFEE67E87543E60C3FE
    @Test()
    void addAndGetTest() {
        //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        double result = target.addAndGet(Double.parseDouble("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001)));
    }

    //BaseRock generated method id: ${addAndGet1Test}, hash: 2DA309F59FFCE24AE16A9ACC53B3B0C8
    @Test()
    void addAndGet1Test() {
        //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        double result = target.addAndGet(0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001)));
    }

    //BaseRock generated method id: ${compareToTest}, hash: 6606843C25A73B09664C7EFE3A8A435A
    @Test()
    void compareToTest() {
        //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("1.0"));
        MutableDouble mutableDouble = new MutableDouble(Double.parseDouble("1.0"));
        
        //Act Statement(s)
        int result = target.compareTo(mutableDouble);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${decrementTest}, hash: C388C3A0B70B1F021C85F8A41442DF6A
    @Test()
    void decrementTest() {
        //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        target.decrement();
    }

    //BaseRock generated method id: ${decrementAndGetTest}, hash: 99893B970E7BD179B69CA6BD65BFC31F
    @Test()
    void decrementAndGetTest() {
        //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        double result = target.decrementAndGet();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("-1.0"), 0.00001)));
    }

    //BaseRock generated method id: ${doubleValueTest}, hash: 84BD52960656D193AF7992C6A033FBD8
    @Test()
    void doubleValueTest() {
        //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        double result = target.doubleValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001)));
    }

    //BaseRock generated method id: ${equalsWhenDoubleDoubleToLongBitsObjValueEqualsDoubleDoubleToLongBitsValue}, hash: 80D2ABA5AF8D530F8538614D3C5E00D2
    @Test()
    void equalsWhenDoubleDoubleToLongBitsObjValueEqualsDoubleDoubleToLongBitsValue() {
        /* Branches:
         * (obj instanceof MutableDouble) : true
         * (Double.doubleToLongBits(((MutableDouble) obj).value) == Double.doubleToLongBits(value)) : true
         */
         //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("0.0"));
        MutableDouble mutableDouble = new MutableDouble();
        
        //Act Statement(s)
        boolean result = target.equals(mutableDouble);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsWhenDoubleDoubleToLongBitsObjValueNotEqualsDoubleDoubleToLongBitsValue}, hash: 5236727878BC446E536A3C3054D9984A
    @Test()
    void equalsWhenDoubleDoubleToLongBitsObjValueNotEqualsDoubleDoubleToLongBitsValue() {
        /* Branches:
         * (obj instanceof MutableDouble) : true
         * (Double.doubleToLongBits(((MutableDouble) obj).value) == Double.doubleToLongBits(value)) : false
         */
         //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("1.0"));
        MutableDouble mutableDouble = new MutableDouble();
        
        //Act Statement(s)
        boolean result = target.equals(mutableDouble);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${floatValueTest}, hash: 2FACF63EA30E96833797EBC3F7E5D58A
    @Test()
    void floatValueTest() {
        //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        float result = target.floatValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Float.parseFloat("0.0"))));
    }

    //BaseRock generated method id: ${getAndAddTest}, hash: 0F8E642BE8379FF01ED6EE209C97D359
    @Test()
    void getAndAddTest() {
        //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        double result = target.getAndAdd(Double.parseDouble("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001)));
    }

    //BaseRock generated method id: ${getAndAdd1Test}, hash: 08E2C417C0CD0300554BA86BE330556A
    @Test()
    void getAndAdd1Test() {
        //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        double result = target.getAndAdd(0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001)));
    }

    //BaseRock generated method id: ${getAndDecrementTest}, hash: BC91C4B79AD145B8C1AF2FA5AECCAD87
    @Test()
    void getAndDecrementTest() {
        //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        double result = target.getAndDecrement();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001)));
    }

    //BaseRock generated method id: ${getAndIncrementTest}, hash: 00C417E28ED8A64BADEB0EF35BEDA0AE
    @Test()
    void getAndIncrementTest() {
        //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        double result = target.getAndIncrement();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001)));
    }

    //BaseRock generated method id: ${getValueTest}, hash: 44D5D1AE2FE84A26E3141E4B42724359
    @Test()
    void getValueTest() {
        //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        Double result = target.getValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001)));
    }

    //BaseRock generated method id: ${incrementTest}, hash: 6932DE276FBFC9F351F234E0B6BD3C2E
    @Test()
    void incrementTest() {
        //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        target.increment();
    }

    //BaseRock generated method id: ${incrementAndGetTest}, hash: 1EBD1A38B0701A2AB3DAEED3170AD74E
    @Test()
    void incrementAndGetTest() {
        //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        double result = target.incrementAndGet();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("1.0"), 0.00001)));
    }

    //BaseRock generated method id: ${intValueTest}, hash: F91D58544B87A4765C963A80B06B4106
    @Test()
    void intValueTest() {
        //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        int result = target.intValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${isInfiniteWhenDoubleIsInfiniteValue}, hash: 513717B9BF0869CBF74BD56A65C183C6
    @Test()
    void isInfiniteWhenDoubleIsInfiniteValue() {
        /* Branches:
         * (Double.isInfinite(value)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        boolean result = target.isInfinite();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isInfiniteWhenDoubleNotIsInfiniteValue}, hash: A580E3E196B0EB65AC64E544A5255AA3
    @Test()
    void isInfiniteWhenDoubleNotIsInfiniteValue() {
        /* Branches:
         * (Double.isInfinite(value)) : false
         */
         //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("1.0"));
        
        //Act Statement(s)
        boolean result = target.isInfinite();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isNaNWhenDoubleIsNaNValue}, hash: 1997C368C6BDFDA3373966648F1F4AB1
    @Test()
    void isNaNWhenDoubleIsNaNValue() {
        /* Branches:
         * (Double.isNaN(value)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        boolean result = target.isNaN();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isNaNWhenDoubleNotIsNaNValue}, hash: 34375412F0D1DD33CE4601FF106A537B
    @Test()
    void isNaNWhenDoubleNotIsNaNValue() {
        /* Branches:
         * (Double.isNaN(value)) : false
         */
         //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("1.0"));
        
        //Act Statement(s)
        boolean result = target.isNaN();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${longValueTest}, hash: 15E8DF6E5A2C67E1F3DEF31468398276
    @Test()
    void longValueTest() {
        //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        long result = target.longValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${setValueTest}, hash: C279C8196028AB0A58855EF229E7D984
    @Test()
    void setValueTest() {
        //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        target.setValue(Double.parseDouble("0.0"));
    }

    //BaseRock generated method id: ${setValue1Test}, hash: 6D0039ABEE6B40A04C445B49250AE4FF
    @Test()
    void setValue1Test() {
        //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        target.setValue(1);
    }

    //BaseRock generated method id: ${subtractTest}, hash: 421B87297ABD04936289673BE05B1B7E
    @Test()
    void subtractTest() {
        //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        target.subtract(Double.parseDouble("0.0"));
    }

    //BaseRock generated method id: ${subtract1Test}, hash: 897B6A5D375BB2881474C470A41E9F4E
    @Test()
    void subtract1Test() {
        //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        target.subtract(0);
    }

    //BaseRock generated method id: ${toDoubleTest}, hash: 643CF26F2DEB218CCB38A3F63ABB898C
    @Test()
    void toDoubleTest() {
        //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        Double result = target.toDouble();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001)));
    }

    //BaseRock generated method id: ${toStringTest}, hash: C3748A2273E84EF0C8D27CB687DA64F0
    @Test()
    void toStringTest() {
        //Arrange Statement(s)
        MutableDouble target = new MutableDouble(Double.parseDouble("0.0"));
        
        //Act Statement(s)
        String result = target.toString();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("0.0")));
    }
}

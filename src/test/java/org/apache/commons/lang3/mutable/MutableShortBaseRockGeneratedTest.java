package org.apache.commons.lang3.mutable;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.mock;
import static org.hamcrest.Matchers.closeTo;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class MutableShortBaseRockGeneratedTest {

    //BaseRock generated method id: ${addTest}, hash: F39FEEE0379845BB8220595781AAB3E8
    @Test()
    void addTest() {
        //Arrange Statement(s)
        MutableShort target = new MutableShort(0);
        
        //Act Statement(s)
        target.add(0);
    }

    //BaseRock generated method id: ${add1Test}, hash: DDE2B24B64083E75A73929162261153D
    @Test()
    void add1Test() {
        //Arrange Statement(s)
        MutableShort target = new MutableShort(0);
        
        //Act Statement(s)
        target.add((short) 0);
    }

    //BaseRock generated method id: ${addAndGetTest}, hash: 19C39BE50B38A83B5EAC060E6ACE1B5B
    @Test()
    void addAndGetTest() {
        //Arrange Statement(s)
        MutableShort target = new MutableShort(0);
        
        //Act Statement(s)
        short result = target.addAndGet(0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((short) 0)));
    }

    //BaseRock generated method id: ${addAndGet1Test}, hash: 421CFB7D752A5DADC6D223C3AC9885C7
    @Test()
    void addAndGet1Test() {
        //Arrange Statement(s)
        MutableShort target = new MutableShort(0);
        
        //Act Statement(s)
        short result = target.addAndGet((short) 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((short) 0)));
    }

    //BaseRock generated method id: ${compareToTest}, hash: 69792458D82D1DBBB11C74122C36559E
    @Test()
    void compareToTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        MutableShort target = new MutableShort(1);
        MutableShort mutableShortMock = mock(MutableShort.class);
        
        //Act Statement(s)
        int result = target.compareTo(mutableShortMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${decrementTest}, hash: 5EE12AC1C69054999C210C11CDEE28FC
    @Test()
    void decrementTest() {
        //Arrange Statement(s)
        MutableShort target = new MutableShort(1);
        
        //Act Statement(s)
        target.decrement();
    }

    //BaseRock generated method id: ${decrementAndGetTest}, hash: 9E70DEFA1ADA8C72C523DD40B49CCE23
    @Test()
    void decrementAndGetTest() {
        //Arrange Statement(s)
        MutableShort target = new MutableShort(1);
        
        //Act Statement(s)
        short result = target.decrementAndGet();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((short) 0)));
    }

    //BaseRock generated method id: ${doubleValueTest}, hash: 9BB5C4D4004F7D476B966084BE5DFEFD
    @Test()
    void doubleValueTest() {
        //Arrange Statement(s)
        MutableShort target = new MutableShort(1);
        
        //Act Statement(s)
        double result = target.doubleValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, closeTo(Double.parseDouble("1.0"), 0.00001)));
    }

    //BaseRock generated method id: ${equalsWhenObjNotInstanceOfMutableShort}, hash: EED1C5B8C36A239A091191304E42A220
    @Test()
    void equalsWhenObjNotInstanceOfMutableShort() {
        /* Branches:
         * (obj instanceof MutableShort) : false
         */
         //Arrange Statement(s)
        MutableShort target = new MutableShort(1);
        Object object = new Object();
        
        //Act Statement(s)
        boolean result = target.equals(object);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equalsWhenValueEqualsObjShortValue}, hash: 14CDAF1393475F306B4AC1540922317D
    @Test()
    void equalsWhenValueEqualsObjShortValue() {
        /* Branches:
         * (obj instanceof MutableShort) : true
         * (value == ((MutableShort) obj).shortValue()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        MutableShort target = new MutableShort(1);
        MutableShort mutableShort = new MutableShort();
        
        //Act Statement(s)
        boolean result = target.equals(mutableShort);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsWhenValueNotEqualsObjShortValue}, hash: 54A917A16DB5A663DC5F62227A076750
    @Test()
    void equalsWhenValueNotEqualsObjShortValue() {
        /* Branches:
         * (obj instanceof MutableShort) : true
         * (value == ((MutableShort) obj).shortValue()) : false
         */
         //Arrange Statement(s)
        MutableShort target = new MutableShort(1);
        MutableShort mutableShort = new MutableShort();
        
        //Act Statement(s)
        boolean result = target.equals(mutableShort);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${floatValueTest}, hash: 63B3D1D79DF5D656347423D6D5EF6388
    @Test()
    void floatValueTest() {
        //Arrange Statement(s)
        MutableShort target = new MutableShort(1);
        
        //Act Statement(s)
        float result = target.floatValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Float.parseFloat("1.0"))));
    }

    //BaseRock generated method id: ${getAndAddTest}, hash: 0D40A54A7FEE5C62ED85247CCFBE09A6
    @Test()
    void getAndAddTest() {
        //Arrange Statement(s)
        MutableShort target = new MutableShort(0);
        
        //Act Statement(s)
        short result = target.getAndAdd(0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((short) 0)));
    }

    //BaseRock generated method id: ${getAndAdd1Test}, hash: C3C13C2B83B27086536E2477CEB10F03
    @Test()
    void getAndAdd1Test() {
        //Arrange Statement(s)
        MutableShort target = new MutableShort(0);
        
        //Act Statement(s)
        short result = target.getAndAdd((short) 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((short) 0)));
    }

    //BaseRock generated method id: ${getAndDecrementTest}, hash: 2DEFE6A38077338BD02896B918978341
    @Test()
    void getAndDecrementTest() {
        //Arrange Statement(s)
        MutableShort target = new MutableShort(1);
        
        //Act Statement(s)
        short result = target.getAndDecrement();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((short) 1)));
    }

    //BaseRock generated method id: ${getAndIncrementTest}, hash: 6CE2EB2D910C2BAEDA6B45190FEC6623
    @Test()
    void getAndIncrementTest() {
        //Arrange Statement(s)
        MutableShort target = new MutableShort(1);
        
        //Act Statement(s)
        short result = target.getAndIncrement();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((short) 1)));
    }

    //BaseRock generated method id: ${getValueTest}, hash: 161AAB5B928344EE2CF642C40643FC3F
    @Test()
    void getValueTest() {
        //Arrange Statement(s)
        MutableShort target = new MutableShort(1);
        
        //Act Statement(s)
        Short result = target.getValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((short) 1)));
    }

    //BaseRock generated method id: ${incrementTest}, hash: 3B3935AB98423240025665BC55192460
    @Test()
    void incrementTest() {
        //Arrange Statement(s)
        MutableShort target = new MutableShort(1);
        
        //Act Statement(s)
        target.increment();
    }

    //BaseRock generated method id: ${incrementAndGetTest}, hash: B853606EFC592B6A5A41F7D7F752AF9B
    @Test()
    void incrementAndGetTest() {
        //Arrange Statement(s)
        MutableShort target = new MutableShort(1);
        
        //Act Statement(s)
        short result = target.incrementAndGet();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((short) 2)));
    }

    //BaseRock generated method id: ${intValueTest}, hash: E1F4B7A65933657245A4C7CE734D9114
    @Test()
    void intValueTest() {
        //Arrange Statement(s)
        MutableShort target = new MutableShort(1);
        
        //Act Statement(s)
        int result = target.intValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat((short) result, equalTo((short) 1)));
    }

    //BaseRock generated method id: ${longValueTest}, hash: D07154A152623D3A21A3B8C20D531D73
    @Test()
    void longValueTest() {
        //Arrange Statement(s)
        MutableShort target = new MutableShort(1);
        
        //Act Statement(s)
        long result = target.longValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1L)));
    }

    //BaseRock generated method id: ${setValueTest}, hash: 7990901E5451439575E794A636B5CBDE
    @Test()
    void setValueTest() {
        //Arrange Statement(s)
        MutableShort target = new MutableShort(1);
        
        //Act Statement(s)
        target.setValue(1);
    }

    //BaseRock generated method id: ${setValue1Test}, hash: 34DE6D6A15EACC99ECB7A6912A8DBCEF
    @Test()
    void setValue1Test() {
        //Arrange Statement(s)
        MutableShort target = new MutableShort(1);
        
        //Act Statement(s)
        target.setValue((short) 0);
    }

    //BaseRock generated method id: ${shortValueTest}, hash: 9696F166B1C2FDB795BA69C387A2E9BE
    @Test()
    void shortValueTest() {
        //Arrange Statement(s)
        MutableShort target = new MutableShort(1);
        
        //Act Statement(s)
        short result = target.shortValue();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((short) 1)));
    }

    //BaseRock generated method id: ${subtractTest}, hash: 36D36F62D979CF19E5B879828A32B5C1
    @Test()
    void subtractTest() {
        //Arrange Statement(s)
        MutableShort target = new MutableShort(0);
        
        //Act Statement(s)
        target.subtract(0);
    }

    //BaseRock generated method id: ${subtract1Test}, hash: 4A6E23D35B53B961BC6CD193627D406E
    @Test()
    void subtract1Test() {
        //Arrange Statement(s)
        MutableShort target = new MutableShort(0);
        
        //Act Statement(s)
        target.subtract((short) 0);
    }

    //BaseRock generated method id: ${toShortTest}, hash: D3325593A55C29641E7CDB5404A73308
    @Test()
    void toShortTest() {
        //Arrange Statement(s)
        MutableShort target = new MutableShort(1);
        
        //Act Statement(s)
        Short result = target.toShort();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((short) 1)));
    }

    //BaseRock generated method id: ${toStringTest}, hash: F03B81F49DDFA7478C1A9C12F2E941D0
    @Test()
    void toStringTest() {
        //Arrange Statement(s)
        MutableShort target = new MutableShort(2);
        
        //Act Statement(s)
        String result = target.toString();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("2")));
    }
}

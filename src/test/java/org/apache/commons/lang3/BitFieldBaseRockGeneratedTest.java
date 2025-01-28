package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.doReturn;
import org.junit.jupiter.api.Disabled;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class BitFieldBaseRockGeneratedTest {

    //BaseRock generated method id: ${clearTest}, hash: 93BF9F0BFF43728654364991018986C6
    @Test()
    void clearTest() {
        //Arrange Statement(s)
        BitField target = new BitField(0);
        //Act Statement(s)
        int result = target.clear(0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${clearByteTest}, hash: 39A3B12BBCE30F313102428C76DFE5AB
    @Test()
    void clearByteTest() {
        //Arrange Statement(s)
        BitField target = new BitField(0);
        //Act Statement(s)
        byte result = target.clearByte((byte) 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((byte) 0)));
    }

    //BaseRock generated method id: ${clearShortTest}, hash: 3F16CF4ED5C4ACDA70EDBC8260265E5C
    @Test()
    void clearShortTest() {
        //Arrange Statement(s)
        BitField target = new BitField(0);
        //Act Statement(s)
        short result = target.clearShort((short) 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((short) 0)));
    }

    //BaseRock generated method id: ${getRawValueTest}, hash: CF46630801BB5AC22602253BC66B6F71
    @Test()
    void getRawValueTest() {
        //Arrange Statement(s)
        BitField target = new BitField(0);
        //Act Statement(s)
        int result = target.getRawValue(0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${getShortRawValueTest}, hash: FE9C1CF20C4D81EB3EC648AEBAB851AE
    @Test()
    void getShortRawValueTest() {
        //Arrange Statement(s)
        BitField target = new BitField(0);
        //Act Statement(s)
        short result = target.getShortRawValue((short) 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((short) 0)));
    }

    //BaseRock generated method id: ${getShortValueTest}, hash: 582C7135EBDD016F475B156715D20D49
    @Test()
    void getShortValueTest() {
        //Arrange Statement(s)
        BitField target = spy(new BitField(0));
        doReturn(0).when(target).getValue((short) 0);
        //Act Statement(s)
        short result = target.getShortValue((short) 0);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo((short) 0));
            verify(target).getValue((short) 0);
        });
    }

    //BaseRock generated method id: ${getValueTest}, hash: 86A118710A6EB623C3876E012F7B597F
    @Test()
    void getValueTest() {
        //Arrange Statement(s)
        BitField target = new BitField(0);
        //Act Statement(s)
        int result = target.getValue(0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${isAllSetWhenHolderAndMaskEqualsMask}, hash: 3CFD6D4CDA4EAAEA12FE7EF65BB9C86B
    @Test()
    void isAllSetWhenHolderAndMaskEqualsMask() {
        /* Branches:
         * ((holder & mask) == mask) : true
         */
        //Arrange Statement(s)
        BitField target = new BitField(0);
        //Act Statement(s)
        boolean result = target.isAllSet(0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isAllSetWhenHolderAndMaskNotEqualsMask}, hash: 1E3410567FFC19B9241621BC7EED34F0
    @Disabled()
    @Test()
    void isAllSetWhenHolderAndMaskNotEqualsMask() {
        /* Branches:
         * ((holder & mask) == mask) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        BitField target = new BitField(0);
        //Act Statement(s)
        boolean result = target.isAllSet(0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isSetWhenHolderAndMaskNotEquals0}, hash: BC3EB225E43A7817C1CBCE4040BEEBAA
    @Disabled()
    @Test()
    void isSetWhenHolderAndMaskNotEquals0() {
        /* Branches:
         * ((holder & mask) != 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        BitField target = new BitField(0);
        //Act Statement(s)
        boolean result = target.isSet(0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isSetWhenHolderAndMaskEquals0}, hash: F965E69BDE27F1107715046C9C1C4372
    @Test()
    void isSetWhenHolderAndMaskEquals0() {
        /* Branches:
         * ((holder & mask) != 0) : false
         */
        //Arrange Statement(s)
        BitField target = new BitField(0);
        //Act Statement(s)
        boolean result = target.isSet(0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${setTest}, hash: 890C5E83033F4CA6B7CEAC7ECF0588B2
    @Test()
    void setTest() {
        //Arrange Statement(s)
        BitField target = new BitField(0);
        //Act Statement(s)
        int result = target.set(0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${setBooleanWhenFlag}, hash: 837E6E289F048332D7BA13C52F0BD9A8
    @Test()
    void setBooleanWhenFlag() {
        /* Branches:
         * (flag) : true
         */
        //Arrange Statement(s)
        BitField target = new BitField(0);
        //Act Statement(s)
        int result = target.setBoolean(0, true);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${setBooleanWhenNotFlag}, hash: ECDEF7088BCEE3095AE1B4D2790E0976
    @Test()
    void setBooleanWhenNotFlag() {
        /* Branches:
         * (flag) : false
         */
        //Arrange Statement(s)
        BitField target = new BitField(0);
        //Act Statement(s)
        int result = target.setBoolean(0, false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${setByteTest}, hash: E08C2DF09C34D570A4C28F848CBB0A6D
    @Test()
    void setByteTest() {
        //Arrange Statement(s)
        BitField target = new BitField(0);
        //Act Statement(s)
        byte result = target.setByte((byte) 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((byte) 0)));
    }

    //BaseRock generated method id: ${setByteBooleanWhenFlag}, hash: FC49A837E750CDF2C788B49DB738D305
    @Test()
    void setByteBooleanWhenFlag() {
        /* Branches:
         * (flag) : true
         */
        //Arrange Statement(s)
        BitField target = spy(new BitField(0));
        doReturn((byte) 0).when(target).setByte((byte) 0);
        //Act Statement(s)
        byte result = target.setByteBoolean((byte) 0, true);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo((byte) 0));
            verify(target).setByte((byte) 0);
        });
    }

    //BaseRock generated method id: ${setByteBooleanWhenNotFlag}, hash: 3F629EC0AF6C412998374A92BE1D2D79
    @Test()
    void setByteBooleanWhenNotFlag() {
        /* Branches:
         * (flag) : false
         */
        //Arrange Statement(s)
        BitField target = spy(new BitField(0));
        doReturn((byte) 0).when(target).clearByte((byte) 0);
        //Act Statement(s)
        byte result = target.setByteBoolean((byte) 0, false);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo((byte) 0));
            verify(target).clearByte((byte) 0);
        });
    }

    //BaseRock generated method id: ${setShortTest}, hash: 1FD31A17DBA18D5F0D698897FBE3D7F5
    @Test()
    void setShortTest() {
        //Arrange Statement(s)
        BitField target = new BitField(0);
        //Act Statement(s)
        short result = target.setShort((short) 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((short) 0)));
    }

    //BaseRock generated method id: ${setShortBooleanWhenFlag}, hash: 3B944665DBCD33220C4CEA1884557658
    @Test()
    void setShortBooleanWhenFlag() {
        /* Branches:
         * (flag) : true
         */
        //Arrange Statement(s)
        BitField target = spy(new BitField(0));
        doReturn((short) 0).when(target).setShort((short) 0);
        //Act Statement(s)
        short result = target.setShortBoolean((short) 0, true);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo((short) 0));
            verify(target).setShort((short) 0);
        });
    }

    //BaseRock generated method id: ${setShortBooleanWhenNotFlag}, hash: 63A7A5BF30C73A32A7BE4EFD6E3C9566
    @Test()
    void setShortBooleanWhenNotFlag() {
        /* Branches:
         * (flag) : false
         */
        //Arrange Statement(s)
        BitField target = spy(new BitField(0));
        doReturn((short) 0).when(target).clearShort((short) 0);
        //Act Statement(s)
        short result = target.setShortBoolean((short) 0, false);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo((short) 0));
            verify(target).clearShort((short) 0);
        });
    }

    //BaseRock generated method id: ${setShortValueTest}, hash: DC21E29BA5219DA9101BF64CE6996F16
    @Test()
    void setShortValueTest() {
        //Arrange Statement(s)
        BitField target = new BitField(0);
        //Act Statement(s)
        short result = target.setShortValue((short) 0, (short) 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo((short) 0)));
    }

    //BaseRock generated method id: ${setValueTest}, hash: 705C9B3F157122308B8EB705E2745823
    @Test()
    void setValueTest() {
        //Arrange Statement(s)
        BitField target = new BitField(0);
        //Act Statement(s)
        int result = target.setValue(0, 0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }
}

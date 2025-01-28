package org.apache.commons.lang3.util;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.BitSet;
import java.util.stream.IntStream;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.hamcrest.Matchers.is;
import org.junit.jupiter.api.Disabled;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class FluentBitSetBaseRockGeneratedTest {

    //BaseRock generated method id: ${andTest}, hash: 3D7DCE11A33DD813505A1E11331F041F
    @Test()
    void andTest() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        BitSet bitSet2 = new BitSet();
        //Act Statement(s)
        FluentBitSet result = target.and(bitSet2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${and1Test}, hash: F28AE705102D1A738ED0FB487DCFE152
    @Test()
    void and1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        FluentBitSet fluentBitSet = new FluentBitSet();
        //Act Statement(s)
        FluentBitSet result = target.and(fluentBitSet);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${andNotTest}, hash: 4F6231793C0784DD0D67232E022B03DA
    @Test()
    void andNotTest() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        BitSet bitSet2 = new BitSet();
        //Act Statement(s)
        FluentBitSet result = target.andNot(bitSet2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${andNot1Test}, hash: A1F12F789E7958E44CFE7590414CE861
    @Test()
    void andNot1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        FluentBitSet fluentBitSet = new FluentBitSet();
        //Act Statement(s)
        FluentBitSet result = target.andNot(fluentBitSet);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${bitSetTest}, hash: DF0596459479081C6F79DBF4B68F84CE
    @Test()
    void bitSetTest() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        BitSet result = target.bitSet();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(bitSet)));
    }

    //BaseRock generated method id: ${cardinalityTest}, hash: 94C57726BE0DEB56684E75662E9B3777
    @Test()
    void cardinalityTest() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        int result = target.cardinality();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${clearTest}, hash: 4C25DED5308F139529271305C67A4E45
    @Test()
    void clearTest() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        FluentBitSet result = target.clear();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${clear1WhenBitIndexArrayIsNotEmpty}, hash: 7BC83E11E214A88A2237C573262E9709
    @Test()
    void clear1WhenBitIndexArrayIsNotEmpty() {
        /* Branches:
         * (for-each(bitIndexArray)) : true
         */
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        int[] intArray = new int[] { 0 };
        //Act Statement(s)
        FluentBitSet result = target.clear(intArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${clear2Test}, hash: 9D5BB3FB72DCF17B08E619BE99D3A343
    @Test()
    void clear2Test() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        FluentBitSet result = target.clear(0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${clear3Test}, hash: 97E80D77BA0219660314DB187E3E0B05
    @Test()
    void clear3Test() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        FluentBitSet result = target.clear(1, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${cloneTest}, hash: D9DBCA944EFFE2F09F1A2EC4F047277D
    @Test()
    void cloneTest() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        Object result = target.clone();
        BitSet bitSet2 = new BitSet();
        FluentBitSet fluentBitSet = new FluentBitSet(bitSet2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(fluentBitSet)));
    }

    //BaseRock generated method id: ${equalsWhenThisEqualsObj}, hash: 09FEC9C1F4F4AF48C92781DB558362A3
    @Test()
    void equalsWhenThisEqualsObj() {
        /* Branches:
         * (this == obj) : true
         */
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        boolean result = target.equals(target);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsWhenObjNotInstanceOfFluentBitSet}, hash: BFD99C7F452F14B3C951F8B5797A1501
    @Test()
    void equalsWhenObjNotInstanceOfFluentBitSet() {
        /* Branches:
         * (this == obj) : false
         * (!(obj instanceof FluentBitSet)) : true
         */
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        Object object = new Object();
        //Act Statement(s)
        boolean result = target.equals(object);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equalsWhenObjectsEqualsBitSetOtherBitSet}, hash: FA19285E68BC7B65AFC5243D67F2A1CA
    @Disabled()
    @Test()
    void equalsWhenObjectsEqualsBitSetOtherBitSet() {
        /* Branches:
         * (this == obj) : false
         * (!(obj instanceof FluentBitSet)) : false
         * (Objects.equals(bitSet, other.bitSet)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        FluentBitSet target = new FluentBitSet((BitSet) null);
        FluentBitSet fluentBitSet = new FluentBitSet();
        //Act Statement(s)
        boolean result = target.equals(fluentBitSet);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsWhenObjectsNotEqualsBitSetOtherBitSet}, hash: 5888549D20788F0E4EC7F8564BB86ECB
    @Disabled()
    @Test()
    void equalsWhenObjectsNotEqualsBitSetOtherBitSet() {
        /* Branches:
         * (this == obj) : false
         * (!(obj instanceof FluentBitSet)) : false
         * (Objects.equals(bitSet, other.bitSet)) : false
         */
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        FluentBitSet fluentBitSet = new FluentBitSet();
        //Act Statement(s)
        boolean result = target.equals(fluentBitSet);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${flipTest}, hash: 59034B607D0E72F5EEA02DEA690EDE3C
    @Test()
    void flipTest() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        FluentBitSet result = target.flip(0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${flip1Test}, hash: 4CC4DD849A9AC5EED5E430290D89B7F4
    @Test()
    void flip1Test() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        FluentBitSet result = target.flip(1, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${getWhenBitSetGetBitIndex}, hash: 0C00D6E0C3857FA884DFC733DF50CD1E
    @Disabled()
    @Test()
    void getWhenBitSetGetBitIndex() {
        /* Branches:
         * (bitSet.get(bitIndex)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        boolean result = target.get(0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${getWhenBitSetNotGetBitIndex}, hash: 72E02FCFB3237A63114251AC42C80981
    @Test()
    void getWhenBitSetNotGetBitIndex() {
        /* Branches:
         * (bitSet.get(bitIndex)) : false
         */
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        boolean result = target.get(0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${get1Test}, hash: 9B5E3F00EC1AC317CCE7DF23B788C5F5
    @Test()
    void get1Test() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        FluentBitSet result = target.get(1, 1);
        BitSet bitSet2 = bitSet.get(1, 1);
        FluentBitSet fluentBitSet = new FluentBitSet(bitSet2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(fluentBitSet)));
    }

    //BaseRock generated method id: ${intersectsWhenBitSetIntersectsSet}, hash: D07AB31383CBA6AC08228E5E9D3D5070
    @Disabled()
    @Test()
    void intersectsWhenBitSetIntersectsSet() {
        /* Branches:
         * (bitSet.intersects(set)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        BitSet bitSet2 = new BitSet();
        //Act Statement(s)
        boolean result = target.intersects(bitSet2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${intersectsWhenBitSetNotIntersectsSet}, hash: 2A9B7BF9677E57B5234544EB7ACEA882
    @Test()
    void intersectsWhenBitSetNotIntersectsSet() {
        /* Branches:
         * (bitSet.intersects(set)) : false
         */
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        BitSet bitSet2 = new BitSet();
        //Act Statement(s)
        boolean result = target.intersects(bitSet2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${intersects1WhenBitSetIntersectsSetBitSet}, hash: C6945BCF815BAA3B10AFBF30482592DE
    @Disabled()
    @Test()
    void intersects1WhenBitSetIntersectsSetBitSet() {
        /* Branches:
         * (bitSet.intersects(set.bitSet)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        FluentBitSet fluentBitSet = new FluentBitSet();
        //Act Statement(s)
        boolean result = target.intersects(fluentBitSet);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${intersects1WhenBitSetNotIntersectsSetBitSet}, hash: 335AB0DE3AB4E9B405B3DB1741022D39
    @Test()
    void intersects1WhenBitSetNotIntersectsSetBitSet() {
        /* Branches:
         * (bitSet.intersects(set.bitSet)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        FluentBitSet fluentBitSet = new FluentBitSet();
        //Act Statement(s)
        boolean result = target.intersects(fluentBitSet);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isEmptyWhenBitSetIsEmpty}, hash: 16DE5A6111367D9A15FE0C69AE7A969C
    @Test()
    void isEmptyWhenBitSetIsEmpty() {
        /* Branches:
         * (bitSet.isEmpty()) : true
         */
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        boolean result = target.isEmpty();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isEmptyWhenBitSetNotIsEmpty}, hash: 285F68851ADA6C4C7D52F0C59E71E493
    @Disabled()
    @Test()
    void isEmptyWhenBitSetNotIsEmpty() {
        /* Branches:
         * (bitSet.isEmpty()) : false
         */
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        boolean result = target.isEmpty();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${lengthTest}, hash: A8F2643567C9BF95A9BBFBE4B251672D
    @Test()
    void lengthTest() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        int result = target.length();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${nextClearBitTest}, hash: 329A1E695F147A18082F29200174543F
    @Test()
    void nextClearBitTest() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        int result = target.nextClearBit(0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${nextSetBitTest}, hash: 352214842B294C2310EF8F2778546586
    @Test()
    void nextSetBitTest() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        int result = target.nextSetBit(0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${orTest}, hash: 2F8B462E9F31A2C7593E9A25222D80D0
    @Test()
    void orTest() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        BitSet bitSet2 = new BitSet();
        //Act Statement(s)
        FluentBitSet result = target.or(bitSet2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${or1WhenSetIsNotEmpty}, hash: 8729F09E83FF4E8E6D48F37B78257276
    @Test()
    void or1WhenSetIsNotEmpty() {
        /* Branches:
         * (for-each(set)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        FluentBitSet fluentBitSet = new FluentBitSet();
        FluentBitSet[] fluentBitSetArray = new FluentBitSet[] { fluentBitSet };
        //Act Statement(s)
        FluentBitSet result = target.or(fluentBitSetArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${or2Test}, hash: 391A56C3C83EEF5BC5B007079D292798
    @Test()
    void or2Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        FluentBitSet fluentBitSet = new FluentBitSet();
        //Act Statement(s)
        FluentBitSet result = target.or(fluentBitSet);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${previousClearBitTest}, hash: C59F9392CF820F8D35E83977158EBDBB
    @Test()
    void previousClearBitTest() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        int result = target.previousClearBit(0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${previousSetBitTest}, hash: 0B3A2BAB34571BBF0092FBCE1417F937
    @Test()
    void previousSetBitTest() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        int result = target.previousSetBit(0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${setWhenBitIndexArrayIsNotEmpty}, hash: EA113116546428EE3CAF992F5C4C908D
    @Test()
    void setWhenBitIndexArrayIsNotEmpty() {
        /* Branches:
         * (for-each(bitIndexArray)) : true
         */
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        int[] intArray = new int[] { 0 };
        //Act Statement(s)
        FluentBitSet result = target.set(intArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${set1Test}, hash: C390638A87201F5CC624BC366B5BB8C5
    @Test()
    void set1Test() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        FluentBitSet result = target.set(0);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${set2Test}, hash: 1267175D3507EA91279B1FD62A8ACD2B
    @Test()
    void set2Test() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        FluentBitSet result = target.set(0, false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${set3Test}, hash: 27CCD62300A8FD5EDF961C5EF6B56300
    @Test()
    void set3Test() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        FluentBitSet result = target.set(1, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${set4Test}, hash: F008B2048A44C9E46C387E1890103A7D
    @Test()
    void set4Test() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        FluentBitSet result = target.set(1, 1, false);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${setInclusiveTest}, hash: F6DBB129D139A7D4F54F90F973E45FA5
    @Test()
    void setInclusiveTest() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        FluentBitSet result = target.setInclusive(1, 1);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${sizeTest}, hash: A685B14A931666C73FC9840A5D88AE0C
    @Disabled()
    @Test()
    void sizeTest() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        int result = target.size();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${streamTest}, hash: E8E41A9AB06D11CEBDE88490396357CE
    @Test()
    void streamTest() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        IntStream result = target.stream();
        //Assert statement(s)
        //TODO: Please implement equals method in IntStream for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${toByteArrayTest}, hash: 1FC78626C34E5DAC9B627AD7BDCC506B
    @Test()
    void toByteArrayTest() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        byte[] result = target.toByteArray();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toLongArrayTest}, hash: F9A46FCA1F7D2F4164D1C3D32020BC11
    @Test()
    void toLongArrayTest() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        long[] result = target.toLongArray();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toStringTest}, hash: 2EE1883278206DA4B977E5EDAFBFE88E
    @Test()
    void toStringTest() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        //Act Statement(s)
        String result = target.toString();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("{}")));
    }

    //BaseRock generated method id: ${xorTest}, hash: D0059AC99F58032548DE7FAE824E146B
    @Test()
    void xorTest() {
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        BitSet bitSet2 = new BitSet();
        //Act Statement(s)
        FluentBitSet result = target.xor(bitSet2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${xor1Test}, hash: EC9B4ACEFE92C59E6D4D7CB8FA538B4F
    @Test()
    void xor1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        BitSet bitSet = new BitSet();
        FluentBitSet target = new FluentBitSet(bitSet);
        FluentBitSet fluentBitSet = new FluentBitSet();
        //Act Statement(s)
        FluentBitSet result = target.xor(fluentBitSet);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }
}

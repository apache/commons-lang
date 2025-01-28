package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.hamcrest.Matchers.is;
import org.junit.jupiter.api.Disabled;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class CharSetBaseRockGeneratedTest {

    //BaseRock generated method id: ${getInstanceWhenSetStrsIsNull}, hash: 8AD5FBBBBBBD80DCB4DFA7CDB9BFAE72
    @Test()
    void getInstanceWhenSetStrsIsNull() {
        /* Branches:
         * (setStrs == null) : true
         */
        //Arrange Statement(s)
        String[] string = null;
        //Act Statement(s)
        CharSet result = CharSet.getInstance(string);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getInstanceWhenCommonIsNotNull}, hash: A40D7DC475BB4F71BFF8F8CA4A0AC48B
    @Disabled()
    @Test()
    void getInstanceWhenCommonIsNotNull() {
        /* Branches:
         * (setStrs == null) : false
         * (setStrs.length == 1) : true
         * (common != null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        String[] stringArray = new String[] { "A" };
        //Act Statement(s)
        CharSet result = CharSet.getInstance(stringArray);
        String[] stringArray2 = new String[] {};
        CharSet charSet = CharSet.getInstance(stringArray2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(charSet)));
    }

    //BaseRock generated method id: ${getInstanceWhenCommonIsNull}, hash: A8DBAD70F2DDCAFDB8B2C5057A265C40
    @Test()
    void getInstanceWhenCommonIsNull() {
        /* Branches:
         * (setStrs == null) : false
         * (setStrs.length == 1) : true
         * (common != null) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        String[] stringArray = new String[] { "A" };
        //Act Statement(s)
        CharSet result = CharSet.getInstance(stringArray);
        CharSet charSet = new CharSet(stringArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(charSet)));
    }

    //BaseRock generated method id: ${addWhenStrCharAtPosPlus1Equals___}, hash: 1E90CCCD0FD38F8714BC4B63836D035A
    @Test()
    void addWhenStrCharAtPosPlus1Equals___() {
        /* Branches:
         * (str == null) : false
         * (pos < len) : true
         * (remainder >= 4) : false
         * (remainder >= 3) : true
         * (str.charAt(pos + 1) == '-') : true
         */
        //Arrange Statement(s)
        String[] stringArray = new String[] {};
        CharSet target = new CharSet(stringArray);
        //Act Statement(s)
        target.add("0-9");
    }

    //BaseRock generated method id: ${containsTest}, hash: 5F8A947BF0E72E9D974471FE4D6EF569
    @Test()
    void containsTest() {
        //Arrange Statement(s)
        String[] stringArray = new String[] {};
        CharSet target = new CharSet(stringArray);
        //Act Statement(s)
        boolean result = target.contains('A');
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equalsWhenObjEqualsThis}, hash: 7C6AF3C23407EE0156DA71981E192077
    @Test()
    void equalsWhenObjEqualsThis() {
        /* Branches:
         * (obj == this) : true
         */
        //Arrange Statement(s)
        String[] stringArray = new String[] {};
        CharSet target = new CharSet(stringArray);
        //Act Statement(s)
        boolean result = target.equals(target);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsWhenObjNotInstanceOfCharSet}, hash: 6B4D7E7FDF09017C28404768F7513875
    @Test()
    void equalsWhenObjNotInstanceOfCharSet() {
        /* Branches:
         * (obj == this) : false
         * (!(obj instanceof CharSet)) : true
         */
        //Arrange Statement(s)
        String[] stringArray = new String[] {};
        CharSet target = new CharSet(stringArray);
        Object object = new Object();
        //Act Statement(s)
        boolean result = target.equals(object);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equalsWhenObjInstanceOfCharSet}, hash: 8A10C81020132AEC432415678F76C51B
    @Disabled()
    @Test()
    void equalsWhenObjInstanceOfCharSet() {
        /* Branches:
         * (obj == this) : false
         * (!(obj instanceof CharSet)) : false
         */
        //Arrange Statement(s)
        String[] stringArray = new String[] {};
        CharSet target = new CharSet(stringArray);
        String[] stringArray2 = new String[] {};
        CharSet charSet = CharSet.getInstance(stringArray2);
        //Act Statement(s)
        boolean result = target.equals(charSet);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${getCharRangesTest}, hash: 89F9986D145F42474BFEB5F056CE026A
    @Test()
    void getCharRangesTest() {
        //Arrange Statement(s)
        String[] stringArray = new String[] {};
        CharSet target = new CharSet(stringArray);
        //Act Statement(s)
        CharRange[] result = target.getCharRanges();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toStringTest}, hash: 9987D55C1A54A22B336EFCA0CB548D97
    @Test()
    void toStringTest() {
        //Arrange Statement(s)
        String[] stringArray = new String[] {};
        CharSet target = new CharSet(stringArray);
        //Act Statement(s)
        String result = target.toString();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("[]")));
    }
}

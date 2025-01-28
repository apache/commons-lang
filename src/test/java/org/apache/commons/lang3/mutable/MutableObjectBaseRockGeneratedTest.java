package org.apache.commons.lang3.mutable;

import org.apache.commons.lang3.mutable.MutableObject;

import org.junit.jupiter.api.Timeout;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import java.io.Serializable;
import org.junit.jupiter.api.Test;
import static org.hamcrest.Matchers.equalTo;
import java.util.Objects;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class MutableObjectBaseRockGeneratedTest {

    //BaseRock generated method id: ${equalsWhenObjIsNull}, hash: 2D3946EB987CC6E0EEF2EFFF443B9412
    @Test()
    void equalsWhenObjIsNull() {
        /* Branches:
         * (obj == null) : true
         */
        //Arrange Statement(s)
        Object object = new Object();
        MutableObject target = new MutableObject(object);
        Object object2 = null;
        //Act Statement(s)
        boolean result = target.equals(object2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equalsWhenThisEqualsObj}, hash: 9CD3442E1C845527A562B2461F74D9FC
    @Test()
    void equalsWhenThisEqualsObj() {
        /* Branches:
         * (obj == null) : false
         * (this == obj) : true
         */
        //Arrange Statement(s)
        Object object = new Object();
        MutableObject target = new MutableObject(object);
        //Act Statement(s)
        boolean result = target.equals(target);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsWhenThisGetClassNotEqualsObjGetClass}, hash: D1A3337BBC6C4C02C9A6C087DF5AA0AA
    @Test()
    void equalsWhenThisGetClassNotEqualsObjGetClass() {
        /* Branches:
         * (obj == null) : false
         * (this == obj) : false
         * (this.getClass() == obj.getClass()) : false
         */
        //Arrange Statement(s)
        Object object = new Object();
        MutableObject target = new MutableObject(object);
        Object object2 = new Object();
        //Act Statement(s)
        boolean result = target.equals(object2);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equalsWhenObjectsEqualsThisValueThatValue}, hash: 32A18E89CA9C0A9F84BB4ABC2D0B9173
    @Test()
    void equalsWhenObjectsEqualsThisValueThatValue() {
        /* Branches:
         * (obj == null) : false
         * (this == obj) : false
         * (this.getClass() == obj.getClass()) : true
         * (Objects.equals(this.value, that.value)) : true
         */
        //Arrange Statement(s)
        Object object = new Object();
        MutableObject<Object> target = new MutableObject<>(object);
        MutableObject<Object> mutableObject = new MutableObject<>();
        mutableObject.setValue(object);
        //Act Statement(s)
        boolean result = target.equals(mutableObject);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsWhenObjectsNotEqualsThisValueThatValue}, hash: D828C52CC4EC6F96C028807D7D4206EA
    @Test()
    void equalsWhenObjectsNotEqualsThisValueThatValue() {
        /* Branches:
         * (obj == null) : false
         * (this == obj) : false
         * (this.getClass() == obj.getClass()) : true
         * (Objects.equals(this.value, that.value)) : false
         */
        //Arrange Statement(s)
        Object object = new Object();
        MutableObject<Object> target = new MutableObject<>(object);
        Object object2 = new Object();
        MutableObject<Object> mutableObject = new MutableObject<>();
        mutableObject.setValue(object2);
        //Act Statement(s)
        boolean result = target.equals(mutableObject);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${getValueTest}, hash: F2DF96A6159094475A92807417CEACD6
    @Test()
    void getValueTest() {
        //Arrange Statement(s)
        Object object = new Object();
        MutableObject target = new MutableObject(object);
        //Act Statement(s)
        Object result = target.getValue();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(object)));
    }

    //BaseRock generated method id: ${setValueTest}, hash: 77F315CAB4EB19D26841209519C7612C
    @Test()
    void setValueTest() {
        //Arrange Statement(s)
        Object object = new Object();
        MutableObject target = new MutableObject(object);
        Object object2 = new Object();
        //Act Statement(s)
        target.setValue(object2);
        //Assert statement(s)
        //TODO: Please implement equals method in Object for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(target.getValue(), is(notNullValue())));
    }

    //BaseRock generated method id: ${toStringTest}, hash: 91F4594E8530FC4B1813C21469630BA7
    @Test()
    void toStringTest() {
        //Arrange Statement(s)
        Object object = new Object();
        MutableObject target = new MutableObject(object);
        //Act Statement(s)
        String result = target.toString();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(object.toString())));
    }
}

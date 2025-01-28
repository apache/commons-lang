package org.apache.commons.lang3.tuple;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import java.util.Map;
import java.util.AbstractMap;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mockStatic;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class MutablePairBaseRockGeneratedTest {

    //BaseRock generated method id: ${emptyArray1Test}, hash: AF28B70B9728B9C53B5FD23823B2B200
    @Test()
    void emptyArray1Test() {
        
        //Act Statement(s)
        MutablePair<Object, Object>[] result = MutablePair.emptyArray();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${of2Test}, hash: 9E0D717E44EF1704212DCA852FBAF580
    @Test()
    void of2Test() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        
        //Act Statement(s)
        MutablePair<Object, Object> result = MutablePair.of(object, object2);
        MutablePair<Object, Object> mutablePair = new MutablePair<>(object, object2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(mutablePair)));
    }

    //BaseRock generated method id: ${of3WhenPairIsNotNull}, hash: 8A09086F950F5E3A0C215804C10D6821
    @Test()
    void of3WhenPairIsNotNull() {
        /* Branches:
         * (pair != null) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        
        //Act Statement(s)
        MutablePair<Object, Object> result = MutablePair.of(new AbstractMap.SimpleEntry<>(object, object2));
        MutablePair<Object, Object> mutablePair = new MutablePair<>(object, object2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(mutablePair)));
    }

    //BaseRock generated method id: ${of3WhenPairIsNull}, hash: C8B645FAB8DBAA54BB7BBE8345C64796
    @Test()
    void of3WhenPairIsNull() {
        /* Branches:
         * (pair != null) : false
         */
         //Arrange Statement(s)
        Map.Entry<Object, Object> mapEntry = null;
        
        //Act Statement(s)
        MutablePair<Object, Object> result = MutablePair.of(mapEntry);
        MutablePair<Object, Object> mutablePair = new MutablePair<>((Object) null, (Object) null);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(mutablePair)));
    }

    //BaseRock generated method id: ${ofNonNull1Test}, hash: 14A8B5829A157642411BE07C635B7AA9
    @Test()
    void ofNonNull1Test() {
        //Arrange Statement(s)
        try (MockedStatic<MutablePair> mutablePair = mockStatic(MutablePair.class, CALLS_REAL_METHODS)) {
            MutablePair<Object, Object> mutablePair2 = new MutablePair<>();
            Object object = new Object();
            Object object2 = new Object();
            mutablePair.when(() -> MutablePair.of(object, object2)).thenReturn(mutablePair2);
            //Act Statement(s)
            MutablePair<Object, Object> result = MutablePair.ofNonNull(object, object2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(mutablePair2));
                mutablePair.verify(() -> MutablePair.of(object, object2), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getLeftTest}, hash: EFEDD27789F04D2A9DA33A918680A1A6
    @Test()
    void getLeftTest() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        MutablePair target = new MutablePair(object, object2);
        
        //Act Statement(s)
        Object result = target.getLeft();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(object)));
    }

    //BaseRock generated method id: ${getRightTest}, hash: D87459F899D110FC8864B866925BE433
    @Test()
    void getRightTest() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        MutablePair target = new MutablePair(object, object2);
        
        //Act Statement(s)
        Object result = target.getRight();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(object2)));
    }

    //BaseRock generated method id: ${setLeftTest}, hash: B60D09012BEDB0EADD5C080A56BEAC61
    @Test()
    void setLeftTest() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        MutablePair target = new MutablePair(object, object2);
        Object object3 = new Object();
        
        //Act Statement(s)
        target.setLeft(object3);
    }

    //BaseRock generated method id: ${setRightTest}, hash: DD7940C15BA136BC82EA9A9DFE7C7A67
    @Test()
    void setRightTest() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        MutablePair target = new MutablePair(object, object2);
        Object object3 = new Object();
        
        //Act Statement(s)
        target.setRight(object3);
    }

    //BaseRock generated method id: ${setValueTest}, hash: F7A818C339BAF5E9C1DBBDEF63F400E5
    @Test()
    void setValueTest() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        MutablePair target = new MutablePair(object, object2);
        Object object3 = new Object();
        
        //Act Statement(s)
        Object result = target.setValue(object3);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(object2)));
    }
}

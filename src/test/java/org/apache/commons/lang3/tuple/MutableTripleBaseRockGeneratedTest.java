package org.apache.commons.lang3.tuple;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mockStatic;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class MutableTripleBaseRockGeneratedTest {

    //BaseRock generated method id: ${emptyArray1Test}, hash: D143AA1EF97798AC6B551A014802E0E4
    @Test()
    void emptyArray1Test() {
        
        //Act Statement(s)
        MutableTriple<Object, Object, Object>[] result = MutableTriple.emptyArray();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${of1Test}, hash: E94A576578C0A68A59767861525BC7C7
    @Test()
    void of1Test() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        Object object3 = new Object();
        
        //Act Statement(s)
        MutableTriple<Object, Object, Object> result = MutableTriple.of(object, object2, object3);
        MutableTriple<Object, Object, Object> mutableTriple = new MutableTriple<>(object, object2, object3);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(mutableTriple)));
    }

    //BaseRock generated method id: ${ofNonNull1Test}, hash: F9BEAA6B5A63728E23BD5EF6623C7A14
    @Test()
    void ofNonNull1Test() {
        //Arrange Statement(s)
        try (MockedStatic<MutableTriple> mutableTriple = mockStatic(MutableTriple.class, CALLS_REAL_METHODS)) {
            MutableTriple<Object, Object, Object> mutableTriple2 = new MutableTriple<>();
            Object object = new Object();
            Object object2 = new Object();
            Object object3 = new Object();
            mutableTriple.when(() -> MutableTriple.of(object, object2, object3)).thenReturn(mutableTriple2);
            //Act Statement(s)
            MutableTriple<Object, Object, Object> result = MutableTriple.ofNonNull(object, object2, object3);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(mutableTriple2));
                mutableTriple.verify(() -> MutableTriple.of(object, object2, object3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getLeftTest}, hash: E3A6BDEBBE82FB57F9521F0F8ADD6133
    @Test()
    void getLeftTest() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        Object object3 = new Object();
        MutableTriple target = new MutableTriple(object, object2, object3);
        
        //Act Statement(s)
        Object result = target.getLeft();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(object)));
    }

    //BaseRock generated method id: ${getMiddleTest}, hash: 8FB4FB041F9AA35900BF81592DC88718
    @Test()
    void getMiddleTest() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        Object object3 = new Object();
        MutableTriple target = new MutableTriple(object, object2, object3);
        
        //Act Statement(s)
        Object result = target.getMiddle();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(object2)));
    }

    //BaseRock generated method id: ${getRightTest}, hash: EEE1C92F8902C22E4D851850FC494EB7
    @Test()
    void getRightTest() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        Object object3 = new Object();
        MutableTriple target = new MutableTriple(object, object2, object3);
        
        //Act Statement(s)
        Object result = target.getRight();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(object3)));
    }

    //BaseRock generated method id: ${setLeftTest}, hash: 79D6116807E09ABDE3AA5B7C7E2AE46F
    @Test()
    void setLeftTest() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        Object object3 = new Object();
        MutableTriple target = new MutableTriple(object, object2, object3);
        Object object4 = new Object();
        
        //Act Statement(s)
        target.setLeft(object4);
    }

    //BaseRock generated method id: ${setMiddleTest}, hash: 16A90B51CF108CDB0F4164907313E3C9
    @Test()
    void setMiddleTest() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        Object object3 = new Object();
        MutableTriple target = new MutableTriple(object, object2, object3);
        Object object4 = new Object();
        
        //Act Statement(s)
        target.setMiddle(object4);
    }

    //BaseRock generated method id: ${setRightTest}, hash: BE47F39EC87EF7FF05E512A2E2A305E6
    @Test()
    void setRightTest() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        Object object3 = new Object();
        MutableTriple target = new MutableTriple(object, object2, object3);
        Object object4 = new Object();
        
        //Act Statement(s)
        target.setRight(object4);
    }
}

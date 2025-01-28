package org.apache.commons.lang3.tuple;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import java.util.Map;
import java.util.AbstractMap;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mockStatic;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class ImmutablePairBaseRockGeneratedTest {

    //BaseRock generated method id: ${emptyArray1Test}, hash: 6B959644927C175CA3786745A4779C6A
    @Test()
    void emptyArray1Test() {
        
        //Act Statement(s)
        ImmutablePair<Object, Object>[] result = ImmutablePair.emptyArray();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${leftTest}, hash: FD831A01FA60F1B24754A77FA955D452
    @Test()
    void leftTest() {
        //Arrange Statement(s)
        try (MockedStatic<ImmutablePair> immutablePair = mockStatic(ImmutablePair.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            ImmutablePair immutablePair2 = new ImmutablePair(object, object2);
            Object object3 = new Object();
            immutablePair.when(() -> ImmutablePair.of(object3, (Object) null)).thenReturn(immutablePair2);
            //Act Statement(s)
            Pair<Object, Object> result = ImmutablePair.left(object3);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(immutablePair2));
                immutablePair.verify(() -> ImmutablePair.of(object3, (Object) null), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nullPairTest}, hash: 46A3F4576FD6F330D5DBE2E3BB15679E
    @Test()
    void nullPairTest() {
        
        //Act Statement(s)
        ImmutablePair<Object, Object> result = ImmutablePair.nullPair();
        ImmutablePair<Object, Object> immutablePair = new ImmutablePair<>((Object) null, (Object) null);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(immutablePair)));
    }

    //BaseRock generated method id: ${of2WhenRightIsNotNull}, hash: 642A25FE9F0ABCB49E24EFFA8C39B06A
    @Test()
    void of2WhenRightIsNotNull() {
        /* Branches:
         * (left != null) : false
         * (right != null) : true
         */
         //Arrange Statement(s)
        Object object = null;
        Object object2 = new Object();
        
        //Act Statement(s)
        ImmutablePair<Object, Object> result = ImmutablePair.of(object, object2);
        ImmutablePair<Object, Object> immutablePair = new ImmutablePair<>((Object) null, object2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(immutablePair)));
    }

    //BaseRock generated method id: ${of2WhenRightIsNull}, hash: 827931EF66E47E045776535714B7F7EE
    @Test()
    void of2WhenRightIsNull() {
        /* Branches:
         * (left != null) : false
         * (right != null) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<ImmutablePair> immutablePair = mockStatic(ImmutablePair.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            ImmutablePair immutablePair2 = new ImmutablePair(object, object2);
            immutablePair.when(() -> ImmutablePair.nullPair()).thenReturn(immutablePair2);
            Object object3 = null;
            Object object4 = null;
            //Act Statement(s)
            ImmutablePair<Object, Object> result = ImmutablePair.of(object3, object4);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(immutablePair2));
                immutablePair.verify(() -> ImmutablePair.nullPair(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${of3WhenPairIsNotNull}, hash: 8EBC51D783A0BA7DC6AD70CB7D573FAD
    @Test()
    void of3WhenPairIsNotNull() {
        /* Branches:
         * (pair != null) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        
        //Act Statement(s)
        ImmutablePair<Object, Object> result = ImmutablePair.of(new AbstractMap.SimpleEntry<>(object, object2));
        ImmutablePair<Object, Object> immutablePair = new ImmutablePair<>(object, object2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(immutablePair)));
    }

    //BaseRock generated method id: ${of3WhenPairIsNull}, hash: 4A206FBDE23FEE460EBA01AF05046D47
    @Test()
    void of3WhenPairIsNull() {
        /* Branches:
         * (pair != null) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<ImmutablePair> immutablePair = mockStatic(ImmutablePair.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            ImmutablePair immutablePair2 = new ImmutablePair(object, object2);
            immutablePair.when(() -> ImmutablePair.nullPair()).thenReturn(immutablePair2);
            Map.Entry<Object, Object> mapEntry = null;
            //Act Statement(s)
            ImmutablePair<Object, Object> result = ImmutablePair.of(mapEntry);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(immutablePair2));
                immutablePair.verify(() -> ImmutablePair.nullPair(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${ofNonNull1Test}, hash: DAE0A6285829E2E0920195245D299847
    @Test()
    void ofNonNull1Test() {
        //Arrange Statement(s)
        try (MockedStatic<ImmutablePair> immutablePair = mockStatic(ImmutablePair.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            ImmutablePair immutablePair2 = new ImmutablePair(object, object2);
            Object object3 = new Object();
            Object object4 = new Object();
            immutablePair.when(() -> ImmutablePair.of(object3, object4)).thenReturn(immutablePair2);
            //Act Statement(s)
            ImmutablePair<Object, Object> result = ImmutablePair.ofNonNull(object3, object4);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(immutablePair2));
                immutablePair.verify(() -> ImmutablePair.of(object3, object4), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${rightTest}, hash: 48A58B8630464BE985838E00BF9C7EDB
    @Test()
    void rightTest() {
        //Arrange Statement(s)
        try (MockedStatic<ImmutablePair> immutablePair = mockStatic(ImmutablePair.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            ImmutablePair immutablePair2 = new ImmutablePair(object, object2);
            Object object3 = new Object();
            immutablePair.when(() -> ImmutablePair.of((Object) null, object3)).thenReturn(immutablePair2);
            //Act Statement(s)
            Pair<Object, Object> result = ImmutablePair.right(object3);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(immutablePair2));
                immutablePair.verify(() -> ImmutablePair.of((Object) null, object3), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getLeftTest}, hash: 5BFEBAC17FB06F4850364B329D5B568F
    @Test()
    void getLeftTest() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        ImmutablePair target = new ImmutablePair(object, object2);
        
        //Act Statement(s)
        Object result = target.getLeft();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(object)));
    }

    //BaseRock generated method id: ${getRightTest}, hash: B3CA89A346CFC3215643895D870AB690
    @Test()
    void getRightTest() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        ImmutablePair target = new ImmutablePair(object, object2);
        
        //Act Statement(s)
        Object result = target.getRight();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(object2)));
    }

    //BaseRock generated method id: ${setValueThrowsUnsupportedOperationException}, hash: 355C277FFC5215DA1712775219EAE809
    @Test()
    void setValueThrowsUnsupportedOperationException() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        ImmutablePair target = new ImmutablePair(object, object2);
        Object object3 = new Object();
        //Act Statement(s)
        final UnsupportedOperationException result = assertThrows(UnsupportedOperationException.class, () -> {
            target.setValue(object3);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }
}

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
class ImmutableTripleBaseRockGeneratedTest {

    //BaseRock generated method id: ${emptyArray1Test}, hash: 7869FA39E09CD16EC2DB983F9755D6FA
    @Test()
    void emptyArray1Test() {
        
        //Act Statement(s)
        ImmutableTriple<Object, Object, Object>[] result = ImmutableTriple.emptyArray();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${nullTripleTest}, hash: 0A80AA65C3D91402E4308C39D34B4671
    @Test()
    void nullTripleTest() {
        
        //Act Statement(s)
        ImmutableTriple<Object, Object, Object> result = ImmutableTriple.nullTriple();
        ImmutableTriple<Object, Object, Object> immutableTriple = new ImmutableTriple<>((Object) null, (Object) null, (Object) null);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(immutableTriple)));
    }

    //BaseRock generated method id: ${of1WhenRightIsNotNull}, hash: 0C41C98732E2A57197294DC3A0ED1732
    @Test()
    void of1WhenRightIsNotNull() {
        /* Branches:
         * (left != null) : true
         * (middle != null) : true
         * (right != null) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        Object object3 = new Object();
        
        //Act Statement(s)
        ImmutableTriple<Object, Object, Object> result = ImmutableTriple.of(object, object2, object3);
        ImmutableTriple<Object, Object, Object> immutableTriple = new ImmutableTriple<>(object, object2, object3);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(immutableTriple)));
    }

    //BaseRock generated method id: ${of1WhenRightIsNull}, hash: 715F1C1A6CE5F4D4D38B4A47BAA81F44
    @Test()
    void of1WhenRightIsNull() {
        /* Branches:
         * (left != null) : false
         * (middle != null) : false
         * (right != null) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<ImmutableTriple> immutableTriple = mockStatic(ImmutableTriple.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            Object object3 = new Object();
            ImmutableTriple immutableTriple2 = new ImmutableTriple(object, object2, object3);
            immutableTriple.when(() -> ImmutableTriple.nullTriple()).thenReturn(immutableTriple2);
            Object object4 = null;
            Object object5 = null;
            Object object6 = null;
            //Act Statement(s)
            ImmutableTriple<Object, Object, Object> result = ImmutableTriple.of(object4, object5, object6);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(immutableTriple2));
                immutableTriple.verify(() -> ImmutableTriple.nullTriple(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${ofNonNull1Test}, hash: E64075F517C8C24B62AF46B5D4EB6914
    @Test()
    void ofNonNull1Test() {
        //Arrange Statement(s)
        try (MockedStatic<ImmutableTriple> immutableTriple = mockStatic(ImmutableTriple.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            Object object2 = new Object();
            Object object3 = new Object();
            ImmutableTriple immutableTriple2 = new ImmutableTriple(object, object2, object3);
            Object object4 = new Object();
            Object object5 = new Object();
            Object object6 = new Object();
            immutableTriple.when(() -> ImmutableTriple.of(object4, object5, object6)).thenReturn(immutableTriple2);
            //Act Statement(s)
            ImmutableTriple<Object, Object, Object> result = ImmutableTriple.ofNonNull(object4, object5, object6);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(immutableTriple2));
                immutableTriple.verify(() -> ImmutableTriple.of(object4, object5, object6), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getLeftTest}, hash: C08DC6A6F6AEC3E2A5CDA74C790DFD43
    @Test()
    void getLeftTest() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        Object object3 = new Object();
        ImmutableTriple target = new ImmutableTriple(object, object2, object3);
        
        //Act Statement(s)
        Object result = target.getLeft();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(object)));
    }

    //BaseRock generated method id: ${getMiddleTest}, hash: A445989B616D144FE65988C43C0A3F1B
    @Test()
    void getMiddleTest() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        Object object3 = new Object();
        ImmutableTriple target = new ImmutableTriple(object, object2, object3);
        
        //Act Statement(s)
        Object result = target.getMiddle();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(object2)));
    }

    //BaseRock generated method id: ${getRightTest}, hash: 77EB7EB4F0A3CDCCA0FABC138D84FC5A
    @Test()
    void getRightTest() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        Object object3 = new Object();
        ImmutableTriple target = new ImmutableTriple(object, object2, object3);
        
        //Act Statement(s)
        Object result = target.getRight();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(object3)));
    }
}

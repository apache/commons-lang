package org.apache.commons.lang3.concurrent;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.mock;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class ConstantInitializerBaseRockGeneratedTest {

    private final ConstantInitializer<?> constantInitializerMock = mock(ConstantInitializer.class);

    //BaseRock generated method id: ${equalsWhenThisEqualsObj}, hash: EE035AABB73149EE23AC0073D93E3953
    @Test()
    void equalsWhenThisEqualsObj() {
        /* Branches:
         * (this == obj) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        ConstantInitializer target = new ConstantInitializer(object);
        
        //Act Statement(s)
        boolean result = target.equals(target);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsWhenObjNotInstanceOfConstantInitializer_}, hash: E188A2F0FAF9A17CBDB248BE6B1B5BE9
    @Test()
    void equalsWhenObjNotInstanceOfConstantInitializer_() {
        /* Branches:
         * (this == obj) : false
         * (!(obj instanceof ConstantInitializer<?>)) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        ConstantInitializer target = new ConstantInitializer(object);
        Object object2 = new Object();
        
        //Act Statement(s)
        boolean result = target.equals(object2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equalsWhenObjectsEqualsGetObjectCGetObject}, hash: 44B6CBA9AC88F047C2F40F624573B4AA
    @Test()
    void equalsWhenObjectsEqualsGetObjectCGetObject() {
        /* Branches:
         * (this == obj) : false
         * (!(obj instanceof ConstantInitializer<?>)) : false
         * (Objects.equals(getObject(), c.getObject())) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ConstantInitializer target = new ConstantInitializer(object);
        
        //Act Statement(s)
        boolean result = target.equals(constantInitializerMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsWhenObjectsNotEqualsGetObjectCGetObject}, hash: D91C125B1F95EC2B696AEEB1E7CB84B6
    @Test()
    void equalsWhenObjectsNotEqualsGetObjectCGetObject() {
        /* Branches:
         * (this == obj) : false
         * (!(obj instanceof ConstantInitializer<?>)) : false
         * (Objects.equals(getObject(), c.getObject())) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ConstantInitializer target = new ConstantInitializer(object);
        
        //Act Statement(s)
        boolean result = target.equals(constantInitializerMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${getTest}, hash: 9D7391D2B52A26EAD842F917E2BD9A12
    @Test()
    void getTest() throws ConcurrentException {
        //Arrange Statement(s)
        Object object = new Object();
        ConstantInitializer target = new ConstantInitializer(object);
        
        //Act Statement(s)
        Object result = target.get();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(object)));
    }

    //BaseRock generated method id: ${getObjectTest}, hash: CBCC9EF7535C2193456915A01CB5F2D4
    @Test()
    void getObjectTest() {
        //Arrange Statement(s)
        Object object = new Object();
        ConstantInitializer target = new ConstantInitializer(object);
        
        //Act Statement(s)
        Object result = target.getObject();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(object)));
    }

    //BaseRock generated method id: ${isInitializedTest}, hash: 529147FBA9F2508389ECFB3CDD911AC4
    @Test()
    void isInitializedTest() {
        //Arrange Statement(s)
        Object object = new Object();
        ConstantInitializer target = new ConstantInitializer(object);
        
        //Act Statement(s)
        boolean result = target.isInitialized();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${toStringTest}, hash: 22ADB9207129BFE643FEF53BC3734768
    @Test()
    void toStringTest() {
        //Arrange Statement(s)
        Object objectMock = mock(Object.class, "obj");
        ConstantInitializer target = new ConstantInitializer(objectMock);
        
        //Act Statement(s)
        String result = target.toString();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }
}

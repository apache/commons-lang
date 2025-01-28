package org.apache.commons.lang3.builder;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.Collection;
import java.util.ArrayList;
import static org.mockito.Mockito.doNothing;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.doReturn;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class RecursiveToStringStyleBaseRockGeneratedTest {

    //BaseRock generated method id: ${acceptTest}, hash: 2405DAB4334949C56F5969D444A18FAA
    @Test()
    void acceptTest() {
        //Arrange Statement(s)
        RecursiveToStringStyle target = new RecursiveToStringStyle();
        
        //Act Statement(s)
        boolean result = target.accept(Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${appendDetail6Test}, hash: 32FADEDF359D3428AAE31CB9EF5D4514
    @Test()
    void appendDetail6Test() {
        //Arrange Statement(s)
        RecursiveToStringStyle target = spy(new RecursiveToStringStyle());
        StringBuffer stringBuffer = new StringBuffer();
        Collection<Object> collection = new ArrayList<>();
        doNothing().when(target).appendClassName(stringBuffer, collection);
        doNothing().when(target).appendIdentityHashCode(stringBuffer, collection);
        Object[] objectArray = new Object[] {};
        doNothing().when(target).appendDetail(stringBuffer, "fieldName1", objectArray);
        
        //Act Statement(s)
        target.appendDetail(stringBuffer, "fieldName1", collection);
        
        //Assert statement(s)
        assertAll("result", () -> {
            verify(target).appendClassName(stringBuffer, collection);
            verify(target).appendIdentityHashCode(stringBuffer, collection);
            verify(target).appendDetail(stringBuffer, "fieldName1", objectArray);
        });
    }

    //BaseRock generated method id: ${appendDetail17WhenAcceptValueGetClass}, hash: B549205825B1E61815CFF72B6B2355C4
    @Test()
    void appendDetail17WhenAcceptValueGetClass() {
        /* Branches:
         * (!ClassUtils.isPrimitiveWrapper(value.getClass())) : true
         * (!String.class.equals(value.getClass())) : true
         * (accept(value.getClass())) : true
         */
         //Arrange Statement(s)
        RecursiveToStringStyle target = spy(new RecursiveToStringStyle());
        doReturn(true).when(target).accept(Object.class);
        StringBuffer stringBuffer = new StringBuffer();
        Object object = new Object();
        
        //Act Statement(s)
        target.appendDetail(stringBuffer, "fieldName1", object);
        
        //Assert statement(s)
        assertAll("result", () -> verify(target).accept(Object.class));
    }

    //BaseRock generated method id: ${appendDetail17WhenAcceptNotValueGetClass}, hash: 1B5C846DE8C11E3CA6511F18F60F2F59
    @Test()
    void appendDetail17WhenAcceptNotValueGetClass() {
        /* Branches:
         * (!ClassUtils.isPrimitiveWrapper(value.getClass())) : true
         * (!String.class.equals(value.getClass())) : true
         * (accept(value.getClass())) : false
         */
         //Arrange Statement(s)
        RecursiveToStringStyle target = spy(new RecursiveToStringStyle());
        doReturn(false).when(target).accept(Object.class);
        StringBuffer stringBuffer = new StringBuffer();
        Object object = new Object();
        
        //Act Statement(s)
        target.appendDetail(stringBuffer, "fieldName1", object);
        
        //Assert statement(s)
        assertAll("result", () -> verify(target).accept(Object.class));
    }
}

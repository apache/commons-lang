package org.apache.commons.lang3.exception;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.List;
import org.apache.commons.lang3.tuple.Pair;
import java.util.Set;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.verify;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.doReturn;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class ContextedRuntimeExceptionBaseRockGeneratedTest {

    //BaseRock generated method id: ${addContextValueTest}, hash: 1EAE7C9B4F4DA0D031634C0A09987291
    @Test()
    void addContextValueTest() {
        //Arrange Statement(s)
        Throwable throwable = new Throwable();
        ContextedRuntimeException target = new ContextedRuntimeException("message1", throwable, (ExceptionContext) null);
        Object object = new Object();
        
        //Act Statement(s)
        ContextedRuntimeException result = target.addContextValue("label1", object);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${getContextEntriesTest}, hash: 19A29FF7915E5EBE142552859D5A8E47
    @Test()
    void getContextEntriesTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: exceptionContext - Method: getContextEntries
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Throwable throwable = new Throwable();
        ContextedRuntimeException target = new ContextedRuntimeException("message1", throwable, (ExceptionContext) null);
        
        //Act Statement(s)
        List<Pair<String, Object>> result = target.getContextEntries();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getContextLabelsTest}, hash: 3CA8F1E830361C9190DC91192777C70F
    @Test()
    void getContextLabelsTest() {
        //Arrange Statement(s)
        Throwable throwable = new Throwable();
        ContextedRuntimeException target = new ContextedRuntimeException("message1", throwable, (ExceptionContext) null);
        
        //Act Statement(s)
        Set<String> result = target.getContextLabels();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getContextValuesTest}, hash: 9DCB4198DC2B99C4CE998054CEC82E54
    @Test()
    void getContextValuesTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: exceptionContext - Method: getContextValues
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Throwable throwable = new Throwable();
        ContextedRuntimeException target = new ContextedRuntimeException("message1", throwable, (ExceptionContext) null);
        
        //Act Statement(s)
        List<Object> result = target.getContextValues("label1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getFirstContextValueTest}, hash: 7B3898E9522A08593F7FDDF2A381CB73
    @Test()
    void getFirstContextValueTest() {
        //Arrange Statement(s)
        Throwable throwable = new Throwable();
        ContextedRuntimeException target = new ContextedRuntimeException("message1", throwable, (ExceptionContext) null);
        
        //Act Statement(s)
        Object result = target.getFirstContextValue("A");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getFormattedExceptionMessageTest}, hash: 3EC036A5E1BB2007C0F6009B99FD01C7
    @Test()
    void getFormattedExceptionMessageTest() {
        //Arrange Statement(s)
        Throwable throwable = new Throwable();
        ContextedRuntimeException target = new ContextedRuntimeException("message1", throwable, (ExceptionContext) null);
        
        //Act Statement(s)
        String result = target.getFormattedExceptionMessage("A");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${getMessageTest}, hash: DD30B75EA520775AC73C7060B21B0563
    @Test()
    void getMessageTest() {
        //Arrange Statement(s)
        Throwable throwable = new Throwable();
        ContextedRuntimeException target = spy(new ContextedRuntimeException("message1", throwable, (ExceptionContext) null));
        doReturn("A").when(target).getMessage();
        
        //Act Statement(s)
        String result = target.getMessage();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("A"));
            verify(target).getMessage();
        });
    }

    //BaseRock generated method id: ${getRawMessageTest}, hash: DE2499A46CDE436438A1794074436923
    @Test()
    void getRawMessageTest() {
        //Arrange Statement(s)
        Throwable throwable = new Throwable();
        ContextedRuntimeException target = spy(new ContextedRuntimeException("message1", throwable, (ExceptionContext) null));
        doReturn("return_of_getMessage1").when(target).getMessage();
        
        //Act Statement(s)
        String result = target.getRawMessage();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_getMessage1"));
            verify(target).getMessage();
        });
    }

    //BaseRock generated method id: ${setContextValueTest}, hash: A3F681C6F183A5D7F4111B6BEBA40309
    @Test()
    void setContextValueTest() {
        //Arrange Statement(s)
        Throwable throwable = new Throwable();
        ContextedRuntimeException target = new ContextedRuntimeException("message1", throwable, (ExceptionContext) null);
        Object object = new Object();
        
        //Act Statement(s)
        ContextedRuntimeException result = target.setContextValue("label1", object);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }
}

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
class ContextedExceptionBaseRockGeneratedTest {

    //BaseRock generated method id: ${addContextValueTest}, hash: 22836226419674AE65F8E7D7F456A3F5
    @Test()
    void addContextValueTest() {
        //Arrange Statement(s)
        Throwable throwable = new Throwable();
        ContextedException target = new ContextedException("message1", throwable, (ExceptionContext) null);
        Object object = new Object();
        
        //Act Statement(s)
        ContextedException result = target.addContextValue("label1", object);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${getContextEntriesTest}, hash: C26509854D17B1F47B319A93F6C0458F
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
        ContextedException target = new ContextedException("message1", throwable, (ExceptionContext) null);
        
        //Act Statement(s)
        List<Pair<String, Object>> result = target.getContextEntries();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getContextLabelsTest}, hash: 30A3F02797CC4DBD1EC3BD6E88D6E9F4
    @Test()
    void getContextLabelsTest() {
        //Arrange Statement(s)
        Throwable throwable = new Throwable();
        ContextedException target = new ContextedException("message1", throwable, (ExceptionContext) null);
        
        //Act Statement(s)
        Set<String> result = target.getContextLabels();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getContextValuesTest}, hash: 99D6A7FBBDE982E86DC77C3946ED877E
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
        ContextedException target = new ContextedException("message1", throwable, (ExceptionContext) null);
        
        //Act Statement(s)
        List<Object> result = target.getContextValues("label1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getFirstContextValueTest}, hash: 20D4DC05A804EF2162B897DCF2C37BE9
    @Test()
    void getFirstContextValueTest() {
        //Arrange Statement(s)
        Throwable throwable = new Throwable();
        ContextedException target = new ContextedException("message1", throwable, (ExceptionContext) null);
        
        //Act Statement(s)
        Object result = target.getFirstContextValue("A");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getFormattedExceptionMessageTest}, hash: 3DFBA8AF885B43A37467BB13426AF830
    @Test()
    void getFormattedExceptionMessageTest() {
        //Arrange Statement(s)
        Throwable throwable = new Throwable();
        ContextedException target = new ContextedException("message1", throwable, (ExceptionContext) null);
        
        //Act Statement(s)
        String result = target.getFormattedExceptionMessage("A");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${getMessageTest}, hash: A58E03B4537721FED4E9A8A14129CD8C
    @Test()
    void getMessageTest() {
        //Arrange Statement(s)
        Throwable throwable = new Throwable();
        ContextedException target = spy(new ContextedException("message1", throwable, (ExceptionContext) null));
        doReturn("A").when(target).getMessage();
        
        //Act Statement(s)
        String result = target.getMessage();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("A"));
            verify(target).getMessage();
        });
    }

    //BaseRock generated method id: ${getRawMessageTest}, hash: DBBBC95701F360E068DDEBD1E056332D
    @Test()
    void getRawMessageTest() {
        //Arrange Statement(s)
        Throwable throwable = new Throwable();
        ContextedException target = spy(new ContextedException("message1", throwable, (ExceptionContext) null));
        doReturn("return_of_getMessage1").when(target).getMessage();
        
        //Act Statement(s)
        String result = target.getRawMessage();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_getMessage1"));
            verify(target).getMessage();
        });
    }

    //BaseRock generated method id: ${setContextValueTest}, hash: 2D1FEF8AB2FA9AAB6FD7F1140614113B
    @Test()
    void setContextValueTest() {
        //Arrange Statement(s)
        Throwable throwable = new Throwable();
        ContextedException target = new ContextedException("message1", throwable, (ExceptionContext) null);
        Object object = new Object();
        
        //Act Statement(s)
        ContextedException result = target.setContextValue("label1", object);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }
}

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
class DefaultExceptionContextBaseRockGeneratedTest {

    //BaseRock generated method id: ${addContextValueTest}, hash: F319F6C1D43930060380044FDBC3322D
    @Test()
    void addContextValueTest() {
        //Arrange Statement(s)
        DefaultExceptionContext target = new DefaultExceptionContext();
        Object object = new Object();
        
        //Act Statement(s)
        DefaultExceptionContext result = target.addContextValue("label1", object);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${getContextEntriesTest}, hash: BA120F87ECC74BC5023CF651C3AB0F9F
    @Test()
    void getContextEntriesTest() {
        //Arrange Statement(s)
        DefaultExceptionContext target = new DefaultExceptionContext();
        
        //Act Statement(s)
        List<Pair<String, Object>> result = target.getContextEntries();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getContextLabelsTest}, hash: B0286A4C2CCA979964165D166DC4133F
    @Test()
    void getContextLabelsTest() {
        //Arrange Statement(s)
        DefaultExceptionContext target = new DefaultExceptionContext();
        
        //Act Statement(s)
        Set<String> result = target.getContextLabels();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getContextValuesTest}, hash: 1577EE49C0B6E438DB636E0D7CAACCC6
    @Test()
    void getContextValuesTest() {
        //Arrange Statement(s)
        DefaultExceptionContext target = new DefaultExceptionContext();
        
        //Act Statement(s)
        List<Object> result = target.getContextValues("label1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${getFirstContextValueTest}, hash: 8C1A610868961495A283F05491948D00
    @Test()
    void getFirstContextValueTest() {
        //Arrange Statement(s)
        DefaultExceptionContext target = new DefaultExceptionContext();
        
        //Act Statement(s)
        Object result = target.getFirstContextValue("label1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getFormattedExceptionMessageWhenContextValuesIsEmpty}, hash: B7E6B0D5E7DEF157E40E561F687D0C57
    @Test()
    void getFormattedExceptionMessageWhenContextValuesIsEmpty() {
        /* Branches:
         * (baseMessage != null) : true
         * (!contextValues.isEmpty()) : false
         */
         //Arrange Statement(s)
        DefaultExceptionContext target = new DefaultExceptionContext();
        
        //Act Statement(s)
        String result = target.getFormattedExceptionMessage("A");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${setContextValueTest}, hash: C0192A3804597D87CCF1F4D3F8EEA522
    @Test()
    void setContextValueTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        DefaultExceptionContext target = spy(new DefaultExceptionContext());
        DefaultExceptionContext defaultExceptionContext = new DefaultExceptionContext();
        Object object = new Object();
        doReturn(defaultExceptionContext).when(target).addContextValue("label1", object);
        
        //Act Statement(s)
        DefaultExceptionContext result = target.setContextValue("label1", object);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).addContextValue("label1", object);
        });
    }
}

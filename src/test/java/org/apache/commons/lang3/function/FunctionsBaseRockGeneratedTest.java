package org.apache.commons.lang3.function;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.function.Function;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doReturn;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class FunctionsBaseRockGeneratedTest {

    private final Function<Object, Object> functionMock = mock(Function.class);

    //BaseRock generated method id: ${applyWhenFunctionIsNotNull}, hash: 1067346CDD1E8A42A43F5D1179C19ED8
    @Test()
    void applyWhenFunctionIsNotNull() {
        /* Branches:
         * (function != null) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        doReturn(object).when(functionMock).apply(object2);
        
        //Act Statement(s)
        Object result = Functions.apply(functionMock, object2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(object));
            verify(functionMock).apply(object2);
        });
    }

    //BaseRock generated method id: ${applyWhenFunctionIsNull}, hash: 1DE0E7B8EB0B2E4F5B982206F9232E07
    @Test()
    void applyWhenFunctionIsNull() {
        /* Branches:
         * (function != null) : false
         */
         //Arrange Statement(s)
        Function<Object, Object> function = null;
        Object object = new Object();
        
        //Act Statement(s)
        Object result = Functions.apply(function, object);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${functionTest}, hash: AB895AA2302D9233586EA1F5094E8E24
    @Test()
    void functionTest() {
        
        //Act Statement(s)
        Function<Object, Object> result = Functions.function(functionMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(functionMock)));
    }
}

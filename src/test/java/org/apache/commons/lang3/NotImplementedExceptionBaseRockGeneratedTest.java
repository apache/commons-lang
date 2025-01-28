package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertAll;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class NotImplementedExceptionBaseRockGeneratedTest {

    //BaseRock generated method id: ${getCodeTest}, hash: 9ABFFC324B970794A45EDD8D0FB3E3FE
    @Test()
    void getCodeTest() {
        //Arrange Statement(s)
        Throwable throwable = new Throwable();
        NotImplementedException target = new NotImplementedException("message1", throwable, "code1");
        
        //Act Statement(s)
        String result = target.getCode();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("code1")));
    }
}

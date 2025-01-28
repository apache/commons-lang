package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import org.apache.commons.lang3.arch.Processor;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.hamcrest.Matchers.is;
import org.junit.jupiter.api.Disabled;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class ArchUtilsBaseRockGeneratedTest {

    //BaseRock generated method id: ${getProcessorTest}, hash: 6090DFB0603B4341F79CAFAEBF268C94
    @Disabled()
    @Test()
    void getProcessorTest() {
        //Act Statement(s)
        Processor result = ArchUtils.getProcessor();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getProcessor1Test}, hash: DCB90E2F06AF73E0CE22DB9DDE7A12F3
    @Test()
    void getProcessor1Test() {
        //Act Statement(s)
        Processor result = ArchUtils.getProcessor("value1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }
}

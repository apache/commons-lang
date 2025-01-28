package org.apache.commons.lang3.concurrent;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class BasicThreadFactoryBaseRockGeneratedTest {

    //BaseRock generated method id: ${builderTest}, hash: E754A1D8EF10A035BAA2653336EC84B2
    @Test()
    void builderTest() {
        
        //Act Statement(s)
        BasicThreadFactory.Builder result = BasicThreadFactory.builder();
        
        //Assert statement(s)
        //TODO: Please implement equals method in Builder for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }
}

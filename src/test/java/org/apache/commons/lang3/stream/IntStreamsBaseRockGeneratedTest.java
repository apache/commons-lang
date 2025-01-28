package org.apache.commons.lang3.stream;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.stream.IntStream;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class IntStreamsBaseRockGeneratedTest {

    //BaseRock generated method id: ${ofWhenValuesIsNull}, hash: 0E36DC25DE03D99A28A39E63AFC4D0FC
    @Test()
    void ofWhenValuesIsNull() {
        /* Branches:
         * (values == null) : true
         */
         //Arrange Statement(s)
        int[] _int = null;
        
        //Act Statement(s)
        IntStream result = IntStreams.of(_int);
        
        //Assert statement(s)
        //TODO: Please implement equals method in IntStream for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${ofWhenValuesIsNotNull}, hash: A28CA0211890DC7271AEEDB3ECD8332C
    @Test()
    void ofWhenValuesIsNotNull() {
        /* Branches:
         * (values == null) : false
         */
         //Arrange Statement(s)
        int[] intArray = new int[] {};
        
        //Act Statement(s)
        IntStream result = IntStreams.of(intArray);
        
        //Assert statement(s)
        //TODO: Please implement equals method in IntStream for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${rangeTest}, hash: A4924B59612D5C223031D93374F2498D
    @Test()
    void rangeTest() {
        
        //Act Statement(s)
        IntStream result = IntStreams.range(1);
        
        //Assert statement(s)
        //TODO: Please implement equals method in IntStream for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${rangeClosedTest}, hash: DCFF7F008BF4B01A03F6683AC51100AB
    @Test()
    void rangeClosedTest() {
        
        //Act Statement(s)
        IntStream result = IntStreams.rangeClosed(1);
        
        //Assert statement(s)
        //TODO: Please implement equals method in IntStream for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }
}

package org.apache.commons.lang3.util;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.List;
import java.util.Spliterator;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;
import java.util.stream.Stream;
import static org.mockito.Mockito.doNothing;
import static org.mockito.ArgumentMatchers.any;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.Mockito.verify;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doReturn;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class IterableStringTokenizerBaseRockGeneratedTest {

    //BaseRock generated method id: ${iteratorTest}, hash: C8543992D39F7EF4947802B0C9C912AE
    @Test()
    void iteratorTest() {
        //Arrange Statement(s)
        IterableStringTokenizer target = new IterableStringTokenizer("str1", "delim1", false);
        
        //Act Statement(s)
        Iterator<String> result = target.iterator();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${toArrayTest}, hash: 21D4F01F16546BE265B2A13B9BCEEB86
    @Test()
    void toArrayTest() {
        //Arrange Statement(s)
        IterableStringTokenizer target = spy(new IterableStringTokenizer("str1", "delim1", false));
        List<String> stringList = new ArrayList<>();
        doReturn(stringList).when(target).toList();
        
        //Act Statement(s)
        String[] result = target.toArray();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result.length, equalTo(0));
            verify(target).toList();
        });
    }

    //BaseRock generated method id: ${toListTest}, hash: CBC6E4110387398169205E1333C072EE
    @Test()
    void toListTest() {
        //Arrange Statement(s)
        IterableStringTokenizer target = spy(new IterableStringTokenizer("str1", "delim1", false));
        doNothing().when(target).forEach((Consumer) any());
        
        //Act Statement(s)
        List<String> result = target.toList();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result.size(), equalTo(0));
            verify(target).forEach((Consumer) any());
        });
    }

    //BaseRock generated method id: ${toStreamTest}, hash: 2275080199634A47E5592F8234B3E372
    @Test()
    void toStreamTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        IterableStringTokenizer target = spy(new IterableStringTokenizer("str1", "delim1", false));
        Spliterator spliteratorMock = mock(Spliterator.class);
        doReturn(spliteratorMock).when(target).spliterator();
        
        //Act Statement(s)
        Stream<String> result = target.toStream();
        
        //Assert statement(s)
        //TODO: Please implement equals method in Stream for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            verify(target).spliterator();
        });
    }
}

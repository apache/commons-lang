package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.io.IOException;
import java.util.Arrays;
import org.apache.commons.lang3.exception.UncheckedException;
import org.apache.commons.lang3.function.FailableBiConsumer;
import java.util.ArrayList;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.verify;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mock;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.mockito.Mockito.doReturn;
import static org.hamcrest.Matchers.is;
import org.junit.jupiter.api.Disabled;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class AppendableJoinerBaseRockGeneratedTest {

    //BaseRock generated method id: ${builderTest}, hash: 836D7F5E547B63618843B6FD8EAE9E60
    @Test()
    void builderTest() {
        //Act Statement(s)
        AppendableJoiner.Builder result = AppendableJoiner.builder();
        //Assert statement(s)
        //TODO: Please implement equals method in Builder for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${joinAWhenILessThanElementsLength}, hash: 8DD6B3A1B785778D8BCBBF79D13F9878
    @Test()
    void joinAWhenILessThanElementsLength() throws IOException {
        /* Branches:
         * (elements != null) : true  #  inside joinArray method
         * (elements.length > 0) : true  #  inside joinArray method
         * (i < elements.length) : true  #  inside joinArray method
         */
        //Arrange Statement(s)
        Appendable appendableMock = mock(Appendable.class);
        Appendable appendableMock2 = mock(Appendable.class);
        doReturn(appendableMock2).when(appendableMock).append("prefix1");
        Appendable appendableMock3 = mock(Appendable.class);
        doReturn(appendableMock3).when(appendableMock).append("delimiter1");
        Appendable appendableMock4 = mock(Appendable.class);
        doReturn(appendableMock4).when(appendableMock).append("suffix1");
        FailableBiConsumer failableBiConsumer = FailableBiConsumer.nop();
        Object object = new Object();
        Object object2 = new Object();
        Object[] objectArray = new Object[] { object, object2 };
        //Act Statement(s)
        Appendable result = AppendableJoiner.joinA(appendableMock, "prefix1", "suffix1", "delimiter1", failableBiConsumer, objectArray);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(appendableMock));
            verify(appendableMock, atLeast(1)).append("prefix1");
            verify(appendableMock, atLeast(1)).append("delimiter1");
            verify(appendableMock, atLeast(1)).append("suffix1");
        });
    }

    //BaseRock generated method id: ${joinIWhenIteratorHasNext}, hash: DFAE60F43D86F53C3A653224A1C80A9E
    @Test()
    void joinIWhenIteratorHasNext() {
        /* Branches:
         * (elements != null) : true  #  inside joinIterable method
         * (iterator.hasNext()) : true  #  inside joinIterable method
         * (iterator.hasNext()) : true  #  inside joinIterable method
         */
        //Arrange Statement(s)
        StringBuilder stringBuilder = new StringBuilder();
        FailableBiConsumer failableBiConsumer = FailableBiConsumer.nop();
        Object object = new Object();
        Object object2 = new Object();
        Iterable<Object> iterable = new ArrayList<>(Arrays.asList(object, object2));
        //Act Statement(s)
        StringBuilder result = AppendableJoiner.joinI(stringBuilder, "prefix1", "suffix1", "delimiter1", failableBiConsumer, iterable);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringBuilder)));
    }

    //BaseRock generated method id: ${joinIWhenIteratorHasNextAndCaughtIOExceptionThrowsUncheckedException}, hash: 1946DCE56252D4752210932B94C498EA
    @Disabled()
    @Test()
    void joinIWhenIteratorHasNextAndCaughtIOExceptionThrowsUncheckedException() {
        /* Branches:
         * (elements != null) : true  #  inside joinIterable method
         * (iterator.hasNext()) : true  #  inside joinIterable method
         * (iterator.hasNext()) : true  #  inside joinIterable method
         * (catch-exception (IOException)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        StringBuilder stringBuilder = new StringBuilder();
        FailableBiConsumer failableBiConsumer = FailableBiConsumer.nop();
        Object object = new Object();
        Object object2 = new Object();
        Iterable<Object> iterable = new ArrayList<>(Arrays.asList(object, object2));
        IOException iOException = new IOException();
        //Act Statement(s)
        final UncheckedException result = assertThrows(UncheckedException.class, () -> {
            AppendableJoiner.joinI(stringBuilder, "prefix1", "suffix1", "delimiter1", failableBiConsumer, iterable);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getCause(), is(instanceOf(iOException.getClass())));
        });
    }

    //BaseRock generated method id: ${joinSBWhenILessThanElementsLength}, hash: 44DD87287647FA55DD8DD02F8AA637AA
    @Test()
    void joinSBWhenILessThanElementsLength() {
        /* Branches:
         * (elements != null) : true  #  inside joinArray method
         * (elements.length > 0) : true  #  inside joinArray method
         * (i < elements.length) : true  #  inside joinArray method
         */
        //Arrange Statement(s)
        StringBuilder stringBuilder = new StringBuilder();
        FailableBiConsumer failableBiConsumer = FailableBiConsumer.nop();
        Object object = new Object();
        Object object2 = new Object();
        Object[] objectArray = new Object[] { object, object2 };
        //Act Statement(s)
        StringBuilder result = AppendableJoiner.joinSB(stringBuilder, "prefix1", "suffix1", "delimiter1", failableBiConsumer, objectArray);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringBuilder)));
    }

    //BaseRock generated method id: ${joinSBWhenILessThanElementsLengthAndCaughtIOExceptionThrowsUncheckedException}, hash: DF1A2B85A5D7D7F38CF4DE5A3C39E9DB
    @Disabled()
    @Test()
    void joinSBWhenILessThanElementsLengthAndCaughtIOExceptionThrowsUncheckedException() {
        /* Branches:
         * (elements != null) : true  #  inside joinArray method
         * (elements.length > 0) : true  #  inside joinArray method
         * (i < elements.length) : true  #  inside joinArray method
         * (catch-exception (IOException)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        StringBuilder stringBuilder = new StringBuilder();
        FailableBiConsumer failableBiConsumer = FailableBiConsumer.nop();
        Object object = new Object();
        Object object2 = new Object();
        Object[] objectArray = new Object[] { object, object2 };
        IOException iOException = new IOException();
        //Act Statement(s)
        final UncheckedException result = assertThrows(UncheckedException.class, () -> {
            AppendableJoiner.joinSB(stringBuilder, "prefix1", "suffix1", "delimiter1", failableBiConsumer, objectArray);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getCause(), is(instanceOf(iOException.getClass())));
        });
    }
}

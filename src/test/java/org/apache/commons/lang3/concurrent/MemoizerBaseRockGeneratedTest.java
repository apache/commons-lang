package org.apache.commons.lang3.concurrent;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.mock;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class MemoizerBaseRockGeneratedTest {

    private final Computable computableMock = mock(Computable.class);

    //BaseRock generated method id: ${computeTest}, hash: 57D5A7E9F5A611930272DA3AFE3747FB
    @Disabled()
    @Test()
    void computeTest() throws InterruptedException {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  Potential harmful system call (Future.get) detected; Learn more: https://github.com/Sapient-AI/docs#disabled-generated-tests
         *  Suggestions:
         *  This method should be avoided from unit testing. This can be covered during integration testing.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Memoizer memoizer = new Memoizer(computableMock);
        Memoizer target = new Memoizer(memoizer, false);
        Object object = new Object();
        
        //Act Statement(s)
        Object result = target.compute(object);
        
        //Assert statement(s)
        //TODO: Please implement equals method in Object for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${computeWhenCaughtCancellationException}, hash: C1134814D754895165F9BE59806A57D0
    @Disabled()
    @Test()
    void computeWhenCaughtCancellationException() throws InterruptedException {
        /* Branches:
         * (catch-exception (CancellationException)) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  Potential harmful system call (Future.get) detected; Learn more: https://github.com/Sapient-AI/docs#disabled-generated-tests
         *  Suggestions:
         *  This method should be avoided from unit testing. This can be covered during integration testing.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Memoizer memoizer = new Memoizer(computableMock);
        Memoizer target = new Memoizer(memoizer, false);
        Object object = new Object();
        
        //Act Statement(s)
        Object result = target.compute(object);
    }

    //BaseRock generated method id: ${computeWhenRecalculateThrowsIllegalStateException}, hash: B4DCB877763663B93AC8BAAF37077178
    @Disabled()
    @Test()
    void computeWhenRecalculateThrowsIllegalStateException() throws InterruptedException {
        /* Branches:
         * (catch-exception (ExecutionException)) : true
         * (recalculate) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  Potential harmful system call (Future.get) detected; Learn more: https://github.com/Sapient-AI/docs#disabled-generated-tests
         *  Suggestions:
         *  This method should be avoided from unit testing. This can be covered during integration testing.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Memoizer memoizer = new Memoizer(computableMock);
        Memoizer memoizer2 = new Memoizer(memoizer);
        Memoizer target = new Memoizer(memoizer2, true);
        Object object = new Object();
        Throwable throwable = new Throwable();
        IllegalStateException illegalStateException = new IllegalStateException("Unchecked exception", throwable);
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            target.compute(object);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
            assertThat(result.getCause(), is(instanceOf(throwable.getClass())));
        });
    }
}

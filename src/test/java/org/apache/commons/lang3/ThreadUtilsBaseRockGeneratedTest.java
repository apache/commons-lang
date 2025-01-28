package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Disabled;
import java.util.Arrays;
import java.util.function.Predicate;
import java.time.Duration;
import java.util.Collection;
import org.mockito.MockedStatic;
import java.util.ArrayList;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.mock;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mockStatic;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class ThreadUtilsBaseRockGeneratedTest {

    private final Predicate<Thread> predicateMock = mock(Predicate.class);

    //BaseRock generated method id: ${findThreadByIdWhenThreadIdLessThanOrEqualsTo0ThrowsIllegalArgumentException}, hash: C8F20DDC4190F2BC643ABD3546A8AD80
    @Test()
    void findThreadByIdWhenThreadIdLessThanOrEqualsTo0ThrowsIllegalArgumentException() {
        /* Branches:
         * (threadId <= 0) : true
         */
        //Arrange Statement(s)
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The thread id must be greater than zero");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            ThreadUtils.findThreadById(-1L);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${findThreadByIdWhenResultIsEmpty}, hash: 8C2978856176C746597BCB9480EB3622
    @Test()
    void findThreadByIdWhenResultIsEmpty() {
        /* Branches:
         * (threadId <= 0) : false
         * (result.isEmpty()) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ThreadUtils> threadUtils = mockStatic(ThreadUtils.class, CALLS_REAL_METHODS)) {
            Collection<Thread> collection = new ArrayList<>();
            threadUtils.when(() -> ThreadUtils.findThreads((Predicate) any())).thenReturn(collection);
            //Act Statement(s)
            Thread result = ThreadUtils.findThreadById(1L);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(nullValue()));
                threadUtils.verify(() -> ThreadUtils.findThreads((Predicate) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${findThreadByIdWhenResultNotIsEmpty}, hash: 7A854707C037B4B6956F32E4E478D382
    @Test()
    void findThreadByIdWhenResultNotIsEmpty() {
        /* Branches:
         * (threadId <= 0) : false
         * (result.isEmpty()) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ThreadUtils> threadUtils = mockStatic(ThreadUtils.class, CALLS_REAL_METHODS)) {
            Thread thread = new Thread();
            Collection<Thread> collection = new ArrayList<>();
            collection.add(thread);
            threadUtils.when(() -> ThreadUtils.findThreads((Predicate) any())).thenReturn(collection);
            //Act Statement(s)
            Thread result = ThreadUtils.findThreadById(1L);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(thread));
                threadUtils.verify(() -> ThreadUtils.findThreads((Predicate) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${findThreadById1WhenThreadGetThreadGroupGetNameEqualsThreadGroupName}, hash: BEF29311D20873C91ECFA21E8AF35D88
    @Disabled()
    @Test()
    void findThreadById1WhenThreadGetThreadGroupGetNameEqualsThreadGroupName() {
        /* Branches:
         * (thread != null) : true
         * (thread.getThreadGroup() != null) : true
         * (thread.getThreadGroup().getName().equals(threadGroupName)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ThreadUtils> threadUtils = mockStatic(ThreadUtils.class, CALLS_REAL_METHODS)) {
            Thread thread = new Thread();
            threadUtils.when(() -> ThreadUtils.findThreadById(0L)).thenReturn(thread);
            //Act Statement(s)
            Thread result = ThreadUtils.findThreadById(0L, "main");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(thread));
                threadUtils.verify(() -> ThreadUtils.findThreadById(0L), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${findThreadById1WhenThreadGetThreadGroupGetNameNotEqualsThreadGroupName}, hash: A053AF946CAD45251C2805462DF3C75D
    @Test()
    void findThreadById1WhenThreadGetThreadGroupGetNameNotEqualsThreadGroupName() {
        /* Branches:
         * (thread != null) : true
         * (thread.getThreadGroup() != null) : true
         * (thread.getThreadGroup().getName().equals(threadGroupName)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ThreadUtils> threadUtils = mockStatic(ThreadUtils.class, CALLS_REAL_METHODS)) {
            Thread thread = new Thread();
            threadUtils.when(() -> ThreadUtils.findThreadById(0L)).thenReturn(thread);
            //Act Statement(s)
            Thread result = ThreadUtils.findThreadById(0L, "B");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(nullValue()));
                threadUtils.verify(() -> ThreadUtils.findThreadById(0L), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${findThreadById2WhenThreadGroupEqualsThreadGetThreadGroup}, hash: E8AA558E41E89C24AF5DEC58928457BD
    @Test()
    void findThreadById2WhenThreadGroupEqualsThreadGetThreadGroup() {
        /* Branches:
         * (thread != null) : true
         * (threadGroup.equals(thread.getThreadGroup())) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<ThreadUtils> threadUtils = mockStatic(ThreadUtils.class, CALLS_REAL_METHODS)) {
            Thread thread = new Thread();
            threadUtils.when(() -> ThreadUtils.findThreadById(0L)).thenReturn(thread);
            ThreadGroup threadGroup = thread.getThreadGroup();
            //Act Statement(s)
            Thread result = ThreadUtils.findThreadById(0L, threadGroup);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(thread));
                threadUtils.verify(() -> ThreadUtils.findThreadById(0L), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${findThreadById2WhenThreadGroupNotEqualsThreadGetThreadGroup}, hash: 53E7F5B75418CDB66FB500CBCA9F4873
    @Test()
    void findThreadById2WhenThreadGroupNotEqualsThreadGetThreadGroup() {
        /* Branches:
         * (thread != null) : true
         * (threadGroup.equals(thread.getThreadGroup())) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<ThreadUtils> threadUtils = mockStatic(ThreadUtils.class, CALLS_REAL_METHODS)) {
            Thread thread = new Thread();
            threadUtils.when(() -> ThreadUtils.findThreadById(0L)).thenReturn(thread);
            ThreadGroup threadGroup = new ThreadGroup("name1");
            //Act Statement(s)
            Thread result = ThreadUtils.findThreadById(0L, threadGroup);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(nullValue()));
                threadUtils.verify(() -> ThreadUtils.findThreadById(0L), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${findThreadGroupsTest}, hash: 990E471C897C451A20F21670AB3C09DA
    @Disabled()
    @Test()
    void findThreadGroupsTest() {
        //Arrange Statement(s)
        Predicate<ThreadGroup> predicateMock = mock(Predicate.class);
        try (MockedStatic<ThreadUtils> threadUtils = mockStatic(ThreadUtils.class, CALLS_REAL_METHODS)) {
            ThreadGroup threadGroup = new ThreadGroup("name1");
            threadUtils.when(() -> ThreadUtils.getSystemThreadGroup()).thenReturn(threadGroup);
            Collection<ThreadGroup> collection = new ArrayList<>();
            threadUtils.when(() -> ThreadUtils.findThreadGroups(threadGroup, true, predicateMock)).thenReturn(collection);
            //Act Statement(s)
            Collection<ThreadGroup> result = ThreadUtils.findThreadGroups(predicateMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(collection));
                threadUtils.verify(() -> ThreadUtils.getSystemThreadGroup(), atLeast(1));
                threadUtils.verify(() -> ThreadUtils.findThreadGroups(threadGroup, true, predicateMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${findThreadGroups1WhenCountLessThanThreadGroupsLength}, hash: 691347D8E2E45783F5BC0DE4CE7FCE9C
    @Test()
    void findThreadGroups1WhenCountLessThanThreadGroupsLength() {
        /* Branches:
         * (count >= threadGroups.length) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        ThreadGroup threadGroup = new ThreadGroup("name1");
        Predicate<ThreadGroup> predicateMock = mock(Predicate.class);
        //Act Statement(s)
        Collection<ThreadGroup> result = ThreadUtils.findThreadGroups(threadGroup, false, predicateMock);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${findThreadGroupsByNameTest}, hash: F8DF6F2F579ADEDAF2E9B3AA053FC049
    @Test()
    void findThreadGroupsByNameTest() {
        //Arrange Statement(s)
        try (MockedStatic<ThreadUtils> threadUtils = mockStatic(ThreadUtils.class, CALLS_REAL_METHODS)) {
            Collection<ThreadGroup> collection = new ArrayList<>();
            threadUtils.when(() -> ThreadUtils.findThreadGroups((Predicate) any())).thenReturn(collection);
            //Act Statement(s)
            Collection<ThreadGroup> result = ThreadUtils.findThreadGroupsByName("threadGroupName1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(collection));
                threadUtils.verify(() -> ThreadUtils.findThreadGroups((Predicate) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${findThreadsTest}, hash: 22869B432D67783EE94B06A551CF1DD0
    @Test()
    void findThreadsTest() {
        //Arrange Statement(s)
        try (MockedStatic<ThreadUtils> threadUtils = mockStatic(ThreadUtils.class, CALLS_REAL_METHODS)) {
            ThreadGroup threadGroup = new ThreadGroup("name1");
            threadUtils.when(() -> ThreadUtils.getSystemThreadGroup()).thenReturn(threadGroup);
            Collection<Thread> collection = new ArrayList<>();
            threadUtils.when(() -> ThreadUtils.findThreads(threadGroup, true, predicateMock)).thenReturn(collection);
            //Act Statement(s)
            Collection<Thread> result = ThreadUtils.findThreads(predicateMock);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(collection));
                threadUtils.verify(() -> ThreadUtils.getSystemThreadGroup(), atLeast(1));
                threadUtils.verify(() -> ThreadUtils.findThreads(threadGroup, true, predicateMock), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${findThreads1WhenCountLessThanThreadsLength}, hash: A4A46F7634F7091D8E79AA5F056F0927
    @Test()
    void findThreads1WhenCountLessThanThreadsLength() {
        /* Branches:
         * (count >= threads.length) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        ThreadGroup threadGroup = new ThreadGroup("name1");
        //Act Statement(s)
        Collection<Thread> result = ThreadUtils.findThreads(threadGroup, false, predicateMock);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${findThreadsByNameTest}, hash: 3CEB0B91D66D393656251E620DA7A474
    @Test()
    void findThreadsByNameTest() {
        //Arrange Statement(s)
        try (MockedStatic<ThreadUtils> threadUtils = mockStatic(ThreadUtils.class, CALLS_REAL_METHODS)) {
            Collection<Thread> collection = new ArrayList<>();
            threadUtils.when(() -> ThreadUtils.findThreads((Predicate) any())).thenReturn(collection);
            //Act Statement(s)
            Collection<Thread> result = ThreadUtils.findThreadsByName("threadName1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(collection));
                threadUtils.verify(() -> ThreadUtils.findThreads((Predicate) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${findThreadsByName1Test}, hash: BB34B47CAD7FE923A887655514A40F43
    @Disabled()
    @Test()
    void findThreadsByName1Test() {
        //Arrange Statement(s)
        try (MockedStatic<ThreadUtils> threadUtils = mockStatic(ThreadUtils.class, CALLS_REAL_METHODS)) {
            ThreadGroup threadGroup = new ThreadGroup("name1");
            Collection<ThreadGroup> collection = new ArrayList<>();
            collection.add(threadGroup);
            threadUtils.when(() -> ThreadUtils.findThreadGroups((Predicate) any())).thenReturn(collection);
            Thread thread = new Thread();
            Collection<Thread> collection2 = new ArrayList<>(Arrays.asList(thread));
            threadUtils.when(() -> ThreadUtils.findThreads(eq(threadGroup), eq(false), (Predicate) any())).thenReturn(collection2);
            //Act Statement(s)
            Collection<Thread> result = ThreadUtils.findThreadsByName("threadName1", "threadGroupName1");
            //Assert statement(s)
            //TODO: Please implement equals method in Thread for verification of the entire object or you need to adjust respective assertion statements
            assertAll("result", () -> {
                assertThat(result.size(), equalTo(1));
                threadUtils.verify(() -> ThreadUtils.findThreadGroups((Predicate) any()), atLeast(1));
                threadUtils.verify(() -> ThreadUtils.findThreads(eq(threadGroup), eq(false), (Predicate) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${findThreadsByName2Test}, hash: 608707EA4A1E99DC35B0C993B5527B86
    @Disabled()
    @Test()
    void findThreadsByName2Test() {
        //Arrange Statement(s)
        try (MockedStatic<ThreadUtils> threadUtils = mockStatic(ThreadUtils.class, CALLS_REAL_METHODS)) {
            Collection<Thread> collection = new ArrayList<>();
            ThreadGroup threadGroup = new ThreadGroup("name1");
            threadUtils.when(() -> ThreadUtils.findThreads(eq(threadGroup), eq(false), (Predicate) any())).thenReturn(collection);
            //Act Statement(s)
            Collection<Thread> result = ThreadUtils.findThreadsByName("threadName1", threadGroup);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(collection));
                threadUtils.verify(() -> ThreadUtils.findThreads(eq(threadGroup), eq(false), (Predicate) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getAllThreadGroupsTest}, hash: D562514448A6D388083E608C364DB1B7
    @Test()
    void getAllThreadGroupsTest() {
        //Arrange Statement(s)
        try (MockedStatic<ThreadUtils> threadUtils = mockStatic(ThreadUtils.class, CALLS_REAL_METHODS)) {
            Collection<ThreadGroup> collection = new ArrayList<>();
            threadUtils.when(() -> ThreadUtils.findThreadGroups((Predicate) any())).thenReturn(collection);
            //Act Statement(s)
            Collection<ThreadGroup> result = ThreadUtils.getAllThreadGroups();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(collection));
                threadUtils.verify(() -> ThreadUtils.findThreadGroups((Predicate) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getAllThreadsTest}, hash: 1DBEB840DE288637AD68141B3996DB66
    @Test()
    void getAllThreadsTest() {
        //Arrange Statement(s)
        try (MockedStatic<ThreadUtils> threadUtils = mockStatic(ThreadUtils.class, CALLS_REAL_METHODS)) {
            Collection<Thread> collection = new ArrayList<>();
            threadUtils.when(() -> ThreadUtils.findThreads((Predicate) any())).thenReturn(collection);
            //Act Statement(s)
            Collection<Thread> result = ThreadUtils.getAllThreads();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(collection));
                threadUtils.verify(() -> ThreadUtils.findThreads((Predicate) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getSystemThreadGroupWhenThreadGroupIsNull}, hash: 8DA5014E31C24E153AFCF6F2DC57795C
    @Disabled()
    @Test()
    void getSystemThreadGroupWhenThreadGroupIsNull() {
        /* Branches:
         * (threadGroup != null) : false
         */
        //Act Statement(s)
        ThreadGroup result = ThreadUtils.getSystemThreadGroup();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${joinTest}, hash: 4B2C3C77C0FA31012A8A72A4C4CCBDB5
    @Disabled()
    @Test()
    void joinTest() throws InterruptedException {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  Potential harmful system call (Thread.join) detected; Learn more: https://github.com/Sapient-AI/docs#disabled-generated-tests
         *  Suggestions:
         *  This method should be avoided from unit testing. This can be covered during integration testing.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Thread thread = new Thread();
        Duration duration = Duration.ofDays(0L);
        //Act Statement(s)
        ThreadUtils.join(thread, duration);
    }

    //BaseRock generated method id: ${sleepTest}, hash: 3D13BCDFD5D6A07296D62878FE9B3D6B
    @Test()
    void sleepTest() throws InterruptedException {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Duration duration = Duration.ofDays(0L);
        //Act Statement(s)
        ThreadUtils.sleep(duration);
    }

    //BaseRock generated method id: ${sleepQuietlyTest}, hash: 170EDB2E3A3AA10E63DA0EEE14287639
    @Test()
    void sleepQuietlyTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Duration duration = Duration.ofDays(0L);
        //Act Statement(s)
        ThreadUtils.sleepQuietly(duration);
    }

    //BaseRock generated method id: ${sleepQuietlyWhenCaughtInterruptedException}, hash: D7AAED0025F79CD39704B24971A3CE65
    @Test()
    void sleepQuietlyWhenCaughtInterruptedException() {
        /* Branches:
         * (catch-exception (InterruptedException)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Duration duration = Duration.ofDays(0L);
        //Act Statement(s)
        ThreadUtils.sleepQuietly(duration);
    }
}

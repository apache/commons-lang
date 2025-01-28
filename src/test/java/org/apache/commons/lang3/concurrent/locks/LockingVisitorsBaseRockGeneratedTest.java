package org.apache.commons.lang3.concurrent.locks;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.concurrent.locks.ReadWriteLock;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class LockingVisitorsBaseRockGeneratedTest {

    //BaseRock generated method id: ${createTest}, hash: 369080DBDDF0AD2905A921741B89593C
    @Test()
    void createTest() {
        //Arrange Statement(s)
        Object object = new Object();
        ReadWriteLock readWriteLockMock = mock(ReadWriteLock.class);
        
        //Act Statement(s)
        LockingVisitors.ReadWriteLockVisitor result = LockingVisitors.create(object, readWriteLockMock);
        
        //Assert statement(s)
        //TODO: Please implement equals method in ReadWriteLockVisitor for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${reentrantReadWriteLockVisitorTest}, hash: 19157CC771047E446CE9280F1F4726A1
    @Test()
    void reentrantReadWriteLockVisitorTest() {
        //Arrange Statement(s)
        LockingVisitors.ReadWriteLockVisitor<Object> lockingVisitorsReadWriteLockVisitorMock = mock(LockingVisitors.ReadWriteLockVisitor.class);
        try (MockedStatic<LockingVisitors> lockingVisitors = mockStatic(LockingVisitors.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            lockingVisitors.when(() -> LockingVisitors.create(eq(object), (ReentrantReadWriteLock) any())).thenReturn(lockingVisitorsReadWriteLockVisitorMock);
            //Act Statement(s)
            LockingVisitors.ReadWriteLockVisitor result = LockingVisitors.reentrantReadWriteLockVisitor(object);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(lockingVisitorsReadWriteLockVisitorMock));
                lockingVisitors.verify(() -> LockingVisitors.create(eq(object), (ReentrantReadWriteLock) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${stampedLockVisitorTest}, hash: AB427C2774128E81767A84F3CC0B66AB
    @Test()
    void stampedLockVisitorTest() {
        //Arrange Statement(s)
        Object object = new Object();
        
        //Act Statement(s)
        LockingVisitors.StampedLockVisitor result = LockingVisitors.stampedLockVisitor(object);
        
        //Assert statement(s)
        //TODO: Please implement equals method in StampedLockVisitor for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }
}

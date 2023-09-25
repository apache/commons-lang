/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang3.concurrent;

import static org.junit.jupiter.api.Assertions.fail;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.concurrent.TimeoutException;
import java.io.IOException;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ForkJoinPool;

/**
 * Test class for {@code LazyInitializerWithDisposer}.
 */
public class LazyInitializerWithDisposerTest extends AbstractConcurrentInitializerTest {
    /** The initializer to be tested. */
    private LazyInitializerWithDisposer<StatefulObject> initializer;

    @BeforeEach
    public void setUp() {
        initializer = new LazyInitializerWithDisposer<StatefulObject>(StatefulObject::new, StatefulObject::dispose);
    }

    /**
     * Returns the initializer to be tested. This implementation returns the
     * {@code LazyInitializer} created in the {@code setUp()} method.
     *
     * @return the initializer to be tested
     */
    @Override
    protected ConcurrentInitializer<Object> createInitializer() {
        return (ConcurrentInitializer) initializer;
    }

    @Test
    public void testGet() throws ConcurrentException {
        assertTrue(initializer.get().getClass().isAssignableFrom(StatefulObject.class));
    }

    @Test
    public void testGetIfPossible() throws ConcurrentException {
        Optional<StatefulObject> optionalStatefulObject = initializer.getIfPossible();
        assertTrue(optionalStatefulObject.isPresent());
        optionalStatefulObject.ifPresent((statefulObject) -> assertFalse(statefulObject.isDisposed()));

        initializer.dispose();

        assertTrue(optionalStatefulObject.isPresent());
        optionalStatefulObject.ifPresent((statefulObject) -> assertTrue(statefulObject.isDisposed()));

        Optional<StatefulObject> optionalStatefulObjectTwo = initializer.getIfPossible();
        assertFalse(optionalStatefulObjectTwo.isPresent());
    }

    @Test
    public void testSupplierOfNull() throws ConcurrentException {
         LazyInitializerWithDisposer<StatefulObject> returnNullInitializer = new LazyInitializerWithDisposer<StatefulObject>(
             () -> { 
                 return null;
             },
             (statefulObject) -> {}
         );

         StatefulObject nullStatefulObject = returnNullInitializer.get();
         assertNull(nullStatefulObject);

         Optional<StatefulObject> optionalStatefulObject = returnNullInitializer.getIfPossible();
         assertFalse(optionalStatefulObject.isPresent());     

         Optional<StatefulObject> optionalStatefulObjectTwo = returnNullInitializer.peek();
         assertFalse(optionalStatefulObjectTwo.isPresent());    
    }

    @Test
    public void testDisposal() throws ConcurrentException {
        StatefulObject statefulObject = initializer.get();
        assertFalse(statefulObject.isDisposed());

        assertTrue(initializer.dispose());
        assertFalse(initializer.dispose());

        assertTrue(statefulObject.isDisposed());

        assertThrows(AlreadyDisposedException.class, initializer::get);        
    }

    @Test
    public void testClose() throws ConcurrentException {
        StatefulObject statefulObject = initializer.get();
        assertFalse(statefulObject.isDisposed());

        assertTrue(initializer.close());
        assertFalse(initializer.close());

        assertFalse(statefulObject.isDisposed());
        assertFalse(initializer.dispose());
        assertFalse(statefulObject.isDisposed());

        assertThrows(AlreadyDisposedException.class, initializer::get);        
    }

    @Test
    public void testPeek() throws ConcurrentException {

        assertFalse(initializer.peek().isPresent());

        initializer.get();
        assertTrue(initializer.peek().isPresent());
        assertEquals(initializer.get(), initializer.peek().get());

        initializer.dispose();
        assertFalse(initializer.peek().isPresent());
    }

    @Test
    public void testBooleanGettersWithDispose() throws ConcurrentException {

        assertFalse(initializer.isReady());
        assertFalse(initializer.isDisposed());

        initializer.get();

        assertTrue(initializer.isReady());
        assertFalse(initializer.isDisposed());

        initializer.dispose();

        assertFalse(initializer.isReady());
        assertTrue(initializer.isDisposed());
    }

    @Test
    public void testBooleanGettersWithClose() throws ConcurrentException {

        assertFalse(initializer.isReady());
        assertFalse(initializer.isDisposed());

        initializer.get();

        assertTrue(initializer.isReady());
        assertFalse(initializer.isDisposed());

        initializer.close();

        assertFalse(initializer.isReady());
        assertTrue(initializer.isDisposed());
    }

    //Tests exceptions are properly wrapped in a ConcurrentException, also tests we can handle lambdas and method signatures with multiple checked exceptions
    @Test
    public void testExceptionWrapping() throws ConcurrentException {
         LazyInitializerWithDisposer<StatefulObjectWithCheckedExceptions> unreliableInitializer = new LazyInitializerWithDisposer<StatefulObjectWithCheckedExceptions>(
             StatefulObjectWithCheckedExceptions::new,
             (statefulObject) -> {
                 String nonsense = "tea";
                 if (nonsense.equals("tea")) {
                     throw new IOException();
                 }
         });

        assertThrows(ConcurrentException.class, unreliableInitializer::get);
    }

    @Test
    public void testSuccessOnRetry() throws ConcurrentException {
         final AtomicInteger tryCount = new AtomicInteger(0);//used for the convience of incrementAndGet

         LazyInitializerWithDisposer<StatefulObject> unreliableInitializer = new LazyInitializerWithDisposer<StatefulObject>(
             () -> { 
                 if (tryCount.incrementAndGet() == 3) {
                     return new StatefulObject();
                 } else {
                     throw new ConcurrentException();
                 }
             },
             (statefulObject) -> {}
         );

        assertThrows(ConcurrentException.class, unreliableInitializer::get);
        assertThrows(ConcurrentException.class, unreliableInitializer::get);

        assertTrue(unreliableInitializer.get() instanceof StatefulObject);
    }

    @Test
    public void testFailPreserved() throws ConcurrentException {
         final AtomicInteger tryCount = new AtomicInteger(0);//used for the convience of incrementAndGet

         LazyInitializerWithDisposer<StatefulObject> unreliableInitializer = new LazyInitializerWithDisposer<StatefulObject>(
             () -> {
                 if (tryCount.incrementAndGet() >= 3) {
                     return new StatefulObject();
                 } else {
                     throw new ConcurrentException();
                 }
             },
             (statefulObject) -> {},
             2 //Allow two tries
         );

         //Don't use a for loop so the line number on a test failure is useful

         assertThrows(ConcurrentException.class, unreliableInitializer::get);
         assertThrows(ConcurrentException.class, unreliableInitializer::get);
         assertThrows(ConcurrentException.class, unreliableInitializer::get);
         assertThrows(ConcurrentException.class, unreliableInitializer::get);
         assertThrows(ConcurrentException.class, unreliableInitializer::get);
    }

    @Test
    public void testRuntimeException() throws ConcurrentException {
         LazyInitializerWithDisposer<StatefulObject> runtimeExceptionInitializer = new LazyInitializerWithDisposer<StatefulObject>(
             () -> {throw new RuntimeException("test exception");},
             (statefulObject) -> {}
         );

         assertThrows(RuntimeException.class, runtimeExceptionInitializer::get);
    }

    @Test
    public void testDisposeDuringGet() throws ConcurrentException {

         //In this test we ensure that a dispose() called while get() is in progress:
         //a) does not prevent get() returning an object.
         //b) disposes of the object get acquires in step a
         //Calling get() than dispose() a microsecond later should have the same behaviour
         //as calling get() then dispose() an hour later. get() returns an object
         //then dispose() disposes it.

         //We can't actually ensure that disposed() has been called because if you call 
         //dispose() while get() is running, it will wait for get() to finish beofore 
         //invoking anything that can send a singal saying dispose has been invoked.

         //Therefore we do the next best thing. We ensure that:
         //1) We are inside the get() supplier on one thread.
         //2) We are immediately prior to calling dispose() on another thread.
         //Then we release thread two and sleep for a bit before releasing thread one.

         final CountDownLatch holdUntilAfterGetCalled = new CountDownLatch(1);
         final CountDownLatch holdUntilJustBeforeDisposedCalled = new CountDownLatch(1);
         final CountDownLatch holdUntilAfterDisposedCalled = new CountDownLatch(1);
         final CountDownLatch holdUntilAfterDisposedCompleted = new CountDownLatch(1);

         final LazyInitializerWithDisposer<StatefulObject> slowInitializer = new LazyInitializerWithDisposer<StatefulObject>(
             () -> {
                 try {
                     holdUntilAfterGetCalled.countDown();      
                     holdUntilAfterDisposedCalled.await();             
                 } catch (InterruptedException e) {
                     fail(e);
                 }
                 return new StatefulObject();
             },
             (statefulObject) -> {
                 statefulObject.dispose();
                 holdUntilAfterDisposedCompleted.countDown();
             }
          );

         //Step one: Call get()
         ForkJoinPool.commonPool().execute(() -> {
             try{
                 StatefulObject statefulObject = slowInitializer.get();

                 holdUntilAfterDisposedCompleted.await();
                 assertTrue(statefulObject.isDisposed());
             } catch(InterruptedException e) {
                 fail(e);
             } catch(ConcurrentException e) {
                 fail(e);
             }
         });

         //Step two: Wait until the get() method has started.
         try {
             holdUntilAfterGetCalled.await();
         } catch(InterruptedException e) {
             fail(e);
         }
         
         //Step three: get call dispose()
         ForkJoinPool.commonPool().execute(() -> {
             try{
                 holdUntilJustBeforeDisposedCalled.countDown();
                 slowInitializer.dispose();
             } catch(ConcurrentException e) {
                 fail(e);
             }
         });

         //Step four: Wait until the dispose() method is about to start.
         try {
             holdUntilJustBeforeDisposedCalled.await();
         } catch(InterruptedException e) {
             fail(e);
         }

         //Step five: Sleep to ensure we're actually in dispose()
         try {
             Thread.sleep(1000);
         } catch(InterruptedException e) {
             fail(e);
         }

         //Step six: Now that get() and dispose() should both be in progress, release get()
         holdUntilAfterDisposedCalled.countDown();
         
         //Step seven takes place inside the thread that calls get(). dispose() will release 
         //the last latch. Then the thread that calls get() will assert that the object is
         //disposed 
    }

    /**
     * A test object 
     */
    private static class StatefulObject {
        private boolean disposed = false;

        public void dispose() {
            disposed = true;
        }

        public boolean isDisposed() {
            return disposed;
        }        
    }

    /**
     * A test object with exceptions declared on the constructor
     */
    private static class StatefulObjectWithCheckedExceptions {
        private boolean disposed = false;

        public StatefulObjectWithCheckedExceptions() throws TimeoutException, IOException {
            String nonsense = "hatter";

            //String equality so the compiler can't tell if its always/never going to pass.
            if (nonsense.equals("hatter")) {
                throw new TimeoutException();
            }
            if (nonsense.equals("doormouse")) {
                throw new IOException();
            }
        }

        public void dispose() {
            disposed = true;
        }

        public boolean isDisposed() {
            return disposed;
        }        
    }
}

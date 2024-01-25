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

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.concurrent.ExecutorService;

import org.apache.commons.lang3.function.FailableConsumer;
import org.apache.commons.lang3.function.FailableSupplier;
import org.junit.jupiter.api.Test;

public class BackgroundInitializerSupplierTest extends BackgroundInitializerTest {

    /**
     * A concrete implementation of BackgroundInitializer. It is designed as a wrapper so the test can
     * use the same builder pattern that real code will.
     */
    protected static final class SupplierBackgroundInitializerTestImpl extends AbstractBackgroundInitializerTestImpl {

        SupplierBackgroundInitializerTestImpl() {
            setSupplierAndCloser((final CloseableCounter cc) -> cc.close());
        }

        SupplierBackgroundInitializerTestImpl(final ExecutorService exec) {
            super(exec);
            setSupplierAndCloser((final CloseableCounter cc) -> cc.close());
        }

        SupplierBackgroundInitializerTestImpl(final FailableConsumer<?, ?> consumer) {
            setSupplierAndCloser(consumer);
        }

        private void setSupplierAndCloser(final FailableConsumer<?, ?> consumer) {
            try {
                // Use reflection here because the constructors we need are private
                final FailableSupplier<?, ?> supplier = this::initializeInternal;
                final Field initializer = AbstractConcurrentInitializer.class.getDeclaredField("initializer");
                initializer.setAccessible(true);
                initializer.set(this, supplier);

                final Field closer = AbstractConcurrentInitializer.class.getDeclaredField("closer");
                closer.setAccessible(true);
                closer.set(this, consumer);
            } catch (NoSuchFieldException | SecurityException | IllegalArgumentException | IllegalAccessException e) {
                fail();
            }
        }
    }

    @Override
    protected AbstractBackgroundInitializerTestImpl getBackgroundInitializerTestImpl() {
        return new SupplierBackgroundInitializerTestImpl();
    }

    @Override
    protected SupplierBackgroundInitializerTestImpl getBackgroundInitializerTestImpl(final ExecutorService exec) {
        return new SupplierBackgroundInitializerTestImpl(exec);
    }

    /**
     * Tests that close() method closes the wrapped object
     *
     * @throws Exception
     */
    @Test
    public void testClose() throws Exception {
        final AbstractBackgroundInitializerTestImpl init = getBackgroundInitializerTestImpl();
        assertFalse(init.getCloseableCounter().isClosed(), "closed without close() call");
        init.close();
        assertFalse(init.getCloseableCounter().isClosed(), "closed() succeeded before start()");
        init.start();
        init.get(); //ensure the Future has completed.
        assertFalse(init.getCloseableCounter().isClosed(), "closed() succeeded after start() but before close()");
        init.close();
        assertTrue(init.getCloseableCounter().isClosed(), "closed() did not succeed");
    }

    /**
     * Tests that close() wraps a checked exception in a ConcurrentException
     *
     * @throws Exception
     */
    @Test
    public void testCloseWithCheckedException() throws Exception {

        final IOException ioException = new IOException();
        final FailableConsumer<?, ?> IOExceptionConsumer = (final CloseableCounter cc) -> {
            throw ioException;
        };

        final AbstractBackgroundInitializerTestImpl init = new SupplierBackgroundInitializerTestImpl(IOExceptionConsumer);
        init.start();
        init.get(); //ensure the Future has completed.
        try {
            init.close();
            fail();
        } catch (final Exception e) {
            assertThat(e, instanceOf(ConcurrentException.class));
            assertSame(ioException, e.getCause());
        }
    }

    /**
     * Tests that close() throws a runtime exception
     *
     * @throws Exception
     */
    @Test
    public void testCloseWithRuntimeException() throws Exception {

        final NullPointerException npe = new NullPointerException();
        final FailableConsumer<?, ?> NullPointerExceptionConsumer = (final CloseableCounter cc) -> {
            throw npe;
        };

        final AbstractBackgroundInitializerTestImpl init = new SupplierBackgroundInitializerTestImpl(NullPointerExceptionConsumer);
        init.start();
        init.get(); //ensure the Future has completed.
        try {
            init.close();
            fail();
        } catch (final Exception e) {
            assertSame(npe, e);
        }
    }
}

/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang3.concurrent;

import org.apache.commons.lang3.function.FailableConsumer;
import org.apache.commons.lang3.function.FailableSupplier;

/**
 * Test class for {@code AtomicInitializer}.
 */
class AtomicInitializerSupplierTest extends AbstractConcurrentInitializerCloseAndExceptionsTest<Object> {
    /**
     * Returns the initializer to be tested.
     *
     * @return the {@code AtomicInitializer}
     */
    @Override
    protected ConcurrentInitializer<Object> createInitializer() {
        return AtomicInitializer.<Object>builder().setInitializer(Object::new).get();
    }

    @Override
    protected ConcurrentInitializer<CloseableObject> createInitializerThatThrowsException(
            final FailableSupplier<CloseableObject, ? extends Exception> supplier,
            final FailableConsumer<CloseableObject, ? extends Exception> closer) {
        return AtomicInitializer.<CloseableObject>builder().setInitializer(supplier).setCloser(closer).get();
    }

}

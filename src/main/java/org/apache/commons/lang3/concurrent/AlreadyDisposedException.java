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

import java.util.concurrent.ExecutionException;

/**
 * An exception class used by LazyInitializerWithDisposer if get() is called after close() or dispose() .
 *
 * <p>
 * The purpose of this exception class is to allow the developer to be sure an exception was caused
 * by a {@link LazyInitializerWithDisposer}, being closed or disposed, and not from an error in the
 * process of creating or disposing of the wrapped object.
 * </p>
 *
 * @since 3.14.0
 */
public class AlreadyDisposedException extends ConcurrentException {
    /**
     * The serial version UID.
     */
    private static final long serialVersionUID = 6622707671812226131L;
}

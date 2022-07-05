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
package org.apache.commons.lang3.exception;

import org.apache.commons.lang3.AbstractLangTest;

/**
 * Base class for testing {@link Exception} descendants
 */
public abstract class AbstractExceptionTest extends AbstractLangTest {

    protected static final String CAUSE_MESSAGE = "Cause message";
    protected static final String EXCEPTION_MESSAGE = "Exception message";

    protected static final String WRONG_EXCEPTION_MESSAGE = "Wrong exception message";
    protected static final String WRONG_CAUSE_MESSAGE = "Wrong cause message";

    protected Exception generateCause() {
        return new Exception(CAUSE_MESSAGE);
    }
}

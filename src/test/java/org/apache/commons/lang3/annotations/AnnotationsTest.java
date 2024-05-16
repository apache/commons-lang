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
package org.apache.commons.lang3.annotations;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import java.lang.annotation.Target;
import java.util.function.Function;

import org.junit.jupiter.api.Test;


/** This class ensures, that the annotations are properly configured
 * with regard to {@link Target}.
 *
 * The so-called test methods are not actually testing anything, because
 * an invalid configuration would be detected by the compiler. However,
 * we have the unit test framework in place, and it is running anyways,
 * so there's no harm in a few additional methods.
 */
public class AnnotationsTest {
    public static class Wrapper {
        private final Object wrappedObject;

        @Insecure
        public Wrapper(Object wrappedObject) {
            this.wrappedObject = wrappedObject;
        }

        Object getWrappedObject() {
            return wrappedObject;
        }
    }

    private static Wrapper newWrapper(Object wrappedObject) {
        return new Wrapper(wrappedObject);
    }

    /** Test, whether we can have an @Insecure annotation on a constructor.
     */
    @Test
    public void testConstructorAnnotatableAsInsecure() {
        final Object unsafeObject = new Object();
        // Static code analysis should reject this, because the
        // parameter (the newly created instance of Object) isn't known
        // to be safe.
        final Wrapper wrapper = new Wrapper(unsafeObject);
        assertNotNull(wrapper);
        assertSame(unsafeObject, wrapper.getWrappedObject());
    }

    /** Test, whether we can have an @Insecure annotation on a method.
     */
    @Test
    public void testMethodAnnotatableAsInsecure() {
        final Object unsafeObject = new Object();
        // Static code analysis should reject this, because the
        // parameter (the newly created instance of Object) isn't known
        // to be safe.
        final Wrapper wrapper = newWrapper(unsafeObject);
        assertNotNull(wrapper);
        assertSame(unsafeObject, wrapper.getWrappedObject());
    }

    /** Test, whether we can have a @Safe annotation on a local variable.
     */
    @Test
    public void testLocalVariablesAnnotatableAsSafe() {
        @Safe final String wrappedString = "Hello, world!";
        // Static code analysis should accept this, because the variable
        // is annotated with @Safe.
        final Wrapper wrapper = newWrapper(wrappedString);
        assertNotNull(wrapper);
        assertSame(wrappedString, wrapper.getWrappedObject());
    }

    /** Test, whether we can have a @Safe annotation on a field.
     */
    @Test
    public void testFieldsAnnotatableAsSafe() {
        // Static code analysis should accept this, because the field
        // is annotated with @Safe.
        final Wrapper wrapper = newWrapper(wrappedStringField);
        assertNotNull(wrapper);
        assertSame(wrappedStringField, wrapper.getWrappedObject());
    }
    @Safe private String wrappedStringField = "Hello, world!";

    /** Test, whether we can have a @Safe annotation on a field.
     */
    @Test
    public void testParametersAnnotatableAsSafe() {
        // Static code analysis should accept this, because the parameter
        // is annotated with @Safe.
        final Function<String, Wrapper> wrapperCreator =
            new Function<String, Wrapper>() {
                @Override
                public Wrapper apply(@Safe String wrappedObject) {
                    return newWrapper(wrappedObject);
                }
            };
        final String helloWorldString = "Hello, world!";
        final Wrapper wrapper = wrapperCreator.apply(helloWorldString);
        assertNotNull(wrapper);
        assertSame(helloWorldString, wrapper.getWrappedObject());
    }
}

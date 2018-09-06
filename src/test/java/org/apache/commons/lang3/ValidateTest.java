/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.commons.lang3;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

/**
 * Unit tests {@link org.apache.commons.lang3.Validate}.
 */
class ValidateTest {

    @Test
    void testConstructor() {
        assertNotNull(new Validate());
        final Constructor<?>[] cons = Validate.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(Validate.class.getModifiers()));
        assertFalse(Modifier.isFinal(Validate.class.getModifiers()));
    }

    @Nested
    class IsTrue {

        @Nested
        class WithoutMessage {

            @Test
            void shouldNotThrowForTrueExpression() {
                Validate.isTrue(true);
            }

            @Test
            void shouldThrowExceptionWithDefaultMessageForFalseExpression() {
                final IllegalArgumentException ex = assertThrows(
                        IllegalArgumentException.class,
                        () -> Validate.isTrue(false));

                assertEquals("The validated expression is false", ex.getMessage());
            }

        }

        @Nested
        class WithMessage {

            @Test
            void shouldNotThrowForTrueExpression() {
                Validate.isTrue(true, "MSG");
            }

            @Test
            void shouldThrowExceptionWithGivenMessageForFalseExpression() {
                final IllegalArgumentException ex = assertThrows(
                        IllegalArgumentException.class,
                        () -> Validate.isTrue(false, "MSG"));

                assertEquals("MSG", ex.getMessage());
            }
        }

        @Nested
        class WithLongTemplate {

            @Test
            void shouldNotThrowForTrueExpression() {
                Validate.isTrue(true, "MSG", 6);
            }

            @Test
            void shouldThrowExceptionWithLongInsertedIntoTemplateMessageForFalseExpression() {
                final IllegalArgumentException ex = assertThrows(
                        IllegalArgumentException.class,
                        () -> Validate.isTrue(false, "MSG %s", 6));

                assertEquals("MSG 6", ex.getMessage());
            }
        }

        @Nested
        class WithDoubleTemplate {

            @Test
            void shouldNotThrowForTrueExpression() {
                Validate.isTrue(true, "MSG", 7.4d);
            }

            @Test
            void shouldThrowExceptionWithDoubleInsertedIntoTemplateMessageForFalseExpression() {
                final IllegalArgumentException ex = assertThrows(
                        IllegalArgumentException.class,
                        () -> Validate.isTrue(false, "MSG %s", 7.4d));

                assertEquals("MSG 7.4", ex.getMessage());
            }
        }

        @Nested
        class WithObjectTemplate {

            @Test
            void shouldNotThrowForTrueExpression() {
                Validate.isTrue(true, "MSG", "Object 1", "Object 2");
            }

            @Test
            void shouldThrowExceptionWithDoubleInsertedIntoTemplateMessageForFalseExpression() {
                final IllegalArgumentException ex = assertThrows(
                        IllegalArgumentException.class,
                        () -> Validate.isTrue(false, "MSG %s %s", "Object 1", "Object 2"));

                assertEquals("MSG Object 1 Object 2", ex.getMessage());
            }
        }
    }

    @Nested
    class NotNull {

        @Nested
        class WithoutMessage {

            @Test
            void shouldNotThrowForNonNullReference() {
                Validate.notNull(new Object());
            }

            @Test
            void shouldReturnTheSameInstance() {
                final String str = "Hi";
                final String result = Validate.notNull(str);

                assertSame(str, result);
            }

            @Test
            void shouldThrowExceptionWithDefaultMessageForNullReference() {
                final NullPointerException ex = assertThrows(
                        NullPointerException.class,
                        () -> Validate.notNull(null));

                assertEquals("The validated object is null", ex.getMessage());
            }
        }

        @Nested
        class WithMessage {

            @Test
            void shouldNotThrowForNonNullReference() {
                Validate.notNull(new Object(), "MSG");
            }

            @Test
            void shouldReturnTheSameInstance() {
                final String str = "Hi";
                final String result = Validate.notNull(str, "MSG");

                assertSame(str, result);
            }

            @Test
            void shouldThrowExceptionWithGivenMessageForNullReference() {
                final NullPointerException ex = assertThrows(
                        NullPointerException.class,
                        () -> Validate.notNull(null, "MSG"));

                assertEquals("MSG", ex.getMessage());
            }
        }
    }

    @Nested
    class NotEmpty {

        @Nested
        class WithArray {

            @Nested
            class WithoutMessage {

                @Test
                void shouldNotThrowExceptionForArrayContainingNullReference() {
                    Validate.notEmpty(new Object[]{null});
                }

                @Test
                void shouldReturnTheSameInstance() {
                    final String[] array = new String[]{"hi"};
                    final String[] result = Validate.notEmpty(array);

                    assertSame(array, result);
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultMessageForNullArray() {
                    final NullPointerException ex = assertThrows(
                            NullPointerException.class,
                            () -> Validate.notEmpty((Object[]) null));

                    assertEquals("The validated array is empty", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageForEmptyArray() {
                    final IllegalArgumentException ex = assertThrows(
                            IllegalArgumentException.class,
                            () -> Validate.notEmpty(new Object[0]));

                    assertEquals("The validated array is empty", ex.getMessage());
                }
            }

            @Nested
            class WithMessage {

                @Test
                void shouldNotThrowExceptionForArrayContainingNullReference() {
                    Validate.notEmpty(new Object[]{null}, "MSG");
                }

                @Test
                void shouldReturnTheSameInstance() {
                    final String[] array = new String[]{"hi"};
                    final String[] result = Validate.notEmpty(array, "MSG");

                    assertSame(array, result);
                }

                @Test
                void shouldThrowNullPointerExceptionWithGivenMessageForNullArray() {
                    final NullPointerException ex = assertThrows(
                            NullPointerException.class,
                            () -> Validate.notEmpty((Object[]) null, "MSG"));

                    assertEquals("MSG", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageForEmptyArray() {
                    final IllegalArgumentException ex = assertThrows(
                            IllegalArgumentException.class,
                            () -> Validate.notEmpty(new Object[0], "MSG"));

                    assertEquals("MSG", ex.getMessage());
                }
            }
        }

        @Nested
        class WithCollection {

            @Nested
            class WithoutMessage {

                @Test
                void shouldNotThrowExceptionForCollectionContainingNullReference() {
                    Validate.notEmpty(Collections.singleton(null));
                }

                @Test
                void shouldReturnTheSameInstance() {
                    final Set<String> col = Collections.singleton("Hi");
                    final Set<String> result = Validate.notEmpty(col);

                    assertSame(col, result);
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultMessageForNullCollection() {
                    final NullPointerException ex = assertThrows(
                            NullPointerException.class,
                            () -> Validate.notEmpty((Collection<?>) null));

                    assertEquals("The validated collection is empty", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageForEmptyCollection() {
                    final IllegalArgumentException ex = assertThrows(
                            IllegalArgumentException.class,
                            () -> Validate.notEmpty(Collections.emptySet()));

                    assertEquals("The validated collection is empty", ex.getMessage());
                }
            }

            @Nested
            class WithMessage {

                @Test
                void shouldNotThrowExceptionForCollectionContainingNullReference() {
                    Validate.notEmpty(Collections.singleton(null), "MSG");
                }

                @Test
                void shouldReturnTheSameInstance() {
                    final Set<String> col = Collections.singleton("Hi");
                    final Set<String> result = Validate.notEmpty(col, "MSG");

                    assertSame(col, result);
                }

                @Test
                void shouldThrowNullPointerExceptionWithGivenMessageForNullCollection() {
                    final NullPointerException ex = assertThrows(
                            NullPointerException.class,
                            () -> Validate.notEmpty((Collection<?>) null, "MSG"));

                    assertEquals("MSG", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithGivenMessageForEmptyCollection() {
                    final IllegalArgumentException ex = assertThrows(
                            IllegalArgumentException.class,
                            () -> Validate.notEmpty(Collections.emptySet(), "MSG"));

                    assertEquals("MSG", ex.getMessage());
                }
            }
        }

        @Nested
        class WithMap {

            @Nested
            class WithoutMessage {

                @Test
                void shouldNotThrowExceptionForMapContainingNullMapping() {
                    Validate.notEmpty(Collections.singletonMap("key", null));
                }

                @Test
                void shouldReturnTheSameInstance() {
                    final Map<String, String> map = Collections.singletonMap("key", "value");
                    final Map<String, String> result = Validate.notEmpty(map);

                    assertSame(map, result);
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultMessageForNullMap() {
                    final NullPointerException ex = assertThrows(
                            NullPointerException.class,
                            () -> Validate.notEmpty((Map<?, ?>) null));

                    assertEquals("The validated map is empty", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageForEmptyMap() {
                    final IllegalArgumentException ex = assertThrows(
                            IllegalArgumentException.class,
                            () -> Validate.notEmpty(Collections.emptyMap()));

                    assertEquals("The validated map is empty", ex.getMessage());
                }
            }

            @Nested
            class WithMessage {

                @Test
                void shouldNotThrowExceptionForMapContainingNullMapping() {
                    Validate.notEmpty(Collections.singletonMap("key", null), "MSG");
                }

                @Test
                void shouldReturnTheSameInstance() {
                    final Map<String, String> map = Collections.singletonMap("key", "value");
                    final Map<String, String> result = Validate.notEmpty(map, "MSG");

                    assertSame(map, result);
                }

                @Test
                void shouldThrowNullPointerExceptionWithGivenMessageForNullMap() {
                    final NullPointerException ex = assertThrows(
                            NullPointerException.class,
                            () -> Validate.notEmpty((Map<?, ?>) null, "MSG"));

                    assertEquals("MSG", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithGivenMessageForEmptyMap() {
                    final IllegalArgumentException ex = assertThrows(
                            IllegalArgumentException.class,
                            () -> Validate.notEmpty(Collections.emptyMap(), "MSG"));

                    assertEquals("MSG", ex.getMessage());
                }
            }
        }

        @Nested
        class WithCharSequence {

            @Nested
            class WithoutMessage {

                @Test
                void shouldNotThrowExceptionForNonEmptyString() {
                    Validate.notEmpty("Hi");
                }

                @Test
                void shouldReturnTheSameInstance() {
                    final String str = "Hi";
                    final String result = Validate.notEmpty(str);

                    assertSame(str, result);
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultMessageForNullCharSequence() {
                    final NullPointerException ex = assertThrows(
                            NullPointerException.class,
                            () -> Validate.notEmpty((CharSequence) null));

                    assertEquals("The validated character sequence is empty", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageForEmptyString() {
                    final IllegalArgumentException ex = assertThrows(
                            IllegalArgumentException.class,
                            () -> Validate.notEmpty(""));

                    assertEquals("The validated character sequence is empty", ex.getMessage());
                }
            }

            @Nested
            class WithMessage {

                @Test
                void shouldNotThrowExceptionForNonEmptyString() {
                    Validate.notEmpty("Hi", "MSG");
                }

                @Test
                void shouldReturnTheSameInstance() {
                    final String str = "Hi";
                    final String result = Validate.notEmpty(str, "MSG");

                    assertSame(str, result);
                }

                @Test
                void shouldThrowNullPointerExceptionWithGivenMessageForNullCharSequence() {
                    final NullPointerException ex = assertThrows(
                            NullPointerException.class,
                            () -> Validate.notEmpty((CharSequence) null, "MSG"));

                    assertEquals("MSG", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithGivenMessageForEmptyString() {
                    final IllegalArgumentException ex = assertThrows(
                            IllegalArgumentException.class,
                            () -> Validate.notEmpty("", "MSG"));

                    assertEquals("MSG", ex.getMessage());
                }
            }
        }
    }

    @Nested
    class NotBlank {

        @Nested
        class WithoutMessage {

            @Test
            void shouldNotThrowExceptionForNonEmptyString() {
                Validate.notBlank("abc");
            }

            @Test
            void shouldNotThrowExceptionForNonEmptyStringContainingSpaces() {
                Validate.notBlank("  abc   ");
            }

            @Test
            void shouldNotThrowExceptionForNonEmptyStringContainingWhitespaceChars() {
                Validate.notBlank(" \n \t abc \r \n ");
            }

            @Test
            void shouldReturnNonBlankValue() {
                final String str = "abc";
                final String result = Validate.notBlank(str);

                assertSame(str, result);
            }

            @Test
            void shouldThrowNullPointerExceptionWithDefaultMessageForNullString() {
                final NullPointerException ex = assertThrows(
                        NullPointerException.class,
                        () -> Validate.notBlank(null));

                assertEquals("The validated character sequence is blank", ex.getMessage());
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithDefaultMessageForEmptyString() {
                final IllegalArgumentException ex = assertThrows(
                        IllegalArgumentException.class,
                        () -> Validate.notBlank(""));

                assertEquals("The validated character sequence is blank", ex.getMessage());
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithDefaultMessageForBlankString() {
                final IllegalArgumentException ex = assertThrows(
                        IllegalArgumentException.class,
                        () -> Validate.notBlank("   "));

                assertEquals("The validated character sequence is blank", ex.getMessage());
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithDefaultMessageForStringContainingOnlyWhitespaceChars() {
                final IllegalArgumentException ex = assertThrows(
                        IllegalArgumentException.class,
                        () -> Validate.notBlank(" \n \t \r \n "));

                assertEquals("The validated character sequence is blank", ex.getMessage());
            }
        }

        @Nested
        class WithMessage {

            @Test
            void shouldNotThrowExceptionForNonEmptyString() {
                Validate.notBlank("abc", "MSG");
            }

            @Test
            void shouldNotThrowExceptionForNonEmptyStringContainingSpaces() {
                Validate.notBlank("  abc   ", "MSG");
            }

            @Test
            void shouldNotThrowExceptionForNonEmptyStringContainingWhitespaceChars() {
                Validate.notBlank(" \n \t abc \r \n ", "MSG");
            }

            @Test
            void shouldReturnNonBlankValue() {
                final String str = "abc";
                final String result = Validate.notBlank(str, "MSG");

                assertSame(str, result);
            }

            @Test
            void shouldThrowNullPointerExceptionWithGivenMessageForNullString() {
                final NullPointerException ex = assertThrows(
                        NullPointerException.class,
                        () -> Validate.notBlank(null, "MSG"));

                assertEquals("MSG", ex.getMessage());
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithGivenMessageForEmptyString() {
                final IllegalArgumentException ex = assertThrows(
                        IllegalArgumentException.class,
                        () -> Validate.notBlank("", "MSG"));

                assertEquals("MSG", ex.getMessage());
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithGivenMessageForBlankString() {
                final IllegalArgumentException ex = assertThrows(
                        IllegalArgumentException.class,
                        () -> Validate.notBlank("   ", "MSG"));

                assertEquals("MSG", ex.getMessage());
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithGivenMessageForStringContainingOnlyWhitespaceChars() {
                final IllegalArgumentException ex = assertThrows(
                        IllegalArgumentException.class,
                        () -> Validate.notBlank(" \n \t \r \n ", "MSG"));

                assertEquals("MSG", ex.getMessage());
            }
        }
    }

    @Nested
    class NoNullElements {

        @Nested
        class WithArray {

            @Nested
            class WithoutMessage {

                @Test
                void shouldNotThrowExceptionForNonEmptyArray() {
                    Validate.noNullElements(new String[]{"a", "b"});
                }

                @Test
                void shouldReturnSameInstance() {
                    final String[] array = {"a", "b"};
                    final String[] result = Validate.noNullElements(array);

                    assertSame(array, result);
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultMessageForNullArray() {
                    final NullPointerException ex = assertThrows(
                            NullPointerException.class,
                            () -> Validate.noNullElements((Object[]) null));

                    assertEquals("The validated object is null", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageForArrayWithNullElement() {
                    final IllegalArgumentException ex = assertThrows(
                            IllegalArgumentException.class,
                            () -> Validate.noNullElements(new String[]{"a", null}));

                    assertEquals("The validated array contains null element at index: 1", ex.getMessage());
                }
            }

            @Nested
            class WithMessage {

                @Test
                void shouldNotThrowExceptionForNonEmptyArray() {
                    Validate.noNullElements(new String[]{"a", "b"}, "MSG");
                }

                @Test
                void shouldReturnSameInstance() {
                    final String[] array = {"a", "b"};
                    final String[] result = Validate.noNullElements(array, "MSG");

                    assertSame(array, result);
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultMessageForNullArray() {
                    final NullPointerException ex = assertThrows(
                            NullPointerException.class,
                            () -> Validate.noNullElements((Object[]) null, "MSG"));

                    assertEquals("The validated object is null", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithGivenMessageForArrayWithNullElement() {
                    final IllegalArgumentException ex = assertThrows(
                            IllegalArgumentException.class,
                            () -> Validate.noNullElements(new String[]{"a", null}, "MSG"));

                    assertEquals("MSG", ex.getMessage());
                }
            }
        }

        @Nested
        class WithCollection {

            @Nested
            class WithoutMessage {

                @Test
                void shouldNotThrowExceptionForNonEmptyCollection() {
                    Validate.noNullElements(Collections.singleton("a"));
                }

                @Test
                void shouldReturnSameInstance() {
                    Set<String> col = Collections.singleton("a");
                    final Set<String> result = Validate.noNullElements(col);

                    assertSame(col, result);
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultMessageForNullCollection() {
                    final NullPointerException ex = assertThrows(
                            NullPointerException.class,
                            () -> Validate.noNullElements((Collection<?>) null));

                    assertEquals("The validated object is null", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageForCollectionWithNullElement() {
                    final IllegalArgumentException ex = assertThrows(
                            IllegalArgumentException.class,
                            () -> Validate.noNullElements(Collections.singleton(null)));

                    assertEquals("The validated collection contains null element at index: 0", ex.getMessage());
                }
            }

            @Nested
            class WithMessage {

                @Test
                void shouldNotThrowExceptionForNonEmptyCollection() {
                    Validate.noNullElements(Collections.singleton("a"), "MSG");
                }

                @Test
                void shouldReturnSameInstance() {
                    Set<String> col = Collections.singleton("a");
                    final Set<String> result = Validate.noNullElements(col, "MSG");

                    assertSame(col, result);
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultMessageForNullCollection() {
                    final NullPointerException ex = assertThrows(
                            NullPointerException.class,
                            () -> Validate.noNullElements((Collection<?>) null, "MSG"));

                    assertEquals("The validated object is null", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithGivenMessageForCollectionWithNullElement() {
                    final IllegalArgumentException ex = assertThrows(
                            IllegalArgumentException.class,
                            () -> Validate.noNullElements(Collections.singleton(null), "MSG"));

                    assertEquals("MSG", ex.getMessage());
                }
            }
        }
    }

    @Nested
    class ValidIndex {

        @Nested
        class WithArray {

            @Nested
            class WithoutMessage {

                @Test
                void shouldNotThrowExceptionForValidIndex() {
                    Validate.validIndex(new String[]{"a"}, 0);
                }

                @Test
                void shouldReturnSameInstance() {
                    final String[] array = {"a"};
                    final String[] result = Validate.validIndex(array, 0);

                    assertSame(array, result);
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultForNullArray() {
                    final NullPointerException ex = assertThrows(
                            NullPointerException.class,
                            () -> Validate.validIndex((Object[]) null, 1));

                    assertEquals("The validated object is null", ex.getMessage());
                }

                @Test
                void shouldThrowIndexOutOfBoundsExceptionWithDefaultMessageForNegativeIndex() {
                    final IndexOutOfBoundsException ex = assertThrows(
                            IndexOutOfBoundsException.class,
                            () -> Validate.validIndex(new String[]{"a"}, -1));

                    assertEquals("The validated array index is invalid: -1", ex.getMessage());
                }

                @Test
                void shouldThrowIndexOutOfBoundsExceptionWithDefaultMessageForIndexOutOfBounds() {
                    final IndexOutOfBoundsException ex = assertThrows(
                            IndexOutOfBoundsException.class,
                            () -> Validate.validIndex(new String[]{"a"}, 1));

                    assertEquals("The validated array index is invalid: 1", ex.getMessage());
                }
            }

            @Nested
            class WithMessage {

                @Test
                void shouldNotThrowExceptionForValidIndex() {
                    Validate.validIndex(new String[]{"a"}, 0, "MSG");
                }

                @Test
                void shouldReturnSameInstance() {
                    final String[] array = {"a"};
                    final String[] result = Validate.validIndex(array, 0, "MSG");

                    assertSame(array, result);
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultMessageForNullArray() {
                    final NullPointerException ex = assertThrows(
                            NullPointerException.class,
                            () -> Validate.validIndex((Object[]) null, 1, "MSG"));

                    assertEquals("The validated object is null", ex.getMessage());
                }

                @Test
                void shouldThrowIndexOutOfBoundsExceptionWithGivenMessageForNegativeIndex() {
                    final IndexOutOfBoundsException ex = assertThrows(
                            IndexOutOfBoundsException.class,
                            () -> Validate.validIndex(new String[]{"a"}, -1, "MSG"));

                    assertEquals("MSG", ex.getMessage());
                }

                @Test
                void shouldThrowIndexOutOfBoundsExceptionWithGivenMessageForIndexOutOfBounds() {
                    final IndexOutOfBoundsException ex = assertThrows(
                            IndexOutOfBoundsException.class,
                            () -> Validate.validIndex(new String[]{"a"}, 1, "MSG"));

                    assertEquals("MSG", ex.getMessage());
                }
            }
        }

        @Nested
        class WithCollection {

            @Nested
            class WithoutMessage {

                @Test
                void shouldNotThrowExceptionForValidIndex() {
                    Validate.validIndex(Collections.singleton("a"), 0);
                }

                @Test
                void shouldReturnSameInstance() {
                    final Set<String> col = Collections.singleton("a");
                    final Set<String> result = Validate.validIndex(col, 0);

                    assertSame(col, result);
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultForNullCollection() {
                    final NullPointerException ex = assertThrows(
                            NullPointerException.class,
                            () -> Validate.validIndex((Collection<?>) null, 1));

                    assertEquals("The validated object is null", ex.getMessage());
                }

                @Test
                void shouldThrowIndexOutOfBoundsExceptionWithDefaultMessageForNegativeIndex() {
                    final IndexOutOfBoundsException ex = assertThrows(
                            IndexOutOfBoundsException.class,
                            () -> Validate.validIndex(Collections.singleton("a"), -1));

                    assertEquals("The validated collection index is invalid: -1", ex.getMessage());
                }

                @Test
                void shouldThrowIndexOutOfBoundsExceptionWithDefaultMessageForIndexOutOfBounds() {
                    final IndexOutOfBoundsException ex = assertThrows(
                            IndexOutOfBoundsException.class,
                            () -> Validate.validIndex(Collections.singleton("a"), 1));

                    assertEquals("The validated collection index is invalid: 1", ex.getMessage());
                }
            }

            @Nested
            class WithMessage {

                @Test
                void shouldNotThrowExceptionForValidIndex() {
                    Validate.validIndex(Collections.singleton("a"), 0, "MSG");
                }

                @Test
                void shouldReturnSameInstance() {
                    final Set<String> col = Collections.singleton("a");
                    final Set<String> result = Validate.validIndex(col, 0, "MSG");

                    assertSame(col, result);
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultMessageForNullCollection() {
                    final NullPointerException ex = assertThrows(
                            NullPointerException.class,
                            () -> Validate.validIndex((Collection<?>) null, 1, "MSG"));

                    assertEquals("The validated object is null", ex.getMessage());
                }

                @Test
                void shouldThrowIndexOutOfBoundsExceptionWithGivenMessageForNegativeIndex() {
                    final IndexOutOfBoundsException ex = assertThrows(
                            IndexOutOfBoundsException.class,
                            () -> Validate.validIndex(Collections.singleton("a"), -1, "MSG"));

                    assertEquals("MSG", ex.getMessage());
                }

                @Test
                void shouldThrowIndexOutOfBoundsExceptionWithGivenMessageForIndexOutOfBounds() {
                    final IndexOutOfBoundsException ex = assertThrows(
                            IndexOutOfBoundsException.class,
                            () -> Validate.validIndex(Collections.singleton("a"), 1, "MSG"));

                    assertEquals("MSG", ex.getMessage());
                }
            }
        }

        @Nested
        class WithCharSequence {

            @Nested
            class WithoutMessage {

                @Test
                void shouldNotThrowExceptionForValidIndex() {
                    Validate.validIndex("a", 0);
                }

                @Test
                void shouldReturnSameInstance() {
                    final String str = "a";
                    final String result = Validate.validIndex(str, 0);

                    assertSame(str, result);
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultForNullString() {
                    final NullPointerException ex = assertThrows(
                            NullPointerException.class,
                            () -> Validate.validIndex((String) null, 1));

                    assertEquals("The validated object is null", ex.getMessage());
                }

                @Test
                void shouldThrowIndexOutOfBoundsExceptionWithDefaultMessageForNegativeIndex() {
                    final IndexOutOfBoundsException ex = assertThrows(
                            IndexOutOfBoundsException.class,
                            () -> Validate.validIndex("a", -1));

                    assertEquals("The validated character sequence index is invalid: -1", ex.getMessage());
                }

                @Test
                void shouldThrowIndexOutOfBoundsExceptionWithDefaultMessageForIndexOutOfBounds() {
                    final IndexOutOfBoundsException ex = assertThrows(
                            IndexOutOfBoundsException.class,
                            () -> Validate.validIndex("a", 1));

                    assertEquals("The validated character sequence index is invalid: 1", ex.getMessage());
                }
            }

            @Nested
            class WithMessage {

                @Test
                void shouldNotThrowExceptionForValidIndex() {
                    Validate.validIndex("a", 0, "MSG");
                }

                @Test
                void shouldReturnSameInstance() {
                    final String str = "a";
                    final String result = Validate.validIndex(str, 0, "MSG");

                    assertSame(str, result);
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultMessageForNullStr() {
                    final NullPointerException ex = assertThrows(
                            NullPointerException.class,
                            () -> Validate.validIndex((String) null, 1, "MSG"));

                    assertEquals("The validated object is null", ex.getMessage());
                }

                @Test
                void shouldThrowIndexOutOfBoundsExceptionWithGivenMessageForNegativeIndex() {
                    final IndexOutOfBoundsException ex = assertThrows(
                            IndexOutOfBoundsException.class,
                            () -> Validate.validIndex("a", -1, "MSG"));

                    assertEquals("MSG", ex.getMessage());
                }

                @Test
                void shouldThrowIndexOutOfBoundsExceptionWithGivenMessageForIndexOutOfBounds() {
                    final IndexOutOfBoundsException ex = assertThrows(
                            IndexOutOfBoundsException.class,
                            () -> Validate.validIndex("a", 1, "MSG"));

                    assertEquals("MSG", ex.getMessage());
                }
            }
        }
    }

    @Nested
    class MatchesPattern {

        @Nested
        class WithoutMessage {

            @Test
            void shouldNotThrowExceptionWhenStringMatchesPattern() {
                Validate.matchesPattern("hi", "[a-z]*");
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithDefaultMessageWhenStringDoesNotMatchPattern() {
                final IllegalArgumentException ex = assertThrows(
                        IllegalArgumentException.class,
                        () -> Validate.matchesPattern("hi", "[0-9]*"));

                assertEquals("The string hi does not match the pattern [0-9]*", ex.getMessage());
            }
        }

        @Nested
        class WithMessage {

            @Test
            void shouldNotThrowExceptionWhenStringMatchesPattern() {
                Validate.matchesPattern("hi", "[a-z]*", "MSG");
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWhenStringDoesNotMatchPattern() {
                final IllegalArgumentException ex = assertThrows(
                        IllegalArgumentException.class,
                        () -> Validate.matchesPattern("hi", "[0-9]*", "MSG"));

                assertEquals("MSG", ex.getMessage());
            }
        }
    }

    @Test
    void testNotNaN1() {
        Validate.notNaN(0.0);
        Validate.notNaN(Double.POSITIVE_INFINITY);
        Validate.notNaN(Double.NEGATIVE_INFINITY);
        try {
            Validate.notNaN(Double.NaN);
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException ex) {
            assertEquals("The validated value is not a number", ex.getMessage());
        }
    }

    @Test
    void testNotNaN2() {
        Validate.notNaN(0.0, "MSG");
        Validate.notNaN(Double.POSITIVE_INFINITY, "MSG");
        Validate.notNaN(Double.NEGATIVE_INFINITY, "MSG");
        try {
            Validate.notNaN(Double.NaN, "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    //-----------------------------------------------------------------------

    @Test
    void testFinite1() {
        Validate.finite(0.0);
        try {
            Validate.finite(Double.POSITIVE_INFINITY);
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException ex) {
            assertEquals("The value is invalid: Infinity", ex.getMessage());
        }
        try {
            Validate.finite(Double.NEGATIVE_INFINITY);
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException ex) {
            assertEquals("The value is invalid: -Infinity", ex.getMessage());
        }
        try {
            Validate.finite(Double.NaN);
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException ex) {
            assertEquals("The value is invalid: NaN", ex.getMessage());
        }
    }

    @Test
    void testFinite2() {
        Validate.finite(0.0, "MSG");
        try {
            Validate.finite(Double.POSITIVE_INFINITY, "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
        try {
            Validate.finite(Double.NEGATIVE_INFINITY, "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
        try {
            Validate.finite(Double.NaN, "MSG");
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException ex) {
            assertEquals("MSG", ex.getMessage());
        }
    }

    //-----------------------------------------------------------------------
    //-----------------------------------------------------------------------

    @Test
    void testInclusiveBetween() {
        Validate.inclusiveBetween("a", "c", "b");
        try {
            Validate.inclusiveBetween("0", "5", "6");
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException e) {
            assertEquals("The value 6 is not in the specified inclusive range of 0 to 5", e.getMessage());
        }
    }

    @Test
    void testInclusiveBetween_withMessage() {
        Validate.inclusiveBetween("a", "c", "b", "Error");
        try {
            Validate.inclusiveBetween("0", "5", "6", "Error");
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException e) {
            assertEquals("Error", e.getMessage());
        }
    }

    @Test
    void testInclusiveBetweenLong() {
        Validate.inclusiveBetween(0, 2, 1);
        Validate.inclusiveBetween(0, 2, 2);
        try {
            Validate.inclusiveBetween(0, 5, 6);
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException e) {
            assertEquals("The value 6 is not in the specified inclusive range of 0 to 5", e.getMessage());
        }
    }

    @Test
    void testInclusiveBetweenLong_withMessage() {
        Validate.inclusiveBetween(0, 2, 1, "Error");
        Validate.inclusiveBetween(0, 2, 2, "Error");
        try {
            Validate.inclusiveBetween(0, 5, 6, "Error");
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException e) {
            assertEquals("Error", e.getMessage());
        }
    }

    @Test
    void testInclusiveBetweenDouble() {
        Validate.inclusiveBetween(0.1, 2.1, 1.1);
        Validate.inclusiveBetween(0.1, 2.1, 2.1);
        try {
            Validate.inclusiveBetween(0.1, 5.1, 6.1);
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException e) {
            assertEquals("The value 6.1 is not in the specified inclusive range of 0.1 to 5.1", e.getMessage());
        }
    }

    @Test
    void testInclusiveBetweenDouble_withMessage() {
        Validate.inclusiveBetween(0.1, 2.1, 1.1, "Error");
        Validate.inclusiveBetween(0.1, 2.1, 2.1, "Error");
        try {
            Validate.inclusiveBetween(0.1, 5.1, 6.1, "Error");
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException e) {
            assertEquals("Error", e.getMessage());
        }
    }

    @Test
    void testExclusiveBetween() {
        Validate.exclusiveBetween("a", "c", "b");
        try {
            Validate.exclusiveBetween("0", "5", "6");
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException e) {
            assertEquals("The value 6 is not in the specified exclusive range of 0 to 5", e.getMessage());
        }
        try {
            Validate.exclusiveBetween("0", "5", "5");
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException e) {
            assertEquals("The value 5 is not in the specified exclusive range of 0 to 5", e.getMessage());
        }
    }

    @Test
    void testExclusiveBetween_withMessage() {
        Validate.exclusiveBetween("a", "c", "b", "Error");
        try {
            Validate.exclusiveBetween("0", "5", "6", "Error");
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException e) {
            assertEquals("Error", e.getMessage());
        }
        try {
            Validate.exclusiveBetween("0", "5", "5", "Error");
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException e) {
            assertEquals("Error", e.getMessage());
        }
    }

    @Test
    void testExclusiveBetweenLong() {
        Validate.exclusiveBetween(0, 2, 1);
        try {
            Validate.exclusiveBetween(0, 5, 6);
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException e) {
            assertEquals("The value 6 is not in the specified exclusive range of 0 to 5", e.getMessage());
        }
        try {
            Validate.exclusiveBetween(0, 5, 5);
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException e) {
            assertEquals("The value 5 is not in the specified exclusive range of 0 to 5", e.getMessage());
        }
    }

    @Test
    void testExclusiveBetweenLong_withMessage() {
        Validate.exclusiveBetween(0, 2, 1, "Error");
        try {
            Validate.exclusiveBetween(0, 5, 6, "Error");
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException e) {
            assertEquals("Error", e.getMessage());
        }
        try {
            Validate.exclusiveBetween(0, 5, 5, "Error");
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException e) {
            assertEquals("Error", e.getMessage());
        }
    }

    @Test
    void testExclusiveBetweenDouble() {
        Validate.exclusiveBetween(0.1, 2.1, 1.1);
        try {
            Validate.exclusiveBetween(0.1, 5.1, 6.1);
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException e) {
            assertEquals("The value 6.1 is not in the specified exclusive range of 0.1 to 5.1", e.getMessage());
        }
        try {
            Validate.exclusiveBetween(0.1, 5.1, 5.1);
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException e) {
            assertEquals("The value 5.1 is not in the specified exclusive range of 0.1 to 5.1", e.getMessage());
        }
    }

    @Test
    void testExclusiveBetweenDouble_withMessage() {
        Validate.exclusiveBetween(0.1, 2.1, 1.1, "Error");
        try {
            Validate.exclusiveBetween(0.1, 5.1, 6.1, "Error");
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException e) {
            assertEquals("Error", e.getMessage());
        }
        try {
            Validate.exclusiveBetween(0.1, 5.1, 5.1, "Error");
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException e) {
            assertEquals("Error", e.getMessage());
        }
    }

    @Test
    void testIsInstanceOf() {
        Validate.isInstanceOf(String.class, "hi");
        Validate.isInstanceOf(Integer.class, 1);
    }

    @Test
    void testIsInstanceOfExceptionMessage() {
        try {
            Validate.isInstanceOf(List.class, "hi");
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException e) {
            assertEquals("Expected type: java.util.List, actual: java.lang.String", e.getMessage());
        }
    }

    @Test
    void testIsInstanceOf_withMessage() {
        Validate.isInstanceOf(String.class, "hi", "Error");
        Validate.isInstanceOf(Integer.class, 1, "Error");
        try {
            Validate.isInstanceOf(List.class, "hi", "Error");
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException e) {
            assertEquals("Error", e.getMessage());
        }
    }

    @Test
    void testIsInstanceOf_withMessageArgs() {
        Validate.isInstanceOf(String.class, "hi", "Error %s=%s", "Name", "Value");
        Validate.isInstanceOf(Integer.class, 1, "Error %s=%s", "Name", "Value");
        try {
            Validate.isInstanceOf(List.class, "hi", "Error %s=%s", "Name", "Value");
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException e) {
            assertEquals("Error Name=Value", e.getMessage());
        }
        try {
            Validate.isInstanceOf(List.class, "hi", "Error %s=%s", List.class, "Value");
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException e) {
            assertEquals("Error interface java.util.List=Value", e.getMessage());
        }
        try {
            Validate.isInstanceOf(List.class, "hi", "Error %s=%s", List.class, null);
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException e) {
            assertEquals("Error interface java.util.List=null", e.getMessage());
        }
    }

    @Test
    void testIsAssignable() {
        Validate.isAssignableFrom(CharSequence.class, String.class);
        Validate.isAssignableFrom(AbstractList.class, ArrayList.class);
    }

    @Test
    void testIsAssignableExceptionMessage() {
        try {
            Validate.isAssignableFrom(List.class, String.class);
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException e) {
            assertEquals("Cannot assign a java.lang.String to a java.util.List", e.getMessage());
        }
    }

    @Test
    void testIsAssignable_withMessage() {
        Validate.isAssignableFrom(CharSequence.class, String.class, "Error");
        Validate.isAssignableFrom(AbstractList.class, ArrayList.class, "Error");
        try {
            Validate.isAssignableFrom(List.class, String.class, "Error");
            fail("Expecting IllegalArgumentException");
        } catch (final IllegalArgumentException e) {
            assertEquals("Error", e.getMessage());
        }
    }

}

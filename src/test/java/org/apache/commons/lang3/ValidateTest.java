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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

/**
 * Tests {@link Validate}.
 */
public class ValidateTest extends AbstractLangTest {

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
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.isTrue(false));
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
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.isTrue(false, "MSG"));
                assertEquals("MSG", ex.getMessage());
            }

            @Test
            void shouldThrowExceptionWithGivenMessageContainingSpecialCharacterForFalseExpression() {
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.isTrue(false, "%"));
                assertEquals("%", ex.getMessage());
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
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.isTrue(false, "MSG %s", 6));
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
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.isTrue(false, "MSG %s", 7.4d));
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
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
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
                assertSame("Hi", Validate.notNull("Hi"));
            }

            @Test
            void shouldThrowExceptionWithDefaultMessageForNullReference() {
                final NullPointerException ex = assertThrows(NullPointerException.class, () -> Validate.notNull(null));
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
                assertSame("Hi", Validate.notNull("Hi", "MSG"));
            }

            @Test
            void shouldThrowExceptionWithGivenMessageForNullReference() {
                final NullPointerException ex = assertThrows(NullPointerException.class, () -> Validate.notNull(null, "MSG"));
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
                    Validate.notEmpty(new Object[] {null});
                }

                @Test
                void shouldReturnTheSameInstance() {
                    final String[] expected = new String[] {"hi"};
                    assertSame(expected, Validate.notEmpty(expected));
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultMessageForNullArray() {
                    final NullPointerException ex = assertThrows(NullPointerException.class, () -> Validate.notEmpty((Object[]) null));
                    assertEquals("The validated array is empty", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageForEmptyArray() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.notEmpty(new Object[0]));
                    assertEquals("The validated array is empty", ex.getMessage());
                }
            }

            @Nested
            class WithMessage {

                @Test
                void shouldNotThrowExceptionForArrayContainingNullReference() {
                    Validate.notEmpty(new Object[] {null}, "MSG");
                }

                @Test
                void shouldReturnTheSameInstance() {
                    final String[] expected = new String[] {"hi"};
                    assertSame(expected, Validate.notEmpty(expected, "MSG"));
                }

                @Test
                void shouldThrowNullPointerExceptionWithGivenMessageForNullArray() {
                    final NullPointerException ex = assertThrows(NullPointerException.class, () -> Validate.notEmpty((Object[]) null, "MSG"));
                    assertEquals("MSG", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageForEmptyArray() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.notEmpty(new Object[0], "MSG"));
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
                    final Set<String> singleton = Collections.singleton("Hi");
                    assertSame(singleton, Validate.notEmpty(singleton));
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultMessageForNullCollection() {
                    final NullPointerException ex = assertThrows(NullPointerException.class, () -> Validate.notEmpty((Collection<?>) null));
                    assertEquals("The validated collection is empty", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageForEmptyCollection() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.notEmpty(Collections.emptySet()));
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
                    final Set<String> singleton = Collections.singleton("Hi");
                    assertSame(singleton, Validate.notEmpty(singleton, "MSG"));
                }

                @Test
                void shouldThrowNullPointerExceptionWithGivenMessageForNullCollection() {
                    final NullPointerException ex = assertThrows(NullPointerException.class, () -> Validate.notEmpty((Collection<?>) null, "MSG"));
                    assertEquals("MSG", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithGivenMessageForEmptyCollection() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.notEmpty(Collections.emptySet(), "MSG"));
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
                    final Map<String, String> singletonMap = Collections.singletonMap("key", "value");
                    assertSame(singletonMap, Validate.notEmpty(singletonMap));
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultMessageForNullMap() {
                    final NullPointerException ex = assertThrows(NullPointerException.class, () -> Validate.notEmpty((Map<?, ?>) null));
                    assertEquals("The validated map is empty", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageForEmptyMap() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.notEmpty(Collections.emptyMap()));
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
                    final Map<String, String> singletonMap = Collections.singletonMap("key", "value");
                    assertSame(singletonMap, Validate.notEmpty(singletonMap, "MSG"));
                }

                @Test
                void shouldThrowNullPointerExceptionWithGivenMessageForNullMap() {
                    final NullPointerException ex = assertThrows(NullPointerException.class, () -> Validate.notEmpty((Map<?, ?>) null, "MSG"));
                    assertEquals("MSG", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithGivenMessageForEmptyMap() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.notEmpty(Collections.emptyMap(), "MSG"));
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
                    assertSame("Hi", Validate.notEmpty("Hi"));
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultMessageForNullCharSequence() {
                    final NullPointerException ex = assertThrows(NullPointerException.class, () -> Validate.notEmpty((CharSequence) null));
                    assertEquals("The validated character sequence is empty", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageForEmptyString() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.notEmpty(""));
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
                    assertSame("Hi", Validate.notEmpty("Hi", "MSG"));
                }

                @Test
                void shouldThrowNullPointerExceptionWithGivenMessageForNullCharSequence() {
                    final NullPointerException ex = assertThrows(NullPointerException.class, () -> Validate.notEmpty((CharSequence) null, "MSG"));
                    assertEquals("MSG", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithGivenMessageForEmptyString() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.notEmpty("", "MSG"));
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
                assertSame("abc", Validate.notBlank("abc"));
            }

            @Test
            void shouldThrowNullPointerExceptionWithDefaultMessageForNullString() {
                final NullPointerException ex = assertThrows(NullPointerException.class, () -> Validate.notBlank(null));
                assertEquals("The validated character sequence is blank", ex.getMessage());
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithDefaultMessageForEmptyString() {
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.notBlank(""));
                assertEquals("The validated character sequence is blank", ex.getMessage());
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithDefaultMessageForBlankString() {
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.notBlank("   "));
                assertEquals("The validated character sequence is blank", ex.getMessage());
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithDefaultMessageForStringContainingOnlyWhitespaceChars() {
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.notBlank(" \n \t \r \n "));
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
                assertSame("abc", Validate.notBlank("abc", "MSG"));
            }

            @Test
            void shouldThrowNullPointerExceptionWithGivenMessageForNullString() {
                final NullPointerException ex = assertThrows(NullPointerException.class, () -> Validate.notBlank(null, "MSG"));
                assertEquals("MSG", ex.getMessage());
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithGivenMessageForEmptyString() {
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.notBlank("", "MSG"));
                assertEquals("MSG", ex.getMessage());
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithGivenMessageForBlankString() {
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.notBlank("   ", "MSG"));
                assertEquals("MSG", ex.getMessage());
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithGivenMessageForStringContainingOnlyWhitespaceChars() {
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.notBlank(" \n \t \r \n ", "MSG"));
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
                    Validate.noNullElements(new String[] {"a", "b"});
                }

                @Test
                void shouldReturnSameInstance() {
                    final String[] expected = new String[] {"a", "b"};
                    assertSame(expected, Validate.noNullElements(expected));
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultMessageForNullArray() {
                    final NullPointerException ex = assertThrows(NullPointerException.class, () -> Validate.noNullElements((Object[]) null));
                    assertEquals("array", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageForArrayWithNullElement() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.noNullElements(new String[] {"a", null}));
                    assertEquals("The validated array contains null element at index: 1", ex.getMessage());
                }
            }

            @Nested
            class WithMessage {

                @Test
                void shouldNotThrowExceptionForNonEmptyArray() {
                    Validate.noNullElements(new String[] {"a", "b"}, "MSG");
                }

                @Test
                void shouldReturnSameInstance() {
                    final String[] array = {"a", "b"};
                    assertSame(array, Validate.noNullElements(array, "MSG"));
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultMessageForNullArray() {
                    final NullPointerException ex = assertThrows(NullPointerException.class, () -> Validate.noNullElements((Object[]) null, "MSG"));
                    assertEquals("array", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithGivenMessageForArrayWithNullElement() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.noNullElements(new String[] {"a", null}, "MSG"));
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
                    final Set<String> col = Collections.singleton("a");
                    assertSame(col, Validate.noNullElements(col));
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultMessageForNullCollection() {
                    final NullPointerException ex = assertThrows(NullPointerException.class, () -> Validate.noNullElements((Collection<?>) null));
                    assertEquals("iterable", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageForCollectionWithNullElement() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
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
                    final Set<String> col = Collections.singleton("a");
                    assertSame(col, Validate.noNullElements(col, "MSG"));
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultMessageForNullCollection() {
                    final NullPointerException ex = assertThrows(NullPointerException.class, () -> Validate.noNullElements((Collection<?>) null, "MSG"));
                    assertEquals("iterable", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithGivenMessageForCollectionWithNullElement() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.noNullElements(Collections.singleton(null), "MSG"));
                    assertEquals("MSG", ex.getMessage());
                }
            }
        }
    }

    @Nested
    class ValidState {

        @Nested
        class WitMessage {
            @Test
            void shouldNotThrowExceptionForValidIndex() {
                Validate.validState(true, "The Message");
            }

            @Test
            void shouldThrowExceptionForTrueExpression() {
                assertThrows(IllegalStateException.class, () -> Validate.validState(false, "The Message"));
            }

        }

        @Nested
        class WithoutMessage {

            @Test
            void shouldNotThrowExceptionForTrueExpression() {
                Validate.validState(true);
            }

            @Test
            void shouldThrowExceptionForTrueExpression() {
                assertThrows(IllegalStateException.class, () -> Validate.validState(false));
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
                    Validate.validIndex(new String[] {"a"}, 0);
                }

                @Test
                void shouldReturnSameInstance() {
                    final String[] array = {"a"};
                    assertSame(array, Validate.validIndex(array, 0));
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultForNullArray() {
                    final NullPointerException ex = assertThrows(NullPointerException.class, () -> Validate.validIndex((Object[]) null, 1));
                    assertEquals("array", ex.getMessage());
                }

                @Test
                void shouldThrowIndexOutOfBoundsExceptionWithDefaultMessageForNegativeIndex() {
                    final IndexOutOfBoundsException ex = assertThrows(IndexOutOfBoundsException.class, () -> Validate.validIndex(new String[] {"a"}, -1));
                    assertEquals("The validated array index is invalid: -1", ex.getMessage());
                }

                @Test
                void shouldThrowIndexOutOfBoundsExceptionWithDefaultMessageForIndexOutOfBounds() {
                    final IndexOutOfBoundsException ex = assertThrows(IndexOutOfBoundsException.class, () -> Validate.validIndex(new String[] {"a"}, 1));
                    assertEquals("The validated array index is invalid: 1", ex.getMessage());
                }
            }

            @Nested
            class WithMessage {

                @Test
                void shouldNotThrowExceptionForValidIndex() {
                    Validate.validIndex(new String[] {"a"}, 0, "MSG");
                }

                @Test
                void shouldReturnSameInstance() {
                    final String[] array = {"a"};
                    assertSame(array, Validate.validIndex(array, 0, "MSG"));
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultMessageForNullArray() {
                    final NullPointerException ex = assertThrows(NullPointerException.class, () -> Validate.validIndex((Object[]) null, 1, "MSG"));
                    assertEquals("array", ex.getMessage());
                }

                @Test
                void shouldThrowIndexOutOfBoundsExceptionWithGivenMessageForNegativeIndex() {
                    final IndexOutOfBoundsException ex = assertThrows(IndexOutOfBoundsException.class,
                        () -> Validate.validIndex(new String[] {"a"}, -1, "MSG"));
                    assertEquals("MSG", ex.getMessage());
                }

                @Test
                void shouldThrowIndexOutOfBoundsExceptionWithGivenMessageForIndexOutOfBounds() {
                    final IndexOutOfBoundsException ex = assertThrows(IndexOutOfBoundsException.class, () -> Validate.validIndex(new String[] {"a"}, 1, "MSG"));
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
                    assertSame(col, Validate.validIndex(col, 0));
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultForNullCollection() {
                    final NullPointerException ex = assertThrows(NullPointerException.class, () -> Validate.validIndex((Collection<?>) null, 1));
                    assertEquals("collection", ex.getMessage());
                }

                @Test
                void shouldThrowIndexOutOfBoundsExceptionWithDefaultMessageForNegativeIndex() {
                    final IndexOutOfBoundsException ex = assertThrows(IndexOutOfBoundsException.class,
                        () -> Validate.validIndex(Collections.singleton("a"), -1));
                    assertEquals("The validated collection index is invalid: -1", ex.getMessage());
                }

                @Test
                void shouldThrowIndexOutOfBoundsExceptionWithDefaultMessageForIndexOutOfBounds() {
                    final IndexOutOfBoundsException ex = assertThrows(IndexOutOfBoundsException.class,
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
                    assertSame(col, Validate.validIndex(col, 0, "MSG"));
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultMessageForNullCollection() {
                    final NullPointerException ex = assertThrows(NullPointerException.class, () -> Validate.validIndex((Collection<?>) null, 1, "MSG"));
                    assertEquals("collection", ex.getMessage());
                }

                @Test
                void shouldThrowIndexOutOfBoundsExceptionWithGivenMessageForNegativeIndex() {
                    final IndexOutOfBoundsException ex = assertThrows(IndexOutOfBoundsException.class,
                        () -> Validate.validIndex(Collections.singleton("a"), -1, "MSG"));
                    assertEquals("MSG", ex.getMessage());
                }

                @Test
                void shouldThrowIndexOutOfBoundsExceptionWithGivenMessageForIndexOutOfBounds() {
                    final IndexOutOfBoundsException ex = assertThrows(IndexOutOfBoundsException.class, () -> Validate.validIndex(Collections.singleton("a"), 1, "MSG"));
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
                    assertSame(str, Validate.validIndex(str, 0));
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultForNullString() {
                    final NullPointerException ex = assertThrows(NullPointerException.class, () -> Validate.validIndex((String) null, 1));
                    assertEquals("chars", ex.getMessage());
                }

                @Test
                void shouldThrowIndexOutOfBoundsExceptionWithDefaultMessageForNegativeIndex() {
                    final IndexOutOfBoundsException ex = assertThrows(IndexOutOfBoundsException.class, () -> Validate.validIndex("a", -1));
                    assertEquals("The validated character sequence index is invalid: -1", ex.getMessage());
                }

                @Test
                void shouldThrowIndexOutOfBoundsExceptionWithDefaultMessageForIndexOutOfBounds() {
                    final IndexOutOfBoundsException ex = assertThrows(IndexOutOfBoundsException.class, () -> Validate.validIndex("a", 1));
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
                    assertSame(str, Validate.validIndex(str, 0, "MSG"));
                }

                @Test
                void shouldThrowNullPointerExceptionWithDefaultMessageForNullStr() {
                    final NullPointerException ex = assertThrows(NullPointerException.class, () -> Validate.validIndex((String) null, 1, "MSG"));
                    assertEquals("chars", ex.getMessage());
                }

                @Test
                void shouldThrowIndexOutOfBoundsExceptionWithGivenMessageForNegativeIndex() {
                    final IndexOutOfBoundsException ex = assertThrows(IndexOutOfBoundsException.class, () -> Validate.validIndex("a", -1, "MSG"));
                    assertEquals("MSG", ex.getMessage());
                }

                @Test
                void shouldThrowIndexOutOfBoundsExceptionWithGivenMessageForIndexOutOfBounds() {
                    final IndexOutOfBoundsException ex = assertThrows(IndexOutOfBoundsException.class, () -> Validate.validIndex("a", 1, "MSG"));
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
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.matchesPattern("hi", "[0-9]*"));
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
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.matchesPattern("hi", "[0-9]*", "MSG"));
                assertEquals("MSG", ex.getMessage());
            }
        }
    }

    @Nested
    class NotNaN {

        @Nested
        class WithoutMessage {

            @Test
            void shouldNotThrowExceptionForNumber() {
                Validate.notNaN(0.0);
            }

            @Test
            void shouldNotThrowExceptionForPositiveInfinity() {
                Validate.notNaN(Double.POSITIVE_INFINITY);
            }

            @Test
            void shouldNotThrowExceptionForNegativeInfinity() {
                Validate.notNaN(Double.NEGATIVE_INFINITY);
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithDefaultMessageForNaN() {
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.notNaN(Double.NaN));
                assertEquals("The validated value is not a number", ex.getMessage());
            }
        }

        @Nested
        class WithMessage {

            @Test
            void shouldNotThrowExceptionForNumber() {
                Validate.notNaN(0.0, "MSG");
            }

            @Test
            void shouldNotThrowExceptionForPositiveInfinity() {
                Validate.notNaN(Double.POSITIVE_INFINITY, "MSG");
            }

            @Test
            void shouldNotThrowExceptionForNegativeInfinity() {
                Validate.notNaN(Double.NEGATIVE_INFINITY, "MSG");
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithGivenMessageForNaN() {
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.notNaN(Double.NaN, "MSG"));
                assertEquals("MSG", ex.getMessage());
            }
        }
    }

    @Nested
    class Finite {

        @Nested
        class WithoutMessage {

            @Test
            void shouldNotThrowExceptionForFiniteValue() {
                Validate.finite(0.0);
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithDefaultMessageForPositiveInfinity() {
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.finite(Double.POSITIVE_INFINITY));
                assertEquals("The value is invalid: Infinity", ex.getMessage());
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithDefaultMessageForNegativeInfinity() {
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.finite(Double.NEGATIVE_INFINITY));
                assertEquals("The value is invalid: -Infinity", ex.getMessage());
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithDefaultMessageForNaN() {
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.finite(Double.NaN));
                assertEquals("The value is invalid: NaN", ex.getMessage());
            }
        }

        @Nested
        class WithMessage {

            @Test
            void shouldNotThrowExceptionForFiniteValue() {
                Validate.finite(0.0, "MSG");
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithDefaultMessageForPositiveInfinity() {
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.finite(Double.POSITIVE_INFINITY, "MSG"));
                assertEquals("MSG", ex.getMessage());
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithDefaultMessageForNegativeInfinity() {
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.finite(Double.NEGATIVE_INFINITY, "MSG"));
                assertEquals("MSG", ex.getMessage());
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithDefaultMessageForNaN() {
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.finite(Double.NaN, "MSG"));
                assertEquals("MSG", ex.getMessage());
            }
        }
    }

    @Nested
    class InclusiveBetween {

        @Nested
        class WithComparable {

            private static final String LOWER_BOUND = "1";
            private static final String UPPER_BOUND = "3";

            @Nested
            class WithoutMessage {

                @Test
                void shouldNotThrowExceptionWhenValueIsBetweenBounds() {
                    Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, "2");
                }

                @Test
                void shouldNotThrowExceptionWhenValueIsLowerBound() {
                    Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, LOWER_BOUND);
                }

                @Test
                void shouldNotThrowExceptionWhenValueIsUpperBound() {
                    Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, UPPER_BOUND);
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageWhenValueIsBelowLowerBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, "0"));
                    assertEquals("The value 0 is not in the specified inclusive range of 1 to 3", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageWhenValueIsAboveUpperBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, "4"));
                    assertEquals("The value 4 is not in the specified inclusive range of 1 to 3", ex.getMessage());
                }
            }

            @Nested
            class WithMessage {

                @Test
                void shouldNotThrowExceptionWhenValueIsBetweenBounds() {
                    Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, "2", "MSG");
                }

                @Test
                void shouldNotThrowExceptionWhenValueIsLowerBound() {
                    Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, LOWER_BOUND, "MSG");
                }

                @Test
                void shouldNotThrowExceptionWhenValueIsUpperBound() {
                    Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, UPPER_BOUND, "MSG");
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithGivenMessageWhenValueIsBelowLowerBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, "0", "MSG"));
                    assertEquals("MSG", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithGivenMessageWhenValueIsAboveUpperBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, "4", "MSG"));
                    assertEquals("MSG", ex.getMessage());
                }
            }
        }

        @Nested
        class WithLong {

            private static final long LOWER_BOUND = 1;
            private static final long UPPER_BOUND = 3;

            @Nested
            class WithoutMessage {

                @Test
                void shouldNotThrowExceptionWhenValueIsBetweenBounds() {
                    Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, 2);
                }

                @Test
                void shouldNotThrowExceptionWhenValueIsLowerBound() {
                    Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, LOWER_BOUND);
                }

                @Test
                void shouldNotThrowExceptionWhenValueIsUpperBound() {
                    Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, UPPER_BOUND);
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageWhenValueIsBelowLowerBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, 0));
                    assertEquals("The value 0 is not in the specified inclusive range of 1 to 3", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageWhenValueIsAboveUpperBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, 4));
                    assertEquals("The value 4 is not in the specified inclusive range of 1 to 3", ex.getMessage());
                }
            }

            @Nested
            class WithMessage {

                @Test
                void shouldNotThrowExceptionWhenValueIsBetweenBounds() {
                    Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, 2, "MSG");
                }

                @Test
                void shouldNotThrowExceptionWhenValueIsLowerBound() {
                    Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, LOWER_BOUND, "MSG");
                }

                @Test
                void shouldNotThrowExceptionWhenValueIsUpperBound() {
                    Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, UPPER_BOUND, "MSG");
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithGivenMessageWhenValueIsBelowLowerBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, 0, "MSG"));
                    assertEquals("MSG", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithGivenMessageWhenValueIsAboveUpperBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, 4, "MSG"));
                    assertEquals("MSG", ex.getMessage());
                }
            }
        }

        @Nested
        class WithDouble {

            private static final double LOWER_BOUND = 0.1;
            private static final double UPPER_BOUND = 3.1;

            @Nested
            class WithoutMessage {

                @Test
                void shouldNotThrowExceptionWhenValueIsBetweenBounds() {
                    Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, 2.1);
                }

                @Test
                void shouldNotThrowExceptionWhenValueIsLowerBound() {
                    Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, LOWER_BOUND);
                }

                @Test
                void shouldNotThrowExceptionWhenValueIsUpperBound() {
                    Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, UPPER_BOUND);
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageWhenValueIsBelowLowerBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, 0.01));
                    assertEquals("The value 0.01 is not in the specified inclusive range of 0.1 to 3.1", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageWhenValueIsAboveUpperBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, 4.1));
                    assertEquals("The value 4.1 is not in the specified inclusive range of 0.1 to 3.1", ex.getMessage());
                }
            }

            @Nested
            class WithMessage {

                @Test
                void shouldNotThrowExceptionWhenValueIsBetweenBounds() {
                    Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, 2.1, "MSG");
                }

                @Test
                void shouldNotThrowExceptionWhenValueIsLowerBound() {
                    Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, LOWER_BOUND, "MSG");
                }

                @Test
                void shouldNotThrowExceptionWhenValueIsUpperBound() {
                    Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, UPPER_BOUND, "MSG");
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithGivenMessageWhenValueIsBelowLowerBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, 0.01, "MSG"));
                    assertEquals("MSG", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithGivenMessageWhenValueIsAboveUpperBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.inclusiveBetween(LOWER_BOUND, UPPER_BOUND, 4.1, "MSG"));
                    assertEquals("MSG", ex.getMessage());
                }
            }
        }
    }

    @Nested
    class ExclusiveBetween {

        @Nested
        class WithComparable {

            private static final String LOWER_BOUND = "1";
            private static final String UPPER_BOUND = "3";

            @Nested
            class WithoutMessage {

                @Test
                void shouldNotThrowExceptionWhenValueIsBetweenBounds() {
                    Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, "2");
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageWhenValueIsLowerBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, LOWER_BOUND));
                    assertEquals("The value 1 is not in the specified exclusive range of 1 to 3", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageWhenValueIsUpperBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, UPPER_BOUND));
                    assertEquals("The value 3 is not in the specified exclusive range of 1 to 3", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageWhenValueIsBelowLowerBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, "0"));
                    assertEquals("The value 0 is not in the specified exclusive range of 1 to 3", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageWhenValueIsAboveUpperBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, "4"));
                    assertEquals("The value 4 is not in the specified exclusive range of 1 to 3", ex.getMessage());
                }
            }

            @Nested
            class WithMessage {

                @Test
                void shouldNotThrowExceptionWhenValueIsBetweenBounds() {
                    Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, "2", "MSG");
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithGivenMessageWhenValueIsLowerBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, LOWER_BOUND, "MSG"));
                    assertEquals("MSG", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithGivenMessageWhenValueIsUpperBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, UPPER_BOUND, "MSG"));
                    assertEquals("MSG", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithGivenMessageWhenValueIsBelowLowerBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, "0", "MSG"));
                    assertEquals("MSG", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageWhenValueIsAboveUpperBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, "4", "MSG"));
                    assertEquals("MSG", ex.getMessage());
                }
            }
        }

        @Nested
        class WithLong {

            private static final long LOWER_BOUND = 1;
            private static final long UPPER_BOUND = 3;

            @Nested
            class WithoutMessage {

                @Test
                void shouldNotThrowExceptionWhenValueIsBetweenBounds() {
                    Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, 2);
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageWhenValueIsLowerBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, LOWER_BOUND));
                    assertEquals("The value 1 is not in the specified exclusive range of 1 to 3", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageWhenValueIsUpperBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, UPPER_BOUND));
                    assertEquals("The value 3 is not in the specified exclusive range of 1 to 3", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageWhenValueIsBelowLowerBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, 0));
                    assertEquals("The value 0 is not in the specified exclusive range of 1 to 3", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageWhenValueIsAboveUpperBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, 4));
                    assertEquals("The value 4 is not in the specified exclusive range of 1 to 3", ex.getMessage());
                }
            }

            @Nested
            class WithMessage {

                @Test
                void shouldNotThrowExceptionWhenValueIsBetweenBounds() {
                    Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, 2, "MSG");
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithGivenMessageWhenValueIsLowerBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, LOWER_BOUND, "MSG"));
                    assertEquals("MSG", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithGivenMessageWhenValueIsUpperBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, UPPER_BOUND, "MSG"));
                    assertEquals("MSG", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithGivenMessageWhenValueIsBelowLowerBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, 0, "MSG"));
                    assertEquals("MSG", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageWhenValueIsAboveUpperBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, 4, "MSG"));
                    assertEquals("MSG", ex.getMessage());
                }
            }
        }

        @Nested
        class WithDouble {

            private static final double LOWER_BOUND = 0.1;
            private static final double UPPER_BOUND = 3.1;

            @Nested
            class WithoutMessage {

                @Test
                void shouldNotThrowExceptionWhenValueIsBetweenBounds() {
                    Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, 2.1);
                }

                @Test
                void shouldThrowIllegalArgumentExcdeptionWhenValueIsLowerBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, LOWER_BOUND));
                    assertEquals("The value 0.1 is not in the specified exclusive range of 0.1 to 3.1", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExcdeptionWhenValueIsUpperBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, UPPER_BOUND));
                    assertEquals("The value 3.1 is not in the specified exclusive range of 0.1 to 3.1", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageWhenValueIsBelowLowerBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, 0.01));
                    assertEquals("The value 0.01 is not in the specified exclusive range of 0.1 to 3.1", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithDefaultMessageWhenValueIsAboveUpperBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, 4.1));
                    assertEquals("The value 4.1 is not in the specified exclusive range of 0.1 to 3.1", ex.getMessage());
                }
            }

            @Nested
            class WithMessage {

                @Test
                void shouldNotThrowExceptionWhenValueIsBetweenBounds() {
                    Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, 2.1, "MSG");
                }

                @Test
                void shouldThrowIllegalArgumentExcdeptionWhenValueIsLowerBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, LOWER_BOUND, "MSG"));
                    assertEquals("MSG", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExcdeptionWhenValueIsUpperBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, UPPER_BOUND, "MSG"));
                    assertEquals("MSG", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithGivenMessageWhenValueIsBelowLowerBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, 0.01, "MSG"));
                    assertEquals("MSG", ex.getMessage());
                }

                @Test
                void shouldThrowIllegalArgumentExceptionWithGivenMessageWhenValueIsAboveUpperBound() {
                    final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                        () -> Validate.exclusiveBetween(LOWER_BOUND, UPPER_BOUND, 4.1, "MSG"));
                    assertEquals("MSG", ex.getMessage());
                }
            }
        }
    }

    @Nested
    class IsInstanceOf {

        @Nested
        class WithoutMessage {

            @Test
            void shouldNotThrowExceptionWhenValueIsInstanceOfClass() {
                Validate.isInstanceOf(String.class, "hi");
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithDefaultMessageWhenValueIsNotInstanceOfClass() {
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.isInstanceOf(List.class, "hi"));
                assertEquals("Expected type: java.util.List, actual: java.lang.String", ex.getMessage());
            }
        }

        @Nested
        class WithMessage {

            @Test
            void shouldNotThrowExceptionWhenValueIsInstanceOfClass() {
                Validate.isInstanceOf(String.class, "hi", "MSG");
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithGivenMessageWhenValueIsNotInstanceOfClass() {
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.isInstanceOf(List.class, "hi", "MSG"));
                assertEquals("MSG", ex.getMessage());
            }
        }

        @Nested
        class WithMessageTemplate {

            @Test
            void shouldNotThrowExceptionWhenValueIsInstanceOfClass() {
                Validate.isInstanceOf(String.class, "hi", "Error %s=%s", "Name", "Value");
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithGivenMessageWhenValueIsNotInstanceOfClass() {
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                    () -> Validate.isInstanceOf(List.class, "hi", "Error %s=%s", "Name", "Value"));
                assertEquals("Error Name=Value", ex.getMessage());
            }
        }
    }

    @Nested
    class IsAssignable {

        @Nested
        class WithoutMessage {

            @Test
            void shouldNotThrowExceptionWhenClassIsAssignable() {
                Validate.isAssignableFrom(CharSequence.class, String.class);
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithDefaultMessageWhenClassIsNotAssignable() {
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.isAssignableFrom(List.class, String.class));
                assertEquals("Cannot assign a java.lang.String to a java.util.List", ex.getMessage());
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithNullSuperType() {
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.isAssignableFrom(null, String.class));
                assertEquals("Cannot assign a java.lang.String to a null type", ex.getMessage());
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithNullType() {
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.isAssignableFrom(List.class, null));
                assertEquals("Cannot assign a null type to a java.util.List", ex.getMessage());
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithNullTypes() {
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> Validate.isAssignableFrom(null, null));
                assertEquals("Cannot assign a null type to a null type", ex.getMessage());
            }
        }

        @Nested
        class WithMessage {

            @Test
            void shouldNotThrowExceptionWhenClassIsAssignable() {
                Validate.isAssignableFrom(CharSequence.class, String.class, "MSG");
            }

            @Test
            void shouldThrowIllegalArgumentExceptionWithGiventMessageWhenClassIsNotAssignable() {
                final IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                    () -> Validate.isAssignableFrom(List.class, String.class, "MSG"));
                assertEquals("MSG", ex.getMessage());
            }
        }
    }

    @Nested
    class UtilClassConventions {

        @Test
        void instancesCanBeConstrcuted() {
            assertNotNull(new Validate());
        }

        @Test
        void hasOnlyOnePublicConstructor() {
            final Constructor<?>[] cons = Validate.class.getDeclaredConstructors();
            assertEquals(1, cons.length);
        }

        @Test
        void isPublicClass() {
            assertTrue(Modifier.isPublic(Validate.class.getModifiers()));
        }

        @Test
        void isNonFinalClass() {
            assertFalse(Modifier.isFinal(Validate.class.getModifiers()));
        }
    }
}

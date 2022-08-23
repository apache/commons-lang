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
package org.apache.commons.lang3.builder;

import java.io.Serializable;
import java.lang.reflect.Array;
import java.util.Collection;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.WeakHashMap;

import org.apache.commons.lang3.ClassUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.StringUtils;

/**
 * Controls {@link String} formatting for {@link ToStringBuilder}.
 * The main public interface is always via {@link ToStringBuilder}.
 *
 * <p>These classes are intended to be used as <em>singletons</em>.
 * There is no need to instantiate a new style each time. A program
 * will generally use one of the predefined constants on this class.
 * Alternatively, the {@link StandardToStringStyle} class can be used
 * to set the individual settings. Thus most styles can be achieved
 * without subclassing.</p>
 *
 * <p>If required, a subclass can override as many or as few of the
 * methods as it requires. Each object type (from {@code boolean}
 * to {@code long} to {@link Object} to {@code int[]}) has
 * its own methods to output it. Most have two versions, detail and summary.
 *
 * <p>For example, the detail version of the array based methods will
 * output the whole array, whereas the summary method will just output
 * the array length.</p>
 *
 * <p>If you want to format the output of certain objects, such as dates, you
 * must create a subclass and override a method.
 * </p>
 * <pre>
 * public class MyStyle extends ToStringStyle {
 *   protected void appendDetail(StringBuffer buffer, String fieldName, Object value) {
 *     if (value instanceof Date) {
 *       value = new SimpleDateFormat("yyyy-MM-dd").format(value);
 *     }
 *     buffer.append(value);
 *   }
 * }
 * </pre>
 *
 * @since 1.0
 */
@SuppressWarnings("deprecation") // StringEscapeUtils
public abstract class ToStringStyle implements Serializable {

    /**
     * Serialization version ID.
     */
    private static final long serialVersionUID = -2587890625525655916L;

    /**
     * The default toString style. Using the {@code Person}
     * example from {@link ToStringBuilder}, the output would look like this:
     *
     * <pre>
     * Person@182f0db[name=John Doe,age=33,smoker=false]
     * </pre>
     */
    public static final ToStringStyle DEFAULT_STYLE = new DefaultToStringStyle();

    /**
     * The multi line toString style. Using the {@code Person}
     * example from {@link ToStringBuilder}, the output would look like this:
     *
     * <pre>
     * Person@182f0db[
     *   name=John Doe
     *   age=33
     *   smoker=false
     * ]
     * </pre>
     */
    public static final ToStringStyle MULTI_LINE_STYLE = new MultiLineToStringStyle();

    /**
     * The no field names toString style. Using the
     * {@code Person} example from {@link ToStringBuilder}, the output
     * would look like this:
     *
     * <pre>
     * Person@182f0db[John Doe,33,false]
     * </pre>
     */
    public static final ToStringStyle NO_FIELD_NAMES_STYLE = new NoFieldNameToStringStyle();

    /**
     * The short prefix toString style. Using the {@code Person} example
     * from {@link ToStringBuilder}, the output would look like this:
     *
     * <pre>
     * Person[name=John Doe,age=33,smoker=false]
     * </pre>
     *
     * @since 2.1
     */
    public static final ToStringStyle SHORT_PREFIX_STYLE = new ShortPrefixToStringStyle();

    /**
     * The simple toString style. Using the {@code Person}
     * example from {@link ToStringBuilder}, the output would look like this:
     *
     * <pre>
     * John Doe,33,false
     * </pre>
     */
    public static final ToStringStyle SIMPLE_STYLE = new SimpleToStringStyle();

    /**
     * The no class name toString style. Using the {@code Person}
     * example from {@link ToStringBuilder}, the output would look like this:
     *
     * <pre>
     * [name=John Doe,age=33,smoker=false]
     * </pre>
     *
     * @since 3.4
     */
    public static final ToStringStyle NO_CLASS_NAME_STYLE = new NoClassNameToStringStyle();

    /**
     * The JSON toString style. Using the {@code Person} example from
     * {@link ToStringBuilder}, the output would look like this:
     *
     * <pre>
     * {"name": "John Doe", "age": 33, "smoker": true}
     * </pre>
     *
     * <strong>Note:</strong> Since field names are mandatory in JSON, this
     * ToStringStyle will throw an {@link UnsupportedOperationException} if no
     * field name is passed in while appending. Furthermore This ToStringStyle
     * will only generate valid JSON if referenced objects also produce JSON
     * when calling {@code toString()} on them.
     *
     * @since 3.4
     * @see <a href="https://www.json.org/">json.org</a>
     */
    public static final ToStringStyle JSON_STYLE = new JsonToStringStyle();

    /**
     * A registry of objects used by {@code reflectionToString} methods
     * to detect cyclical object references and avoid infinite loops.
     *
     */
    private static final ThreadLocal<WeakHashMap<Object, Object>> REGISTRY = new ThreadLocal<>();
    /*
     * Note that objects of this class are generally shared between threads, so
     * an instance variable would not be suitable here.
     *
     * In normal use the registry should always be left empty, because the caller
     * should call toString() which will clean up.
     *
     * See LANG-792
     */

    /**
     * Returns the registry of objects being traversed by the {@code reflectionToString}
     * methods in the current thread.
     *
     * @return Set the registry of objects being traversed
     */
    public static Map<Object, Object> getRegistry() {
        return REGISTRY.get();
    }

    /**
     * Returns {@code true} if the registry contains the given object.
     * Used by the reflection methods to avoid infinite loops.
     *
     * @param value
     *                  The object to lookup in the registry.
     * @return boolean {@code true} if the registry contains the given
     *             object.
     */
    static boolean isRegistered(final Object value) {
        final Map<Object, Object> m = getRegistry();
        return m != null && m.containsKey(value);
    }

    /**
     * Registers the given object. Used by the reflection methods to avoid
     * infinite loops.
     *
     * @param value
     *                  The object to register.
     */
    static void register(final Object value) {
        if (value != null) {
            final Map<Object, Object> m = getRegistry();
            if (m == null) {
                REGISTRY.set(new WeakHashMap<>());
            }
            getRegistry().put(value, null);
        }
    }

    /**
     * Unregisters the given object.
     *
     * <p>
     * Used by the reflection methods to avoid infinite loops.
     * </p>
     *
     * @param value
     *                  The object to unregister.
     */
    static void unregister(final Object value) {
        if (value != null) {
            final Map<Object, Object> m = getRegistry();
            if (m != null) {
                m.remove(value);
                if (m.isEmpty()) {
                    REGISTRY.remove();
                }
            }
        }
    }

    /**
     * Whether to use the field names, the default is {@code true}.
     */
    private boolean useFieldNames = true;

    /**
     * Whether to use the class name, the default is {@code true}.
     */
    private boolean useClassName = true;

    /**
     * Whether to use short class names, the default is {@code false}.
     */
    private boolean useShortClassName;

    /**
     * Whether to use the identity hash code, the default is {@code true}.
     */
    private boolean useIdentityHashCode = true;

    /**
     * The content start {@code '['}.
     */
    private String contentStart = "[";

    /**
     * The content end {@code ']'}.
     */
    private String contentEnd = "]";

    /**
     * The field name value separator {@code '='}.
     */
    private String fieldNameValueSeparator = "=";

    /**
     * Whether the field separator should be added before any other fields.
     */
    private boolean fieldSeparatorAtStart;

    /**
     * Whether the field separator should be added after any other fields.
     */
    private boolean fieldSeparatorAtEnd;

    /**
     * The field separator {@code ','}.
     */
    private String fieldSeparator = ",";

    /**
     * The array start <code>'{'</code>.
     */
    private String arrayStart = "{";

    /**
     * The array separator {@code ','}.
     */
    private String arraySeparator = ",";

    /**
     * The detail for array content.
     */
    private boolean arrayContentDetail = true;

    /**
     * The array end {@code '}'}.
     */
    private String arrayEnd = "}";

    /**
     * The value to use when fullDetail is {@code null},
     * the default value is {@code true}.
     */
    private boolean defaultFullDetail = true;

    /**
     * The {@code null} text {@code '&lt;null&gt;'}.
     */
    private String nullText = "<null>";

    /**
     * The summary size text start {@code '&lt;size'}.
     */
    private String sizeStartText = "<size=";

    /**
     * The summary size text start {@code '&gt;'}.
     */
    private String sizeEndText = ">";

    /**
     * The summary object text start {@code '&lt;'}.
     */
    private String summaryObjectStartText = "<";

    /**
     * The summary object text start {@code '&gt;'}.
     */
    private String summaryObjectEndText = ">";

    /**
     * Constructor.
     */
    protected ToStringStyle() {
    }

    /**
     * Appends to the {@code toString} the superclass toString.
     * <p>NOTE: It assumes that the toString has been created from the same ToStringStyle.</p>
     *
     * <p>A {@code null} {@code superToString} is ignored.</p>
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param superToString  the {@code super.toString()}
     * @since 2.0
     */
    public void appendSuper(final StringBuffer buffer, final String superToString) {
        appendToString(buffer, superToString);
    }

    /**
     * Appends to the {@code toString} another toString.
     * <p>NOTE: It assumes that the toString has been created from the same ToStringStyle.</p>
     *
     * <p>A {@code null} {@code toString} is ignored.</p>
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param toString  the additional {@code toString}
     * @since 2.0
     */
    public void appendToString(final StringBuffer buffer, final String toString) {
        if (toString != null) {
            final int pos1 = toString.indexOf(contentStart) + contentStart.length();
            final int pos2 = toString.lastIndexOf(contentEnd);
            if (pos1 != pos2 && pos1 >= 0 && pos2 >= 0) {
                if (fieldSeparatorAtStart) {
                    removeLastFieldSeparator(buffer);
                }
                buffer.append(toString, pos1, pos2);
                appendFieldSeparator(buffer);
            }
        }
    }

    /**
     * Appends to the {@code toString} the start of data indicator.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param object  the {@link Object} to build a {@code toString} for
     */
    public void appendStart(final StringBuffer buffer, final Object object) {
        if (object != null) {
            appendClassName(buffer, object);
            appendIdentityHashCode(buffer, object);
            appendContentStart(buffer);
            if (fieldSeparatorAtStart) {
                appendFieldSeparator(buffer);
            }
        }
    }

    /**
     * Appends to the {@code toString} the end of data indicator.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param object  the {@link Object} to build a
     *  {@code toString} for.
     */
    public void appendEnd(final StringBuffer buffer, final Object object) {
        if (!this.fieldSeparatorAtEnd) {
            removeLastFieldSeparator(buffer);
        }
        appendContentEnd(buffer);
        unregister(object);
    }

    /**
     * Remove the last field separator from the buffer.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @since 2.0
     */
    protected void removeLastFieldSeparator(final StringBuffer buffer) {
        if (StringUtils.endsWith(buffer, fieldSeparator)) {
            buffer.setLength(buffer.length() - fieldSeparator.length());
        }
    }

    /**
     * Appends to the {@code toString} an {@link Object}
     * value, printing the full {@code toString} of the
     * {@link Object} passed in.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name
     * @param value  the value to add to the {@code toString}
     * @param fullDetail  {@code true} for detail, {@code false}
     *  for summary info, {@code null} for style decides
     */
    public void append(final StringBuffer buffer, final String fieldName, final Object value, final Boolean fullDetail) {
        appendFieldStart(buffer, fieldName);

        if (value == null) {
            appendNullText(buffer, fieldName);

        } else {
            appendInternal(buffer, fieldName, value, isFullDetail(fullDetail));
        }

        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Appends to the {@code toString} an {@link Object},
     * correctly interpreting its type.
     *
     * <p>This method performs the main lookup by Class type to correctly
     * route arrays, {@link Collection}s, {@link Map}s and
     * {@link Objects} to the appropriate method.</p>
     *
     * <p>Either detail or summary views can be specified.</p>
     *
     * <p>If a cycle is detected, an object will be appended with the
     * {@code Object.toString()} format.</p>
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param value  the value to add to the {@code toString},
     *  not {@code null}
     * @param detail  output detail or not
     */
    protected void appendInternal(final StringBuffer buffer, final String fieldName, final Object value, final boolean detail) {
        if (isRegistered(value)
            && !(value instanceof Number || value instanceof Boolean || value instanceof Character)) {
           appendCyclicObject(buffer, fieldName, value);
           return;
        }

        register(value);

        try {
            if (value instanceof Collection<?>) {
                if (detail) {
                    appendDetail(buffer, fieldName, (Collection<?>) value);
                } else {
                    appendSummarySize(buffer, fieldName, ((Collection<?>) value).size());
                }

            } else if (value instanceof Map<?, ?>) {
                if (detail) {
                    appendDetail(buffer, fieldName, (Map<?, ?>) value);
                } else {
                    appendSummarySize(buffer, fieldName, ((Map<?, ?>) value).size());
                }

            } else if (value instanceof long[]) {
                if (detail) {
                    appendDetail(buffer, fieldName, (long[]) value);
                } else {
                    appendSummary(buffer, fieldName, (long[]) value);
                }

            } else if (value instanceof int[]) {
                if (detail) {
                    appendDetail(buffer, fieldName, (int[]) value);
                } else {
                    appendSummary(buffer, fieldName, (int[]) value);
                }

            } else if (value instanceof short[]) {
                if (detail) {
                    appendDetail(buffer, fieldName, (short[]) value);
                } else {
                    appendSummary(buffer, fieldName, (short[]) value);
                }

            } else if (value instanceof byte[]) {
                if (detail) {
                    appendDetail(buffer, fieldName, (byte[]) value);
                } else {
                    appendSummary(buffer, fieldName, (byte[]) value);
                }

            } else if (value instanceof char[]) {
                if (detail) {
                    appendDetail(buffer, fieldName, (char[]) value);
                } else {
                    appendSummary(buffer, fieldName, (char[]) value);
                }

            } else if (value instanceof double[]) {
                if (detail) {
                    appendDetail(buffer, fieldName, (double[]) value);
                } else {
                    appendSummary(buffer, fieldName, (double[]) value);
                }

            } else if (value instanceof float[]) {
                if (detail) {
                    appendDetail(buffer, fieldName, (float[]) value);
                } else {
                    appendSummary(buffer, fieldName, (float[]) value);
                }

            } else if (value instanceof boolean[]) {
                if (detail) {
                    appendDetail(buffer, fieldName, (boolean[]) value);
                } else {
                    appendSummary(buffer, fieldName, (boolean[]) value);
                }

            } else if (ObjectUtils.isArray(value)) {
                if (detail) {
                    appendDetail(buffer, fieldName, (Object[]) value);
                } else {
                    appendSummary(buffer, fieldName, (Object[]) value);
                }

            } else if (detail) {
                appendDetail(buffer, fieldName, value);
            } else {
                appendSummary(buffer, fieldName, value);
            }
        } finally {
            unregister(value);
        }
    }

    /**
     * Appends to the {@code toString} an {@link Object}
     * value that has been detected to participate in a cycle. This
     * implementation will print the standard string value of the value.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param value  the value to add to the {@code toString},
     *  not {@code null}
     *
     * @since 2.2
     */
    protected void appendCyclicObject(final StringBuffer buffer, final String fieldName, final Object value) {
       ObjectUtils.identityToString(buffer, value);
    }

    /**
     * Appends to the {@code toString} an {@link Object}
     * value, printing the full detail of the {@link Object}.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param value  the value to add to the {@code toString},
     *  not {@code null}
     */
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final Object value) {
        buffer.append(value);
    }

    /**
     * Appends to the {@code toString} a {@link Collection}.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param coll  the {@link Collection} to add to the
     *  {@code toString}, not {@code null}
     */
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final Collection<?> coll) {
        buffer.append(coll);
    }

    /**
     * Appends to the {@code toString} a {@link Map}.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param map  the {@link Map} to add to the {@code toString},
     *  not {@code null}
     */
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final Map<?, ?> map) {
        buffer.append(map);
    }

    /**
     * Appends to the {@code toString} an {@link Object}
     * value, printing a summary of the {@link Object}.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param value  the value to add to the {@code toString},
     *  not {@code null}
     */
    protected void appendSummary(final StringBuffer buffer, final String fieldName, final Object value) {
        buffer.append(summaryObjectStartText);
        buffer.append(getShortClassName(value.getClass()));
        buffer.append(summaryObjectEndText);
    }

    /**
     * <p>Appends to the {@code toString} a {@code long}
     * value.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name
     * @param value  the value to add to the {@code toString}
     */
    public void append(final StringBuffer buffer, final String fieldName, final long value) {
        appendFieldStart(buffer, fieldName);
        appendDetail(buffer, fieldName, value);
        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Appends to the {@code toString} a {@code long}
     * value.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param value  the value to add to the {@code toString}
     */
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final long value) {
        buffer.append(value);
    }

    /**
     * Appends to the {@code toString} an {@code int}
     * value.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name
     * @param value  the value to add to the {@code toString}
     */
    public void append(final StringBuffer buffer, final String fieldName, final int value) {
        appendFieldStart(buffer, fieldName);
        appendDetail(buffer, fieldName, value);
        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Appends to the {@code toString} an {@code int}
     * value.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param value  the value to add to the {@code toString}
     */
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final int value) {
        buffer.append(value);
    }

    /**
     * Appends to the {@code toString} a {@code short}
     * value.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name
     * @param value  the value to add to the {@code toString}
     */
    public void append(final StringBuffer buffer, final String fieldName, final short value) {
        appendFieldStart(buffer, fieldName);
        appendDetail(buffer, fieldName, value);
        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Appends to the {@code toString} a {@code short}
     * value.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param value  the value to add to the {@code toString}
     */
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final short value) {
        buffer.append(value);
    }

    /**
     * Appends to the {@code toString} a {@code byte}
     * value.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name
     * @param value  the value to add to the {@code toString}
     */
    public void append(final StringBuffer buffer, final String fieldName, final byte value) {
        appendFieldStart(buffer, fieldName);
        appendDetail(buffer, fieldName, value);
        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Appends to the {@code toString} a {@code byte}
     * value.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param value  the value to add to the {@code toString}
     */
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final byte value) {
        buffer.append(value);
    }

    /**
     * Appends to the {@code toString} a {@code char}
     * value.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name
     * @param value  the value to add to the {@code toString}
     */
    public void append(final StringBuffer buffer, final String fieldName, final char value) {
        appendFieldStart(buffer, fieldName);
        appendDetail(buffer, fieldName, value);
        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Appends to the {@code toString} a {@code char}
     * value.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param value  the value to add to the {@code toString}
     */
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final char value) {
        buffer.append(value);
    }

    /**
     * Appends to the {@code toString} a {@code double}
     * value.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name
     * @param value  the value to add to the {@code toString}
     */
    public void append(final StringBuffer buffer, final String fieldName, final double value) {
        appendFieldStart(buffer, fieldName);
        appendDetail(buffer, fieldName, value);
        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Appends to the {@code toString} a {@code double}
     * value.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param value  the value to add to the {@code toString}
     */
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final double value) {
        buffer.append(value);
    }

    /**
     * Appends to the {@code toString} a {@code float}
     * value.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name
     * @param value  the value to add to the {@code toString}
     */
    public void append(final StringBuffer buffer, final String fieldName, final float value) {
        appendFieldStart(buffer, fieldName);
        appendDetail(buffer, fieldName, value);
        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Appends to the {@code toString} a {@code float}
     * value.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param value  the value to add to the {@code toString}
     */
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final float value) {
        buffer.append(value);
    }

    /**
     * Appends to the {@code toString} a {@code boolean}
     * value.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name
     * @param value  the value to add to the {@code toString}
     */
    public void append(final StringBuffer buffer, final String fieldName, final boolean value) {
        appendFieldStart(buffer, fieldName);
        appendDetail(buffer, fieldName, value);
        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Appends to the {@code toString} a {@code boolean}
     * value.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param value  the value to add to the {@code toString}
     */
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final boolean value) {
        buffer.append(value);
    }

    /**
     * Appends to the {@code toString} an {@link Object}
     * array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name
     * @param array  the array to add to the toString
     * @param fullDetail  {@code true} for detail, {@code false}
     *  for summary info, {@code null} for style decides
     */
    public void append(final StringBuffer buffer, final String fieldName, final Object[] array, final Boolean fullDetail) {
        appendFieldStart(buffer, fieldName);

        if (array == null) {
            appendNullText(buffer, fieldName);

        } else if (isFullDetail(fullDetail)) {
            appendDetail(buffer, fieldName, array);

        } else {
            appendSummary(buffer, fieldName, array);
        }

        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Appends to the {@code toString} the detail of an
     * {@link Object} array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the {@code toString},
     *  not {@code null}
     */
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final Object[] array) {
        buffer.append(arrayStart);
        for (int i = 0; i < array.length; i++) {
            appendDetail(buffer, fieldName, i, array[i]);
        }
        buffer.append(arrayEnd);
    }

    /**
     * Appends to the {@code toString} the detail of an
     * {@link Object} array item.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param i the array item index to add
     * @param item the array item to add
     * @since 3.11
     */
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final int i, final Object item) {
        if (i > 0) {
            buffer.append(arraySeparator);
        }
        if (item == null) {
            appendNullText(buffer, fieldName);
        } else {
            appendInternal(buffer, fieldName, item, arrayContentDetail);
        }
    }

    /**
     * Appends to the {@code toString} the detail of an array type.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the {@code toString},
     *  not {@code null}
     * @since 2.0
     */
    protected void reflectionAppendArrayDetail(final StringBuffer buffer, final String fieldName, final Object array) {
        buffer.append(arrayStart);
        final int length = Array.getLength(array);
        for (int i = 0; i < length; i++) {
            appendDetail(buffer, fieldName, i, Array.get(array, i));
        }
        buffer.append(arrayEnd);
    }

    /**
     * Appends to the {@code toString} a summary of an
     * {@link Object} array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the {@code toString},
     *  not {@code null}
     */
    protected void appendSummary(final StringBuffer buffer, final String fieldName, final Object[] array) {
        appendSummarySize(buffer, fieldName, array.length);
    }

    /**
     * Appends to the {@code toString} a {@code long}
     * array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name
     * @param array  the array to add to the {@code toString}
     * @param fullDetail  {@code true} for detail, {@code false}
     *  for summary info, {@code null} for style decides
     */
    public void append(final StringBuffer buffer, final String fieldName, final long[] array, final Boolean fullDetail) {
        appendFieldStart(buffer, fieldName);

        if (array == null) {
            appendNullText(buffer, fieldName);

        } else if (isFullDetail(fullDetail)) {
            appendDetail(buffer, fieldName, array);

        } else {
            appendSummary(buffer, fieldName, array);
        }

        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Appends to the {@code toString} the detail of a
     * {@code long} array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the {@code toString},
     *  not {@code null}
     */
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final long[] array) {
        buffer.append(arrayStart);
        for (int i = 0; i < array.length; i++) {
            if (i > 0) {
                buffer.append(arraySeparator);
            }
            appendDetail(buffer, fieldName, array[i]);
        }
        buffer.append(arrayEnd);
    }

    /**
     * Appends to the {@code toString} a summary of a
     * {@code long} array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the {@code toString},
     *  not {@code null}
     */
    protected void appendSummary(final StringBuffer buffer, final String fieldName, final long[] array) {
        appendSummarySize(buffer, fieldName, array.length);
    }

    /**
     * Appends to the {@code toString} an {@code int}
     * array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name
     * @param array  the array to add to the {@code toString}
     * @param fullDetail  {@code true} for detail, {@code false}
     *  for summary info, {@code null} for style decides
     */
    public void append(final StringBuffer buffer, final String fieldName, final int[] array, final Boolean fullDetail) {
        appendFieldStart(buffer, fieldName);

        if (array == null) {
            appendNullText(buffer, fieldName);

        } else if (isFullDetail(fullDetail)) {
            appendDetail(buffer, fieldName, array);

        } else {
            appendSummary(buffer, fieldName, array);
        }

        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Appends to the {@code toString} the detail of an
     * {@code int} array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the {@code toString},
     *  not {@code null}
     */
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final int[] array) {
        buffer.append(arrayStart);
        for (int i = 0; i < array.length; i++) {
            if (i > 0) {
                buffer.append(arraySeparator);
            }
            appendDetail(buffer, fieldName, array[i]);
        }
        buffer.append(arrayEnd);
    }

    /**
     * Appends to the {@code toString} a summary of an
     * {@code int} array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the {@code toString},
     *  not {@code null}
     */
    protected void appendSummary(final StringBuffer buffer, final String fieldName, final int[] array) {
        appendSummarySize(buffer, fieldName, array.length);
    }

    /**
     * Appends to the {@code toString} a {@code short}
     * array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name
     * @param array  the array to add to the {@code toString}
     * @param fullDetail  {@code true} for detail, {@code false}
     *  for summary info, {@code null} for style decides
     */
    public void append(final StringBuffer buffer, final String fieldName, final short[] array, final Boolean fullDetail) {
        appendFieldStart(buffer, fieldName);

        if (array == null) {
            appendNullText(buffer, fieldName);

        } else if (isFullDetail(fullDetail)) {
            appendDetail(buffer, fieldName, array);

        } else {
            appendSummary(buffer, fieldName, array);
        }

        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Appends to the {@code toString} the detail of a
     * {@code short} array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the {@code toString},
     *  not {@code null}
     */
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final short[] array) {
        buffer.append(arrayStart);
        for (int i = 0; i < array.length; i++) {
            if (i > 0) {
                buffer.append(arraySeparator);
            }
            appendDetail(buffer, fieldName, array[i]);
        }
        buffer.append(arrayEnd);
    }

    /**
     * Appends to the {@code toString} a summary of a
     * {@code short} array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the {@code toString},
     *  not {@code null}
     */
    protected void appendSummary(final StringBuffer buffer, final String fieldName, final short[] array) {
        appendSummarySize(buffer, fieldName, array.length);
    }

    /**
     * Appends to the {@code toString} a {@code byte}
     * array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name
     * @param array  the array to add to the {@code toString}
     * @param fullDetail  {@code true} for detail, {@code false}
     *  for summary info, {@code null} for style decides
     */
    public void append(final StringBuffer buffer, final String fieldName, final byte[] array, final Boolean fullDetail) {
        appendFieldStart(buffer, fieldName);

        if (array == null) {
            appendNullText(buffer, fieldName);

        } else if (isFullDetail(fullDetail)) {
            appendDetail(buffer, fieldName, array);

        } else {
            appendSummary(buffer, fieldName, array);
        }

        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Appends to the {@code toString} the detail of a
     * {@code byte} array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the {@code toString},
     *  not {@code null}
     */
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final byte[] array) {
        buffer.append(arrayStart);
        for (int i = 0; i < array.length; i++) {
            if (i > 0) {
                buffer.append(arraySeparator);
            }
            appendDetail(buffer, fieldName, array[i]);
        }
        buffer.append(arrayEnd);
    }

    /**
     * Appends to the {@code toString} a summary of a
     * {@code byte} array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the {@code toString},
     *  not {@code null}
     */
    protected void appendSummary(final StringBuffer buffer, final String fieldName, final byte[] array) {
        appendSummarySize(buffer, fieldName, array.length);
    }

    /**
     * Appends to the {@code toString} a {@code char}
     * array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name
     * @param array  the array to add to the {@code toString}
     * @param fullDetail  {@code true} for detail, {@code false}
     *  for summary info, {@code null} for style decides
     */
    public void append(final StringBuffer buffer, final String fieldName, final char[] array, final Boolean fullDetail) {
        appendFieldStart(buffer, fieldName);

        if (array == null) {
            appendNullText(buffer, fieldName);

        } else if (isFullDetail(fullDetail)) {
            appendDetail(buffer, fieldName, array);

        } else {
            appendSummary(buffer, fieldName, array);
        }

        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Appends to the {@code toString} the detail of a
     * {@code char} array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the {@code toString},
     *  not {@code null}
     */
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final char[] array) {
        buffer.append(arrayStart);
        for (int i = 0; i < array.length; i++) {
            if (i > 0) {
                buffer.append(arraySeparator);
            }
            appendDetail(buffer, fieldName, array[i]);
        }
        buffer.append(arrayEnd);
    }

    /**
     * Appends to the {@code toString} a summary of a
     * {@code char} array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the {@code toString},
     *  not {@code null}
     */
    protected void appendSummary(final StringBuffer buffer, final String fieldName, final char[] array) {
        appendSummarySize(buffer, fieldName, array.length);
    }

    /**
     * Appends to the {@code toString} a {@code double}
     * array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name
     * @param array  the array to add to the toString
     * @param fullDetail  {@code true} for detail, {@code false}
     *  for summary info, {@code null} for style decides
     */
    public void append(final StringBuffer buffer, final String fieldName, final double[] array, final Boolean fullDetail) {
        appendFieldStart(buffer, fieldName);

        if (array == null) {
            appendNullText(buffer, fieldName);

        } else if (isFullDetail(fullDetail)) {
            appendDetail(buffer, fieldName, array);

        } else {
            appendSummary(buffer, fieldName, array);
        }

        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Appends to the {@code toString} the detail of a
     * {@code double} array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the {@code toString},
     *  not {@code null}
     */
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final double[] array) {
        buffer.append(arrayStart);
        for (int i = 0; i < array.length; i++) {
            if (i > 0) {
                buffer.append(arraySeparator);
            }
            appendDetail(buffer, fieldName, array[i]);
        }
        buffer.append(arrayEnd);
    }

    /**
     * Appends to the {@code toString} a summary of a
     * {@code double} array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the {@code toString},
     *  not {@code null}
     */
    protected void appendSummary(final StringBuffer buffer, final String fieldName, final double[] array) {
        appendSummarySize(buffer, fieldName, array.length);
    }

    /**
     * Appends to the {@code toString} a {@code float}
     * array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name
     * @param array  the array to add to the toString
     * @param fullDetail  {@code true} for detail, {@code false}
     *  for summary info, {@code null} for style decides
     */
    public void append(final StringBuffer buffer, final String fieldName, final float[] array, final Boolean fullDetail) {
        appendFieldStart(buffer, fieldName);

        if (array == null) {
            appendNullText(buffer, fieldName);

        } else if (isFullDetail(fullDetail)) {
            appendDetail(buffer, fieldName, array);

        } else {
            appendSummary(buffer, fieldName, array);
        }

        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Appends to the {@code toString} the detail of a
     * {@code float} array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the {@code toString},
     *  not {@code null}
     */
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final float[] array) {
        buffer.append(arrayStart);
        for (int i = 0; i < array.length; i++) {
            if (i > 0) {
                buffer.append(arraySeparator);
            }
            appendDetail(buffer, fieldName, array[i]);
        }
        buffer.append(arrayEnd);
    }

    /**
     * Appends to the {@code toString} a summary of a
     * {@code float} array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the {@code toString},
     *  not {@code null}
     */
    protected void appendSummary(final StringBuffer buffer, final String fieldName, final float[] array) {
        appendSummarySize(buffer, fieldName, array.length);
    }

    /**
     * Appends to the {@code toString} a {@code boolean}
     * array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name
     * @param array  the array to add to the toString
     * @param fullDetail  {@code true} for detail, {@code false}
     *  for summary info, {@code null} for style decides
     */
    public void append(final StringBuffer buffer, final String fieldName, final boolean[] array, final Boolean fullDetail) {
        appendFieldStart(buffer, fieldName);

        if (array == null) {
            appendNullText(buffer, fieldName);

        } else if (isFullDetail(fullDetail)) {
            appendDetail(buffer, fieldName, array);

        } else {
            appendSummary(buffer, fieldName, array);
        }

        appendFieldEnd(buffer, fieldName);
    }

    /**
     * Appends to the {@code toString} the detail of a
     * {@code boolean} array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the {@code toString},
     *  not {@code null}
     */
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final boolean[] array) {
        buffer.append(arrayStart);
        for (int i = 0; i < array.length; i++) {
            if (i > 0) {
                buffer.append(arraySeparator);
            }
            appendDetail(buffer, fieldName, array[i]);
        }
        buffer.append(arrayEnd);
    }

    /**
     * Appends to the {@code toString} a summary of a
     * {@code boolean} array.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param array  the array to add to the {@code toString},
     *  not {@code null}
     */
    protected void appendSummary(final StringBuffer buffer, final String fieldName, final boolean[] array) {
        appendSummarySize(buffer, fieldName, array.length);
    }

    /**
     * Appends to the {@code toString} the class name.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param object  the {@link Object} whose name to output
     */
    protected void appendClassName(final StringBuffer buffer, final Object object) {
        if (useClassName && object != null) {
            register(object);
            if (useShortClassName) {
                buffer.append(getShortClassName(object.getClass()));
            } else {
                buffer.append(object.getClass().getName());
            }
        }
    }

    /**
     * Appends the {@link System#identityHashCode(java.lang.Object)}.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param object  the {@link Object} whose id to output
     */
    protected void appendIdentityHashCode(final StringBuffer buffer, final Object object) {
        if (this.isUseIdentityHashCode() && object != null) {
            register(object);
            buffer.append('@');
            buffer.append(ObjectUtils.identityHashCodeHex(object));
        }
    }

    /**
     * Appends to the {@code toString} the content start.
     *
     * @param buffer  the {@link StringBuffer} to populate
     */
    protected void appendContentStart(final StringBuffer buffer) {
        buffer.append(contentStart);
    }

    /**
     * Appends to the {@code toString} the content end.
     *
     * @param buffer  the {@link StringBuffer} to populate
     */
    protected void appendContentEnd(final StringBuffer buffer) {
        buffer.append(contentEnd);
    }

    /**
     * Appends to the {@code toString} an indicator for {@code null}.
     *
     * <p>The default indicator is {@code '&lt;null&gt;'}.</p>
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     */
    protected void appendNullText(final StringBuffer buffer, final String fieldName) {
        buffer.append(nullText);
    }

    /**
     * Appends to the {@code toString} the field separator.
     *
     * @param buffer  the {@link StringBuffer} to populate
     */
    protected void appendFieldSeparator(final StringBuffer buffer) {
        buffer.append(fieldSeparator);
    }

    /**
     * Appends to the {@code toString} the field start.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name
     */
    protected void appendFieldStart(final StringBuffer buffer, final String fieldName) {
        if (useFieldNames && fieldName != null) {
            buffer.append(fieldName);
            buffer.append(fieldNameValueSeparator);
        }
    }

    /**
     * Appends to the {@code toString} the field end.
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     */
    protected void appendFieldEnd(final StringBuffer buffer, final String fieldName) {
        appendFieldSeparator(buffer);
    }

    /**
     * Appends to the {@code toString} a size summary.
     *
     * <p>The size summary is used to summarize the contents of
     * {@link Collection}s, {@link Map}s and arrays.</p>
     *
     * <p>The output consists of a prefix, the passed in size
     * and a suffix.</p>
     *
     * <p>The default format is {@code '&lt;size=n&gt;'}.</p>
     *
     * @param buffer  the {@link StringBuffer} to populate
     * @param fieldName  the field name, typically not used as already appended
     * @param size  the size to append
     */
    protected void appendSummarySize(final StringBuffer buffer, final String fieldName, final int size) {
        buffer.append(sizeStartText);
        buffer.append(size);
        buffer.append(sizeEndText);
    }

    /**
     * Is this field to be output in full detail.
     *
     * <p>This method converts a detail request into a detail level.
     * The calling code may request full detail ({@code true}),
     * but a subclass might ignore that and always return
     * {@code false}. The calling code may pass in
     * {@code null} indicating that it doesn't care about
     * the detail level. In this case the default detail level is
     * used.</p>
     *
     * @param fullDetailRequest  the detail level requested
     * @return whether full detail is to be shown
     */
    protected boolean isFullDetail(final Boolean fullDetailRequest) {
        if (fullDetailRequest == null) {
            return defaultFullDetail;
        }
        return fullDetailRequest.booleanValue();
    }

    /**
     * Gets the short class name for a class.
     *
     * <p>The short class name is the classname excluding
     * the package name.</p>
     *
     * @param cls  the {@link Class} to get the short name of
     * @return the short name
     */
    protected String getShortClassName(final Class<?> cls) {
        return ClassUtils.getShortClassName(cls);
    }

    // Setters and getters for the customizable parts of the style
    // These methods are not expected to be overridden, except to make public
    // (They are not public so that immutable subclasses can be written)
    /**
     * Gets whether to use the class name.
     *
     * @return the current useClassName flag
     */
    protected boolean isUseClassName() {
        return useClassName;
    }

    /**
     * Sets whether to use the class name.
     *
     * @param useClassName  the new useClassName flag
     */
    protected void setUseClassName(final boolean useClassName) {
        this.useClassName = useClassName;
    }

    /**
     * Gets whether to output short or long class names.
     *
     * @return the current useShortClassName flag
     * @since 2.0
     */
    protected boolean isUseShortClassName() {
        return useShortClassName;
    }

    /**
     * Sets whether to output short or long class names.
     *
     * @param useShortClassName  the new useShortClassName flag
     * @since 2.0
     */
    protected void setUseShortClassName(final boolean useShortClassName) {
        this.useShortClassName = useShortClassName;
    }

    /**
     * Gets whether to use the identity hash code.
     *
     * @return the current useIdentityHashCode flag
     */
    protected boolean isUseIdentityHashCode() {
        return useIdentityHashCode;
    }

    /**
     * Sets whether to use the identity hash code.
     *
     * @param useIdentityHashCode  the new useIdentityHashCode flag
     */
    protected void setUseIdentityHashCode(final boolean useIdentityHashCode) {
        this.useIdentityHashCode = useIdentityHashCode;
    }

    /**
     * Gets whether to use the field names passed in.
     *
     * @return the current useFieldNames flag
     */
    protected boolean isUseFieldNames() {
        return useFieldNames;
    }

    /**
     * Sets whether to use the field names passed in.
     *
     * @param useFieldNames  the new useFieldNames flag
     */
    protected void setUseFieldNames(final boolean useFieldNames) {
        this.useFieldNames = useFieldNames;
    }

    /**
     * Gets whether to use full detail when the caller doesn't
     * specify.
     *
     * @return the current defaultFullDetail flag
     */
    protected boolean isDefaultFullDetail() {
        return defaultFullDetail;
    }

    /**
     * Sets whether to use full detail when the caller doesn't
     * specify.
     *
     * @param defaultFullDetail  the new defaultFullDetail flag
     */
    protected void setDefaultFullDetail(final boolean defaultFullDetail) {
        this.defaultFullDetail = defaultFullDetail;
    }

    /**
     * Gets whether to output array content detail.
     *
     * @return the current array content detail setting
     */
    protected boolean isArrayContentDetail() {
        return arrayContentDetail;
    }

    /**
     * Sets whether to output array content detail.
     *
     * @param arrayContentDetail  the new arrayContentDetail flag
     */
    protected void setArrayContentDetail(final boolean arrayContentDetail) {
        this.arrayContentDetail = arrayContentDetail;
    }

    /**
     * Gets the array start text.
     *
     * @return the current array start text
     */
    protected String getArrayStart() {
        return arrayStart;
    }

    /**
     * Sets the array start text.
     *
     * <p>{@code null} is accepted, but will be converted to
     * an empty String.</p>
     *
     * @param arrayStart  the new array start text
     */
    protected void setArrayStart(String arrayStart) {
        if (arrayStart == null) {
            arrayStart = StringUtils.EMPTY;
        }
        this.arrayStart = arrayStart;
    }

    /**
     * Gets the array end text.
     *
     * @return the current array end text
     */
    protected String getArrayEnd() {
        return arrayEnd;
    }

    /**
     * Sets the array end text.
     *
     * <p>{@code null} is accepted, but will be converted to
     * an empty String.</p>
     *
     * @param arrayEnd  the new array end text
     */
    protected void setArrayEnd(String arrayEnd) {
        if (arrayEnd == null) {
            arrayEnd = StringUtils.EMPTY;
        }
        this.arrayEnd = arrayEnd;
    }

    /**
     * Gets the array separator text.
     *
     * @return the current array separator text
     */
    protected String getArraySeparator() {
        return arraySeparator;
    }

    /**
     * Sets the array separator text.
     *
     * <p>{@code null} is accepted, but will be converted to
     * an empty String.</p>
     *
     * @param arraySeparator  the new array separator text
     */
    protected void setArraySeparator(String arraySeparator) {
        if (arraySeparator == null) {
            arraySeparator = StringUtils.EMPTY;
        }
        this.arraySeparator = arraySeparator;
    }

    /**
     * Gets the content start text.
     *
     * @return the current content start text
     */
    protected String getContentStart() {
        return contentStart;
    }

    /**
     * Sets the content start text.
     *
     * <p>{@code null} is accepted, but will be converted to
     * an empty String.</p>
     *
     * @param contentStart  the new content start text
     */
    protected void setContentStart(String contentStart) {
        if (contentStart == null) {
            contentStart = StringUtils.EMPTY;
        }
        this.contentStart = contentStart;
    }

    /**
     * Gets the content end text.
     *
     * @return the current content end text
     */
    protected String getContentEnd() {
        return contentEnd;
    }

    /**
     * Sets the content end text.
     *
     * <p>{@code null} is accepted, but will be converted to
     * an empty String.</p>
     *
     * @param contentEnd  the new content end text
     */
    protected void setContentEnd(String contentEnd) {
        if (contentEnd == null) {
            contentEnd = StringUtils.EMPTY;
        }
        this.contentEnd = contentEnd;
    }

    /**
     * Gets the field name value separator text.
     *
     * @return the current field name value separator text
     */
    protected String getFieldNameValueSeparator() {
        return fieldNameValueSeparator;
    }

    /**
     * Sets the field name value separator text.
     *
     * <p>{@code null} is accepted, but will be converted to
     * an empty String.</p>
     *
     * @param fieldNameValueSeparator  the new field name value separator text
     */
    protected void setFieldNameValueSeparator(String fieldNameValueSeparator) {
        if (fieldNameValueSeparator == null) {
            fieldNameValueSeparator = StringUtils.EMPTY;
        }
        this.fieldNameValueSeparator = fieldNameValueSeparator;
    }

    /**
     * Gets the field separator text.
     *
     * @return the current field separator text
     */
    protected String getFieldSeparator() {
        return fieldSeparator;
    }

    /**
     * Sets the field separator text.
     *
     * <p>{@code null} is accepted, but will be converted to
     * an empty String.</p>
     *
     * @param fieldSeparator  the new field separator text
     */
    protected void setFieldSeparator(String fieldSeparator) {
        if (fieldSeparator == null) {
            fieldSeparator = StringUtils.EMPTY;
        }
        this.fieldSeparator = fieldSeparator;
    }

    /**
     * Gets whether the field separator should be added at the start
     * of each buffer.
     *
     * @return the fieldSeparatorAtStart flag
     * @since 2.0
     */
    protected boolean isFieldSeparatorAtStart() {
        return fieldSeparatorAtStart;
    }

    /**
     * Sets whether the field separator should be added at the start
     * of each buffer.
     *
     * @param fieldSeparatorAtStart  the fieldSeparatorAtStart flag
     * @since 2.0
     */
    protected void setFieldSeparatorAtStart(final boolean fieldSeparatorAtStart) {
        this.fieldSeparatorAtStart = fieldSeparatorAtStart;
    }

    /**
     * Gets whether the field separator should be added at the end
     * of each buffer.
     *
     * @return fieldSeparatorAtEnd flag
     * @since 2.0
     */
    protected boolean isFieldSeparatorAtEnd() {
        return fieldSeparatorAtEnd;
    }

    /**
     * Sets whether the field separator should be added at the end
     * of each buffer.
     *
     * @param fieldSeparatorAtEnd  the fieldSeparatorAtEnd flag
     * @since 2.0
     */
    protected void setFieldSeparatorAtEnd(final boolean fieldSeparatorAtEnd) {
        this.fieldSeparatorAtEnd = fieldSeparatorAtEnd;
    }

    /**
     * Gets the text to output when {@code null} found.
     *
     * @return the current text to output when null found
     */
    protected String getNullText() {
        return nullText;
    }

    /**
     * Sets the text to output when {@code null} found.
     *
     * <p>{@code null} is accepted, but will be converted to
     * an empty String.</p>
     *
     * @param nullText  the new text to output when null found
     */
    protected void setNullText(String nullText) {
        if (nullText == null) {
            nullText = StringUtils.EMPTY;
        }
        this.nullText = nullText;
    }

    /**
     * Gets the start text to output when a {@link Collection},
     * {@link Map} or array size is output.
     *
     * <p>This is output before the size value.</p>
     *
     * @return the current start of size text
     */
    protected String getSizeStartText() {
        return sizeStartText;
    }

    /**
     * Sets the start text to output when a {@link Collection},
     * {@link Map} or array size is output.
     *
     * <p>This is output before the size value.</p>
     *
     * <p>{@code null} is accepted, but will be converted to
     * an empty String.</p>
     *
     * @param sizeStartText  the new start of size text
     */
    protected void setSizeStartText(String sizeStartText) {
        if (sizeStartText == null) {
            sizeStartText = StringUtils.EMPTY;
        }
        this.sizeStartText = sizeStartText;
    }

    /**
     * Gets the end text to output when a {@link Collection},
     * {@link Map} or array size is output.
     *
     * <p>This is output after the size value.</p>
     *
     * @return the current end of size text
     */
    protected String getSizeEndText() {
        return sizeEndText;
    }

    /**
     * Sets the end text to output when a {@link Collection},
     * {@link Map} or array size is output.
     *
     * <p>This is output after the size value.</p>
     *
     * <p>{@code null} is accepted, but will be converted to
     * an empty String.</p>
     *
     * @param sizeEndText  the new end of size text
     */
    protected void setSizeEndText(String sizeEndText) {
        if (sizeEndText == null) {
            sizeEndText = StringUtils.EMPTY;
        }
        this.sizeEndText = sizeEndText;
    }

    /**
     * Gets the start text to output when an {@link Object} is
     * output in summary mode.
     *
     * <p>This is output before the size value.</p>
     *
     * @return the current start of summary text
     */
    protected String getSummaryObjectStartText() {
        return summaryObjectStartText;
    }

    /**
     * Sets the start text to output when an {@link Object} is
     * output in summary mode.
     *
     * <p>This is output before the size value.</p>
     *
     * <p>{@code null} is accepted, but will be converted to
     * an empty String.</p>
     *
     * @param summaryObjectStartText  the new start of summary text
     */
    protected void setSummaryObjectStartText(String summaryObjectStartText) {
        if (summaryObjectStartText == null) {
            summaryObjectStartText = StringUtils.EMPTY;
        }
        this.summaryObjectStartText = summaryObjectStartText;
    }

    /**
     * Gets the end text to output when an {@link Object} is
     * output in summary mode.
     *
     * <p>This is output after the size value.</p>
     *
     * @return the current end of summary text
     */
    protected String getSummaryObjectEndText() {
        return summaryObjectEndText;
    }

    /**
     * Sets the end text to output when an {@link Object} is
     * output in summary mode.
     *
     * <p>This is output after the size value.</p>
     *
     * <p>{@code null} is accepted, but will be converted to
     * an empty String.</p>
     *
     * @param summaryObjectEndText  the new end of summary text
     */
    protected void setSummaryObjectEndText(String summaryObjectEndText) {
        if (summaryObjectEndText == null) {
            summaryObjectEndText = StringUtils.EMPTY;
        }
        this.summaryObjectEndText = summaryObjectEndText;
    }

    /**
     * Default {@link ToStringStyle}.
     *
     * <p>This is an inner class rather than using
     * {@link StandardToStringStyle} to ensure its immutability.</p>
     */
    private static final class DefaultToStringStyle extends ToStringStyle {

        /**
         * Required for serialization support.
         *
         * @see java.io.Serializable
         */
        private static final long serialVersionUID = 1L;

        /**
         * Constructor.
         *
         * <p>Use the static constant rather than instantiating.</p>
         */
        DefaultToStringStyle() {
        }

        /**
         * Ensure Singleton after serialization.
         *
         * @return the singleton
         */
        private Object readResolve() {
            return DEFAULT_STYLE;
        }

    }

    /**
     * {@link ToStringStyle} that does not print out
     * the field names.
     *
     * <p>This is an inner class rather than using
     * {@link StandardToStringStyle} to ensure its immutability.
     */
    private static final class NoFieldNameToStringStyle extends ToStringStyle {

        private static final long serialVersionUID = 1L;

        /**
         * Constructor.
         *
         * <p>Use the static constant rather than instantiating.</p>
         */
        NoFieldNameToStringStyle() {
            this.setUseFieldNames(false);
        }

        /**
         * Ensure Singleton after serialization.
         *
         * @return the singleton
         */
        private Object readResolve() {
            return NO_FIELD_NAMES_STYLE;
        }

    }

    /**
     * {@link ToStringStyle} that prints out the short
     * class name and no identity hashcode.
     *
     * <p>This is an inner class rather than using
     * {@link StandardToStringStyle} to ensure its immutability.</p>
     */
    private static final class ShortPrefixToStringStyle extends ToStringStyle {

        private static final long serialVersionUID = 1L;

        /**
         * Constructor.
         *
         * <p>Use the static constant rather than instantiating.</p>
         */
        ShortPrefixToStringStyle() {
            this.setUseShortClassName(true);
            this.setUseIdentityHashCode(false);
        }

        /**
         * Ensure <code>Singleton</ode> after serialization.
         * @return the singleton
         */
        private Object readResolve() {
            return SHORT_PREFIX_STYLE;
        }

    }

    /**
     * {@link ToStringStyle} that does not print out the
     * classname, identity hashcode, content start or field name.
     *
     * <p>This is an inner class rather than using
     * {@link StandardToStringStyle} to ensure its immutability.</p>
     */
    private static final class SimpleToStringStyle extends ToStringStyle {

        private static final long serialVersionUID = 1L;

        /**
         * Constructor.
         *
         * <p>Use the static constant rather than instantiating.</p>
         */
        SimpleToStringStyle() {
            this.setUseClassName(false);
            this.setUseIdentityHashCode(false);
            this.setUseFieldNames(false);
            this.setContentStart(StringUtils.EMPTY);
            this.setContentEnd(StringUtils.EMPTY);
        }

        /**
         * Ensure <code>Singleton</ode> after serialization.
         * @return the singleton
         */
        private Object readResolve() {
            return SIMPLE_STYLE;
        }

    }

    /**
     * {@link ToStringStyle} that outputs on multiple lines.
     *
     * <p>This is an inner class rather than using
     * {@link StandardToStringStyle} to ensure its immutability.</p>
     */
    private static final class MultiLineToStringStyle extends ToStringStyle {

        private static final long serialVersionUID = 1L;

        /**
         * Constructor.
         *
         * <p>Use the static constant rather than instantiating.</p>
         */
        MultiLineToStringStyle() {
            this.setContentStart("[");
            this.setFieldSeparator(System.lineSeparator() + "  ");
            this.setFieldSeparatorAtStart(true);
            this.setContentEnd(System.lineSeparator() + "]");
        }

        /**
         * Ensure Singleton after serialization.
         *
         * @return the singleton
         */
        private Object readResolve() {
            return MULTI_LINE_STYLE;
        }

    }

    /**
     * {@link ToStringStyle} that does not print out the classname
     * and identity hash code but prints content start and field names.
     *
     * <p>This is an inner class rather than using
     * {@link StandardToStringStyle} to ensure its immutability.</p>
     */
    private static final class NoClassNameToStringStyle extends ToStringStyle {

        private static final long serialVersionUID = 1L;

        /**
         * Constructor.
         *
         * <p>Use the static constant rather than instantiating.</p>
         */
        NoClassNameToStringStyle() {
            this.setUseClassName(false);
            this.setUseIdentityHashCode(false);
        }

        /**
         * Ensure Singleton after serialization.
         *
         * @return the singleton
         */
        private Object readResolve() {
            return NO_CLASS_NAME_STYLE;
        }

    }

    /**
     * {@link ToStringStyle} that outputs with JSON format.
     *
     * <p>
     * This is an inner class rather than using
     * {@link StandardToStringStyle} to ensure its immutability.
     * </p>
     *
     * @since 3.4
     * @see <a href="https://www.json.org/">json.org</a>
     */
    private static final class JsonToStringStyle extends ToStringStyle {

        private static final long serialVersionUID = 1L;

        private static final String FIELD_NAME_QUOTE = "\"";

        /**
         * Constructor.
         *
         * <p>
         * Use the static constant rather than instantiating.
         * </p>
         */
        JsonToStringStyle() {
            this.setUseClassName(false);
            this.setUseIdentityHashCode(false);

            this.setContentStart("{");
            this.setContentEnd("}");

            this.setArrayStart("[");
            this.setArrayEnd("]");

            this.setFieldSeparator(",");
            this.setFieldNameValueSeparator(":");

            this.setNullText("null");

            this.setSummaryObjectStartText("\"<");
            this.setSummaryObjectEndText(">\"");

            this.setSizeStartText("\"<size=");
            this.setSizeEndText(">\"");
        }

        @Override
        public void append(final StringBuffer buffer, final String fieldName,
                           final Object[] array, final Boolean fullDetail) {

            if (fieldName == null) {
                throw new UnsupportedOperationException(
                        "Field names are mandatory when using JsonToStringStyle");
            }
            if (!isFullDetail(fullDetail)) {
                throw new UnsupportedOperationException(
                        "FullDetail must be true when using JsonToStringStyle");
            }

            super.append(buffer, fieldName, array, fullDetail);
        }

        @Override
        public void append(final StringBuffer buffer, final String fieldName, final long[] array,
                           final Boolean fullDetail) {

            if (fieldName == null) {
                throw new UnsupportedOperationException(
                        "Field names are mandatory when using JsonToStringStyle");
            }
            if (!isFullDetail(fullDetail)) {
                throw new UnsupportedOperationException(
                        "FullDetail must be true when using JsonToStringStyle");
            }

            super.append(buffer, fieldName, array, fullDetail);
        }

        @Override
        public void append(final StringBuffer buffer, final String fieldName, final int[] array,
                           final Boolean fullDetail) {

            if (fieldName == null) {
                throw new UnsupportedOperationException(
                        "Field names are mandatory when using JsonToStringStyle");
            }
            if (!isFullDetail(fullDetail)) {
                throw new UnsupportedOperationException(
                        "FullDetail must be true when using JsonToStringStyle");
            }

            super.append(buffer, fieldName, array, fullDetail);
        }

        @Override
        public void append(final StringBuffer buffer, final String fieldName,
                           final short[] array, final Boolean fullDetail) {

            if (fieldName == null) {
                throw new UnsupportedOperationException(
                        "Field names are mandatory when using JsonToStringStyle");
            }
            if (!isFullDetail(fullDetail)) {
                throw new UnsupportedOperationException(
                        "FullDetail must be true when using JsonToStringStyle");
            }

            super.append(buffer, fieldName, array, fullDetail);
        }

        @Override
        public void append(final StringBuffer buffer, final String fieldName, final byte[] array,
                           final Boolean fullDetail) {

            if (fieldName == null) {
                throw new UnsupportedOperationException(
                        "Field names are mandatory when using JsonToStringStyle");
            }
            if (!isFullDetail(fullDetail)) {
                throw new UnsupportedOperationException(
                        "FullDetail must be true when using JsonToStringStyle");
            }

            super.append(buffer, fieldName, array, fullDetail);
        }

        @Override
        public void append(final StringBuffer buffer, final String fieldName, final char[] array,
                           final Boolean fullDetail) {

            if (fieldName == null) {
                throw new UnsupportedOperationException(
                        "Field names are mandatory when using JsonToStringStyle");
            }
            if (!isFullDetail(fullDetail)) {
                throw new UnsupportedOperationException(
                        "FullDetail must be true when using JsonToStringStyle");
            }

            super.append(buffer, fieldName, array, fullDetail);
        }

        @Override
        public void append(final StringBuffer buffer, final String fieldName,
                           final double[] array, final Boolean fullDetail) {

            if (fieldName == null) {
                throw new UnsupportedOperationException(
                        "Field names are mandatory when using JsonToStringStyle");
            }
            if (!isFullDetail(fullDetail)) {
                throw new UnsupportedOperationException(
                        "FullDetail must be true when using JsonToStringStyle");
            }

            super.append(buffer, fieldName, array, fullDetail);
        }

        @Override
        public void append(final StringBuffer buffer, final String fieldName,
                           final float[] array, final Boolean fullDetail) {

            if (fieldName == null) {
                throw new UnsupportedOperationException(
                        "Field names are mandatory when using JsonToStringStyle");
            }
            if (!isFullDetail(fullDetail)) {
                throw new UnsupportedOperationException(
                        "FullDetail must be true when using JsonToStringStyle");
            }

            super.append(buffer, fieldName, array, fullDetail);
        }

        @Override
        public void append(final StringBuffer buffer, final String fieldName,
                           final boolean[] array, final Boolean fullDetail) {

            if (fieldName == null) {
                throw new UnsupportedOperationException(
                        "Field names are mandatory when using JsonToStringStyle");
            }
            if (!isFullDetail(fullDetail)) {
                throw new UnsupportedOperationException(
                        "FullDetail must be true when using JsonToStringStyle");
            }

            super.append(buffer, fieldName, array, fullDetail);
        }

        @Override
        public void append(final StringBuffer buffer, final String fieldName, final Object value,
                           final Boolean fullDetail) {

            if (fieldName == null) {
                throw new UnsupportedOperationException(
                        "Field names are mandatory when using JsonToStringStyle");
            }
            if (!isFullDetail(fullDetail)) {
                throw new UnsupportedOperationException(
                        "FullDetail must be true when using JsonToStringStyle");
            }

            super.append(buffer, fieldName, value, fullDetail);
        }

        @Override
        protected void appendDetail(final StringBuffer buffer, final String fieldName, final char value) {
            appendValueAsString(buffer, String.valueOf(value));
        }

        @Override
        protected void appendDetail(final StringBuffer buffer, final String fieldName, final Object value) {

            if (value == null) {
                appendNullText(buffer, fieldName);
                return;
            }

            if (value instanceof String || value instanceof Character) {
                appendValueAsString(buffer, value.toString());
                return;
            }

            if (value instanceof Number || value instanceof Boolean) {
                buffer.append(value);
                return;
            }

            final String valueAsString = value.toString();
            if (isJsonObject(valueAsString) || isJsonArray(valueAsString)) {
                buffer.append(value);
                return;
            }

            appendDetail(buffer, fieldName, valueAsString);
        }

        @Override
        protected void appendDetail(final StringBuffer buffer, final String fieldName, final Collection<?> coll) {
            if (coll != null && !coll.isEmpty()) {
                buffer.append(getArrayStart());
                int i = 0;
                for (final Object item : coll) {
                    appendDetail(buffer, fieldName, i++, item);
                }
                buffer.append(getArrayEnd());
                return;
            }

            buffer.append(coll);
        }

        @Override
        protected void appendDetail(final StringBuffer buffer, final String fieldName, final Map<?, ?> map) {
            if (map != null && !map.isEmpty()) {
                buffer.append(getContentStart());

                boolean firstItem = true;
                for (final Entry<?, ?> entry : map.entrySet()) {
                    final String keyStr = Objects.toString(entry.getKey(), null);
                    if (keyStr != null) {
                        if (firstItem) {
                            firstItem = false;
                        } else {
                            appendFieldEnd(buffer, keyStr);
                        }
                        appendFieldStart(buffer, keyStr);
                        final Object value = entry.getValue();
                        if (value == null) {
                            appendNullText(buffer, keyStr);
                        } else {
                            appendInternal(buffer, keyStr, value, true);
                        }
                    }
                }

                buffer.append(getContentEnd());
                return;
            }

            buffer.append(map);
        }

        private boolean isJsonArray(final String valueAsString) {
            return valueAsString.startsWith(getArrayStart())
                    && valueAsString.endsWith(getArrayEnd());
        }

        private boolean isJsonObject(final String valueAsString) {
            return valueAsString.startsWith(getContentStart())
                    && valueAsString.endsWith(getContentEnd());
        }

        /**
         * Appends the given String enclosed in double-quotes to the given StringBuffer.
         *
         * @param buffer the StringBuffer to append the value to.
         * @param value the value to append.
         */
        private void appendValueAsString(final StringBuffer buffer, final String value) {
            buffer.append('"').append(StringEscapeUtils.escapeJson(value)).append('"');
        }

        @Override
        protected void appendFieldStart(final StringBuffer buffer, final String fieldName) {

            if (fieldName == null) {
                throw new UnsupportedOperationException(
                        "Field names are mandatory when using JsonToStringStyle");
            }

            super.appendFieldStart(buffer, FIELD_NAME_QUOTE + StringEscapeUtils.escapeJson(fieldName)
                    + FIELD_NAME_QUOTE);
        }

        /**
         * Ensure Singleton after serialization.
         *
         * @return the singleton
         */
        private Object readResolve() {
            return JSON_STYLE;
        }

    }
}

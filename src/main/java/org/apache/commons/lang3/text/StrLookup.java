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
package org.apache.commons.lang3.text;

import java.util.Map;

import org.apache.commons.lang3.SystemProperties;

/**
 * Lookup a String key to a String value.
 * <p>
 * This class represents the simplest form of a string to string map.
 * It has a benefit over a map in that it can create the result on
 * demand based on the key.
 * </p>
 * <p>
 * This class comes complete with various factory methods.
 * If these do not suffice, you can subclass and implement your own matcher.
 * </p>
 * <p>
 * For example, it would be possible to implement a lookup that used the
 * key as a primary key, and looked up the value on demand from the database.
 * </p>
 *
 * @param <V> Unused.
 * @since 2.2
 * @deprecated As of 3.6, use Apache Commons Text
 * <a href="https://commons.apache.org/proper/commons-text/javadocs/api-release/org/apache/commons/text/lookup/StringLookupFactory.html">
 * StringLookupFactory</a> instead
 */
@Deprecated
public abstract class StrLookup<V> {

    /**
     * Lookup that always returns null.
     */
    private static final StrLookup<String> NONE_LOOKUP = new MapStrLookup<>(null);

    /**
     * Lookup based on system properties.
     */
    private static final StrLookup<String> SYSTEM_PROPERTIES_LOOKUP = new SystemPropertiesStrLookup();

    /**
     * Returns a lookup which always returns null.
     *
     * @return a lookup that always returns null, not null
     */
    public static StrLookup<?> noneLookup() {
        return NONE_LOOKUP;
    }

    /**
     * Returns a new lookup which uses a copy of the current
     * {@link System#getProperties() System properties}.
     * <p>
     * If a security manager blocked access to system properties, then null will
     * be returned from every lookup.
     * </p>
     * <p>
     * If a null key is used, this lookup will throw a NullPointerException.
     * </p>
     *
     * @return a lookup using system properties, not null
     */
    public static StrLookup<String> systemPropertiesLookup() {
        return SYSTEM_PROPERTIES_LOOKUP;
    }

    /**
     * Returns a lookup which looks up values using a map.
     * <p>
     * If the map is null, then null will be returned from every lookup.
     * The map result object is converted to a string using toString().
     * </p>
     *
     * @param <V> the type of the values supported by the lookup
     * @param map  the map of keys to values, may be null
     * @return a lookup using the map, not null
     */
    public static <V> StrLookup<V> mapLookup(final Map<String, V> map) {
        return new MapStrLookup<>(map);
    }

    /**
     * Constructor.
     */
    protected StrLookup() {
    }

    /**
     * Looks up a String key to a String value.
     * <p>
     * The internal implementation may use any mechanism to return the value.
     * The simplest implementation is to use a Map. However, virtually any
     * implementation is possible.
     * </p>
     * <p>
     * For example, it would be possible to implement a lookup that used the
     * key as a primary key, and looked up the value on demand from the database
     * Or, a numeric based implementation could be created that treats the key
     * as an integer, increments the value and return the result as a string -
     * converting 1 to 2, 15 to 16 etc.
     * </p>
     * <p>
     * The {@link #lookup(String)} method always returns a String, regardless of
     * the underlying data, by converting it as necessary. For example:
     * </p>
     * <pre>
     * Map&lt;String, Object&gt; map = new HashMap&lt;String, Object&gt;();
     * map.put("number", Integer.valueOf(2));
     * assertEquals("2", StrLookup.mapLookup(map).lookup("number"));
     * </pre>
     * @param key  the key to be looked up, may be null
     * @return the matching value, null if no match
     */
    public abstract String lookup(String key);

    /**
     * Lookup implementation that uses a Map.
     *
     * @param <V> the type of mapped values.
     */
    static class MapStrLookup<V> extends StrLookup<V> {

        /** Map keys are variable names and value. */
        private final Map<String, V> map;

        /**
         * Creates a new instance backed by a Map.
         *
         * @param map  the map of keys to values, may be null
         */
        MapStrLookup(final Map<String, V> map) {
            this.map = map;
        }

        /**
         * Looks up a String key to a String value using the map.
         * <p>
         * If the map is null, then null is returned.
         * The map result object is converted to a string using toString().
         * </p>
         *
         * @param key  the key to be looked up, may be null
         * @return the matching value, null if no match
         */
        @Override
        public String lookup(final String key) {
            if (map == null) {
                return null;
            }
            final Object obj = map.get(key);
            if (obj == null) {
                return null;
            }
            return obj.toString();
        }
    }

    /**
     * Lookup implementation based on system properties.
     */
    private static class SystemPropertiesStrLookup extends StrLookup<String> {
        /**
         * {@inheritDoc} This implementation directly accesses system properties.
         */
        @Override
        public String lookup(final String key) {
            return SystemProperties.getProperty(key);
        }
    }
}

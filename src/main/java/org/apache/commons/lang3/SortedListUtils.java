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
package org.apache.commons.lang3;

import java.util.Comparator;
import java.util.List;
import java.util.function.Function;


/**
 * Operations on sorted {@link List}.
 */
public class SortedListUtils {
    /**
     * Finds element in sorted list.
     *
     * @param list
     *      list sorted by key field
     * @param key
     *      key to search for
     * @param keyExtractor
     *      function to extract key from element
     * @param comparator
     *      comparator for keys
     *
     * @return
     *      index of the search key, if it is contained in the list within specified range; otherwise,
     *      (-first_greater - 1).  The first_greater is the index of lowest greater element in the list - if all elements
     *      are lower, the first_greater is defined as toIndex.
     *
     * @param <T>
     *     type of list element
     * @param <K>
     *     type of key
     */
    public static <K, T> int binarySearch(
            List<T> list,
            K key,
            Function<T, K> keyExtractor, Comparator<? super K> comparator
    ) {
        return binarySearch(list, 0, list.size(), key, keyExtractor, comparator);
    }

    /**
     * Finds element in sorted list, within range fromIndex - toIndex (inclusive - exclusive).
     *
     * @param list
     *      list sorted by key field
     * @param fromIndex
     *      start index
     * @param toIndex
     *      end index (exclusive)
     * @param key
     *      key to search for
     * @param keyExtractor
     *      function to extract key from element
     * @param comparator
     *      comparator for keys
     *
     * @return
     *      index of the search key, if it is contained in the list within specified range; otherwise,
     *      (-first_greater - 1).  The first_greater is the index of lowest greater element in the list - if all elements
     *      are lower, the first_greater is defined as toIndex.
     *
     * @param <T>
     *     type of array element
     * @param <K>
     *     type of key
     */
    public static <T, K> int binarySearch(
            List<T> list,
            int fromIndex, int toIndex,
            K key,
            Function<T, K> keyExtractor, Comparator<? super K> comparator
    ) {
        int l = fromIndex;
        int h = toIndex - 1;

        while (l <= h) {
            int m = (l + h) >>> 1; // unsigned shift to avoid overflow
            K value = keyExtractor.apply(list.get(m));
            int c = comparator.compare(value, key);
            if (c < 0) {
                l = m + 1;
            } else if (c > 0) {
                h = m - 1;
            } else {
                // 0, found
                return m;
            }
        }

        // not found, the l points to the lowest higher match:
        return -l - 1;
    }
}

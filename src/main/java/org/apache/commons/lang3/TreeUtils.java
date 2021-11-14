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


import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * list and tree translate util class
 * @author <a href="mailto:hjm0928@gmail.com">ejums</a>
 */
public class TreeUtils {
    /**
     * list to tree; this method will change original list!
     * @param list original list
     * @param topConsumer  the top node will be consumed by it
     * @param idGetter  to get id from node
     * @param pidGetter to get parent id from node
     * @param comparator to sort same level node
     * @param generator to subset parent relation association is carried out operation
     * @param <T> the type of the object
     * @return tree list
     */
    public static <T, E> List<T> buildTree(List<T> list, Consumer<T> topConsumer, Function<T, E> idGetter,
                                           Function<T, E> pidGetter, Comparator<T> comparator,
                                           BiConsumer<T, List<T>> generator){

        Set<E> idSet = new HashSet<>();
        Map<E, List<T>> pidGroup = new HashMap<>(list.size() >> 1);
        // make group by pid
        for (T t : list) {
            idSet.add(idGetter.apply(t));
            final E pid = pidGetter.apply(t);
            final List<T> itemList = pidGroup.getOrDefault(pid, new ArrayList<>());
            itemList.add(t);
            pidGroup.put(pid, itemList);
        }
        // get the top node: pid cannot find in id set
        List<T> topList = pidGroup.keySet().stream()
                .filter(v -> !idSet.contains(v))
                // find all top node
                .flatMap(v -> pidGroup.getOrDefault(v, Collections.emptyList()).stream())
                // consumer this top node
                .peek(v -> {
                    if (topConsumer != null) {
                        topConsumer.accept(v);
                    }
                })
                .collect(Collectors.toList());
        // if comparator set, make top node list sorted
        if (comparator != null) {
            topList.sort(comparator);
        }

        // build tree with no recursive
        List<T> children = new ArrayList<>();
        List<T> temp = new ArrayList<>(topList);
        while (!temp.isEmpty()) {
            temp.forEach(v -> {
                E apply = idGetter.apply(v);
                if (apply != null) {
                    List<T> child = pidGroup.get(apply);
                    if (child != null) {
                        if (comparator != null) {
                            child.sort(comparator);
                        }
                        generator.accept(v, child);
                        children.addAll(child);
                    }
                }
            });
            temp.clear();
            temp.addAll(children);
            children.clear();
        }
        return topList;
    }
}

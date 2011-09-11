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
package org.apache.commons.lang3.compare;

import java.util.Iterator;

/** 
 * Decorates an iterator such that it cannot be modified.
 * <p>
 * Attempts to modify it will result in an UnsupportedOperationException. 
 *
 * @since Commons Collections 3.0
 * @version $Revision: 1148801 $ $Date: 2011-07-20 07:44:46 -0700 (Wed, 20 Jul 2011) $
 */
final class UnmodifiableIterator<E> implements Iterator<E> {

    /** The iterator being decorated */
    private final Iterator<E> iterator;

    //-----------------------------------------------------------------------
    /**
     * Constructor.
     *
     * @param iterator  the iterator to decorate
     */
    public UnmodifiableIterator(Iterator<E> iterator) {
        super();
        if (iterator == null) {
            throw new IllegalArgumentException("Iterator must not be null");
        }
        this.iterator = iterator;
    }

    //-----------------------------------------------------------------------
    public boolean hasNext() {
        return iterator.hasNext();
    }

    public E next() {
        return iterator.next();
    }

    public void remove() {
        throw new UnsupportedOperationException("remove() is not supported");
    }

}

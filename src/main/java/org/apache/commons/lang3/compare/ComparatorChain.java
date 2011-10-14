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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

/**
 * <p>A ComparatorChain is a Comparator that wraps one or
 * more Comparators in sequence.  The ComparatorChain
 * calls each Comparator in sequence until either 1)
 * any single Comparator returns a non-zero result
 * (and that result is then returned),
 * or 2) the ComparatorChain is exhausted (and zero is
 * returned).  This type of sorting is very similar
 * to multi-column sorting in SQL, and this class
 * allows Java classes to emulate that kind of behaviour
 * when sorting a List.</p>
 *
 * <p>Instances of ComparatorChain are not synchronized.
 * The class is not thread-safe at construction time, but
 * it <i>is</i> thread-safe to perform multiple comparisons
 * after all the setup operations are complete.</p>
 *
 * @since Commons Collections 2.0
 * @version $Revision$ $Date$
 */
public class ComparatorChain<E> implements Comparator<E>, Serializable, Iterable {

    /** The list of comparators in the chain. */
    protected List<Comparator<E>> comparatorChain = null;

    //-----------------------------------------------------------------------
    /**
     * Construct a ComparatorChain with a single Comparator,
     * sorting in the forward order
     *
     * @param comparators Comparators in the Comparator chain
     */
    public ComparatorChain(Comparator<E>... comparators) {
        this.comparatorChain = new ArrayList<Comparator<E>>();
        Collections.addAll(this.comparatorChain, comparators);
    }

    /**
     * Construct a ComparatorChain from the Comparators in the
     * List.  All Comparators will default to the forward
     * sort order.
     *
     * @param list   List of Comparators
     */
    public ComparatorChain(List<Comparator<E>> list) {
        this.comparatorChain = new ArrayList<Comparator<E>>();
        this.comparatorChain.addAll(list);
    }

    //-----------------------------------------------------------------------
    /**
     * Number of Comparators in the current ComparatorChain.
     *
     * @return Comparator count
     */
    public int size() {
        return comparatorChain.size();
    }

    /**
     * Perform comparisons on the Objects as per Comparator.compare(o1,o2).
     *
     * @param o1  the first object to compare
     * @param o2  the second object to compare
     * @return -1, 0, or 1
     */
    public int compare(E o1, E o2) {

        // iterate over all comparators in the chain
        Iterator<Comparator<E>> comparators = comparatorChain.iterator();
        for (int comparatorIndex = 0; comparators.hasNext(); ++comparatorIndex) {

            Comparator<E> comparator = comparators.next();
            int retval = comparator.compare(o1,o2);
            if (retval != 0) {
                return retval;
            }
        }

        // if comparators are exhausted, return 0
        return 0;
    }

    //-----------------------------------------------------------------------
    /**
     * Iterate through the chained comparators. 
     *
     * @return Unmodifiable iterator over the chained comparators
     */
    public Iterator<Comparator<E>> iterator() {
        return new UnmodifiableIterator(comparatorChain.iterator());
    }

    //-----------------------------------------------------------------------
    /**
     * Implement a hash code for this comparator that is consistent with
     * {@link #equals(Object) equals}.
     *
     * @return a suitable hash code
     * @since Commons Collections 3.0
     */
    @Override
    public int hashCode() {
        int hash = 0;
        if (comparatorChain != null) {
            hash ^= comparatorChain.hashCode();
        }
        return hash;
    }

    /**
     * Returns <code>true</code> iff <i>that</i> Object is
     * is a {@link Comparator} whose ordering is known to be
     * equivalent to mine.
     * <p>
     * This implementation returns <code>true</code>
     * iff <code><i>object</i>.{@link Object#getClass() getClass()}</code>
     * equals <code>this.getClass()</code>, and the underlying
     * comparators and order bits are equal.
     * Subclasses may want to override this behavior to remain consistent
     * with the {@link Comparator#equals(Object)} contract.
     *
     * @param object  the object to compare with
     * @return true if equal
     * @since Commons Collections 3.0
     */
    @Override
    public boolean equals(Object object) {
        if (this == object) {
            return true;
        }
        if (object == null) {
            return false;
        }
        if (object.getClass().equals(this.getClass())) {
            ComparatorChain<?> chain = (ComparatorChain<?>) object;
            return ( (comparatorChain == null ? chain.comparatorChain == null
                    : comparatorChain.equals(chain.comparatorChain)));
        }
        return false;
    }

}

/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */
package org.apache.commons.lang.functor;

import java.io.Serializable;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

import org.apache.commons.lang.functor.Predicate;
import org.apache.commons.lang.functor.PredicateUtils;
import org.apache.commons.lang.functor.Transformer;
import org.apache.commons.lang.functor.TransformerException;
import org.apache.commons.lang.functor.TransformerUtils;
/**
 * <code>ExecutorUtils</code> provides reference implementations and utilities
 * for the Executor functor interface. The supplied executors are:
 * <ul>
 * <li>Invoker - invokes a method on the input object
 * <li>For - repeatedly calls an executor for a fixed number of times
 * <li>While - repeatedly calls an executor while a predicate is true
 * <li>DoWhile - repeatedly calls an executor while a predicate is true
 * <li>Chained - chains two or more executors together
 * <li>Switch - calls one executor based on one or more predicates
 * <li>SwitchMap - calls one executor looked up from a Map
 * <li>Transformer - wraps a Transformer as an Executor
 * <li>NOP - does nothing
 * <li>Exception - always throws an exception
 * </ul>
 * All the supplied executors are Serializable.
 *
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id: ExecutorUtils.java,v 1.2 2002/11/14 21:54:49 scolebourne Exp $
 */
public class ExecutorUtils {

    /**
     * A Executor that always throws an exception
     */
    private static final Executor EXCEPTION_EXECUTOR = new ExceptionExecutor();
    /**
     * A Executor that does nothing
     */
    private static final Executor NOP_EXECUTOR = new NOPExecutor();

    /**
     * Restrictive constructor
     */
    protected ExecutorUtils() {
        super();
    }

    /**
     * Gets a Executor that always throws an exception.
     * This could be useful during testing as a placeholder.
     *
     * @return the executor
     */
    public static Executor exceptionExecutor() {
        return EXCEPTION_EXECUTOR;
    }

    /**
     * Gets a Executor that will do nothing.
     * This could be useful during testing as a placeholder.
     *
     * @return the executor
     */
    public static Executor nopExecutor() {
        return NOP_EXECUTOR;
    }

    /**
     * Creates a Executor that calls a Transformer each time it is called.
     * The transformer will be called using the executor's input object.
     * The transformer's result will be ignored.
     *
     * @param transformer  the transformer to run each time in the executor
     * @return the executor.
     */
    public static Executor asExecutor(Transformer transformer) {
        if (transformer == null) {
            throw new IllegalArgumentException("The transformer must not be null");
        }
        return new TransformerExecutor(transformer);
    }

    /**
     * Creates a Executor that will call the executor <code>count</code> times.
     *
     * @param count  the number of times to loop
     * @param executor  the executor to call repeatedly
     * @return the <code>for</code> executor
     * @throws IllegalArgumentException if either argument is null
     */
    public static Executor forExecutor(int count, Executor executor) {
        if (count < 0) {
            throw new IllegalArgumentException("The loop count must not be less than zero, it was " + count);
        }
        if (executor == null) {
            throw new IllegalArgumentException("The executor must not be null");
        }
        return new ForExecutor(count, executor);
    }

    /**
     * Creates a Executor that will call the executor repeatedly until the 
     * predicate returns false.
     *
     * @param predicate  the predicate to use as an end of loop test
     * @param executor  the executor to call repeatedly
     * @return the <code>while</code> executor
     * @throws IllegalArgumentException if either argument is null
     */
    public static Executor whileExecutor(Predicate predicate, Executor executor) {
        if (predicate == null) {
            throw new IllegalArgumentException("The predicate must not be null");
        }
        if (executor == null) {
            throw new IllegalArgumentException("The executor must not be null");
        }
        return new WhileExecutor(predicate, executor, false);
    }

    /**
     * Creates a Executor that will call the executor once and then repeatedly
     * until the predicate returns false.
     *
     * @param executor  the executor to call repeatedly
     * @param predicate  the predicate to use as an end of loop test
     * @return the <code>do-while</code> executor
     * @throws IllegalArgumentException if either argument is null
     */
    public static Executor doWhileExecutor(Executor executor, Predicate predicate) {
        if (executor == null) {
            throw new IllegalArgumentException("The executor must not be null");
        }
        if (predicate == null) {
            throw new IllegalArgumentException("The predicate must not be null");
        }
        return new WhileExecutor(predicate, executor, true);
    }

    /**
     * Creates a Executor that will invoke a specific method on the executor's
     * input object by reflection.
     *
     * @param methodName  the name of the method
     * @return the <code>invoker</code> executor
     * @throws IllegalArgumentException if the method name is null
     */
    public static Executor invokerExecutor(String methodName) {
        // reuse transformer as it has caching - this is lazy really, should have inner class here
        return asExecutor(TransformerUtils.invokerTransformer(methodName, null, null));
    }

    /**
     * Creates a Executor that will invoke a specific method on the executor's
     * input object by reflection.
     *
     * @param methodName  the name of the method
     * @param paramTypes  the parameter types
     * @param args  the arguments
     * @return the <code>invoker</code> executor
     * @throws IllegalArgumentException if the method name is null
     * @throws IllegalArgumentException if the paramTypes and args don't match
     */
    public static Executor invokerExecutor(String methodName, Class[] paramTypes, Object[] args) {
        // reuse transformer as it has caching - this is lazy really, should have inner class here
        return asExecutor(TransformerUtils.invokerTransformer(methodName, paramTypes, args));
    }

    /**
     * Create a new Executor that calls two Executors, passing the result of
     * the first into the second.
     * 
     * @param executor1  the first executor
     * @param executor2  the second executor
     * @return the <code>chained</code> executor
     * @throws IllegalArgumentException if either executor is null
     */
    public static Executor chainedExecutor(Executor executor1, Executor executor2) {
        Executor[] executors = new Executor[] { executor1, executor2 };
        validate(executors);
        return new ChainedExecutor(executors);
    }

    /**
     * Create a new Executor that calls each executor in turn, passing the 
     * result into the next executor.
     * 
     * @param executors  an array of executors to chain
     * @return the <code>chained</code> executor
     * @throws IllegalArgumentException if the executors array is null
     * @throws IllegalArgumentException if the executors array has 0 elements
     * @throws IllegalArgumentException if any executor in the array is null
     */
    public static Executor chainedExecutor(Executor[] executors) {
        executors = copy(executors);
        validate(executors);
        return new ChainedExecutor(executors);
    }

    /**
     * Create a new Executor that calls each executor in turn, passing the 
     * result into the next executor. The ordering is that of the iterator()
     * method on the collection.
     * 
     * @param executors  a collection of executors to chain
     * @return the <code>chained</code> executor
     * @throws IllegalArgumentException if the executors collection is null
     * @throws IllegalArgumentException if the executors collection is empty
     * @throws IllegalArgumentException if any executor in the collection is null
     */
    public static Executor chainedExecutor(Collection executors) {
        if (executors == null) {
            throw new IllegalArgumentException("The executor collection must not be null");
        }
        // convert to array like this to guarantee iterator() ordering
        Executor[] cmds = new Executor[executors.size()];
        int i = 0;
        for (Iterator it = executors.iterator(); it.hasNext();) {
            cmds[i++] = (Executor) it.next();
        }
        validate(cmds);
        return new ChainedExecutor(cmds);
    }

    /**
     * Create a new Executor that calls one of two executors depending 
     * on the specified predicate.
     * 
     * @param predicate  the predicate to switch on
     * @param trueExecutor  the executor called if the predicate is true
     * @param falseExecutor  the executor called if the predicate is false
     * @return the <code>switch</code> executor
     * @throws IllegalArgumentException if the predicate is null
     * @throws IllegalArgumentException if either executor is null
     */
    public static Executor switchExecutor(Predicate predicate, Executor trueExecutor, Executor falseExecutor) {
        return switchExecutorInternal(new Predicate[] { predicate }, new Executor[] { trueExecutor }, falseExecutor);
    }

    /**
     * Create a new Executor that calls one of the executors depending 
     * on the predicates.
     * <p>
     * The executor at array location 0 is called if the predicate at array 
     * location 0 returned true. Each predicate is evaluated
     * until one returns true.
     * 
     * @param predicates  an array of predicates to check
     * @param executors  an array of executors to call
     * @return the <code>switch</code> executor
     * @throws IllegalArgumentException if the either array is null
     * @throws IllegalArgumentException if the either array has 0 elements
     * @throws IllegalArgumentException if any element in the arrays is null
     * @throws IllegalArgumentException if the arrays are different sizes
     */
    public static Executor switchExecutor(Predicate[] predicates, Executor[] executors) {
        return switchExecutorInternal(copy(predicates), copy(executors), null);
    }

    /**
     * Create a new Executor that calls one of the executors depending 
     * on the predicates.
     * <p>
     * The executor at array location 0 is called if the predicate at array
     * location 0 returned true. Each predicate is evaluated
     * until one returns true. If no predicates evaluate to true, the default
     * executor is called.
     * 
     * @param predicates  an array of predicates to check
     * @param executors  an array of executors to call
     * @param defaultExecutor  the default to call if no predicate matches
     * @return the <code>switch</code> executor
     * @throws IllegalArgumentException if the either array is null
     * @throws IllegalArgumentException if the either array has 0 elements
     * @throws IllegalArgumentException if any element in the arrays is null
     * @throws IllegalArgumentException if the arrays are different sizes
     */
    public static Executor switchExecutor(Predicate[] predicates, Executor[] executors, Executor defaultExecutor) {
        return switchExecutorInternal(copy(predicates), copy(executors), defaultExecutor);
    }
    
    /**
     * Create a new Executor that calls one of the executors depending 
     * on the predicates. 
     * <p>
     * The Map consists of Predicate keys and Executor values. A executor 
     * is called if its matching predicate returns true. Each predicate is evaluated
     * until one returns true. If no predicates evaluate to true, the default
     * executor is called. The default executor is set in the map with a 
     * null key. The ordering is that of the iterator() method on the entryset 
     * collection of the map.
     * 
     * @param predicatesAndExecutors  a map of predicates to executors
     * @return the <code>switch</code> executor
     * @throws IllegalArgumentException if the map is null
     * @throws IllegalArgumentException if the map is empty
     * @throws IllegalArgumentException if any executor in the map is null
     * @throws ClassCastException  if the map elements are of the wrong type
     */
    public static Executor switchExecutor(Map predicatesAndExecutors) {
        Executor[] trs = null;
        Predicate[] preds = null;
        if (predicatesAndExecutors == null) {
            throw new IllegalArgumentException("The predicate and executor map must not be null");
        }
        // convert to array like this to guarantee iterator() ordering
        Executor def = (Executor) predicatesAndExecutors.remove(null);
        int size = predicatesAndExecutors.size();
        trs = new Executor[size];
        preds = new Predicate[size];
        int i = 0;
        for (Iterator it = predicatesAndExecutors.entrySet().iterator(); it.hasNext();) {
            Map.Entry entry = (Map.Entry) it.next();
            preds[i] = (Predicate) entry.getKey();
            trs[i] = (Executor) entry.getValue();
            i++;
        }
        return switchExecutorInternal(preds, trs, def);
    }

    /**
     * Validate input and create executor.
     * 
     * @param predicates  an array of predicates to check
     * @param executors  an array of executors to call
     * @param defaultExecutor  the default to call if no predicate matches
     * @return the <code>switch</code> executor
     * @throws IllegalArgumentException if the either array is null
     * @throws IllegalArgumentException if the either array has 0 elements
     * @throws IllegalArgumentException if any element in the arrays is null
     * @throws IllegalArgumentException if the arrays are different sizes
     */
    private static Executor switchExecutorInternal(Predicate[] predicates, Executor[] executors, Executor defaultExecutor) {
        validate(predicates);
        validate(executors);
        if (predicates.length != executors.length) {
            throw new IllegalArgumentException("The predicate and executor arrays must be the same size");
        }
        if (defaultExecutor == null) {
            defaultExecutor = nopExecutor();
        }
        return new SwitchExecutor(predicates, executors, defaultExecutor);
    }

    /**
     * Create a new Executor that uses the input object as a key to find the
     * executor to call. 
     * <p>
     * The Map consists of object keys and Executor values. A executor 
     * is called if the input object equals the key. If there is no match, the
     * default executor is called. The default executor is set in the map
     * using a null key.
     * 
     * @param objectsAndExecutors  a map of objects to executors
     * @return the executor
     * @throws IllegalArgumentException if the map is null
     * @throws IllegalArgumentException if the map is empty
     * @throws IllegalArgumentException if any executor in the map is null
     */
    public static Executor switchMapExecutor(Map objectsAndExecutors) {
        Executor[] trs = null;
        Predicate[] preds = null;
        if (objectsAndExecutors == null) {
            throw new IllegalArgumentException("The obejct and executor map must not be null");
        }
        Executor def = (Executor) objectsAndExecutors.remove(null);
        int size = objectsAndExecutors.size();
        trs = new Executor[size];
        preds = new Predicate[size];
        int i = 0;
        for (Iterator it = objectsAndExecutors.entrySet().iterator(); it.hasNext();) {
            Map.Entry entry = (Map.Entry) it.next();
            preds[i] = PredicateUtils.equalPredicate(entry.getKey());
            trs[i] = (Executor) entry.getValue();
            i++;
        }
        return switchExecutor(preds, trs, def);
    }

    /**
     * Clone the predicates to ensure that the internal reference can't be messed with.
     * 
     * @param predicates  the predicates to copy
     * @return the cloned predicates
     */
    private static Predicate[] copy(Predicate[] predicates) {
        if (predicates == null) {
            return null;
        }
        return (Predicate[]) predicates.clone();
    }
    
    /**
     * Validate the predicates to ensure that all is well.
     * 
     * @param predicates  the predicates to validate
     * @return the validated predicates
     */
    private static void validate(Predicate[] predicates) {
        if (predicates == null) {
            throw new IllegalArgumentException("The predicate array must not be null");
        }
        if (predicates.length < 1) {
            throw new IllegalArgumentException(
                "At least 1 predicate must be specified in the predicate array, size was " + predicates.length);
        }
        for (int i = 0; i < predicates.length; i++) {
            if (predicates[i] == null) {
                throw new IllegalArgumentException("The predicate array must not contain a null predicate, index " + i + " was null");
            }
        }
    }

    /**
     * Clone the executors to ensure that the internal reference can't be messed with.
     * 
     * @param executors  the executors to copy
     * @return the cloned executors
     */
    private static Executor[] copy(Executor[] executors) {
        if (executors == null) {
            return null;
        }
        return (Executor[]) executors.clone();
    }
    
    /**
     * Validate the executors to ensure that all is well.
     * 
     * @param executors  the executors to validate
     * @return the validated executors
     */
    private static void validate(Executor[] executors) {
        if (executors == null) {
            throw new IllegalArgumentException("The executor array must not be null");
        }
        if (executors.length < 1) {
            throw new IllegalArgumentException(
                "At least 1 executor must be specified in the executor array, size was " + executors.length);
        }
        for (int i = 0; i < executors.length; i++) {
            if (executors[i] == null) {
                throw new IllegalArgumentException("The executor array must not contain a null executor, index " + i + " was null");
            }
        }
    }

    // ExceptionExecutor
    //----------------------------------------------------------------------------------

    /**
     * ExceptionExecutor always throws an exception
     */
    private static class ExceptionExecutor implements Executor, Serializable {

        /**
         * Constructor
         */
        private ExceptionExecutor() {
            super();
        }

        /**
         * Always throw an exception
         */
        public void execute(Object input) {
            throw new ExecutorException("ExceptionExecutor invoked");
        }
    }

    // NOPExecutor
    //----------------------------------------------------------------------------------

    /**
     * NOPExecutor does nothing
     */
    private static class NOPExecutor implements Executor, Serializable {

        /**
         * Constructor
         */
        private NOPExecutor() {
            super();
        }

        /**
         * Do nothing
         */
        public void execute(Object input) {
            // do nothing
        }
    }

    // TransformerExecutor
    //----------------------------------------------------------------------------------

    /**
     * TransformerExecutor calls a Transformer using the input object and ignore the result.
     */
    private static class TransformerExecutor implements Executor, Serializable {
        /** The transformer to wrap */
        private final Transformer iTransformer;

        /**
         * Constructor to store transformer
         */
        private TransformerExecutor(Transformer transformer) {
            super();
            iTransformer = transformer;
        }

        /**
         * Call the transformer
         */
        public void execute(Object input) {
            try {
                iTransformer.transform(input);

            } catch (TransformerException ex) {
                throw new ExecutorException("TransformerExecutor: " + ex.getMessage(), ex);
            }
        }
    }

    // ChainedExecutor
    //----------------------------------------------------------------------------------

    /**
     * ChainedExecutor calls a list of executors.
     */
    private static class ChainedExecutor implements Executor, Serializable {
        /** The executors to call in turn */
        private final Executor[] iExecutors;

        /**
         * Constructor to store params
         */
        private ChainedExecutor(Executor[] executors) {
            super();
            iExecutors = executors;
        }

        /**
         * Execute a list of executors
         */
        public void execute(Object input) {
            for (int i = 0; i < iExecutors.length; i++) {
                iExecutors[i].execute(input);
            }
        }
    }

    // SwitchExecutor
    //----------------------------------------------------------------------------------

    /**
     * SwitchExecutor calls the executor whose predicate returns true.
     */
    private static class SwitchExecutor implements Executor, Serializable {
        /** The tests to consider */
        private final Predicate[] iPredicates;
        /** The matching executors to call */
        private final Executor[] iExecutors;
        /** The default executor to call if no tests match */
        private final Executor iDefault;

        /**
         * Constructor to store params
         */
        private SwitchExecutor(Predicate[] predicates, Executor[] executors, Executor defaultExecutor) {
            super();
            iPredicates = predicates;
            iExecutors = executors;
            iDefault = defaultExecutor;
        }

        /**
         * Execute the executor whose predicate returns true
         */
        public void execute(Object input) {
            for (int i = 0; i < iPredicates.length; i++) {
                if (iPredicates[i].evaluate(input) == true) {
                    iExecutors[i].execute(input);
                    return;
                }
            }
            iDefault.execute(input);
        }
    }

    // ForExecutor
    //----------------------------------------------------------------------------------

    /**
     * ForExecutor calls the executor a fixed nunmber of times.
     */
    private static class ForExecutor implements Executor, Serializable {
        /** The number of times to loop */
        private final int iCount;
        /** The executor to call */
        private final Executor iExecutor;

        /**
         * Constructor to store params
         */
        private ForExecutor(int count, Executor executor) {
            super();
            iCount = count;
            iExecutor = executor;
        }

        /**
         * Execute the executor count times
         */
        public void execute(Object input) {
            for (int i = 0; i < iCount; i++) {
                iExecutor.execute(input);
            }
        }
    }

    // WhileExecutor
    //----------------------------------------------------------------------------------

    /**
     * WhileExecutor calls the executor until the predicate is false.
     */
    private static class WhileExecutor implements Executor, Serializable {
        /** The test condition */
        private final Predicate iPredicate;
        /** The executor to call */
        private final Executor iExecutor;
        /** The flag, true is a do loop, false is a while */
        private final boolean iDoLoop;

        /**
         * Constructor to store params
         */
        private WhileExecutor(Predicate predicate, Executor executor, boolean doLoop) {
            super();
            iPredicate = predicate;
            iExecutor = executor;
            iDoLoop = doLoop;
        }

        /**
         * Execute the executor until the predicate is false
         */
        public void execute(Object input) {
            if (iDoLoop) {
                iExecutor.execute(input);
            }
            while (iPredicate.evaluate(input)) {
                iExecutor.execute(input);
            }
        }
    }

}

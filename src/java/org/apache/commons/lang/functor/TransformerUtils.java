/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002-2003 The Apache Software Foundation.  All rights
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
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
/**
 * <code>TransformerUtils</code> provides reference implementations and 
 * utilities for the Transformer functor interface. The supplied transformers are:
 * <ul>
 * <li>Invoker - returns the result of a method call on the input object
 * <li>Clone - returns a clone of the input object
 * <li>Constant - always returns the same object
 * <li>Executor - performs a Executor and returns the input object
 * <li>Predicate - returns the result of the predicate as a Boolean
 * <li>Factory - returns a new object from a factory
 * <li>Chained - chains two or more transformers together
 * <li>Switch - calls one transformer based on one or more predicates
 * <li>SwitchMap - calls one transformer looked up from a Map
 * <li>Instantiate - the Class input object is instantiated
 * <li>Map - returns an object from a supplied Map
 * <li>Null - always returns null
 * <li>NOP - returns the input object, which should be immutable
 * <li>Exception - always throws an exception
 * </ul>
 * All the supplied transformers are Serializable.
 *
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id: TransformerUtils.java,v 1.4 2003/03/23 17:50:02 scolebourne Exp $
 */
public class TransformerUtils {

    /**
     * A transformer that always throws an exception
     */
    private static final Transformer EXCEPTION_TRANSFORMER = new ExceptionTransformer();
    /**
     * A transformer that always returns null
     */
    private static final Transformer NULL_TRANSFORMER = new ConstantTransformer(null);
    /**
     * A transformer that returns the input object
     */
    private static final Transformer NOP_TRANSFORMER = new NOPTransformer();
    /**
     * A transformer that clones the input object
     */
    private static final Transformer CLONE_TRANSFORMER = new CloneTransformer();
    /**
     * A transformer that creates an object from a Class
     */
    private static final Transformer INSTANTIATE_TRANSFORMER = new InstantiateTransformer(null, null);

    /**
     * Restrictive constructor
     */
    protected TransformerUtils() {
        super();
    }

    /**
     * Gets a transformer that always throws an exception.
     * This could be useful during testing as a placeholder.
     * 
     * @return the transformer
     */
    public static Transformer exceptionTransformer() {
        return EXCEPTION_TRANSFORMER;
    }

    /**
     * Gets a transformer that always returns null.
     * 
     * @return the transformer
     */
    public static Transformer nullTransformer() {
        return NULL_TRANSFORMER;
    }

    /**
     * Gets a transformer that returns the input object.
     * The input object should be immutable to maintain the
     * contract of Transformer (although this is not checked).
     * 
     * @return the transformer
     */
    public static Transformer nopTransformer() {
        return NOP_TRANSFORMER;
    }

    /**
     * Gets a transformer that returns a clone of the input
     * object. The input object will be cloned using one of these
     * techniques (in order):
     * <ul>
     * <li>public clone method
     * <li>public copy constructor
     * <li>serialization clone
     * <ul>
     * 
     * @return the transformer
     */
    public static Transformer cloneTransformer() {
        return CLONE_TRANSFORMER;
    }

    /**
     * Creates a Transformer that will return the same object each time the 
     * transformer is used.
     *
     * @param constantToReturn  the constant object to return each time in the transformer
     * @return the transformer.
     */
    public static Transformer constantTransformer(Object constantToReturn) {
        return new ConstantTransformer(constantToReturn);
    }

    /**
     * Creates a Transformer that calls a Executor each time the transformer is used.
     * The transformer returns the input object.
     *
     * @param command  the command to run each time in the transformer
     * @return the transformer.
     */
    public static Transformer asTransformer(Executor executor) {
        if (executor == null) {
            throw new IllegalArgumentException("The executor must not be null");
        }
        return new ExecutorTransformer(executor);
    }

    /**
     * Creates a Transformer that calls a Predicate each time the transformer is used.
     * The transformer will return either Boolean.TRUE or Boolean.FALSE.
     *
     * @param predicate  the predicate to run each time in the transformer
     * @return the transformer.
     */
    public static Transformer asTransformer(Predicate predicate) {
        if (predicate == null) {
            throw new IllegalArgumentException("The predicate must not be null");
        }
        return new PredicateTransformer(predicate);
    }

    /**
     * Creates a Transformer that calls a Factory each time the transformer is used.
     * The transformer will return the value returned by the factory.
     *
     * @param factory  the factory to run each time in the transformer
     * @return the transformer.
     */
    public static Transformer asTransformer(Factory factory) {
        if (factory == null) {
            throw new IllegalArgumentException("The factory must not be null");
        }
        return new FactoryTransformer(factory);
    }

    /**
     * Create a new Transformer that calls two transformers, passing the result of
     * the first into the second.
     * 
     * @param transformer1  the first transformer
     * @param transformer2  the second transformer
     * @return the transformer
     * @throws IllegalArgumentException if either transformer is null
     */
    public static Transformer chainedTransformer(Transformer transformer1, Transformer transformer2) {
        Transformer[] trs = new Transformer[] {transformer1, transformer2};
        validate(trs);
        return new ChainedTransformer(trs);
    }

    /**
     * Create a new Transformer that calls each transformer in turn, passing the 
     * result into the next transformer.
     * 
     * @param transformers  an array of transformers to chain
     * @return the transformer
     * @throws IllegalArgumentException if the transformers array is null
     * @throws IllegalArgumentException if the transformers array has 0 elements
     * @throws IllegalArgumentException if any transformer in the array is null
     */
    public static Transformer chainedTransformer(Transformer[] transformers) {
        Transformer[] trs = copy(transformers);
        validate(trs);
        return new ChainedTransformer(trs);
    }

    /**
     * Create a new Transformer that calls each transformer in turn, passing the 
     * result into the next transformer. The ordering is that of the iterator()
     * method on the collection.
     * 
     * @param transformers  a collection of transformers to chain
     * @return the transformer
     * @throws IllegalArgumentException if the transformers collection is null
     * @throws IllegalArgumentException if the transformers collection is empty
     * @throws IllegalArgumentException if any transformer in the collection is null
     */
    public static Transformer chainedTransformer(Collection transformers) {
        Transformer[] trs = null;
        if (transformers == null) {
            throw new IllegalArgumentException("The transformer collection must not be null");
        }
        // convert to array like this to guarantee iterator() ordering
        trs = new Transformer[transformers.size()];
        int i = 0;
        for (Iterator it = transformers.iterator(); it.hasNext();) {
            trs[i++] = (Transformer) it.next();
        }
        validate(trs);
        return new ChainedTransformer(trs);
    }

    /**
     * Create a new Transformer that calls one of two transformers depending 
     * on the specified predicate.
     * 
     * @param predicate  the predicate to switch on
     * @param trueTransformer  the transformer called if the predicate is true
     * @param falseTransformer  the transformer called if the predicate is false
     * @return the transformer
     * @throws IllegalArgumentException if the predicate is null
     * @throws IllegalArgumentException if either transformer is null
     */
    public static Transformer switchTransformer(Predicate predicate, Transformer trueTransformer, Transformer falseTransformer) {
        return switchTransformerInternal(new Predicate[] { predicate }, new Transformer[] { trueTransformer }, falseTransformer);
    }

    /**
     * Create a new Transformer that calls one of the transformers depending 
     * on the predicates. The transformer at array location 0 is called if the
     * predicate at array location 0 returned true. Each predicate is evaluated
     * until one returns true. If no predicates evaluate to true, null is returned.
     * 
     * @param predicates  an array of predicates to check
     * @param transformers  an array of transformers to call
     * @return the transformer
     * @throws IllegalArgumentException if the either array is null
     * @throws IllegalArgumentException if the either array has 0 elements
     * @throws IllegalArgumentException if any element in the arrays is null
     * @throws IllegalArgumentException if the arrays are different sizes
     */
    public static Transformer switchTransformer(Predicate[] predicates, Transformer[] transformers) {
        return switchTransformerInternal(copy(predicates), copy(transformers), null);
    }

    /**
     * Create a new Transformer that calls one of the transformers depending 
     * on the predicates. The transformer at array location 0 is called if the
     * predicate at array location 0 returned true. Each predicate is evaluated
     * until one returns true. If no predicates evaluate to true, the default
     * transformer is called.
     * 
     * @param predicates  an array of predicates to check
     * @param transformers  an array of transformers to call
     * @param defaultTransformer  the default to call if no predicate matches
     * @return the transformer
     * @throws IllegalArgumentException if the either array is null
     * @throws IllegalArgumentException if the either array has 0 elements
     * @throws IllegalArgumentException if any element in the arrays is null
     * @throws IllegalArgumentException if the arrays are different sizes
     */
    public static Transformer switchTransformer(Predicate[] predicates, Transformer[] transformers, Transformer defaultTransformer) {
        return switchTransformerInternal(copy(predicates), copy(transformers), defaultTransformer);
    }

    /**
     * Create a new Transformer that calls one of the transformers depending 
     * on the predicates. 
     * <p>
     * The Map consists of Predicate keys and Transformer values. A transformer 
     * is called if its matching predicate returns true. Each predicate is evaluated
     * until one returns true. If no predicates evaluate to true, the default
     * transformer is called. The default transformer is set in the map with a 
     * null key. If no default transformer is set, null will be returned in a default
     * case. The ordering is that of the iterator() method on the entryset collection 
     * of the map.
     * 
     * @param predicatesAndTransformers  a map of predicates to transformers
     * @return the transformer
     * @throws IllegalArgumentException if the map is null
     * @throws IllegalArgumentException if the map is empty
     * @throws IllegalArgumentException if any transformer in the map is null
     * @throws ClassCastException  if the map elements are of the wrong type
     */
    public static Transformer switchTransformer(Map predicatesAndTransformers) {
        Transformer[] trs = null;
        Predicate[] preds = null;
        if (predicatesAndTransformers == null) {
            throw new IllegalArgumentException("The predicate and transformer map must not be null");
        }
        // convert to array like this to guarantee iterator() ordering
        Transformer def = (Transformer) predicatesAndTransformers.remove(null);
        int size = predicatesAndTransformers.size();
        trs = new Transformer[size];
        preds = new Predicate[size];
        int i = 0;
        for (Iterator it = predicatesAndTransformers.entrySet().iterator(); it.hasNext();) {
            Map.Entry entry = (Map.Entry) it.next();
            preds[i] = (Predicate) entry.getKey();
            trs[i] = (Transformer) entry.getValue();
            i++;
        }
        return switchTransformerInternal(preds, trs, def);
    }

    /**
     * Validate input and create transformer
     */
    private static Transformer switchTransformerInternal(Predicate[] predicates, Transformer[] transformers, Transformer defaultTransformer) {
        validate(predicates);
        validate(transformers);
        if (predicates.length != transformers.length) {
            throw new IllegalArgumentException("The predicate and transformer arrays must be the same size");
        }
        if (defaultTransformer == null) {
            defaultTransformer = nullTransformer();
        }
        return new SwitchTransformer(predicates, transformers, defaultTransformer);
    }
    
    /**
     * Create a new Transformer that uses the input object as a key to find the
     * transformer to call. 
     * <p>
     * The Map consists of object keys and Transformer values. A transformer 
     * is called if the input object equals the key. If there is no match, the
     * default transformer is called. The default transformer is set in the map
     * using a null key. If no default is set, null will be returned in a default case.
     * 
     * @param objectsAndTransformers  a map of objects to transformers
     * @return the transformer
     * @throws IllegalArgumentException if the map is null
     * @throws IllegalArgumentException if the map is empty
     * @throws IllegalArgumentException if any transformer in the map is null
     */
    public static Transformer switchMapTransformer(Map objectsAndTransformers) {
        Transformer[] trs = null;
        Predicate[] preds = null;
        if (objectsAndTransformers == null) {
            throw new IllegalArgumentException("The obejct and transformer map must not be null");
        }
        Transformer def = (Transformer) objectsAndTransformers.remove(null);
        int size = objectsAndTransformers.size();
        trs = new Transformer[size];
        preds = new Predicate[size];
        int i = 0;
        for (Iterator it = objectsAndTransformers.entrySet().iterator(); it.hasNext();) {
            Map.Entry entry = (Map.Entry) it.next();
            preds[i] = PredicateUtils.equalPredicate(entry.getKey());
            trs[i] = (Transformer) entry.getValue();
            i++;
        }
        return switchTransformer(preds, trs, def);
    }

    /**
     * Gets a Transformer that expects an input Class object that it will instantiate.
     * 
     * @return the transformer
     */
    public static Transformer instantiateTransformer() {
        return INSTANTIATE_TRANSFORMER;
    }

    /** 
     * Creates a Transformer that expects an input Class object that it will 
     * instantiate. The constructor used is determined by the arguments specified
     * to this method.
     *
     * @param paramTypes  parameter types for the constructor, can be null
     * @param args  the arguments to pass to the constructor, can be null
     * @return the transformer
     * @throws IllegalArgumentException if the paramTypes and args don't match
     */
    public static Transformer instantiateTransformer(Class[] paramTypes, Object[] args) {
        return new InstantiateTransformer(paramTypes, args);
    }

    /** 
     * Creates a Transformer that uses the passed in Map to transform the input 
     * object (as a simple lookup).
     *
     * @param map  the map to use to transform the objects
     * @return the transformer
     * @throws IllegalArgumentException if the map is null
     */
    public static Transformer mapTransformer(Map map) {
        if (map == null) {
            throw new IllegalArgumentException("The map must not be null");
        }
        return new MapTransformer(map);
    }

    /**
     * Gets a Transformer that invokes a method on the input object.
     * The method must have no parameters. If the input object is null, 
     * null is returned.
     * <p>
     * For example, <code>TransformerUtils.invokerTransformer("getName");</code>
     * will call the <code>getName/code> method on the input object to 
     * determine the transformer result.
     * 
     * @param methodName  the method name to call on the input object, may not be null
     * @return the transformer
     * @throws IllegalArgumentException if the methodName is null.
     */
    public static Transformer invokerTransformer(String methodName){
        return new InvokerTransformer(methodName, null, null);
    }

    /**
     * Gets a Transformer that invokes a method on the input object.
     * The method parameters are specified. If the input object is null, 
     * null is returned.
     * 
     * @param methodName  the name of the method
     * @param paramTypes  the parameter types
     * @param args  the arguments
     * @return the transformer
     * @throws IllegalArgumentException if the method name is null
     * @throws IllegalArgumentException if the paramTypes and args don't match
     */
    public static Transformer invokerTransformer(String methodName, Class[] paramTypes, Object[] args){
        return new InvokerTransformer(methodName, paramTypes, args);
    }

    /**
     * Copy method
     * 
     * @param predicates  the predicates to copy
     */
    private static Predicate[] copy(Predicate[] predicates) {
        if (predicates == null) {
            return null;
        }
        return (Predicate[]) predicates.clone();
    }
    
    /**
     * Validate method
     * 
     * @param predicates  the predicates to validate
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
                throw new IllegalArgumentException(
                    "The predicate array must not contain a null predicate, index " + i + " was null");
            }
        }
    }

    /**
     * Copy method
     * 
     * @param transformers  the transformers to copy
     */
    private static Transformer[] copy(Transformer[] transformers) {
        if (transformers == null) {
            return null;
        }
        return (Transformer[]) transformers.clone();
    }
    
    /**
     * Validate method
     * 
     * @param transformers  the transformers to validate
     */
    private static void validate(Transformer[] transformers) {
        if (transformers == null) {
            throw new IllegalArgumentException("The transformer array must not be null");
        }
        if (transformers.length < 1) {
            throw new IllegalArgumentException(
                "At least 1 transformer must be specified in the transformer array, size was " + transformers.length);
        }
        for (int i = 0; i < transformers.length; i++) {
            if (transformers[i] == null) {
                throw new IllegalArgumentException(
                    "The transformer array must not contain a null transformer, index " + i + " was null");
            }
        }
    }

    // ExceptionTransformer
    //----------------------------------------------------------------------------------

    /**
     * ExceptionTransformer always throws an exception.
     */
    private static class ExceptionTransformer implements Transformer, Serializable {

        /**
         * Constructor
         */
        private ExceptionTransformer() {
            super();
        }

        /**
         * Always throw exception
         */
        public Object transform(Object input) {
            throw new TransformerException("ExceptionTransformer invoked");
        }
    }

    // NOPTransformer
    //----------------------------------------------------------------------------------

    /**
     * NOPTransformer returns the input object.
     */
    private static class NOPTransformer implements Transformer, Serializable {

        /**
         * Constructor
         */
        private NOPTransformer() {
            super();
        }

        /**
         * Return the input object
         */
        public Object transform(Object input) {
            return input;
        }
    }

    // CloneTransformer
    //----------------------------------------------------------------------------------

    /**
     * CloneTransformer returns a clone of the input object.
     */
    private static class CloneTransformer implements Transformer, Serializable {

        /**
         * Constructor
         */
        private CloneTransformer() {
            super();
        }

        /**
         * Returns a clone of the input object
         */
        public Object transform(Object input) {
            if (input == null) {
                return null;
            }
            try {
                return FactoryUtils.prototypeFactory(input).create();

            } catch (IllegalArgumentException ex) {
                throw new TransformerException("CloneTransformer", ex);
            } catch (FactoryException ex) {
                throw new TransformerException("CloneTransformer", ex);
            }
        }
    }

    // ConstantTransformer
    //----------------------------------------------------------------------------------

    /**
     * ConstantTransformer returns the same instance each time.
     */
    private static class ConstantTransformer implements Transformer, Serializable {
        /** The constant to return each time */
        private final Object iConstant;

        /**
         * Constructor to store constant.
         */
        private ConstantTransformer(Object constant) {
            super();
            iConstant = constant;
        }

        /**
         * Always return constant.
         */
        public Object transform(Object input) {
            return iConstant;
        }
    }

    // ExecutorTransformer
    //----------------------------------------------------------------------------------

    /**
     * ExecutorTransformer executes a Executor object.
     */
    private static class ExecutorTransformer implements Transformer, Serializable {
        /** The executor to call each time */
        private final Executor iExecutor;

        /**
         * Constructor to store executor.
         */
        private ExecutorTransformer(Executor executor) {
            super();
            iExecutor = executor;
        }

        /**
         * Exceute the executor and return the input.
         */
        public Object transform(Object input) {
            try {
                iExecutor.execute(input);
                return input;

            } catch (ExecutorException ex) {
                throw new TransformerException("ExecutorTransformer: " + ex.getMessage(), ex);
            }
        }
    }

    // PredicateTransformer
    //----------------------------------------------------------------------------------

    /**
     * PredicateTransformer evaluates a Predicate object.
     */
    private static class PredicateTransformer implements Transformer, Serializable {
        /** The predicate to call each time */
        private final Predicate iPredicate;

        /**
         * Constructor to store predicate.
         */
        private PredicateTransformer(Predicate predicate) {
            super();
            iPredicate = predicate;
        }

        /**
         * Evaluate the predicate and return the result as a Boolean.
         */
        public Object transform(Object input) {
            try {
                return new Boolean(iPredicate.evaluate(input));

            } catch (PredicateException ex) {
                throw new TransformerException("PredicateTransformer: " + ex.getMessage(), ex);
            }
        }
    }

    // FactoryTransformer
    //----------------------------------------------------------------------------------

    /**
     * FactoryTransformer returns the result of calling a Factory.
     */
    private static class FactoryTransformer implements Transformer, Serializable {
        /** The factory to call each time */
        private final Factory iFactory;

        /**
         * Constructor to store factory.
         */
        private FactoryTransformer(Factory factory) {
            super();
            iFactory = factory;
        }

        /**
         * Return the result of calling the factory.
         */
        public Object transform(Object input) {
            try {
                return iFactory.create();

            } catch (FactoryException ex) {
                throw new TransformerException("FactoryTransformer: " + ex.getMessage(), ex);
            }
        }
    }

    // ChainedTransformer
    //----------------------------------------------------------------------------------

    /**
     * ChainedTransformer returns the result of calling a list of transformers.
     */
    private static class ChainedTransformer implements Transformer, Serializable {
        /** The array of transformers to call */
        private final Transformer[] iTransformers;

        /**
         * Constructor to store params.
         */
        private ChainedTransformer(Transformer[] transformers) {
            super();
            iTransformers = transformers;
        }

        /**
         * Returns the result of calling a list of transformers.
         */
        public Object transform(Object object) {
            for (int i = 0; i < iTransformers.length; i++) {
                object = iTransformers[i].transform(object);
            }
            return object;
        }
    }

    // SwitchTransformer
    //----------------------------------------------------------------------------------

    /**
     * SwitchTransformer returns the result of the transformer whose predicate returns true.
     */
    private static class SwitchTransformer implements Transformer, Serializable {
        /** The array of predicates to switch on */
        private final Predicate[] iPredicates;
        /** The array of transformers to call */
        private final Transformer[] iTransformers;
        /** The default transformer called if no predicate matches */
        private final Transformer iDefault;

        /**
         * Constructor to store params.
         */
        private SwitchTransformer(Predicate[] predicates, Transformer[] transformers, Transformer defaultTransformer) {
            super();
            iPredicates = predicates;
            iTransformers = transformers;
            iDefault = defaultTransformer;
        }

        /**
         * Returns the result of the transformer whose predicate returns true.
         */
        public Object transform(Object input) {
            for (int i = 0; i < iPredicates.length; i++) {
                if (iPredicates[i].evaluate(input) == true) {
                    return iTransformers[i].transform(input);
                }
            }
            return iDefault.transform(input);
        }
    }

    // InstantiateTransformer
    //----------------------------------------------------------------------------------

    /**
     * InstantiateTransformer returns the result of instantiating the input Class object.
     */
    private static class InstantiateTransformer implements Transformer, Serializable {
        /** The array of reflection parameter types */
        private final Class[] iParamTypes;
        /** The array of reflection arguments */
        private final Object[] iArgs;

        /**
         * Constructor to store params.
         */
        private InstantiateTransformer(Class[] paramTypes, Object[] args) {
            super();
            if (((paramTypes == null) && (args != null))
                || ((paramTypes != null) && (args == null))
                || ((paramTypes != null) && (args != null) && (paramTypes.length != args.length))) {
                throw new IllegalArgumentException("InstantiateTransformer: The parameter types must match the arguments");
            }
            if ((paramTypes == null) && (args == null)) {
                iParamTypes = null;
                iArgs = null;
            } else {
                iParamTypes = (Class[]) paramTypes.clone();
                iArgs = (Object[]) args.clone();
            }
        }

        /**
         * Return the result of instantiating the input Class object.
         */
        public Object transform(Object input) {
            try {
                if (input instanceof Class == false) {
                    throw new TransformerException(
                        "InstantiateTransformer: Input object was not an instanceof Class, it was a "
                            + (input == null ? "null object" : input.getClass().getName()));
                }
                return FactoryUtils.reflectionFactory((Class) input, iParamTypes, iArgs).create();

            } catch (IllegalArgumentException ex) {
                throw new TransformerException("InstantiateTransformer", ex);
            } catch (FactoryException ex) {
                throw new TransformerException("InstantiateTransformer", ex);
            }
        }
    }

    // MapTransformer
    //----------------------------------------------------------------------------------

    /**
     * MapTransformer returns the result by looking up in the map.
     */
    private static class MapTransformer implements Transformer, Serializable {
        /** The map of data to lookup in */
        private final Map iMap;

        /**
         * Constructor to store map.
         */
        private MapTransformer(Map map) {
            super();
            iMap = map;
        }

        /**
         * Returns the result by looking up in the map.
         */
        public Object transform(Object input) {
            return iMap.get(input);
        }
    }

    // InvokerTransformer
    //----------------------------------------------------------------------------------

    /**
     * InvokerTransformer returns the result of invoking the specified method on 
     * the input object.
     */
    private static class InvokerTransformer implements Transformer, Serializable {
        /** The method name to call */
        private final String iMethodName;
        /** The array of reflection parameter types */
        private final Class[] iParamTypes;
        /** The array of reflection arguments */
        private final Object[] iArgs;

        /**
         * Constructor.
         */
        public InvokerTransformer(String methodName, Class[] paramTypes, Object[] args) {
            super();
            if (methodName == null) {
                throw new IllegalArgumentException("InvokerTransformer: The method to invoke must not be null");
            }
            if (((paramTypes == null) && (args != null))
                || ((paramTypes != null) && (args == null))
                || ((paramTypes != null) && (args != null) && (paramTypes.length != args.length))) {
                throw new IllegalArgumentException("InvokerTransformer: The parameter types must match the arguments");
            }

            iMethodName = methodName;
            if ((paramTypes == null) && (args == null)) {
                iParamTypes = null;
                iArgs = null;
            } else {
                iParamTypes = (Class[]) paramTypes.clone();
                iArgs = (Object[]) args.clone();
            }
        }

        /**
         * Invoke the specified method on the input object.
         */
        public Object transform(Object input) {
            if (input == null) {
                return null;
            }
            try {
                Class cls = input.getClass();
                Method method = cls.getMethod(iMethodName, iParamTypes);
                return method.invoke(input, iArgs);
                
            } catch (NoSuchMethodException ex) {
                throw new TransformerException("InvokerTransformer: The method '" + iMethodName + "' on '" + input.getClass() + "' does not exist");
            } catch (IllegalAccessException ex) {
                throw new TransformerException("InvokerTransformer: The method '" + iMethodName + "' on '" + input.getClass() + "' cannot be accessed");
            } catch (InvocationTargetException ex) {
                throw new TransformerException("InvokerTransformer: The method '" + iMethodName + "' on '" + input.getClass() + "' threw an exception", ex);
            }
        }
    }

}

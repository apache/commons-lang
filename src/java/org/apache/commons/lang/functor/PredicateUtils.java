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
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.apache.commons.lang.functor.Transformer;
import org.apache.commons.lang.functor.TransformerException;
import org.apache.commons.lang.functor.TransformerUtils;
/**
 * <code>PredicateUtils</code> provides reference implementations and utilities
 * for the Predicate functor interface. The supplied predicates are:
 * <ul>
 * <li>Invoker - returns the result of a method call on the input object
 * <li>InstanceOf - true if the object is an instanceof a class
 * <li>Equal - true if the object equals() a specified object
 * <li>Identity - true if the object == a specified object
 * <li>Null - true if the object is null
 * <li>NotNull - true if the object is not null
 * <li>Unique - true if the object has not already been evaluated
 * <li>And/All - true if all of the predicates are true
 * <li>Or/Any - true if any of the predicates is true
 * <li>Either/One - true if only one of the predicate is true
 * <li>Neither/None - true if none of the predicates are true
 * <li>Not - true if the predicate is false, and vice versa
 * <li>Transformer - wraps a Transformer as a Predicate
 * <li>True - always return true
 * <li>False - always return false
 * <li>Exception - always throws an exception
 * <li>NullIsException/NullIsFalse/NullIsTrue - check for null input
 * </ul>
 * All the supplied predicates are Serializable.
 *
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @author Ola Berg
 * @version $Id: PredicateUtils.java,v 1.3 2003/03/23 17:50:02 scolebourne Exp $
 */
public class PredicateUtils {

    /**
     * A predicate that always throws an exception
     */
    private static final Predicate EXCEPTION_PREDICATE = new ExceptionPredicate();
    /**
     * A predicate that always returns true
     */
    private static final Predicate TRUE_PREDICATE = new ConstantPredicate(true);
    /**
     * A predicate that always returns false
     */
    private static final Predicate FALSE_PREDICATE = new ConstantPredicate(false);
    /**
     * A predicate that returns true if the object is null
     */
    private static final Predicate NULL_PREDICATE = new IdentityPredicate(null);
    /**
     * A predicate that returns true if the object is not null
     */
    private static final Predicate NOT_NULL_PREDICATE = new NotPredicate(NULL_PREDICATE);

    /**
     * Restrictive constructor
     */
    protected PredicateUtils() {
        super();
    }

    // Simple predicates
    //-----------------------------------------------------------------------------

    /** 
     * Gets a Predicate that always throws an exception.
     * This could be useful during testing as a placeholder.
     *
     * @return the predicate
     */
    public static Predicate exceptionPredicate() {
        return EXCEPTION_PREDICATE;
    }

    /**
     * Gets a Predicate that always returns true.
     * 
     * @return the predicate
     */
    public static Predicate truePredicate() {
        return TRUE_PREDICATE;
    }

    /**
     * Gets a Predicate that always returns false.
     * 
     * @return the predicate
     */
    public static Predicate falsePredicate() {
        return FALSE_PREDICATE;
    }

    /**
     * Gets a Predicate that checks if the input object passed in is null.
     * 
     * @return the predicate
     */
    public static Predicate nullPredicate() {
        return NULL_PREDICATE;
    }

    /**
     * Gets a Predicate that checks if the input object passed in is not null.
     * 
     * @return the predicate
     */
    public static Predicate notNullPredicate() {
        return NOT_NULL_PREDICATE;
    }

    /**
     * Creates a Predicate that checks if the input object is equal to the
     * specified object using equals().
     * 
     * @param value  the value to compare against
     * @return the predicate
     */
    public static Predicate equalPredicate(Object value) {
        if (value == null) {
            return NULL_PREDICATE;
        }
        return new EqualPredicate(value);
    }

    /**
     * Creates a Predicate that checks if the input object is equal to the
     * specified object by identity.
     * 
     * @param value  the value to compare against
     * @return the predicate
     */
    public static Predicate identityPredicate(Object value) {
        if (value == null) {
            return NULL_PREDICATE;
        }
        return new IdentityPredicate(value);
    }
    
    /**
     * Creates a Predicate that checks if the object passed in is of
     * a particular type, using instanceof. A <code>null</code> input
     * object will return <code>false</code>.
     * 
     * @param type  the type to check for, may not be null
     * @return the predicate
     * @throws IllegalArgumentException if the class is null
     */
    public static Predicate instanceofPredicate(Class type) {
        if (type == null) {
            throw new IllegalArgumentException("The type to check instanceof must not be null");
        }
        return new InstanceofPredicate(type);
    }

    /**
     * Creates a Predicate that returns true the first time an object is
     * encoutered, and false if the same object is received 
     * again. The comparison is by equals(). A <code>null</code> input object
     * is accepted and will return true the first time, and false subsequently
     * as well.
     * 
     * @return the predicate
     */
    public static Predicate uniquePredicate() {
        // must return new instance each time
        return new UniquePredicate();
    }

    /**
     * Creates a Predicate that invokes a method on the input object.
     * The method must return either a boolean or a non-null Boolean,
     * and have no parameters. If the input object is null, a 
     * PredicateException is thrown.
     * <p>
     * For example, <code>PredicateUtils.invokerPredicate("isEmpty");</code>
     * will call the <code>isEmpty</code> method on the input object to 
     * determine the predicate result.
     * 
     * @param methodName  the method name to call on the input object, may not be null
     * @return the predicate
     * @throws IllegalArgumentException if the methodName is null.
     */
    public static Predicate invokerPredicate(String methodName){
        // reuse transformer as it has caching - this is lazy really, should have inner class here
        return asPredicate(TransformerUtils.invokerTransformer(methodName));
    }

    /**
     * Creates a Predicate that invokes a method on the input object.
     * The method must return either a boolean or a non-null Boolean,
     * and have no parameters. If the input object is null, a 
     * PredicateException is thrown.
     * <p>
     * For example, <code>PredicateUtils.invokerPredicate("isEmpty");</code>
     * will call the <code>isEmpty</code> method on the input object to 
     * determine the predicate result.
     * 
     * @param methodName  the method name to call on the input object, may not be null
     * @param paramTypes  the parameter types
     * @param args  the arguments
     * @return the predicate
     * @throws IllegalArgumentException if the method name is null
     * @throws IllegalArgumentException if the paramTypes and args don't match
     */
    public static Predicate invokerPredicate(String methodName, Class[] paramTypes, Object[] args){
        // reuse transformer as it has caching - this is lazy really, should have inner class here
        return asPredicate(TransformerUtils.invokerTransformer(methodName, paramTypes, args));
    }

    // Boolean combinations
    //-----------------------------------------------------------------------------

    /**
     * Create a new Predicate that returns true only if both of the specified
     * predicates are true.
     * 
     * @param predicate1  the first predicate, may not be null
     * @param predicate2  the second predicate, may not be null
     * @return the <code>and</code> predicate
     * @throws IllegalArgumentException if either predicate is null
     */
    public static Predicate andPredicate(Predicate predicate1, Predicate predicate2) {
        return allPredicate(new Predicate[] { predicate1, predicate2 });
    }

    /**
     * Create a new Predicate that returns true only if all of the specified
     * predicates are true.
     * 
     * @param predicates  an array of predicates to check, may not be null
     * @return the <code>all</code> predicate
     * @throws IllegalArgumentException if the predicates array is null
     * @throws IllegalArgumentException if the predicates array has less than 2 elements
     * @throws IllegalArgumentException if any predicate in the array is null
     */
    public static Predicate allPredicate(Predicate[] predicates) {
        return new AllPredicate(validate(predicates));
    }

    /**
     * Create a new Predicate that returns true only if all of the specified
     * predicates are true. The predicates are checked in iterator order.
     * 
     * @param predicates  a collection of predicates to check, may not be null
     * @return the <code>all</code> predicate
     * @throws IllegalArgumentException if the predicates collection is null
     * @throws IllegalArgumentException if the predicates collection has less than 2 elements
     * @throws IllegalArgumentException if any predicate in the collection is null
     */
    public static Predicate allPredicate(Collection predicates) {
        return new AllPredicate(validate(predicates));
    }

    /**
     * Create a new Predicate that returns true if either of the specified
     * predicates are true.
     * 
     * @param predicate1  the first predicate, may not be null
     * @param predicate2  the second predicate, may not be null
     * @return the <code>or</code> predicate
     * @throws IllegalArgumentException if either predicate is null
     */
    public static Predicate orPredicate(Predicate predicate1, Predicate predicate2) {
        return anyPredicate(new Predicate[] { predicate1, predicate2 });
    }

    /**
     * Create a new Predicate that returns true if any of the specified
     * predicates are true.
     * 
     * @param predicates  an array of predicates to check, may not be null
     * @return the <code>any</code> predicate
     * @throws IllegalArgumentException if the predicates array is null
     * @throws IllegalArgumentException if the predicates array has less than 2 elements
     * @throws IllegalArgumentException if any predicate in the array is null
     */
    public static Predicate anyPredicate(Predicate[] predicates) {
        return new AnyPredicate(validate(predicates));
    }

    /**
     * Create a new Predicate that returns true if any of the specified
     * predicates are true. The predicates are checked in iterator order.
     * 
     * @param predicates  a collection of predicates to check, may not be null
     * @return the <code>any</code> predicate
     * @throws IllegalArgumentException if the predicates collection is null
     * @throws IllegalArgumentException if the predicates collection has less than 2 elements
     * @throws IllegalArgumentException if any predicate in the collection is null
     */
    public static Predicate anyPredicate(Collection predicates) {
        return new AnyPredicate(validate(predicates));
    }

    /**
     * Create a new Predicate that returns true if one, but not both, of the
     * specified predicates are true.
     * 
     * @param predicate1  the first predicate, may not be null
     * @param predicate2  the second predicate, may not be null
     * @return the <code>either</code> predicate
     * @throws IllegalArgumentException if either predicate is null
     */
    public static Predicate eitherPredicate(Predicate predicate1, Predicate predicate2) {
        return onePredicate(new Predicate[] { predicate1, predicate2 });
    }

    /**
     * Create a new Predicate that returns true if only one of the specified
     * predicates are true.
     * 
     * @param predicates  an array of predicates to check, may not be null
     * @return the <code>one</code> predicate
     * @throws IllegalArgumentException if the predicates array is null
     * @throws IllegalArgumentException if the predicates array has less than 2 elements
     * @throws IllegalArgumentException if any predicate in the array is null
     */
    public static Predicate onePredicate(Predicate[] predicates) {
        return new OnePredicate(validate(predicates));
    }

    /**
     * Create a new Predicate that returns true if only one of the specified
     * predicates are true. The predicates are checked in iterator order.
     * 
     * @param predicates  a collection of predicates to check, may not be null
     * @return the <code>one</code> predicate
     * @throws IllegalArgumentException if the predicates collection is null
     * @throws IllegalArgumentException if the predicates collection has less than 2 elements
     * @throws IllegalArgumentException if any predicate in the collection is null
     */
    public static Predicate onePredicate(Collection predicates) {
        return new OnePredicate(validate(predicates));
    }

    /**
     * Create a new Predicate that returns true if neither of the specified 
     * predicates are true.
     * 
     * @param predicate1  the first predicate, may not be null
     * @param predicate2  the second predicate, may not be null
     * @return the <code>neither</code> predicate
     * @throws IllegalArgumentException if either predicate is null
     */
    public static Predicate neitherPredicate(Predicate predicate1, Predicate predicate2) {
        return nonePredicate(new Predicate[] { predicate1, predicate2 });
    }

    /**
     * Create a new Predicate that returns true if none of the specified
     * predicates are true.
     * 
     * @param predicates  an array of predicates to check, may not be null
     * @return the <code>none</code> predicate
     * @throws IllegalArgumentException if the predicates array is null
     * @throws IllegalArgumentException if the predicates array has less than 2 elements
     * @throws IllegalArgumentException if any predicate in the array is null
     */
    public static Predicate nonePredicate(Predicate[] predicates) {
        Predicate[] preds = validate(predicates);
        for (int i = 0; i < preds.length; i++) {
            preds[i] = notPredicate(preds[i]);
        }
        return new AllPredicate(preds);
    }

    /**
     * Create a new Predicate that returns true if none of the specified
     * predicates are true. The predicates are checked in iterator order.
     * 
     * @param predicates  a collection of predicates to check, may not be null
     * @return the <code>none</code> predicate
     * @throws IllegalArgumentException if the predicates collection is null
     * @throws IllegalArgumentException if the predicates collection has less than 2 elements
     * @throws IllegalArgumentException if any predicate in the collection is null
     */
    public static Predicate nonePredicate(Collection predicates) {
        Predicate[] preds = validate(predicates);
        for (int i = 0; i < preds.length; i++) {
            preds[i] = notPredicate(preds[i]);
        }
        return new AllPredicate(preds);
    }

    /**
     * Create a new Predicate that returns true if the specified predicate
     * returns false and vice versa.
     * 
     * @param predicate  the predicate to not
     * @return the <code>not</code> predicate
     * @throws IllegalArgumentException if the predicate is null
     */
    public static Predicate notPredicate(Predicate predicate) {
        if (predicate == null) {
            throw new IllegalArgumentException("The predicate must not be null");
        }
        return new NotPredicate(predicate);
    }

    // Adaptors
    //-----------------------------------------------------------------------------

    /**
     * Create a new Predicate that wraps a Transformer. The Transformer must
     * return either Boolean.TRUE or Boolean.FALSE otherwise a PredicateException
     * will be thrown.
     * 
     * @param transformer  the transformer to wrap, may not be null
     * @return the transformer wrapping predicate
     * @throws IllegalArgumentException if the transformer is null
     */
    public static Predicate asPredicate(Transformer transformer) {
        if (transformer == null) {
            throw new IllegalArgumentException("The transformer to call must not be null");
        }
        return new TransformerPredicate(transformer);
    }

    // Null handlers
    //-----------------------------------------------------------------------------

    /**
     * Gets a Predicate that throws an exception if the input object is null, 
     * otherwise it calls the specified Predicate. This allows null handling 
     * behaviour to be added to Predicates that don't support nulls.
     * 
     * @param predicate  the predicate to wrap, may not be null
     * @return the predicate
     * @throws IllegalArgumentException if the predicate is null.
     */
    public static Predicate nullIsExceptionPredicate(Predicate predicate){
        if (predicate == null) {
            throw new IllegalArgumentException("The predicate must not be null");
        }
        return new NullIsExceptionPredicate( predicate);
    }

    /**
     * Gets a Predicate that returns false if the input object is null, otherwise
     * it calls the specified Predicate. This allows null handling behaviour to
     * be added to Predicates that don't support nulls.
     * 
     * @param predicate  the predicate to wrap, may not be null
     * @return the predicate
     * @throws IllegalArgumentException if the predicate is null.
     */
    public static Predicate nullIsFalsePredicate(Predicate predicate){
        if (predicate == null) {
            throw new IllegalArgumentException("The predicate must not be null");
        }
        return new NullIsFalsePredicate(predicate);
    }

    /**
     * Gets a Predicate that returns true if the input object is null, otherwise
     * it calls the specified Predicate. This allows null handling behaviour to
     * be added to Predicates that don't support nulls.
     * 
     * @param predicate  the predicate to wrap, may not be null
     * @return the predicate
     * @throws IllegalArgumentException if the predicate is null.
     */
    public static Predicate nullIsTruePredicate(Predicate predicate){
        if (predicate == null) {
            throw new IllegalArgumentException("The predicate must not be null");
        }
        return new NullIsTruePredicate(predicate);
    }

    /**
     * Convert a collection to an array using the iterator.
     * 
     * @param predicates  the predicates to validate
     * @return predicate array
     */
    private static Predicate[] validate(Collection predicates) {
        if (predicates == null) {
            throw new IllegalArgumentException("The predicate collection must not be null");
        }
        if (predicates.size() < 2) {
            throw new IllegalArgumentException(
                "At least 2 predicates must be specified in the predicate collection, size was " + predicates.size());
        }
        // convert to array like this to guarantee iterator() ordering
        Predicate[] preds = new Predicate[predicates.size()];
        int i = 0;
        for (Iterator it = predicates.iterator(); it.hasNext();) {
            preds[i] = (Predicate) it.next();
            if (preds[i] == null) {
                throw new IllegalArgumentException("The predicate collection must not contain a null predicate, index " + i + " was null");
            }
            i++;
        }
        return preds;
    }
    
    /**
     * Validate method shared amongst predicate implementations.
     * 
     * @param predicates  the predicates to validate
     * @return predicate array (copy)
     */
    private static Predicate[] validate(Predicate[] predicates) {
        if (predicates == null) {
            throw new IllegalArgumentException("The predicate array must not be null");
        }
        if (predicates.length < 2) {
            throw new IllegalArgumentException(
                "At least 2 predicates must be specified in the predicate array, size was " + predicates.length);
        }
        Predicate[] preds = new Predicate[predicates.length];
        for (int i = 0; i < predicates.length; i++) {
            if (predicates[i] == null) {
                throw new IllegalArgumentException("The predicate array must not contain a null predicate, index " + i + " was null");
            }
            preds[i] = predicates[i];
        }
        return preds;
    }

    // ExceptionPredicate
    //----------------------------------------------------------------------------------

    /**
     * ExceptionPredicate always throws an exception
     */
    private static class ExceptionPredicate implements Predicate, Serializable {

        /**
         * Constructor
         */
        private ExceptionPredicate() {
            super();
        }

        /**
         * Always throw an exception
         */
        public boolean evaluate(Object object) {
            throw new PredicateException("ExceptionPredicate invoked");
        }
    }

    // ConstantPredicate
    //----------------------------------------------------------------------------------

    /**
     * ConstantPredicate returns the same instance each time.
     */
    private static class ConstantPredicate implements Predicate, Serializable {
        /** The constant value to return each time */
        private final boolean iConstant;

        /**
         * Constructor to store constant
         */
        private ConstantPredicate(boolean constant) {
            super();
            iConstant = constant;
        }

        /**
         * Always return constant
         */
        public boolean evaluate(Object object) {
            return iConstant;
        }
    }

    // AllPredicate
    //----------------------------------------------------------------------------------

    /**
     * AllPredicate returns true if all predicates return true
     */
    private static class AllPredicate implements Predicate, Serializable {
        /** The array of predicates to call */
        private final Predicate[] iPredicates;

        /**
         * Constructor
         */
        private AllPredicate(Predicate[] predicates) {
            super();
            iPredicates = predicates;
        }

        /**
         * Return true if all predicates return true
         */
        public boolean evaluate(Object object) {
            for (int i = 0; i < iPredicates.length; i++) {
                if (iPredicates[i].evaluate(object) == false) {
                    return false;
                }
            }
            return true;
        }
    }

    // AnyPredicate
    //----------------------------------------------------------------------------------

    /**
     * AnyPredicate returns true if one of the predicates return true
     */
    private static class AnyPredicate implements Predicate, Serializable {
        /** The array of predicates to call */
        private final Predicate[] iPredicates;

        /**
         * Constructor
         */
        private AnyPredicate(Predicate[] predicates) {
            super();
            iPredicates = predicates;
        }

        /**
         * Return true if one of the predicates returns true
         */
        public boolean evaluate(Object object) {
            for (int i = 0; i < iPredicates.length; i++) {
                if (iPredicates[i].evaluate(object)) {
                    return true;
                }
            }
            return false;
        }
    }

    // OnePredicate
    //----------------------------------------------------------------------------------

    /**
     * OnePredicate returns true if only one of the predicates return true
     */
    private static class OnePredicate implements Predicate, Serializable {
        /** The array of predicates to call */
        private final Predicate[] iPredicates;

        /**
         * Constructor
         */
        private OnePredicate(Predicate[] predicates) {
            super();
            iPredicates = predicates;
        }

        /**
         * Return true if only one of the predicates returns true
         */
        public boolean evaluate(Object object) {
            boolean match = false;
            for (int i = 0; i < iPredicates.length; i++) {
                if (iPredicates[i].evaluate(object)) {
                    if (match) {
                        return false;
                    }
                    match = true;
                }
            }
            return match;
        }
    }

    // NotPredicate
    //----------------------------------------------------------------------------------

    /**
     * NotPredicate returns the opposite of the wrapped predicate
     */
    private static class NotPredicate implements Predicate, Serializable {
        /** The predicate to call */
        private final Predicate iPredicate;

        /**
         * Constructor
         */
        private NotPredicate(Predicate predicate) {
            super();
            iPredicate = predicate;
        }

        /**
         * Return true if the wrapped predicate returns false, and vice versa
         */
        public boolean evaluate(Object object) {
            return !iPredicate.evaluate(object);
        }
    }

    // InstanceofPredicate
    //----------------------------------------------------------------------------------

    /**
     * InstanceofPredicate checks the type of an object
     */
    private static class InstanceofPredicate implements Predicate, Serializable {
        /** The type to check for */
        private final Class iType;

        /**
         * Constructor
         */
        public InstanceofPredicate(Class type) {
            super();
            iType = type;
        }

        /**
         * Return true if the object is an instanceof the type of the predicate.
         */
        public boolean evaluate(Object object) {
            return iType.isInstance(object);
        }
    }

    // EqualPredicate
    //----------------------------------------------------------------------------------

    /**
     * EqualPredicate that checks if the object is a particular value by equals().
     */
    private static class EqualPredicate implements Predicate, Serializable {
        /** The object to compare to */
        private final Object iValue;

        /**
         * Constructor
         */
        public EqualPredicate(Object value) {
            super();
            iValue = value;
        }

        /**
         * Return true if the object is equals() to the value stored in the predicate.
         */
        public boolean evaluate(Object object) {
            return iValue.equals(object);
        }
    }

    // IdentityPredicate
    //----------------------------------------------------------------------------------

    /**
     * IdentityPredicate that checks if the object is a particular value by identity.
     */
    private static class IdentityPredicate implements Predicate, Serializable {
        /** The object to compare identity to */
        private final Object iValue;

        /**
         * Constructor
         */
        public IdentityPredicate(Object value) {
            super();
            iValue = value;
        }

        /**
         * Return true if the object is equals() to the value stored in the predicate.
         */
        public boolean evaluate(Object object) {
            return iValue == object;
        }
    }

    // UniquePredicate
    //----------------------------------------------------------------------------------

    /**
     * UniquePredicate returns true the first time an object is
     * encoutered, and false if the same object is received 
     * again using equals().
     */
    private static class UniquePredicate implements Predicate, Serializable {
        /** The set of previously seen objects */
        private final Set iSet = new HashSet();

        /**
         * Constructor
         */
        public UniquePredicate() {
            super();
        }

        /**
         * Return true the first time, and false subsequant times
         * that an object is encountered, using equals().
         */
        public boolean evaluate(Object object) {
            return iSet.add(object);
        }
    }

    // TransformerPredicate
    //----------------------------------------------------------------------------------

    /**
     * TransformerPredicate returns the result of the Transformer as a boolean.
     */
    private static class TransformerPredicate implements Predicate, Serializable {
        /** The transformer to call */
        private final Transformer iTransformer;

        /**
         * Constructor
         */
        public TransformerPredicate(Transformer transformer) {
            super();
            iTransformer = transformer;
        }

        /**
         * Return the boolean result of a Transformer
         */
        public boolean evaluate(Object object) {
            Object result = null;
            try {
                result = iTransformer.transform(object);

            } catch (TransformerException ex) {
                throw new PredicateException("TransformerPredicate: " + ex.getMessage(), ex);
            }
            if (result instanceof Boolean == false) {
                throw new PredicateException(
                    "TransformerPredicate: Transformer must return an instanceof Boolean, it was a "
                        + (result == null ? "null object" : result.getClass().getName()));
            }
            return ((Boolean) result).booleanValue();
        }
    }

    // NullIsExceptionPredicate
    //----------------------------------------------------------------------------------

    /**
     * NullIsExceptionPredicate returns an exception if null is passed in.
     */
    private static class NullIsExceptionPredicate implements Predicate, Serializable {
        /** The predicate to call */
        private final Predicate iPredicate;
        
        /**
         * Constructor
         */
        private NullIsExceptionPredicate(Predicate predicate){
            super();
            iPredicate = predicate;
        }
        
        /**
         * Return an exception if null
         */
        public boolean evaluate(Object object){
            if (object == null) {
                throw new PredicateException("NullIsExceptionPredicate: Input Object must not be null");
            }
            return iPredicate.evaluate(object);
        }
    };

    // NullIsFalsePredicate
    //----------------------------------------------------------------------------------

    /**
     * NullIsFalsePredicate returns false if null is passed in.
     */
    private static class NullIsFalsePredicate implements Predicate, Serializable {
        /** The predicate to call */
        private final Predicate iPredicate;
        
        /**
         * Constructor
         */
        private NullIsFalsePredicate(Predicate predicate){
            super();
            iPredicate = predicate;
        }
        
        /**
         * Return false if null
         */
        public boolean evaluate(Object object){
            if (object == null) {
                return false;
            }
            return iPredicate.evaluate(object);
        }
    };

    // NullIsTruePredicate
    //----------------------------------------------------------------------------------

    /**
     * NullIsTruePredicate returns true if null is passed in.
     */
    private static class NullIsTruePredicate implements Predicate, Serializable {
        /** The predicate to call */
        private final Predicate iPredicate;
        
        /**
         * Constructor
         */
        private NullIsTruePredicate(Predicate predicate){
            super();
            iPredicate = predicate;
        }
        
        /**
         * Return true if null
         */
        public boolean evaluate(Object object){
            if (object == null) {
                return true;
            }
            return iPredicate.evaluate(object);
        }
    };

}

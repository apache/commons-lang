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
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.apache.commons.lang.SerializationException;
import org.apache.commons.lang.SerializationUtils;
/**
 * <code>FactoryUtils</code> provides reference implementations and utilities
 * for the Factory functor interface. The supplied factories are:
 * <ul>
 * <li>Prototype - clones a specified object
 * <li>Reflection - creates objects using reflection
 * <li>Constant - always returns the same object
 * <li>Null - always returns null
 * <li>Exception - always throws an exception
 * </ul>
 * All the supplied factories are Serializable.
 *
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id: FactoryUtils.java,v 1.2 2002/11/14 21:54:49 scolebourne Exp $
 */
public class FactoryUtils {

    /**
     * A factory that always throws an exception
     */
    private static final Factory EXCEPTION_FACTORY = new ExceptionFactory();
    /**
     * A factory that always returns null
     */
    private static final Factory NULL_FACTORY = new ConstantFactory(null);

    /**
     * Restrictive constructor
     */
    protected FactoryUtils() {
        super();
    }

    /**
     * Gets a Factory that always throws an exception.
     * This could be useful during testing as a placeholder.
     *
     * @return the factory
     */
    public static Factory exceptionFactory() {
        return EXCEPTION_FACTORY;
    }

    /**
     * Gets a Factory that will return null each time the factory is used.
     * This could be useful during testing as a placeholder.
     *
     * @return the factory
     */
    public static Factory nullFactory() {
        return NULL_FACTORY;
    }

    /**
     * Creates a Factory that will return the same object each time the factory
     * is used. No check is made that the object is immutable. In general, only
     * immutable objects should use the constant factory. Mutable objects should
     * use the prototype factory.
     *
     * @param constantToReturn  the constant object to return each time in the factory
     * @return the <code>constant</code> factory.
     */
    public static Factory constantFactory(Object constantToReturn) {
        return new ConstantFactory(constantToReturn);
    }

    /**
     * Creates a Factory that will return a clone of the same prototype object
     * each time the factory is used. The prototype will be cloned using one of these
     * techniques (in order):
     * <ul>
     * <li>public clone method
     * <li>public copy constructor
     * <li>serialization clone
     * <ul>
     *
     * @param prototype  the object to clone each time in the factory
     * @return the <code>prototype</code> factory
     * @throws IllegalArgumentException if the prototype is null
     * @throws IllegalArgumentException if the prototype cannot be cloned
     */
    public static Factory prototypeFactory(Object prototype) {
        if (prototype == null) {
            throw new IllegalArgumentException("The prototype must not be null");
        }
        try {
            prototype.getClass().getMethod("clone", null);
            return new PrototypeCloneFactory(prototype);

        } catch (NoSuchMethodException ex) {
            try {
                prototype.getClass().getConstructor(new Class[] { prototype.getClass()});
                return new ReflectionFactory(prototype.getClass(), new Class[] { prototype.getClass()}, new Object[] { prototype });

            } catch (NoSuchMethodException ex2) {
                if (prototype instanceof Serializable) {
                    return new PrototypeSerializationFactory((Serializable) prototype);
                }
            }
        }
        throw new IllegalArgumentException("The prototype must be cloneable via a public clone method");
    }

    /**
     * Creates a Factory that can create objects of a specific type using
     * a no-args constructor.
     *
     * @param classToInstantiate  the Class to instantiate each time in the factory
     * @return the <code>reflection</code> factory
     * @throws IllegalArgumentException if the classToInstantiate is null
     */
    public static Factory reflectionFactory(Class classToInstantiate) {
        return new ReflectionFactory(classToInstantiate);
    }

    /**
     * Creates a Factory that can create objects of a specific type using
     * the arguments specified to this method.
     *
     * @param classToInstantiate  the Class to instantiate each time in the factory
     * @param paramTypes  parameter types for the constructor, can be null
     * @param args  the arguments to pass to the constructor, can be null
     * @return the <code>reflection</code> factory
     * @throws IllegalArgumentException if the classToInstantiate is null
     * @throws IllegalArgumentException if the paramTypes and args don't match
     * @throws IllegalArgumentException if the constructor doesn't exist
     */
    public static Factory reflectionFactory(Class classToInstantiate, Class[] paramTypes, Object[] args) {
        return new ReflectionFactory(classToInstantiate, paramTypes, args);
    }

    // ExceptionFactory
    //----------------------------------------------------------------------------------

    /**
     * ExceptionFactory always throws an exception
     */
    private static class ExceptionFactory implements Factory, Serializable {

        /**
         * Constructor
         */
        private ExceptionFactory() {
            super();
        }

        /**
         * Always throw an exception
         */
        public Object create() {
            throw new FactoryException("ExceptionFactory invoked");
        }
    }

    // ConstantFactory
    //----------------------------------------------------------------------------------

    /**
     * ConstantFactory returns the same instance each time.
     */
    private static class ConstantFactory implements Factory, Serializable {
        /** The constant to return each time */
        private final Object iConstant;

        /**
         * Constructor to store constant
         */
        private ConstantFactory(Object constant) {
            super();
            iConstant = constant;
        }

        /**
         * Always return constant
         */
        public Object create() {
            return iConstant;
        }
    }

    // PrototypeCloneFactory
    //----------------------------------------------------------------------------------

    /**
     * PrototypeCloneFactory creates objects by copying a prototype using the clone method.
     */
    private static class PrototypeCloneFactory implements Factory, Serializable {
        /** The object to clone each time */
        private final Object iPrototype;
        /** The method used to clone */
        private transient Method iCloneMethod;

        /**
         * Constructor to store prototype
         */
        private PrototypeCloneFactory(Object prototype) {
            super();
            if (prototype == null) {
                throw new IllegalArgumentException("PrototypeCloneFactory: The prototype must not be null");
            }
            iPrototype = prototype;

            findCloneMethod();
        }

        /**
         * Find the Clone method for the class specified.
         */
        private void findCloneMethod() {
            try {
                iCloneMethod = iPrototype.getClass().getMethod("clone", null);

            } catch (NoSuchMethodException ex) {
                throw new IllegalArgumentException("PrototypeCloneFactory: The clone method must exist and be public ");
            }
        }

        /**
         * Return clone of prototype
         */
        public Object create() {
            // needed for post-serialization
            if (iCloneMethod == null) {
                findCloneMethod();
            }

            try {
                return iCloneMethod.invoke(iPrototype, null);

            } catch (IllegalAccessException ex) {
                throw new FactoryException("ReflectionFactory: Clone method must be public", ex);
            } catch (InvocationTargetException ex) {
                throw new FactoryException("ReflectionFactory: Clone method threw an exception", ex);
            }
        }
    }

    // PrototypeSerializationFactory
    //----------------------------------------------------------------------------------

    /**
     * PrototypeSerializationFactory creates objects by cloning a prototype using serialization.
     */
    private static class PrototypeSerializationFactory implements Factory, Serializable {
        /** The object to clone via serialization each time */
        private final Serializable iPrototype;

        /**
         * Constructor to store prototype
         */
        private PrototypeSerializationFactory(Serializable prototype) {
            super();
            if (prototype == null) {
                throw new IllegalArgumentException("PrototypeSerializationFactory: The prototype must not be null");
            }
            iPrototype = prototype;
        }

        /**
         * Return clone of prototype by serialization
         */
        public Object create() {
            try {
                return SerializationUtils.clone(iPrototype);

            } catch (SerializationException ex) {
                throw new FactoryException("PrototypeSerializationFactory: Unable to clone by serialization", ex);
            }
        }
    }

    // ReflectionFactory
    //----------------------------------------------------------------------------------

    /**
     * ReflectionFactory creates objects using reflection.
     */
    private static class ReflectionFactory implements Factory, Serializable {
        /** The class to create */
        private final Class iClassToInstantiate;
        /** The constructor parameter types */
        private final Class[] iParamTypes;
        /** The constructor arguments */
        private final Object[] iArgs;
        /** The constructor */
        private transient Constructor iConstructor = null;

        /**
         * Constructor
         */
        public ReflectionFactory(Class classToInstantiate) {
            this(classToInstantiate, null, null);
        }

        /* builds the object factory taking all the options needed to provide
         * arguments to a constructor.
         */
        public ReflectionFactory(Class classToInstantiate, Class[] paramTypes, Object[] args) {
            super();
            if (classToInstantiate == null) {
                throw new IllegalArgumentException("ReflectionFactory: The class to instantiate must not be null");
            }
            if (((paramTypes == null) && (args != null))
                || ((paramTypes != null) && (args == null))
                || ((paramTypes != null) && (args != null) && (paramTypes.length != args.length))) {
                throw new IllegalArgumentException("ReflectionFactory: The parameter types must match the arguments");
            }

            iClassToInstantiate = classToInstantiate;
            if ((paramTypes == null) && (args == null)) {
                iParamTypes = null;
                iArgs = null;
            } else {
                iParamTypes = (Class[]) paramTypes.clone();
                iArgs = (Object[]) args.clone();
            }

            findConstructor();
        }

        /**
         * Find the Constructor for the class specified.
         */
        private void findConstructor() {
            try {
                iConstructor = iClassToInstantiate.getConstructor(iParamTypes);

            } catch (NoSuchMethodException ex) {
                throw new IllegalArgumentException("ReflectionFactory: The constructor must exist and be public ");
            }
        }

        /**
         * Create the object using a constructor
         */
        public Object create() {
            // needed for post-serialization
            if (iConstructor == null) {
                findConstructor();
            }

            try {
                return iConstructor.newInstance(iArgs);

            } catch (InstantiationException ex) {
                throw new FactoryException("ReflectionFactory: InstantiationException", ex);
            } catch (IllegalAccessException ex) {
                throw new FactoryException("ReflectionFactory: Constructor must be public", ex);
            } catch (InvocationTargetException ex) {
                throw new FactoryException("ReflectionFactory: Constructor threw an exception", ex);
            }
        }
    }

}

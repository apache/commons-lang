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
package org.apache.commons.lang;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
/**
 * <code>ClassUtils</code> contains utility methods for working for
 * classes without using reflection.
 *
 * @author <a href="mailto:scolebourne@apache.org">Stephen Colebourne</a>
 * @version $Id: ClassUtils.java,v 1.3 2002/10/24 23:12:54 scolebourne Exp $
 */
public class ClassUtils {

    /**
     * ClassUtils instances should NOT be constructed in standard programming.
     * Instead, the class should be used as <code>ClassUtils.getShortClassName(cls)</code>.
     * This constructor is public to permit tools that require a JavaBean instance
     * to operate.
     */
    public ClassUtils() {
    }

    // -------------------------------------------------------------------------
    
    /**
     * Gets the class name minus the package name from a Class.
     * 
     * @param cls  the class to get the short name for, must not be null
     * @return the class name without the package name
     * @throws IllegalArgumentException if the class is null
     */
    public static String getShortClassName(Class cls) {
        if (cls == null) {
            throw new IllegalArgumentException("The class must not be null");
        }
        return getShortClassName(cls.getName());
    }
    
    /**
     * Gets the class name minus the package name for an Object.
     * 
     * @param object  the class to get the short name for, must not be null
     * @return the class name of the object without the package name
     * @throws IllegalArgumentException if the object is null
     */
    public static String getShortClassName(Object object) {
        if (object == null) {
            throw new IllegalArgumentException("The object must not be null");
        }
        return getShortClassName(object.getClass().getName());
    }
    
    /**
     * Gets the class name minus the package name from a String.
     * <p>
     * The string passed in is assumed to be a class name - it is not
     * checked.
     * 
     * @param className  the className to get the short name for, must not be empty
     * @return the class name of the class without the package name
     * @throws IllegalArgumentException if the className is empty
     */
    public static String getShortClassName(String className) {
        if (StringUtils.isEmpty(className)) {
            throw new IllegalArgumentException("The class name must not be empty");
        }
        char[] chars = className.toCharArray();
        int lastDot = 0;
        for (int i = 0; i < chars.length; i++) {
            if (chars[i] == '.') {
                lastDot = i + 1;
            } else if (chars[i] == '$') {  // handle inner classes
                chars[i] = '.';
            }
        }
        return new String(chars, lastDot, chars.length - lastDot);
    }
    
    // -------------------------------------------------------------------------
    
    /**
     * Gets the package name of a Class.
     * 
     * @param cls  the class to get the package name for, must not be null
     * @return the package name
     * @throws IllegalArgumentException if the class is null
     */
    public static String getPackageName(Class cls) {
        if (cls == null) {
            throw new IllegalArgumentException("The class must not be null");
        }
        return getPackageName(cls.getName());
    }
    
    /**
     * Gets the package name of an Object.
     * 
     * @param object  the class to get the package name for, must not be null
     * @return the package name
     * @throws IllegalArgumentException if the object is null
     */
    public static String getPackageName(Object object) {
        if (object == null) {
            throw new IllegalArgumentException("The object must not be null");
        }
        return getPackageName(object.getClass().getName());
    }
    
    /**
     * Gets the package name from a String.
     * <p>
     * The string passed in is assumed to be a class name - it is not
     * checked.
     * 
     * @param className  the className to get the package name for, must not be empty
     * @return the package name
     * @throws IllegalArgumentException if the className is empty
     */
    public static String getPackageName(String className) {
        if (StringUtils.isEmpty(className)) {
            throw new IllegalArgumentException("The class name must not be empty");
        }
        int i = className.lastIndexOf('.');
        if (i == -1) {
            return "";
        }
        return className.substring(0, i);
    }
    
    // -------------------------------------------------------------------------
    
    /**
     * Gets a list of superclasses for the given class.
     * 
     * @param cls  the class to look up, must not be null
     * @return the list of superclasses in order going up from this one
     * @throws IllegalArgumentException if the class is null
     */
    public static List getAllSuperclasses(Class cls) {
        if (cls == null) {
            throw new IllegalArgumentException("The class must not be null");
        }
        List classes = new ArrayList();
        Class superclass = cls.getSuperclass();
        while (superclass != null) {
            classes.add(superclass);
            superclass = superclass.getSuperclass();
        }
        return classes;
    }
    
    /**
     * Gets a list of all interfaces implemented by the given class.
     * <p>
     * The order is determined by looking through each interface in turn as
     * declared in the source file and following its hieracrchy up. Later
     * duplicates are ignored, so the order is maintained.
     * 
     * @param cls  the class to look up, must not be null
     * @return the list of interfaces in order
     * @throws IllegalArgumentException if the class is null
     */
    public static List getAllInterfaces(Class cls) {
        if (cls == null) {
            throw new IllegalArgumentException("The class must not be null");
        }
        List list = new ArrayList();
        Class[] interfaces = cls.getInterfaces();
        for (int i = 0; i < interfaces.length; i++) {
            if (list.contains(interfaces[i]) == false) {
                list.add(interfaces[i]);
            }
            List superInterfaces = getAllInterfaces(interfaces[i]);
            for (Iterator it = superInterfaces.iterator(); it.hasNext();) {
                Class intface = (Class) it.next();
                if (list.contains(intface) == false) {
                    list.add(intface);
                }
            }
        }
        return list;
    }
    
    /**
     * Gets a list of subclasses of the specified class.
     * <p>
     * This method searches the classpath to find all the subclasses
     * of a particular class available. No classes are loaded, the 
     * returned list contains class names, not classes.
     *
     * @param cls  the class to find subclasses for
     * @return the list of subclass String class names
     * @throws IllegalArgumentException if the class is null
     */
    public static List getAllSubclassNames(Class cls) {
        if (cls == null) {
            throw new IllegalArgumentException("The class must not be null");
        }
        // TODO Use JavaWorld tip for searching the classpath
        return null;
    }

    /**
     * Gets a list of subclasses of the specified class.
     * <p>
     * This method searches the classpath to find all the subclasses
     * of a particular class available.
     *
     * @param cls  the class to find subclasses for
     * @return the list of subclasses
     * @throws IllegalArgumentException if the class is null
     */
    public static List getAllSubclasses(Class cls) {
        List names = getAllSubclassNames(cls);
        return convertClassNamesToClasses(names);
    }

    /**
     * Gets a list of implementations of the specified interface.
     * <p>
     * This method searches the classpath to find all the implementations
     * of a particular interface available. No classes are loaded, the 
     * returned list contains class names, not classes.
     *
     * @param cls  the class to find sub classes for
     * @return the list of implementation String class names
     * @throws IllegalArgumentException if the class is null
     */
    public static List getAllImplementationClassNames(Class cls) {
        if (cls == null) {
            throw new IllegalArgumentException("The class must not be null");
        }
        // TODO Use JavaWorld tip for searching the classpath
        return null;
    }

    /**
     * Given a list of class names, this method converts them into classes.
     * A new list is returned. If the class name cannot be found, null is
     * stored in the list. If the class name in the list is null, null is 
     * stored in the output list.
     * 
     * @param classes  the classNames to change, the class is stored back
     *  into the list. null will be stored in the list if no class is found.
     * @return the list of Class objects corresponding to the class names
     * @throws IllegalArgumentException if the classNames is null
     */
    public static List convertClassNamesToClasses(List classNames) {
        if (classNames == null) {
            throw new IllegalArgumentException("The class names must not be null");
        }
        List classes = new ArrayList(classNames.size());
        for (Iterator it = classNames.iterator(); it.hasNext();) {
            String className = (String) it.next();
            try {
                classes.add(Class.forName(className));
            } catch (Exception ex) {
                classes.add(null);
            }
        }
        return classes;
    }
    
    /**
     * Given a list of classes, this method finds all those which are
     * subclasses or implementations of a specified superclass.
     * 
     * @param classes  the classes to check
     * @param superclass  the superclass to check for
     * @return the list of subclasses or implementations
     * @throws IllegalArgumentException if the classes or superClass is null
     */
    public static List getAssignableFrom(List classes, Class superclass) {
        if (classes == null) {
            throw new IllegalArgumentException("The classes must not be null");
        }
        if (superclass == null) {
            throw new IllegalArgumentException("The superclass must not be null");
        }
        List subs = new ArrayList();
        Iterator it = classes.iterator();
        while (it.hasNext()) {
            Class cls = (Class) it.next();
            if (cls == null) {
                throw new IllegalArgumentException("The class list must not contain nulls");
            }
            if (isAssignable(cls, superclass)) {
                subs.add(cls);
            }
        }
        return subs;
    }

    /**
     * Checks if an array of Classes can be assigned to another array of Classes.
     * <p>
     * This can be used to check if parameter types are suitably compatable for
     * reflection invocation.
     * <p>
     * Unlike the Class.isAssignableFrom method, this method takes into 
     * account widenings of primitive classes and nulls.
     * <p>
     * Primitive widenings allow an int to be assigned to a long, float or 
     * double. This method returns the correct result for these cases.
     * <p>
     * Null may be assigned to any reference type. This method will return
     * true if null is passed in and the toClass is non-primitive.
     * <p>
     * Specifically, this method tests whether the type represented by the
     * specified <code>Class</code> parameter can be converted to the type
     * represented by this <code>Class</code> object via an identity conversion
     * widening primitive or widening reference conversion. See 
     * <em>The Java Language Specification</em>, sections 5.1.1, 5.1.2 and 
     * 5.1.4 for details.
     *
     * @param classArray  the array of Classes to check, may be null
     * @param toClassArray  the array of Classes to try to assign into, may be null
     * @return true if assignment possible
     */
    public static boolean isAssignable(Class[] classArray, Class[] toClassArray) {
        if (ArrayUtils.isSameLength(classArray, toClassArray) == false) {
            return false;
        }
        if (classArray == null) {
            classArray = ArrayUtils.EMPTY_CLASS_ARRAY;
        }
        if (toClassArray == null) {
            toClassArray = ArrayUtils.EMPTY_CLASS_ARRAY;
        }
        for (int i = 0; i < classArray.length; i++) {
            if (isAssignable(classArray[i], toClassArray[i]) == false) {
                return false;
            }
        }
        return true;
    }
    
    /**
     * Checks if one Class can be assigned to a variable of another Class.
     * <p>
     * Unlike the Class.isAssignableFrom method, this method takes into 
     * account widenings of primitive classes and nulls.
     * <p>
     * Primitive widenings allow an int to be assigned to a long, float or 
     * double. This method returns the correct result for these cases.
     * <p>
     * Null may be assigned to any reference type. This method will return
     * true if null is passed in and the toClass is non-primitive.
     * <p>
     * Specifically, this method tests whether the type represented by the
     * specified <code>Class</code> parameter can be converted to the type
     * represented by this <code>Class</code> object via an identity conversion
     * widening primitive or widening reference conversion. See 
     * <em>The Java Language Specification</em>, sections 5.1.1, 5.1.2 and 
     * 5.1.4 for details.
     *
     * @param cls  the Class to check, may be null
     * @param toClass  the Class to try to assign into, must not be null
     * @return true if assignment possible
     * @throws IllegalArgumentException if the toClass is null
     */
    public static boolean isAssignable(Class cls, Class toClass) {
        if (toClass == null) {
            throw new IllegalArgumentException("The class must not be null");
        }
        // have to check for null, as isAssignableFrom doesn't
        if (cls == null) {
            return !(toClass.isPrimitive());
        }
        if (cls.equals(toClass)) {
            return true;
        }
        if (cls.isPrimitive()) {
            if (toClass.isPrimitive() == false) {
                return false;
            }
            if (Integer.TYPE.equals(cls)) {
                return Long.TYPE.equals(toClass) 
                    || Float.TYPE.equals(toClass) 
                    || Double.TYPE.equals(toClass);
            }
            if (Long.TYPE.equals(cls)) {
                return Float.TYPE.equals(toClass) 
                    || Double.TYPE.equals(toClass);
            }
            if (Boolean.TYPE.equals(cls)) {
                return false;
            }
            if (Double.TYPE.equals(cls)) {
                return false;
            }
            if (Float.TYPE.equals(cls)) {
                return Double.TYPE.equals(toClass);
            }
            if (Character.TYPE.equals(cls)) {
                return Integer.TYPE.equals(toClass) 
                    || Long.TYPE.equals(toClass) 
                    || Float.TYPE.equals(toClass) 
                    || Double.TYPE.equals(toClass);
            }
            if (Short.TYPE.equals(cls)) {
                return Integer.TYPE.equals(toClass) 
                    || Long.TYPE.equals(toClass) 
                    || Float.TYPE.equals(toClass) 
                    || Double.TYPE.equals(toClass);
            }
            if (Byte.TYPE.equals(cls)) {
                return Short.TYPE.equals(toClass) 
                    || Integer.TYPE.equals(toClass) 
                    || Long.TYPE.equals(toClass) 
                    || Float.TYPE.equals(toClass) 
                    || Double.TYPE.equals(toClass);
            }
            // should never get here
            return false;
        }
        return toClass.isAssignableFrom(cls);
    }
    
    /**
     * Is the specified class an inner class or static nested class.
     * 
     * @param cls  the class to check
     * @return true if the class is an inner or static nested class
     * @throws IllegalArgumentException if the class is null
     */
    public static boolean isInnerClass(Class cls) {
        if (cls == null) {
            throw new IllegalArgumentException("The class must not be null");
        }
        return (cls.getDeclaringClass() != null);
    }
    
}

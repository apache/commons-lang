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
package org.apache.commons.lang;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * <p>Operates on classes without using reflection.</p>
 *
 * <p>This class handles invalid <code>null</code> inputs as best it can.
 * Each method documents its behaviour in more detail.</p>
 *
 * @author Stephen Colebourne
 * @author Gary Gregory
 * @author Norm Deane
 * @author Alban Peignier
 * @since 2.0
 * @version $Id$
 */
public class ClassUtils {

    /**
     * <p>The package separator character: <code>'&#x2e;' == {@value}</code>.</p>
     */
    public static final char PACKAGE_SEPARATOR_CHAR = '.';

    /**
     * <p>The package separator String: <code>"&#x2e;"</code>.</p>
     */
    public static final String PACKAGE_SEPARATOR = String.valueOf(PACKAGE_SEPARATOR_CHAR);

    /**
     * <p>The inner class separator character: <code>'$' == {@value}</code>.</p>
     */
    public static final char INNER_CLASS_SEPARATOR_CHAR = '$';

    /**
     * <p>The inner class separator String: <code>"$"</code>.</p>
     */
    public static final String INNER_CLASS_SEPARATOR = String.valueOf(INNER_CLASS_SEPARATOR_CHAR);

    /**
     * Maps primitive <code>Class</code>es to their corresponding wrapper <code>Class</code>.
     */
    private static Map  primitiveWrapperMap = new HashMap();
    static {
         primitiveWrapperMap.put(Boolean.TYPE, Boolean.class);
         primitiveWrapperMap.put(Byte.TYPE, Byte.class);
         primitiveWrapperMap.put(Character.TYPE, Character.class);
         primitiveWrapperMap.put(Short.TYPE, Short.class);
         primitiveWrapperMap.put(Integer.TYPE, Integer.class);
         primitiveWrapperMap.put(Long.TYPE, Long.class);
         primitiveWrapperMap.put(Double.TYPE, Double.class);
         primitiveWrapperMap.put(Float.TYPE, Float.class);
         primitiveWrapperMap.put(Void.TYPE, Void.TYPE);
    }

    /**
     * Maps a primitive class name to its corresponding abbreviation used in array class names.
     */
    private static Map abbreviationMap = new HashMap();
    static {
        abbreviationMap.put( "int", "I" );
        abbreviationMap.put( "boolean", "Z" );
        abbreviationMap.put( "float", "F" );
        abbreviationMap.put( "long", "J" );
        abbreviationMap.put( "short", "S" );
        abbreviationMap.put( "byte", "B" );
        abbreviationMap.put( "double", "D" );
        abbreviationMap.put( "char", "C" );
    }

    /**
     * <p>ClassUtils instances should NOT be constructed in standard programming.
     * Instead, the class should be used as
     * <code>ClassUtils.getShortClassName(cls)</code>.</p>
     *
     * <p>This constructor is public to permit tools that require a JavaBean
     * instance to operate.</p>
     */
    public ClassUtils() {
      super();
    }

    // Short class name
    // ----------------------------------------------------------------------
    /**
     * <p>Gets the class name minus the package name for an <code>Object</code>.</p>
     *
     * @param object  the class to get the short name for, may be null
     * @param valueIfNull  the value to return if null
     * @return the class name of the object without the package name, or the null value
     */
    public static String getShortClassName(Object object, String valueIfNull) {
        if (object == null) {
            return valueIfNull;
        }
        return getShortClassName(object.getClass().getName());
    }

    /**
     * <p>Gets the class name minus the package name from a <code>Class</code>.</p>
     *
     * @param cls  the class to get the short name for.
     * @return the class name without the package name or an empty string
     */
    public static String getShortClassName(Class cls) {
        if (cls == null) {
            return StringUtils.EMPTY;
        }
        return getShortClassName(cls.getName());
    }

    /**
     * <p>Gets the class name minus the package name from a String.</p>
     *
     * <p>The string passed in is assumed to be a class name - it is not checked.</p>
     *
     * @param className  the className to get the short name for
     * @return the class name of the class without the package name or an empty string
     */
    public static String getShortClassName(String className) {
        if (className == null) {
            return StringUtils.EMPTY;
        }
        if (className.length() == 0) {
            return StringUtils.EMPTY;
        }
        char[] chars = className.toCharArray();
        int lastDot = 0;
        for (int i = 0; i < chars.length; i++) {
            if (chars[i] == PACKAGE_SEPARATOR_CHAR) {
                lastDot = i + 1;
            } else if (chars[i] == INNER_CLASS_SEPARATOR_CHAR) {  // handle inner classes
                chars[i] = PACKAGE_SEPARATOR_CHAR;
            }
        }
        return new String(chars, lastDot, chars.length - lastDot);
    }

    // Package name
    // ----------------------------------------------------------------------
    /**
     * <p>Gets the package name of an <code>Object</code>.</p>
     *
     * @param object  the class to get the package name for, may be null
     * @param valueIfNull  the value to return if null
     * @return the package name of the object, or the null value
     */
    public static String getPackageName(Object object, String valueIfNull) {
        if (object == null) {
            return valueIfNull;
        }
        return getPackageName(object.getClass().getName());
    }

    /**
     * <p>Gets the package name of a <code>Class</code>.</p>
     *
     * @param cls  the class to get the package name for, may be <code>null</code>.
     * @return the package name or an empty string
     */
    public static String getPackageName(Class cls) {
        if (cls == null) {
            return StringUtils.EMPTY;
        }
        return getPackageName(cls.getName());
    }

    /**
     * <p>Gets the package name from a <code>String</code>.</p>
     *
     * <p>The string passed in is assumed to be a class name - it is not checked.</p>
     * <p>If the class is unpackaged, return an empty string.</p>
     *
     * @param className  the className to get the package name for, may be <code>null</code>
     * @return the package name or an empty string
     */
    public static String getPackageName(String className) {
        if (className == null) {
            return StringUtils.EMPTY;
        }
        int i = className.lastIndexOf(PACKAGE_SEPARATOR_CHAR);
        if (i == -1) {
            return StringUtils.EMPTY;
        }
        return className.substring(0, i);
    }

    // Superclasses/Superinterfaces
    // ----------------------------------------------------------------------
    /**
     * <p>Gets a <code>List</code> of superclasses for the given class.</p>
     *
     * @param cls  the class to look up, may be <code>null</code>
     * @return the <code>List</code> of superclasses in order going up from this one
     *  <code>null</code> if null input
     */
    public static List getAllSuperclasses(Class cls) {
        if (cls == null) {
            return null;
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
     * <p>Gets a <code>List</code> of all interfaces implemented by the given
     * class and its superclasses.</p>
     *
     * <p>The order is determined by looking through each interface in turn as
     * declared in the source file and following its hierarchy up. Then each
     * superclass is considered in the same way. Later duplicates are ignored,
     * so the order is maintained.</p>
     *
     * @param cls  the class to look up, may be <code>null</code>
     * @return the <code>List</code> of interfaces in order,
     *  <code>null</code> if null input
     */
    public static List getAllInterfaces(Class cls) {
        if (cls == null) {
            return null;
        }
        List list = new ArrayList();
        while (cls != null) {
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
            cls = cls.getSuperclass();
        }
        return list;
    }

    // Convert list
    // ----------------------------------------------------------------------
    /**
     * <p>Given a <code>List</code> of class names, this method converts them into classes.</p>
     *
     * <p>A new <code>List</code> is returned. If the class name cannot be found, <code>null</code>
     * is stored in the <code>List</code>. If the class name in the <code>List</code> is
     * <code>null</code>, <code>null</code> is stored in the output <code>List</code>.</p>
     *
     * @param classNames  the classNames to change
     * @return a <code>List</code> of Class objects corresponding to the class names,
     *  <code>null</code> if null input
     * @throws ClassCastException if classNames contains a non String entry
     */
    public static List convertClassNamesToClasses(List classNames) {
        if (classNames == null) {
            return null;
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
     * <p>Given a <code>List</code> of <code>Class</code> objects, this method converts
     * them into class names.</p>
     *
     * <p>A new <code>List</code> is returned. <code>null</code> objects will be copied into
     * the returned list as <code>null</code>.</p>
     *
     * @param classes  the classes to change
     * @return a <code>List</code> of class names corresponding to the Class objects,
     *  <code>null</code> if null input
     * @throws ClassCastException if <code>classes</code> contains a non-<code>Class</code> entry
     */
    public static List convertClassesToClassNames(List classes) {
        if (classes == null) {
            return null;
        }
        List classNames = new ArrayList(classes.size());
        for (Iterator it = classes.iterator(); it.hasNext();) {
            Class cls = (Class) it.next();
            if (cls == null) {
                classNames.add(null);
            } else {
                classNames.add(cls.getName());
            }
        }
        return classNames;
    }

    // Is assignable
    // ----------------------------------------------------------------------
    /**
     * <p>Checks if an array of Classes can be assigned to another array of Classes.</p>
     *
     * <p>This method calls {@link #isAssignable(Class, Class) isAssignable} for each
     * Class pair in the input arrays. It can be used to check if a set of arguments
     * (the first parameter) are suitably compatible with a set of method parameter types
     * (the second parameter).</p>
     *
     * <p>Unlike the {@link Class#isAssignableFrom(java.lang.Class)} method, this
     * method takes into account widenings of primitive classes and
     * <code>null</code>s.</p>
     *
     * <p>Primitive widenings allow an int to be assigned to a <code>long</code>,
     * <code>float</code> or <code>double</code>. This method returns the correct
     * result for these cases.</p>
     *
     * <p><code>Null</code> may be assigned to any reference type. This method will
     * return <code>true</code> if <code>null</code> is passed in and the toClass is
     * non-primitive.</p>
     *
     * <p>Specifically, this method tests whether the type represented by the
     * specified <code>Class</code> parameter can be converted to the type
     * represented by this <code>Class</code> object via an identity conversion
     * widening primitive or widening reference conversion. See
     * <em><a href="http://java.sun.com/docs/books/jls/">The Java Language Specification</a></em>,
     * sections 5.1.1, 5.1.2 and 5.1.4 for details.</p>
     *
     * @param classArray  the array of Classes to check, may be <code>null</code>
     * @param toClassArray  the array of Classes to try to assign into, may be <code>null</code>
     * @return <code>true</code> if assignment possible
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
     * <p>Checks if one <code>Class</code> can be assigned to a variable of
     * another <code>Class</code>.</p>
     *
     * <p>Unlike the {@link Class#isAssignableFrom(java.lang.Class)} method,
     * this method takes into account widenings of primitive classes and
     * <code>null</code>s.</p>
     *
     * <p>Primitive widenings allow an int to be assigned to a long, float or
     * double. This method returns the correct result for these cases.</p>
     *
     * <p><code>Null</code> may be assigned to any reference type. This method
     * will return <code>true</code> if <code>null</code> is passed in and the
     * toClass is non-primitive.</p>
     *
     * <p>Specifically, this method tests whether the type represented by the
     * specified <code>Class</code> parameter can be converted to the type
     * represented by this <code>Class</code> object via an identity conversion
     * widening primitive or widening reference conversion. See
     * <em><a href="http://java.sun.com/docs/books/jls/">The Java Language Specification</a></em>,
     * sections 5.1.1, 5.1.2 and 5.1.4 for details.</p>
     *
     * @param cls  the Class to check, may be null
     * @param toClass  the Class to try to assign into, returns false if null
     * @return <code>true</code> if assignment possible
     */
    public static boolean isAssignable(Class cls, Class toClass) {
        if (toClass == null) {
            return false;
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
     * <p>Converts the specified primitive Class object to its corresponding
     * wrapper Class object.</p>
     *
     * <p>NOTE: From v2.2, this method handles <code>Void.TYPE</code>,
     * returning <code>Void.TYPE</code>.</p>
     *
     * @param cls  the class to convert, may be null
     * @return the wrapper class for <code>cls</code> or <code>cls</code> if
     * <code>cls</code> is not a primitive. <code>null</code> if null input.
     * @since 2.1
     */
    public static Class primitiveToWrapper(Class cls) {
        Class convertedClass = cls;
        if (cls != null && cls.isPrimitive()) {
            convertedClass = (Class) primitiveWrapperMap.get(cls);
        }
        return convertedClass;
    }

    /**
     * <p>Converts the specified array of primitive Class objects to an array of
     * its corresponding wrapper Class objects.</p>
     *
     * @param classes  the class array to convert, may be null or empty
     * @return an array which contains for each given class, the wrapper class or
     * the original class if class is not a primitive. <code>null</code> if null input.
     * Empty array if an empty array passed in.
     * @since 2.1
     */
    public static Class[] primitivesToWrappers(Class[] classes) {
        if (classes == null) {
            return null;
        }

        if (classes.length == 0) {
            return classes;
        }

        Class[] convertedClasses = new Class[classes.length];
        for (int i=0; i < classes.length; i++) {
            convertedClasses[i] = primitiveToWrapper( classes[i] );
        }
        return convertedClasses;
    }

    // Inner class
    // ----------------------------------------------------------------------
    /**
     * <p>Is the specified class an inner class or static nested class.</p>
     *
     * @param cls  the class to check, may be null
     * @return <code>true</code> if the class is an inner or static nested class,
     *  false if not or <code>null</code>
     */
    public static boolean isInnerClass(Class cls) {
        if (cls == null) {
            return false;
        }
        return cls.getName().indexOf(INNER_CLASS_SEPARATOR_CHAR) >= 0;
    }

    // Class loading
    // ----------------------------------------------------------------------
    /**
     * Returns the class represented by <code>className</code> using the
     * <code>classLoader</code>.  This implementation supports names like
     * "<code>java.lang.String[]</code>" as well as "<code>[Ljava.lang.String;</code>".
     *
     * @param classLoader  the class loader to use to load the class
     * @param className  the class name
     * @param initialize  whether the class must be initialized
     * @return the class represented by <code>className</code> using the <code>classLoader</code>
     * @throws ClassNotFoundException if the class is not found
     */
    public static Class getClass(
            ClassLoader classLoader, String className, boolean initialize) throws ClassNotFoundException {
        Class clazz;
        if (abbreviationMap.containsKey(className)) {
            String clsName = "[" + abbreviationMap.get(className);
            clazz = Class.forName(clsName, initialize, classLoader).getComponentType();
        } else {
            clazz = Class.forName(toProperClassName(className), initialize, classLoader);
        }
        return clazz;
    }

    /**
     * Returns the (initialized) class represented by <code>className</code>
     * using the <code>classLoader</code>.  This implementation supports names
     * like "<code>java.lang.String[]</code>" as well as
     * "<code>[Ljava.lang.String;</code>".
     *
     * @param classLoader  the class loader to use to load the class
     * @param className  the class name
     * @return the class represented by <code>className</code> using the <code>classLoader</code>
     * @throws ClassNotFoundException if the class is not found
     */
    public static Class getClass(ClassLoader classLoader, String className) throws ClassNotFoundException {
        return getClass(classLoader, className, true);
    }

    /**
     * Returns the (initialized )class represented by <code>className</code>
     * using the current thread's context class loader. This implementation
     * supports names like "<code>java.lang.String[]</code>" as well as
     * "<code>[Ljava.lang.String;</code>".
     *
     * @param className  the class name
     * @return the class represented by <code>className</code> using the current thread's context class loader
     * @throws ClassNotFoundException if the class is not found
     */
    public static Class getClass(String className) throws ClassNotFoundException {
        return getClass(className, true);
    }

    /**
     * Returns the class represented by <code>className</code> using the
     * current thread's context class loader. This implementation supports
     * names like "<code>java.lang.String[]</code>" as well as
     * "<code>[Ljava.lang.String;</code>".
     *
     * @param className  the class name
     * @param initialize  whether the class must be initialized
     * @return the class represented by <code>className</code> using the current thread's context class loader
     * @throws ClassNotFoundException if the class is not found
     */
    public static Class getClass(String className, boolean initialize) throws ClassNotFoundException {
        ClassLoader contextCL = Thread.currentThread().getContextClassLoader();
        ClassLoader loader = contextCL == null ? ClassUtils.class.getClassLoader() : contextCL;
        return getClass(loader, className, initialize );
    }

    // Public method
    // ----------------------------------------------------------------------
    /**
     * <p>Returns the desired Method much like <code>Class.getMethod</code>, however 
     * it ensures that the returned Method is from a public class or interface and not 
     * from an anonymous inner class. This means that the Method is invokable and 
     * doesn't fall foul of Java bug 
     * <a href="http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4071957">4071957</a>).
     *
     *  <code><pre>Set set = Collections.unmodifiableSet(...);
     *  Method method = ClassUtils.getPublicMethod(set.getClass(), "isEmpty",  new Class[0]);
     *  Object result = method.invoke(set, new Object[]);</pre></code>
     * </p>
     *
     * @param cls  the class to check, not null
     * @param methodName  the name of the method
     * @param parameterTypes  the list of parameters
     * @return the method
     * @throws NullPointerException if the class is null
     * @throws SecurityException if a a security violation occured
     * @throws NoSuchMethodException if the method is not found in the given class
     *  or if the metothod doen't conform with the requirements
     */
    public static Method getPublicMethod(Class cls, String methodName, Class parameterTypes[]) 
            throws SecurityException, NoSuchMethodException {
        
        Method declaredMethod = cls.getMethod(methodName, parameterTypes);
        if (Modifier.isPublic(declaredMethod.getDeclaringClass().getModifiers())) {
            return declaredMethod;
        }
        
        List candidateClasses = new ArrayList();
        candidateClasses.addAll(getAllInterfaces(cls));
        candidateClasses.addAll(getAllSuperclasses(cls));
        
        for (Iterator it = candidateClasses.iterator(); it.hasNext(); ) {
            Class candidateClass = (Class) it.next();
            if (!Modifier.isPublic(candidateClass.getModifiers())) {
                continue;
            }
            Method candidateMethod;
            try {
                candidateMethod = candidateClass.getMethod(methodName, parameterTypes);
            } catch (NoSuchMethodException ex) {
                continue;
            }
            if (Modifier.isPublic(candidateMethod.getDeclaringClass().getModifiers())) {
                return candidateMethod;
            }
        }
        
        throw new NoSuchMethodException("Can't find a public method for " +
                methodName + " " + ArrayUtils.toString(parameterTypes));
    }

    // ----------------------------------------------------------------------
    /**
     * Converts a class name to a JLS style class name.
     *
     * @param className  the class name
     * @return the converted name
     */
    private static String toProperClassName(String className) {
        className = StringUtils.deleteWhitespace(className);
        if (className == null) {
            throw new NullArgumentException("className");
        } else if (className.endsWith("[]")) {
            StringBuffer classNameBuffer = new StringBuffer();
            while (className.endsWith("[]")) {
                className = className.substring(0, className.length() - 2);
                classNameBuffer.append("[");
            }
            String abbreviation = (String) abbreviationMap.get(className);
            if (abbreviation != null) {
                classNameBuffer.append(abbreviation);
            } else {
                classNameBuffer.append("L").append(className).append(";");
            }
            className = classNameBuffer.toString();
        }
        return className;
    }

}

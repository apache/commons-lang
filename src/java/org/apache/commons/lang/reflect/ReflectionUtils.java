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
package org.apache.commons.lang.reflect;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Member;
import java.lang.reflect.Modifier;

import org.apache.commons.lang.ArrayUtils;
import org.apache.commons.lang.ClassUtils;
import org.apache.commons.lang.StringUtils;
/**
 * <code>ReflectionUtils</code> contains utility methods for working for
 * reflection.
 *
 * @author <a href="mailto:scolebourne@apache.org">Stephen Colebourne</a>
 * @version $Id: ReflectionUtils.java,v 1.6 2002/12/25 22:03:00 scolebourne Exp $
 */
public class ReflectionUtils {
    
    /**
     * ReflectionUtils instances should NOT be constructed in standard programming.
     * Instead, the class should be used as <code>ReflectionUtils.getShortClassName(obj)</code>.
     * This constructor is public to permit tools that require a JavaBean instance
     * to operate.
     */
    public ReflectionUtils() {
    }

    // -------------------------------------------------------------------------
    
    /**
     * Tests whether the specified field or method is 
     * <code>static</code>.
     * 
     * @param member  the member to test, must not be null
     * @return true if the member is static
     */
    public static boolean isStatic(Member member) {
        if (member == null) {
            throw new IllegalArgumentException("The member must not be null");
        }    
        return Modifier.isStatic(member.getModifiers());
    }

    /**
     * Tests whether the specified field or method is 
     * <code>final</code>.
     * 
     * @param member  the member to test, must not be null
     * @return true if the member is final
     */
    public static boolean isFinal(Member member) {
        if (member == null) {
            throw new IllegalArgumentException("The member must not be null");
        }    
        return Modifier.isFinal(member.getModifiers());
    }

    /**
     * Tests whether the specified field, method or constructor is 
     * <code>public</code>.
     * 
     * @param member  the member to test, must not be null
     * @return true if the member is public scoped
     */
    public static boolean isPublicScope(Member member) {
        if (member == null) {
            throw new IllegalArgumentException("The member must not be null");
        }    
        return Modifier.isPublic(member.getModifiers());
    }

    /**
     * Tests whether the specified field, method or constructor is 
     * <code>protected</code>.
     * 
     * @param member  the member to test, must not be null
     * @return true if the member is protected scoped
     */
    public static boolean isProtectedScope(Member member) {
        if (member == null) {
            throw new IllegalArgumentException("The member must not be null");
        }    
        return Modifier.isProtected(member.getModifiers());
    }

    /**
     * Tests whether the specified field, method or constructor is 
     * package (default) scoped.
     * 
     * @param member  the member to test, must not be null
     * @return true if the member is package scoped
     */
    public static boolean isPackageScope(Member member) {
        return !(isPublicScope(member) || isProtectedScope(member) || isPrivateScope(member));
    }

    /**
     * Tests whether the specified field, method or constructor is 
     * <code>private</code>.
     * 
     * @param member  the member to test, must not be null
     * @return true if the member is private scoped
     */
    public static boolean isPrivateScope(Member member) {
        if (member == null) {
            throw new IllegalArgumentException("The member must not be null");
        }    
        return Modifier.isPrivate(member.getModifiers());
    }

    // -------------------------------------------------------------------------
    
    /**
     * Gets a class object for the specified string.
     *
     * @param className  fully qualified class name to find, must not be empty
     * @return Class object for class
     * @throws ReflectionException if an error occurs during reflection
     * @throws IllegalArgumentException if the class name is empty
     */
    public static Class getClass(String className) throws ReflectionException {
        if (StringUtils.isEmpty(className)) {
            throw new IllegalArgumentException("The class name must not be null");
        }
        try {
            return Class.forName(className);
    
        } catch (LinkageError ex) {
            throw new ReflectionException(getThrowableText(ex, "getting class", className, null, null), ex);
        } catch (Exception ex) {
            throw new ReflectionException(getThrowableText(ex, "getting class", className, null, null), ex);
        }
    }
    
    // -------------------------------------------------------------------------
    
    /**
     * Checks if the requested Class array is compatible with the specified
     * parameter array.
     * Primitive classes are handled correctly .
     * <p>
     * In other words, a <code>boolean</code> Class will be converted to 
     * a <code>Boolean</code> Class and so on.</p>
     *
     * <p>This method also handles widening for primitives as given in section 5.1.2 of the
     * <em><a href="http://java.sun.com/docs/books/jls/">The Java Language Specification</a></em>.
     *
     * @param requestedTypes  the class array requested
     * @param paramTypes  the actual class array for the method
     * @return true if the parameters are compatible
     */
    public static boolean isCompatible(Class[] requestedTypes, Class[] paramTypes) {
        if (ArrayUtils.isSameLength(requestedTypes, paramTypes) == false) {
            return false;
        }
        if (requestedTypes == null) {
            requestedTypes = ArrayUtils.EMPTY_CLASS_ARRAY;
        }
        if (paramTypes == null) {
            paramTypes = ArrayUtils.EMPTY_CLASS_ARRAY;
        }
        for (int i = 0; i < requestedTypes.length; i++) {
            if (isCompatible(requestedTypes[i], paramTypes[i]) == false) {
                return false;
            }
        }
        return true;
    }
    

    /**
     * <p>Determine whether a type can be used as a parameter in a method invocation.
     * This method handles primitive conversions correctly.</p>
     *
     * <p>This method also handles widening for primitives as given in section 5.1.2 of the
     * <em><a href="http://java.sun.com/docs/books/jls/">The Java Language Specification</a></em>.
     *
     * @param parameterType the type of parameter accepted by the method
     * @param requestedType the type of parameter being requested 
     *
     * @return true if the assignement is compatible.
     */
    public static boolean isCompatible(Class requestedType, Class parameterType) {
        // try plain assignment
        if (ClassUtils.isAssignable(requestedType, parameterType)) {
            return true;
        }
        
        if (parameterType.isPrimitive()) {
            // also, this method does *not* do widening - you must specify exactly
            // is this the right behaviour?
            if (boolean.class.equals(parameterType)) {
                return Boolean.class.equals(requestedType);
            }              
            
            if (byte.class.equals(parameterType)) {
                return Byte.class.equals(requestedType);
            }
            
            if (short.class.equals(parameterType)) {
                return (Short.class.equals(requestedType)
                        || Byte.class.equals(requestedType));
            }                    
            
            if (char.class.equals(parameterType)) {
                return Character.class.equals(requestedType);
            }
                               
            if (int.class.equals(parameterType)) {
                return (Integer.class.equals(requestedType)
                        || Character.class.equals(requestedType)
                        || Short.class.equals(requestedType)
                        || Byte.class.equals(requestedType));
            }       
            if (long.class.equals(parameterType)) {
                return (Long.class.equals(requestedType) 
                        || Integer.class.equals(requestedType)
                        || Character.class.equals(requestedType)
                        || Short.class.equals(requestedType)
                        || Byte.class.equals(requestedType));
            }                   
                
            if (float.class.equals(parameterType)) {
                return (Float.class.equals(requestedType)
                        || Long.class.equals(requestedType)
                        || Integer.class.equals(requestedType)
                        || Character.class.equals(requestedType)
                        || Short.class.equals(requestedType)
                        || Byte.class.equals(requestedType));
            }     
               
            if (double.class.equals(parameterType)) {
                return (Double.class.equals(requestedType)	
                        || Float.class.equals(requestedType)
                        || Long.class.equals(requestedType)
                        || Integer.class.equals(requestedType)
                        || Character.class.equals(requestedType)
                        || Short.class.equals(requestedType)
                        || Byte.class.equals(requestedType));
            }   
        }
        
        return false;
    }

    
    /**
     * Converts a primitive class to its matching object class.
     * Non-primitive classes are unaffected.
     * <p>
     * In other words, a <code>boolean</code> Class will be converted to 
     * a <code>Boolean</code> Class and so on.
     *
     * @param cls  the class to convert
     * @return converted class
     * @throws IllegalArgumentException if the class is null
     */
    public static Class convertPrimitiveClass(Class cls) {
        if (cls == null) {
            throw new IllegalArgumentException("The class must not be null");
        }
        if (cls.isPrimitive()) {
            if (Integer.TYPE.equals(cls)) {
                return Integer.class;
            } else if (Long.TYPE.equals(cls)) {
                return Long.class;
            } else if (Boolean.TYPE.equals(cls)) {
                return Boolean.class;
            } else if (Double.TYPE.equals(cls)) {
                return Double.class;
            } else if (Float.TYPE.equals(cls)) {
                return Float.class;
            } else if (Character.TYPE.equals(cls)) {
                return Character.class;
            } else if (Short.TYPE.equals(cls)) {
                return Short.class;
            } else if (Byte.TYPE.equals(cls)) {
                return Byte.class;
            }         
        }
        return cls;
    }
    
    // -------------------------------------------------------------------------
    
    /**
     * Produces nicely formatted informational error messages for reflection errors.
     * 
     * @param th  the throwable
     * @param desc  the short description of the action, such as 'getting field'
     * @param className  the class name being used
     * @param types  the parameter types
     * @param memberName  the name of the field or method
     * @return a suitable error message
     */
    public static String getThrowableText(Throwable th, String desc, String className, Class[] types, String memberName) {
        String message = null;
        try {
            throw th;
    
        } catch (NoSuchMethodException ex) {
            message = "the method does not exist";
        } catch (NoSuchFieldException ex) {
            message = "the field does not exist";
        } catch (ClassNotFoundException ex) {
            message = "the class could not be found in the classpath";
        } catch (InvocationTargetException ex) {
            message = "the method threw an exception";
        } catch (InstantiationException ex) {
            message = "the class is abstract/interface/array/primitive";
        } catch (IllegalAccessException ex) {
            message = "the method was not public/accessible";
        } catch (IllegalArgumentException ex) {
            message = "the parameters did not match those expected";
        } catch (SecurityException ex) {
            message = "the security manager prevents reflection";
        } catch (ExceptionInInitializerError ex) {
            message = "the class initialization for static variables threw an exception";
        } catch (ClassCircularityError ex) {
            message = "a circularity has been detected while initializing a class";
        } catch (ClassFormatError ex) {
            message = "the class file is malformed or otherwise cannot be interpreted as a class";
        } catch (IncompatibleClassChangeError ex) {
            message = "the method references another class that has changed incompatibly since compile time";
        } catch (UnsatisfiedLinkError ex) {
            message = "no implementation found for a native method";
        } catch (VerifyError ex) {
            message = "the class file contains an internal inconsistency or security problem";
        } catch (NoClassDefFoundError ex) {
            message = "the class references another class that was present at compile time but is no longer available";
        } catch (LinkageError ex) {
            message = "the class references another class that has changed incompatibly since compile time";
        } catch (Throwable ex) {
            message = null;
        }
        StringBuffer buf = new StringBuffer();
        buf.append(ClassUtils.getShortClassName(th, ""));
        buf.append(" while ");
        buf.append(desc);
        buf.append(" on Class '");
        buf.append(className);
        buf.append("'");
        if (types != null) {
            buf.append(" for types ");
            buf.append(ArrayUtils.toString(types));
        }
        if (memberName != null) {
            buf.append(" for method '");
            buf.append(memberName);
            buf.append("'");
        }
        if (message != null) {
            buf.append(" - ");
            buf.append(message);
        }
        return buf.toString();
    }
    
}

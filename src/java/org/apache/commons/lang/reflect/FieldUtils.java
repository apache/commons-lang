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
 *    any, must include the following acknowledgement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowledgement may appear in the software itself,
 *    if and wherever such third-party acknowledgements normally appear.
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

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
/**
 * <p><code>FieldUtils</code> contains utility methods for working with
 * fields by reflection.</p>
 *
 * <p>The ability is provided to break the scoping restrictions coded by the
 * programmer. This can allow fields to be changed that shouldn't be. This
 * facility should be used with care.</p>
 *
 * @author <a href="mailto:scolebourne@apache.org">Stephen Colebourne</a>
 * @version $Id: FieldUtils.java,v 1.4 2003/09/07 14:32:35 psteitz Exp $
 */
public class FieldUtils {
    
    /**
     * An empty field array.
     */
    public static final Field[] EMPTY_FIELD_ARRAY = new Field[0];
    
    /**
     * <p>FieldUtils instances should NOT be constructed in standard programming.</p>
     *
     * <p>Instead, the class should be used as <code>FieldUtils.getField(cls, name)</code>.
     * This constructor is public to permit tools that require a JavaBean instance
     * to operate.</p>
     */
    public FieldUtils() {
    }

    // -------------------------------------------------------------------------
    
    /**
     * <p>Gets an accessible <code>Field</code> by name respecting scope.
     * Superclasses/interfaces will be considered.</p>
     *
     * @param cls  the class to reflect, must not be <code>null</code>
     * @param fieldName  the field name to obtain
     * @return the Field object
     * @throws IllegalArgumentException if the class or field name
     *  is <code>null</code>
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Field getField(Class cls, String fieldName) {
        return getField(cls, fieldName, false);
    }
    
    /**
     * <p>Gets an accessible <code>Field</code> by name breaking scope
     * if requested. Superclasses/interfaces will be considered.</p>
     *
     * @param cls  the class to reflect, must not be <code>null</code>
     * @param fieldName  the field name to obtain
     * @param breakScope  whether to break scope restrictions using the
     *  <code>setAccessible</code> method. <code>False</code> will only
     *  match public fields.
     * @return the Field object
     * @throws IllegalArgumentException if the class or field name
     *  is <code>null</code>
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Field getField(Class cls, String fieldName, boolean breakScope) {
        if (cls == null) {
            throw new IllegalArgumentException("The class must not be null");
        }
        if (fieldName == null) {
            throw new IllegalArgumentException("The field name must not be null");
        }
        // Sun Java 1.3 has a bugged implementation of getField hence we write the
        // code ourselves
        
        // getField() will return the Field object with the declaring class
        // set correctly to the class that declares the field. Thus requesting the
        // field on a subclass will return the field from the superclass.
        //
        // priority order for lookup:
        // searchclass private/protected/package/public
        // superclass protected/package/public
        //  private/different package blocks access to further superclasses
        // implementedinterface public
        try {
            // check up the superclass hierarchy
            Class acls = cls;
            Field match = null;
            while (acls != null && acls != Object.class) {
                // getDeclaredField checks for non-public scopes as well
                // and it returns accurate results
                try {
                    Field field = acls.getDeclaredField(fieldName);
                    if (Modifier.isPublic(field.getModifiers()) == false) {
                        field.setAccessible(breakScope);
                        return field;
                    }
                    if (breakScope == false) {
                        // only public acceptable if not breaking scope
                        throw new IllegalAccessException("The field '" + fieldName + 
                            "' was found, but it's scope prevents direct access by reflection");
                    }
                    field.setAccessible(true);
                    match = field;
                    break;
                    
                } catch (NoSuchFieldException ex) {
                    // ignore
                }
                // next superclass
                acls = acls.getSuperclass();
            }
            // check the public interface case. This must be manually searched for
            // incase there is a public supersuperclass field hidden by a private/package
            // superclass field.
            // check up the superclass hierarchy
            Class[] ints = cls.getInterfaces();
            for (int i = 0; i < ints.length; i++) {
                // getField is fine here, because everything is public, and thus it works
                try {
                    Field field = ints[i].getField(fieldName);
                    return field;
                    
                } catch (NoSuchFieldException ex) {
                    // ignore
                }
            }
            if (match != null) {
                return match;
            }
            throw new NoSuchFieldException("The field '" + fieldName + "' could not be found");
    
        } catch (ReflectionException ex) {
            throw ex;
        } catch (LinkageError ex) {
            throw new ReflectionException(ReflectionUtils.getThrowableText(
                ex, "getting field", cls.getName(), null, fieldName), ex);
        } catch (Exception ex) {
            throw new ReflectionException(ReflectionUtils.getThrowableText(
                ex, "getting field", cls.getName(), null, fieldName), ex);
        }
    }
    
    // -------------------------------------------------------------------------
    
    /**
     * <p>Gets an accessible <code>Field</code> by name respecting scope.
     * Only the specified class will be considered.</p>
     *
     * @param cls  the class to reflect, must not be <code>null</code>
     * @param fieldName  the field name to obtain
     * @return the Field object
     * @throws IllegalArgumentException if the class or field name
     *  is <code>null</code>
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Field getFieldExact(Class cls, String fieldName) {
        return getFieldExact(cls, fieldName, false);
    }
    
    /**
     * <p>Gets an accessible <code>Field</code> by name breaking scope
     * if requested. Only the specified class will be considered.</p>
     *
     * @param cls  the class to reflect, must not be <code>null</code>
     * @param fieldName  the field name to obtain
     * @param breakScope  whether to break scope restrictions using the
     *  <code>setAccessible</code> method. False will only match public fields.
     * @return the Field object
     * @throws IllegalArgumentException if the class or field name
     *  is <code>null</code>
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Field getFieldExact(Class cls, String fieldName, boolean breakScope) {
        if (cls == null) {
            throw new IllegalArgumentException("The class must not be null");
        }
        if (fieldName == null) {
            throw new IllegalArgumentException("The field name must not be null");
        }
        try {
            // only consider the specified class by using getDeclaredField()
            Field field = cls.getDeclaredField(fieldName);
            if (Modifier.isPublic(field.getModifiers()) == false) {
                if (breakScope) {
                    field.setAccessible(true);
                } else {
                    throw new IllegalAccessException("The field '" + fieldName + "' was found, but it's scope prevents direct access by reflection");
                }
            }
            return field;
    
        } catch (ReflectionException ex) {
            throw ex;
        } catch (LinkageError ex) {
            throw new ReflectionException(ReflectionUtils.getThrowableText(
                ex, "getting field", cls.getName(), null, fieldName), ex);
        } catch (Exception ex) {
            throw new ReflectionException(ReflectionUtils.getThrowableText(
                ex, "getting field", cls.getName(), null, fieldName), ex);
        }
    }
    
    // -------------------------------------------------------------------------
    
    /**
     * <p>Gets a static Field value from a <code>Field</code> object.</p>
     * 
     * @param field  the field to use
     * @return the field value
     * @throws IllegalArgumentException if the field is
     *  <code>null</code> or not static
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Object getStaticFieldValue(Field field) {
        if (field == null) {
            throw new IllegalArgumentException("The field must not be null");
        }
        if (Modifier.isStatic(field.getModifiers()) == false) {
            throw new IllegalArgumentException("The field '" + field.getName() + "' is not static");
        }
        return getFieldValue(field, (Object) null, false);
    }
    
    /**
     * <p>Gets a static Field value from a <code>Field</code> object.</p>
     * 
     * @param field  the field to use
     * @param breakScope  whether to break scope restrictions using the
     *  <code>setAccessible</code> method. <code>False</code> will only
     *  match public methods.
     * @return the field value
     * @throws IllegalArgumentException if the field is <code>null</code>
     *  or not static
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Object getStaticFieldValue(Field field, boolean breakScope) {
        if (field == null) {
            throw new IllegalArgumentException("The field must not be null");
        }
        if (Modifier.isStatic(field.getModifiers()) == false) {
            throw new IllegalArgumentException("The field '" + field.getName() + "' is not static");
        }
        return getFieldValue(field, (Object) null, breakScope);
    }
    
    /**
     * <p>Gets a Field value from a <code>Field</code> object.</p>
     * 
     * @param field  the field to use
     * @param object  the object to call on, may be <code>null</code>
     *  for static fields
     * @return the field value
     * @throws IllegalArgumentException if the field is <code>null</code>
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Object getFieldValue(Field field, Object object) {
        return getFieldValue(field, object, false);
    }
    
    /**
     * <p>Gets a Field value from a Field object.</p>
     * 
     * @param field  the field to use
     * @param object  the object to call on, may be <code>null</code>
     *  for static fields
     * @param breakScope  whether to break scope restrictions using the
     *  <code>setAccessible</code> method. <code>False</code> will only
     *  match public methods.
     * @return the field value
     * @throws IllegalArgumentException if the field is <code>null</code>
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Object getFieldValue(Field field, Object object, boolean breakScope) {
        if (field == null) {
            throw new IllegalArgumentException("The field must not be null");
        }
        try {
            if (breakScope && Modifier.isPublic(field.getModifiers()) == false) {
                field.setAccessible(true);
            }
            return field.get(object);
    
        } catch (ReflectionException ex) {
            throw ex;
        } catch (LinkageError ex) {
            throw new ReflectionException(ReflectionUtils.getThrowableText(
                ex, "getting field value", field.getDeclaringClass().getName(), null, field.getName()), ex);
        } catch (Exception ex) {
            throw new ReflectionException(ReflectionUtils.getThrowableText(
                ex, "getting field value", field.getDeclaringClass().getName(), null, field.getName()), ex);
        }
    }
    
    // -------------------------------------------------------------------------
    
    /**
     * <p>Gets a static Field value by name. The field must be public.
     * Superclasses will be considered.</p>
     *
     * @param cls  the class to reflect, must not be <code>null</code>
     * @param fieldName  the field name to obtain
     * @return the value of the field
     * @throws IllegalArgumentException if the class or field name
     *  is <code>null</code>
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Object getStaticFieldValue(Class cls, String fieldName) {
        return getStaticFieldValue(cls, fieldName, false);
    }
    
    /**
     * <p>Gets a static Field value by name. Only the specified class
     * will be considered.</p>
     *
     * @param cls  the class to reflect, must not be <code>null</code>
     * @param fieldName  the field name to obtain
     * @param breakScope  whether to break scope restrictions using the
     *  <code>setAccessible</code> method. <code>False</code> will only
     *  match public fields.
     * @return the Field object
     * @throws IllegalArgumentException if the class or field name
     *  is <code>null</code>
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Object getStaticFieldValue(Class cls, String fieldName, boolean breakScope) {
        try {
            Field field = getField(cls, fieldName, breakScope);
            if (Modifier.isStatic(field.getModifiers()) == false) {
                throw new NoSuchMethodException("The field '" + fieldName + "' is not static");
            }
            return getStaticFieldValue(field, breakScope);
            
        } catch (ReflectionException ex) {
            throw ex;
        } catch (LinkageError ex) {
            throw new ReflectionException(ReflectionUtils.getThrowableText(
                ex, "getting field value", cls.getName(), null, fieldName), ex);
        } catch (Exception ex) {
            throw new ReflectionException(ReflectionUtils.getThrowableText(
                ex, "getting field value", cls.getName(), null, fieldName), ex);
        }
    }
    
    // -------------------------------------------------------------------------
    
    /**
     * <p>Gets a static Field value by name. The field must be public.
     * Only the specified class will be considered.</p>
     *
     * @param cls  the class to reflect, must not be <code>null</code>
     * @param fieldName  the field name to obtain
     * @return the value of the field
     * @throws IllegalArgumentException if the class or field name
     *  is <code>null</code>
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Object getStaticFieldValueExact(Class cls, String fieldName) {
        return getStaticFieldValueExact(cls, fieldName, false);
    }
    
    /**
     * <p>Gets a static Field value by name. Only the specified class will
     * be considered.</p>
     *
     * @param cls  the class to reflect, must not be <code>null</code>
     * @param fieldName  the field name to obtain
     * @param breakScope  whether to break scope restrictions using the
     *  <code>setAccessible</code> method. <code>False</code> will only
     *  match public fields.
     * @return the Field object
     * @throws IllegalArgumentException if the class or field name
     *  is <code>null</code>
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Object getStaticFieldValueExact(Class cls, String fieldName, boolean breakScope) {
        try {
            Field field = getFieldExact(cls, fieldName, breakScope);
            if (Modifier.isStatic(field.getModifiers()) == false) {
                throw new NoSuchMethodException("The field '" + fieldName + "' is not static");
            }
            return getStaticFieldValue(field, breakScope);
            
        } catch (ReflectionException ex) {
            throw ex;
        } catch (LinkageError ex) {
            throw new ReflectionException(ReflectionUtils.getThrowableText(
                ex, "getting field value", cls.getName(), null, fieldName), ex);
        } catch (Exception ex) {
            throw new ReflectionException(ReflectionUtils.getThrowableText(
                ex, "getting field value", cls.getName(), null, fieldName), ex);
        }
    }
    
    // -------------------------------------------------------------------------
    
    /**
     * <p>Gets a Field value by name. The field must be public. Superclasses
     * will be considered.</p>
     *
     * @param object  the object to reflect, must not be <code>null</code>
     * @param fieldName  the field name to obtain
     * @return the value of the field
     * @throws IllegalArgumentException if the class or field name
     *  is <code>null</code>
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Object getFieldValue(Object object, String fieldName) {
        return getFieldValue(object, fieldName, false);
    }
    
    /**
     * <p>Gets a Field value by name. Only the specified class will be
     * considered.</p>
     *
     * @param object  the object to reflect, must not be <code>null</code>
     * @param fieldName  the field name to obtain
     * @param breakScope  whether to break scope restrictions using the
     *  <code>setAccessible</code> method. <code>False</code> will only
     *  match public fields.
     * @return the Field object
     * @throws IllegalArgumentException if the class or field name
     *  is <code>null</code>
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Object getFieldValue(Object object, String fieldName, boolean breakScope) {
        Field field = getField(object.getClass(), fieldName, breakScope);
        return getFieldValue(field, object, breakScope);
    }
    
    // -------------------------------------------------------------------------
    
    /**
     * <p>Gets a Field value by name. The field must be public.
     * Only the class of the specified object will be considered.</p>
     *
     * @param object  the object to reflect, must not be <code>null</code>
     * @param fieldName  the field name to obtain
     * @return the value of the field
     * @throws IllegalArgumentException if the class or field name
     *  is <code>null</code>
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Object getFieldValueExact(Object object, String fieldName) {
        return getFieldValueExact(object, fieldName, false);
    }
    
    /**
     * <p<>Gets a Field value by name. Only the class of the specified
     * object will be considered.</p>
     *
     * @param object  the object to reflect, must not be <code>null</code>
     * @param fieldName  the field name to obtain
     * @param breakScope  whether to break scope restrictions using the
     *  <code>setAccessible</code> method. <code>False</code> will only
     *  match public fields.
     * @return the Field object
     * @throws IllegalArgumentException if the class or field name
     *  is <code>null</code>
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Object getFieldValueExact(Object object, String fieldName, boolean breakScope) {
        Field field = getFieldExact(object.getClass(), fieldName, breakScope);
        return getFieldValue(field, object, breakScope);
    }
    
}

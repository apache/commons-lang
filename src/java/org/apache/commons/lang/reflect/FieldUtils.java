/*
 * Copyright 2003,2004 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.reflect;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

/**
 * Utilities for working with fields by reflection.
 * <p>
 * The ability is provided to break the scoping restrictions coded by the
 * programmer. This can allow fields to be changed that shouldn't be. This
 * facility should be used with care.
 *
 * @author Stephen Colebourne
 * @version $Id$
 * @since Commons Reflect 1.0
 */
public class FieldUtils {
    
    /**
     * An empty field array.
     */
    public static final Field[] EMPTY_FIELD_ARRAY = new Field[0];
    
    /**
     * FieldUtils instances should NOT be constructed in standard programming.
     * <p>
     * This constructor is public to permit tools that require a JavaBean instance
     * to operate.
     */
    public FieldUtils() {
        super();
    }

    //-----------------------------------------------------------------------
    /**
     * Gets an accessible <code>Field</code> by name repecting scope.
     * Superclasses/interfaces will be considered.
     *
     * @param cls  the class to reflect, must not be null
     * @param fieldName  the field name to obtain
     * @return the Field object
     * @throws IllegalArgumentException if the class or field name is null
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Field getField(Class cls, String fieldName) {
        return getField(cls, fieldName, false);
    }
    
    /**
     * Gets an accessible <code>Field</code> by name breaking scope
     * if requested. Superclasses/interfaces will be considered.
     *
     * @param cls  the class to reflect, must not be null
     * @param fieldName  the field name to obtain
     * @param breakScope  whether to break scope restrictions using the
     *  <code>setAccessible</code> method. <code>False</code> will only
     *  match public fields.
     * @return the Field object
     * @throws IllegalArgumentException if the class or field name is null
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
    
    //-----------------------------------------------------------------------
    /**
     * Gets an accessible <code>Field</code> by name respecting scope.
     * Only the specified class will be considered.
     *
     * @param cls  the class to reflect, must not be null
     * @param fieldName  the field name to obtain
     * @return the Field object
     * @throws IllegalArgumentException if the class or field name is null
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Field getFieldExact(Class cls, String fieldName) {
        return getFieldExact(cls, fieldName, false);
    }
    
    /**
     * Gets an accessible <code>Field</code> by name breaking scope
     * if requested. Only the specified class will be considered.
     *
     * @param cls  the class to reflect, must not be null
     * @param fieldName  the field name to obtain
     * @param breakScope  whether to break scope restrictions using the
     *  <code>setAccessible</code> method. False will only match public fields.
     * @return the Field object
     * @throws IllegalArgumentException if the class or field name is null
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
    
    //-----------------------------------------------------------------------
    /**
     * Gets a static Field value from a <code>Field</code> object.
     * 
     * @param field  the field to use
     * @return the field value
     * @throws IllegalArgumentException if the field is null or not static
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
     * Gets a static Field value from a <code>Field</code> object.
     * 
     * @param field  the field to use
     * @param breakScope  whether to break scope restrictions using the
     *  <code>setAccessible</code> method. <code>False</code> will only
     *  match public methods.
     * @return the field value
     * @throws IllegalArgumentException if the field is null or not static
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
     * Gets a Field value from a <code>Field</code> object.
     * 
     * @param field  the field to use
     * @param object  the object to call on, may be null for static fields
     * @return the field value
     * @throws IllegalArgumentException if the field is null
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Object getFieldValue(Field field, Object object) {
        return getFieldValue(field, object, false);
    }
    
    /**
     * Gets a Field value from a Field object.
     * 
     * @param field  the field to use
     * @param object  the object to call on, may be null for static fields
     * @param breakScope  whether to break scope restrictions using the
     *  <code>setAccessible</code> method. <code>False</code> will only
     *  match public methods.
     * @return the field value
     * @throws IllegalArgumentException if the field is null
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
    
    //-----------------------------------------------------------------------
    /**
     * Gets a static Field value by name. The field must be public.
     * Superclasses will be considered.
     *
     * @param cls  the class to reflect, must not be null
     * @param fieldName  the field name to obtain
     * @return the value of the field
     * @throws IllegalArgumentException if the class or field name is null
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Object getStaticFieldValue(Class cls, String fieldName) {
        return getStaticFieldValue(cls, fieldName, false);
    }
    
    /**
     * Gets a static Field value by name. Only the specified class
     * will be considered.
     *
     * @param cls  the class to reflect, must not be null
     * @param fieldName  the field name to obtain
     * @param breakScope  whether to break scope restrictions using the
     *  <code>setAccessible</code> method. <code>False</code> will only
     *  match public fields.
     * @return the Field object
     * @throws IllegalArgumentException if the class or field name is null
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
    
    //-----------------------------------------------------------------------
    /**
     * Gets a static Field value by name. The field must be public.
     * Only the specified class will be considered.
     *
     * @param cls  the class to reflect, must not be null
     * @param fieldName  the field name to obtain
     * @return the value of the field
     * @throws IllegalArgumentException if the class or field name is null
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Object getStaticFieldValueExact(Class cls, String fieldName) {
        return getStaticFieldValueExact(cls, fieldName, false);
    }
    
    /**
     * Gets a static Field value by name. Only the specified class will
     * be considered.
     *
     * @param cls  the class to reflect, must not be null
     * @param fieldName  the field name to obtain
     * @param breakScope  whether to break scope restrictions using the
     *  <code>setAccessible</code> method. <code>False</code> will only
     *  match public fields.
     * @return the Field object
     * @throws IllegalArgumentException if the class or field name is null
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
    
    //-----------------------------------------------------------------------
    /**
     * Gets a Field value by name. The field must be public. Superclasses
     * will be considered.
     *
     * @param object  the object to reflect, must not be null
     * @param fieldName  the field name to obtain
     * @return the value of the field
     * @throws IllegalArgumentException if the class or field name is null
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Object getFieldValue(Object object, String fieldName) {
        return getFieldValue(object, fieldName, false);
    }
    
    /**
     * Gets a Field value by name. Only the specified class will be
     * considered.
     *
     * @param object  the object to reflect, must not be null
     * @param fieldName  the field name to obtain
     * @param breakScope  whether to break scope restrictions using the
     *  <code>setAccessible</code> method. <code>False</code> will only
     *  match public fields.
     * @return the Field object
     * @throws IllegalArgumentException if the class or field name is null
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Object getFieldValue(Object object, String fieldName, boolean breakScope) {
        Field field = getField(object.getClass(), fieldName, breakScope);
        return getFieldValue(field, object, breakScope);
    }
    
    //-----------------------------------------------------------------------
    /**
     * Gets a Field value by name. The field must be public.
     * Only the class of the specified object will be considered.
     *
     * @param object  the object to reflect, must not be null
     * @param fieldName  the field name to obtain
     * @return the value of the field
     * @throws IllegalArgumentException if the class or field name is null
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Object getFieldValueExact(Object object, String fieldName) {
        return getFieldValueExact(object, fieldName, false);
    }
    
    /**
     * <p<>Gets a Field value by name. Only the class of the specified
     * object will be considered.
     *
     * @param object  the object to reflect, must not be null
     * @param fieldName  the field name to obtain
     * @param breakScope  whether to break scope restrictions using the
     *  <code>setAccessible</code> method. <code>False</code> will only
     *  match public fields.
     * @return the Field object
     * @throws IllegalArgumentException if the class or field name is null
     * @throws ReflectionException if an error occurs during reflection
     */
    public static Object getFieldValueExact(Object object, String fieldName, boolean breakScope) {
        Field field = getFieldExact(object.getClass(), fieldName, breakScope);
        return getFieldValue(field, object, breakScope);
    }
    
}

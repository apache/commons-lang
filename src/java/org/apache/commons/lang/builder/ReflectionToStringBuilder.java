package org.apache.commons.lang.builder;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.HashSet;
import java.util.Set;

import org.apache.commons.lang.ClassUtils;

/**
 * <p>Builds <code>toString()</code> values using reflection.</p>
 *
 * <p>This class uses reflection to determine the fields to append. 
 * Because these fields are usually private, the class, 
 * uses <code>Field.setAccessible</code> to
 * change the visibility of the fields. This will fail under a security manager,
 * unless the appropriate permissions are set up correctly.</p>
 *
 * <p>A typical invocation for this method would look like:</p>
 * <pre>
 * public String toString() {
 *   return ReflectionToStringBuilder.toString(this);
 * }
 * </pre>
 *
 * <p>You can also use the builder to debug 3rd party objects:</p>
 * <pre>
 * System.out.println("An object: " + ReflectionToStringBuilder.toString(anObject));
 * </pre>
 * 
 * <p>A subclass can control field output by overriding the methods:
 * <ul> 
 * <li>{@link #accept(java.lang.reflect.Field)}</li>
 * <li>{@link #getValue(java.lang.reflect.Field)}</li>
 * </ul>
 * </p>
 * 
 * <p>The exact format of the <code>toString</code> is determined by
 * the {@link ToStringStyle} passed into the constructor.</p>
 *
 * @author <a href="mailto:ggregory@seagullsw.com">Gary Gregory</a>
 * @author Stephen Colebourne
 * @since 2.0
 * @version $Id: ReflectionToStringBuilder.java,v 1.1 2003/06/03 03:51:56 ggregory Exp $
 */
public class ReflectionToStringBuilder extends ToStringBuilder {

    /**
     * A registry of objects used by <code>reflectionToString</code> methods to detect cyclical object references 
     * and avoid infinite loops.
     */
    private static ThreadLocal registry = new ThreadLocal() {
        protected synchronized Object initialValue() {
            // The HashSet implementation is not synchronized, 
            // which is just what we need here. 
            return new HashSet();
        }
    };

    /**
     * Returns the registry of objects being traversed by the 
     * <code>reflectionToString</code> methods in the current thread.
     * @return Set the registry of objects being traversed 
     */
    static Set getRegistry() {
        return (Set) registry.get();
    }

    /**
     * Returns <code>true</code> if the registry contains the given object.
     * Used by the reflection methods to avoid infinite loops.
     * 
     * @param value The object to lookup in the registry.
     * @return boolean <code>true</code> if the registry contains the given object.
     */
    static boolean isRegistered(Object value) {
        return getRegistry().contains(value);
    }

    /**
     * Registers the given object.
     * Used by the reflection methods to avoid infinite loops.
     * 
     * @param value The object to register.
     */
    static void register(Object value) {
        getRegistry().add(value);
    }
    
    /**
     * <p>This method uses reflection to build a suitable
     * <code>toString</code> using the default <code>ToStringStyle</code>.
     *
     * <p>It uses <code>Field.setAccessible</code> to gain access to private
     * fields. This means that it will throw a security exception if run
     * under a security manger, if the permissions are not set up correctly.
     * It is also not as efficient as testing explicitly.</p>
     *
     * <p>Transient members will be not be included, as they are likely derived.
     * Static fields will not be included. Superclass fields will be appended.</p>
     *
     * @param object  the Object to be output
     * @return the String result
     * @throws IllegalArgumentException if the Object is <code>null</code>
     */
    public static String toString(Object object) {
        return toString(object, null, false, null);
    }

    /**
     * <p>This method uses reflection to build a suitable
     * <code>toString</code>.</p>
     *
     * <p>It uses <code>Field.setAccessible</code> to gain access to private
     * fields. This means that it will throw a security exception if run
     * under a security manger, if the permissions are not set up correctly.
     * It is also not as efficient as testing explicitly.</p>
     *
     * <p>Transient members will be not be included, as they are likely derived.
     * Static fields will not be included. Superclass fields will be appended.</p>
     *
     * <p>If the style is <code>null</code>, the default
     * <code>ToStringStyle</code> is used.</p>
     * 
     * @param object  the Object to be output
     * @param style  the style of the <code>toString</code> to create,
     *  may be <code>null</code>
     * @return the String result
     * @throws IllegalArgumentException if the Object or
     *  <code>ToStringStyle</code> is <code>null</code>
     */
    public static String toString(Object object, ToStringStyle style) {
        return toString(object, style, false, null);
    }

    /**
     * <p>This method uses reflection to build a suitable
     * <code>toString</code>.</p>
     *
     * <p>It uses <code>Field.setAccessible</code> to gain access to private
     * fields. This means that it will throw a security exception if run
     * under a security manger, if the permissions are not set up correctly.
     * It is also not as efficient as testing explicitly. </p>
     *
     * <p>If the <code>outputTransients</code> is <code>true</code>,
     * transient members will be output, otherwise they are ignored,
     * as they are likely derived fields, and not part of the value of the
     * Object.</p>
     *
     * <p>Static fields will not be included. Superclass fields will be appended.</p>
     *
     * <p>
     * If the style is <code>null</code>, the default
     * <code>ToStringStyle</code> is used.</p>
     * 
     * @param object  the Object to be output
     * @param style  the style of the <code>toString</code> to create,
     *  may be <code>null</code>
     * @param outputTransients  whether to include transient fields
     * @return the String result
     * @throws IllegalArgumentException if the Object is <code>null</code>
     */
    public static String toString(Object object, ToStringStyle style, boolean outputTransients) {
        return toString(object, style, outputTransients, null);
    }

    /**
     * <p>This method uses reflection to build a suitable
     * <code>toString</code>.</p>
     *
     * <p>It uses <code>Field.setAccessible</code> to gain access to private
     * fields. This means that it will throw a security exception if run
     * under a security manger, if the permissions are not set up correctly.
     * It is also not as efficient as testing explicitly. </p>
     *
     * <p>If the <code>outputTransients</code> is <code>true</code>,
     * transient members will be output, otherwise they are ignored,
     * as they are likely derived fields, and not part of the value of the
     * Object.</p>
     *
     * <p>Static fields will not be included. Superclass fields will be appended
     * up to and including the specified superclass. A null superclass is treated
     * as <code>java.lang.Object</code>.</p>
     *
     * <p>If the style is <code>null</code>, the default
     * <code>ToStringStyle</code> is used.</p>
     * 
     * @param object  the Object to be output
     * @param style  the style of the <code>toString</code> to create,
     *  may be <code>null</code>
     * @param outputTransients  whether to include transient fields
     * @param reflectUpToClass  the superclass to reflect up to (inclusive), may be null
     * @return the String result
     * @throws IllegalArgumentException if the Object is <code>null</code>
     */
    public static String toString(
        Object object,
        ToStringStyle style,
        boolean outputTransients,
        Class reflectUpToClass) {
        return new ReflectionToStringBuilder(object, style, null, reflectUpToClass, outputTransients).toString();
    }

    /**
     * Unregisters the given object.
     * Used by the reflection methods to avoid infinite loops.
     * 
     * @param value The object to unregister.
     */
    static void unregister(Object value) {
        getRegistry().remove(value);
    }

    /**
     * Whether or not to append transient fields.
     */
    private boolean appendTransients = false;

    /**
     * The last super class to stop appending fields for.
     */
    private Class upToClass = null;

    /**
     * <p>Constructs a new instance.</p>
     *
     * <p>This constructor outputs using the default style set with
     * <code>setDefaultStyle</code>.</p>
     * 
     * @param object  the Object to build a <code>toString</code> for,
     *  must not be <code>null</code>
     * @throws IllegalArgumentException  if the Object passed in is
     *  <code>null</code>
     */
    public ReflectionToStringBuilder(Object object) {
        super(object);
    }

    /**
     * <p>Constructor specifying the output style.</p>
     *
     * <p>If the style is <code>null</code>, the default style is used.</p>
     * 
     * @param object  the Object to build a <code>toString</code> for,
     *  must not be <code>null</code>
     * @param style  the style of the <code>toString</code> to create,
     *  may be <code>null</code>
     * @throws IllegalArgumentException  if the Object passed in is
     *  <code>null</code>
     */
    public ReflectionToStringBuilder(Object object, ToStringStyle style) {
        super(object, style);
    }

    /**
     * <p>Constructors a new instance.</p>
     *
     * <p>If the style is <code>null</code>, the default style is used.</p>
     *
     * <p>If the buffer is <code>null</code>, a new one is created.</p>
     * 
     * @param object  the Object to build a <code>toString</code> for,
     *  must not be <code>null</code>
     * @param style  the style of the <code>toString</code> to create,
     *  may be <code>null</code>
     * @param buffer  the <code>StringBuffer</code> to populate, may be
     *  <code>null</code>
     * @throws IllegalArgumentException  if the Object passed in is
     *  <code>null</code>
     */
    public ReflectionToStringBuilder(Object object, ToStringStyle style, StringBuffer buffer) {
        super(object, style, buffer);
    }

    /**
     * Constructs a new instance.
     * 
     * @param object  the Object to build a <code>toString</code> for,
     *  must not be <code>null</code>
     * @param style  the style of the <code>toString</code> to create,
     *  may be <code>null</code>
     * @param buffer  the <code>StringBuffer</code> to populate, may be
     *  <code>null</code>
     */
    public ReflectionToStringBuilder(
        Object object,
        ToStringStyle style,
        StringBuffer buffer,
        Class reflectUpToClass,
        boolean outputTransients) {
        super(object, style, buffer);
        this.setUpToClass(reflectUpToClass);
        this.setAppendTransients(outputTransients);
    }

    /**
     * Returns whether or not to append the given <code>Field</code>.
     * <ul>
     * <li>Static fields are not appended.</li>
     * <li>Transient fields are appended only if {@link #isAppendTransients()} returns <code>true</code>.
     * <li>Inner class fields are not appened.</li>
     * </ul>
     * @param field The Field to test.
     * @return Whether or not to append the given <code>Field</code>.
     */
    protected boolean accept(Field field) {
        String fieldName = field.getName();
        return (fieldName.indexOf(ClassUtils.INNER_CLASS_SEPARATOR_CHAR) == -1)
            && (this.isAppendTransients() || !Modifier.isTransient(field.getModifiers()))
            && (!Modifier.isStatic(field.getModifiers()));
    }

    /**
     * Appends the fields and values defined by the given object of the
     * given Class. If a cycle is detected as an objects is "toString()'ed",
     * such an object is rendered as if <code>Object.toString()</code> 
     * had been called and not implemented by the object.
     * 
     * @param clazz The class of object parameter
     */
    protected void appendFieldsIn(Class clazz) {
        if (isRegistered(this.getObject())) {
            // The object has already been appended, therefore we have an object cycle. 
            // Append a simple Object.toString style string. The field name is already appended at this point.
            this.appendAsObjectToString(this.getObject());
            return;
        }
        try {
            this.registerObject();
            if (clazz.isArray()) {
                this.reflectionAppendArray(this.getObject());
                return;
            }
            Field[] fields = clazz.getDeclaredFields();
            Field.setAccessible(fields, true);
            for (int i = 0; i < fields.length; i++) {
                Field field = fields[i];
                String fieldName = field.getName();
                if (this.accept(field)) {
                    try {
                        // Warning: Field.get(Object) creates wrappers objects for primitive types.
                        Object fieldValue = this.getValue(field);
                        if (isRegistered(fieldValue) && !field.getType().isPrimitive()) {
                            // A known field value has already been appended, therefore we have an object cycle, 
                            // append a simple Object.toString style string.
                            this.getStyle().appendFieldStart(this.getStringBuffer(), fieldName);
                            this.appendAsObjectToString(fieldValue);
                            // The recursion out of 
                            //    builder.append(fieldName, fieldValue); 
                            // below will append the field 
                            // end marker.
                        } else {
                            try {
                                this.registerObject();
                                this.append(fieldName, fieldValue);
                            } finally {
                                this.unregisterObject();
                            }
                        }
                    } catch (IllegalAccessException ex) {
                        //this can't happen. Would get a Security exception instead
                        //throw a runtime exception in case the impossible happens.
                        throw new InternalError("Unexpected IllegalAccessException: " + ex.getMessage());
                    }
                }
            }
        } finally {
            this.unregisterObject();
        }
    }
    
    /**
     * Gets the last super class to stop appending fields for.
     * 
     * @return The last super class to stop appending fields for.
     */
    public Class getUpToClass() {
        return this.upToClass;
    }

    /**
     * Calls <code>java.lang.reflect.Field.get(Object)</code>
     * @see java.lang.reflect.Field#get(Object)
     * @throws IllegalArgumentException
     * @throws IllegalAccessException
     */
    protected Object getValue(Field field) throws IllegalArgumentException, IllegalAccessException {
        return field.get(this.getObject());
    }

    /**
     * Returns whether or not to append transient fields.
     * 
     * @return Whether or not to append transient fields.
     */
    public boolean isAppendTransients() {
        return this.appendTransients;
    }
    
    /**
     * <p>Append to the <code>toString</code> an <code>Object</code>
     * array.</p>
     *
     * @param array  the array to add to the <code>toString</code>
     * @return this
     */
    public ToStringBuilder reflectionAppendArray(Object array) {
        this.getStyle().reflectionAppendArrayDetail(this.getStringBuffer(), null, array);
        return this;
    }

    /**
     * Registers this builder's source object to avoid infinite loops processing circular object references.
     */
    void registerObject() {
        register(this.getObject());
    }

    /**
     * Sets whether or not to append transient fields.
     * 
     * @param appendTransients Whether or not to append transient fields.
     */
    public void setAppendTransients(boolean appendTransients) {
        this.appendTransients = appendTransients;
    }

    /**
     * Sets the last super class to stop appending fields for.
     * 
     * @param clazz The last super class to stop appending fields for.
     */
    public void setUpToClass(Class clazz) {
        this.upToClass = clazz;
    }

    public String toString() {
        if (this.getObject() == null) {
            return this.getStyle().getNullText();
        }
        Class clazz = this.getObject().getClass();
        this.appendFieldsIn(clazz);
        while (clazz.getSuperclass() != null && clazz != this.getUpToClass()) {
            clazz = clazz.getSuperclass();
            this.appendFieldsIn(clazz);
        }
        return super.toString();
    }

    /**
     * Unegisters this builder's source object to avoid infinite loops processing circular object references.
     */
    void unregisterObject() {
        unregister(this.getObject());
    }

}

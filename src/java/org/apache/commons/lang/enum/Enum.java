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
package org.apache.commons.lang.enum;

import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.ClassUtils;
import org.apache.commons.lang.StringUtils;

/**
 * <p>Abstract superclass for type-safe enums.</p>
 *
 * <p>One feature of the C programming language lacking in Java is enumerations. The
 * C implementation based on ints was poor and open to abuse. The original Java
 * recommendation and most of the JDK also uses int constants. It has been recognised
 * however that a more robust type-safe class-based solution can be designed. This
 * class follows the basic Java type-safe enumeration pattern.</p>
 *
 * <p><em>NOTE:</em>Due to the way in which Java ClassLoaders work, comparing
 * Enum objects should always be done using <code>equals()</code>, not <code>==</code>.
 * The equals() method will try == first so in most cases the effect is the same.</p>
 * 
 * <p>Of course, if you actually want (or don't mind) Enums in different class
 * loaders being non-equal, then you can use <code>==</code>.</p>
 * 
 * <h4>Simple Enums</h4>
 *
 * <p>To use this class, it must be subclassed. For example:</p>
 *
 * <pre>
 * public final class ColorEnum extends Enum {
 *   public static final ColorEnum RED = new ColorEnum("Red");
 *   public static final ColorEnum GREEN = new ColorEnum("Green");
 *   public static final ColorEnum BLUE = new ColorEnum("Blue");
 *
 *   private ColorEnum(String color) {
 *     super(color);
 *   }
 * 
 *   public static ColorEnum getEnum(String color) {
 *     return (ColorEnum) getEnum(ColorEnum.class, color);
 *   }
 * 
 *   public static Map getEnumMap() {
 *     return getEnumMap(ColorEnum.class);
 *   }
 * 
 *   public static List getEnumList() {
 *     return getEnumList(ColorEnum.class);
 *   }
 * 
 *   public static Iterator iterator() {
 *     return iterator(ColorEnum.class);
 *   }
 * }
 * </pre>
 *
 * <p>As shown, each enum has a name. This can be accessed using <code>getName</code>.</p>
 *
 * <p>The <code>getEnum</code> and <code>iterator</code> methods are recommended.
 * Unfortunately, Java restrictions require these to be coded as shown in each subclass.
 * An alternative choice is to use the {@link EnumUtils} class.</p>
 * 
 * <h4>Subclassed Enums</h4>
 * <p>A hierarchy of Enum classes can be built. In this case, the superclass is
 * unaffected by the addition of subclasses (as per normal Java). The subclasses
 * may add additional Enum constants <em>of the type of the superclass</em>. The
 * query methods on the subclass will return all of the Enum constants from the
 * superclass and subclass.</p>
 *
 * <pre>
 * public final class ExtraColorEnum extends ColorEnum {
 *   // NOTE: Color enum declared above is final, change that to get this
 *   // example to compile.
 *   public static final ColorEnum YELLOW = new ExtraColorEnum("Yellow");
 *
 *   private ExtraColorEnum(String color) {
 *     super(color);
 *   }
 * 
 *   public static ColorEnum getEnum(String color) {
 *     return (ColorEnum) getEnum(ExtraColorEnum.class, color);
 *   }
 * 
 *   public static Map getEnumMap() {
 *     return getEnumMap(ExtraColorEnum.class);
 *   }
 * 
 *   public static List getEnumList() {
 *     return getEnumList(ExtraColorEnum.class);
 *   }
 * 
 *   public static Iterator iterator() {
 *     return iterator(ExtraColorEnum.class);
 *   }
 * }
 * </pre>
 *
 * <p>This example will return RED, GREEN, BLUE, YELLOW from the List and iterator
 * methods in that order. The RED, GREEN and BLUE instances will be the same (==) 
 * as those from the superclass ColorEnum. Note that YELLOW is declared as a
 * ColorEnum and not an ExtraColorEnum.</p>
 * 
 * <h4>Functional Enums</h4>
 *
 * <p>The enums can have functionality by defining subclasses and
 * overriding the <code>getEnumClass()</code> method:</p>
 * 
 * <pre>
 *   public static final OperationEnum PLUS = new PlusOperation();
 *   private static final class PlusOperation extends OperationEnum {
 *     private PlusOperation() {
 *       super("Plus");
 *     }
 *     public int eval(int a, int b) {
 *       return (a + b);
 *     }
 *   }
 *   public static final OperationEnum MINUS = new MinusOperation();
 *   private static final class MinusOperation extends OperationEnum {
 *     private MinusOperation() {
 *       super("Minus");
 *     }
 *     public int eval(int a, int b) {
 *       return (a - b);
 *     }
 *   }
 *
 *   private OperationEnum(String color) {
 *     super(color);
 *   }
 * 
 *   public final Class getEnumClass() {     // NOTE: new method!
 *     return OperationEnum.class;
 *   }
 *
 *   public abstract double eval(double a, double b);
 * 
 *   public static OperationEnum getEnum(String name) {
 *     return (OperationEnum) getEnum(OperationEnum.class, name);
 *   }
 * 
 *   public static Map getEnumMap() {
 *     return getEnumMap(OperationEnum.class);
 *   }
 * 
 *   public static List getEnumList() {
 *     return getEnumList(OperationEnum.class);
 *   }
 * 
 *   public static Iterator iterator() {
 *     return iterator(OperationEnum.class);
 *   }
 * }
 * </pre>
 * <p>The code above will work on JDK 1.2. If JDK1.3 and later is used,
 * the subclasses may be defined as anonymous.</p>
 *
 * @author Apache Avalon project
 * @author Stephen Colebourne
 * @author Chris Webb
 * @author Mike Bowler
 * @since 1.0
 * @version $Id: Enum.java,v 1.19 2003/08/05 00:24:02 scolebourne Exp $
 */
public abstract class Enum implements Comparable, Serializable {

    /** Lang version 1.0.1 serial compatability */
    private static final long serialVersionUID = -487045951170455942L;
    
    // After discussion, the default size for HashMaps is used, as the
    // sizing algorithm changes across the JDK versions
    /**
     * An empty <code>Map</code>, as JDK1.2 didn't have an empty map.
     */
    private static final Map EMPTY_MAP = Collections.unmodifiableMap(new HashMap(0));
    /**
     * <code>Map</code>, key of class name, value of <code>Entry</code>.
     */
    private static final Map cEnumClasses = new HashMap();
    /**
     * The string representation of the Enum.
     */
    private final String iName;
    /**
     * The hashcode representation of the Enum.
     */
    private transient final int iHashCode;
    /**
     * The toString representation of the Enum.
     */
    protected transient String iToString = null;

    /**
     * <p>Enable the iterator to retain the source code order.</p>
     */
    private static class Entry {
        /**
         * Map of Enum name to Enum.
         */
        final Map map = new HashMap();
        /**
         * Map of Enum name to Enum.
         */
        final Map unmodifiableMap = Collections.unmodifiableMap(map);
        /**
         * List of Enums in source code order.
         */
        final List list = new ArrayList(25);
        /**
         * Map of Enum name to Enum.
         */
        final List unmodifiableList = Collections.unmodifiableList(list);

        /**
         * <p>Restrictive constructor.</p>
         */
        private Entry() {
        }
    }

    /**
     * <p>Constructor to add a new named item to the enumeration.</p>
     *
     * @param name  the name of the enum object,
     *  must not be empty or <code>null</code>
     * @throws IllegalArgumentException if the name is <code>null</code>
     *  or an empty string
     * @throws IllegalArgumentException if the getEnumClass() method returns
     *  a null or invalid Class
     */
    protected Enum(String name) {
        super();
        init(name);
        iName = name;
        iHashCode = 7 + getEnumClass().hashCode() + 3 * name.hashCode();
        // cannot create toString here as subclasses may want to include other data
    }

    /**
     * Initializes the enumeration.
     * 
     * @param name  the enum name
     * @throws IllegalArgumentException if the name is null or empty or duplicate
     * @throws IllegalArgumentException if the enumClass is null or invalid
     */
    private void init(String name) {
        if (StringUtils.isEmpty(name)) {
            throw new IllegalArgumentException("The Enum name must not be empty or null");
        }
        
        Class enumClass = getEnumClass();
        if (enumClass == null) {
            throw new IllegalArgumentException("getEnumClass() must not be null");
        }
        Class cls = getClass();
        boolean ok = false;
        while (cls != null && cls != Enum.class && cls != ValuedEnum.class) {
            if (cls == enumClass) {
                ok = true;
                break;
            }
            cls = cls.getSuperclass();
        }
        if (ok == false) {
            throw new IllegalArgumentException("getEnumClass() must return a superclass of this class");
        }
        
        // create entry
        Entry entry = (Entry) cEnumClasses.get(enumClass);
        if (entry == null) {
            entry = createEntry(enumClass);
            cEnumClasses.put(enumClass, entry);
        }
        if (entry.map.containsKey(name)) {
            throw new IllegalArgumentException("The Enum name must be unique, '" + name + "' has already been added");
        }
        entry.map.put(name, this);
        entry.list.add(this);
    }

    /**
     * <p>Handle the deserialization of the class to ensure that multiple
     * copies are not wastefully created, or illegal enum types created.</p>
     *
     * @return the resolved object
     */
    protected Object readResolve() {
        Entry entry = (Entry) cEnumClasses.get(getEnumClass());
        if (entry == null) {
            return null;
        }
        return (Enum) entry.map.get(getName());
    }
    
    //--------------------------------------------------------------------------------

    /**
     * <p>Gets an <code>Enum</code> object by class and name.</p>
     * 
     * @param enumClass  the class of the Enum to get, must not
     *  be <code>null</code>
     * @param name  the name of the <code>Enum</code> to get,
     *  may be <code>null</code>
     * @return the enum object, or null if the enum does not exist
     * @throws IllegalArgumentException if the enum class
     *  is <code>null</code>
     */
    protected static Enum getEnum(Class enumClass, String name) {
        Entry entry = getEntry(enumClass);
        if (entry == null) {
            return null;
        }
        return (Enum) entry.map.get(name);
    }

    /**
     * <p>Gets the <code>Map</code> of <code>Enum</code> objects by
     * name using the <code>Enum</code> class.</p>
     *
     * <p>If the requested class has no enum objects an empty
     * <code>Map</code> is returned.</p>
     * 
     * @param enumClass  the class of the <code>Enum</code> to get,
     *  must not be <code>null</code>
     * @return the enum object Map
     * @throws IllegalArgumentException if the enum class is <code>null</code>
     * @throws IllegalArgumentException if the enum class is not a subclass of Enum
     */
    protected static Map getEnumMap(Class enumClass) {
        Entry entry = getEntry(enumClass);
        if (entry == null) {
            return EMPTY_MAP;
        }
        return entry.unmodifiableMap;
    }

    /**
     * <p>Gets the <code>List</code> of <code>Enum</code> objects using the
     * <code>Enum</code> class.</p>
     *
     * <p>The list is in the order that the objects were created (source code order).
     * If the requested class has no enum objects an empty <code>List</code> is
     * returned.</p>
     * 
     * @param enumClass  the class of the <code>Enum</code> to get,
     *  must not be <code>null</code>
     * @return the enum object Map
     * @throws IllegalArgumentException if the enum class is <code>null</code>
     * @throws IllegalArgumentException if the enum class is not a subclass of Enum
     */
    protected static List getEnumList(Class enumClass) {
        Entry entry = getEntry(enumClass);
        if (entry == null) {
            return Collections.EMPTY_LIST;
        }
        return entry.unmodifiableList;
    }

    /**
     * <p>Gets an <code>Iterator</code> over the <code>Enum</code> objects in
     * an <code>Enum</code> class.</p>
     *
     * <p>The <code>Iterator</code> is in the order that the objects were
     * created (source code order). If the requested class has no enum
     * objects an empty <code>Iterator</code> is returned.</p>
     * 
     * @param enumClass  the class of the <code>Enum</code> to get,
     *  must not be <code>null</code>
     * @return an iterator of the Enum objects
     * @throws IllegalArgumentException if the enum class is <code>null</code>
     * @throws IllegalArgumentException if the enum class is not a subclass of Enum
     */
    protected static Iterator iterator(Class enumClass) {
        return Enum.getEnumList(enumClass).iterator();
    }

    //-----------------------------------------------------------------------
    /**
     * <p>Gets an <code>Entry</code> from the map of Enums.</p>
     * 
     * @param enumClass  the class of the <code>Enum</code> to get
     * @return the enum entry
     */
    private static Entry getEntry(Class enumClass) {
        if (enumClass == null) {
            throw new IllegalArgumentException("The Enum Class must not be null");
        }
        if (Enum.class.isAssignableFrom(enumClass) == false) {
            throw new IllegalArgumentException("The Class must be a subclass of Enum");
        }
        Entry entry = (Entry) cEnumClasses.get(enumClass);
        return entry;
    }
    
    /**
     * <p>Creates an <code>Entry</code> for storing the Enums.</p>
     *
     * <p>This accounts for subclassed Enums.</p>
     * 
     * @param enumClass  the class of the <code>Enum</code> to get
     * @return the enum entry
     */
    private static Entry createEntry(Class enumClass) {
        Entry entry = new Entry();
        Class cls = enumClass.getSuperclass();
        while (cls != null && cls != Enum.class && cls != ValuedEnum.class) {
            Entry loopEntry = (Entry) cEnumClasses.get(cls);
            if (loopEntry != null) {
                entry.list.addAll(loopEntry.list);
                entry.map.putAll(loopEntry.map);
                break;  // stop here, as this will already have had superclasses added
            }
            cls = cls.getSuperclass();
        }
        return entry;
    }
    
    //-----------------------------------------------------------------------
    /**
     * <p>Retrieve the name of this Enum item, set in the constructor.</p>
     * 
     * @return the <code>String</code> name of this Enum item
     */
    public final String getName() {
        return iName;
    }

    /**
     * <p>Retrieves the Class of this Enum item, set in the constructor.</p>
     * 
     * <p>This is normally the same as <code>getClass()</code>, but for
     * advanced Enums may be different. If overridden, it must return a
     * constant value.</p>
     * 
     * @return the <code>Class</code> of the enum
     */
    public Class getEnumClass() {
        return getClass();
    }

    /**
     * <p>Tests for equality.</p>
     *
     * <p>Two Enum objects are considered equal
     * if they have the same class names and the same names.
     * Identity is tested for first, so this method usually runs fast.</p>
     *
     * @param other  the other object to compare for equality
     * @return <code>true</code> if the Enums are equal
     */
    public final boolean equals(Object other) {
        if (other == this) {
            return true;
        } else if (other == null) {
            return false;
        } else if (other.getClass() == this.getClass()) {
            // shouldn't happen, but...
            return iName.equals(((Enum) other).iName);
        } else if (((Enum) other).getEnumClass().getName().equals(getEnumClass().getName())) {
            // different classloaders
            try {
                // try to avoid reflection
                return iName.equals(((Enum) other).iName);

            } catch (ClassCastException ex) {
                // use reflection
                try {
                    Method mth = other.getClass().getMethod("getName", null);
                    String name = (String) mth.invoke(other, null);
                    return iName.equals(name);
                } catch (NoSuchMethodException ex2) {
                    // ignore - should never happen
                } catch (IllegalAccessException ex2) {
                    // ignore - should never happen
                } catch (InvocationTargetException ex2) {
                    // ignore - should never happen
                }
                return false;
            }
        } else {
            return false;
        }
    }

    /**
     * <p>Returns a suitable hashCode for the enumeration.</p>
     *
     * @return a hashcode based on the name
     */
    public final int hashCode() {
        return iHashCode;
    }

    /**
     * <p>Tests for order.</p>
     *
     * <p>The default ordering is alphabetic by name, but this
     * can be overridden by subclasses.</p>
     * 
     * @see java.lang.Comparable#compareTo(Object)
     * @param other  the other object to compare to
     * @return -ve if this is less than the other object, +ve if greater
     *  than, <code>0</code> of equal
     * @throws ClassCastException if other is not an Enum
     * @throws NullPointerException if other is <code>null</code>
     */
    public int compareTo(Object other) {
        if (other == this) {
            return 0;
        }
        return iName.compareTo(((Enum) other).iName);
    }

    /**
     * <p>Human readable description of this Enum item.</p>
     * 
     * @return String in the form <code>type[name]</code>, for example:
     * <code>Color[Red]</code>. Note that the package name is stripped from
     * the type name.
     */
    public String toString() {
        if (iToString == null) {
            String shortName = ClassUtils.getShortClassName(getEnumClass());
            iToString = shortName + "[" + getName() + "]";
        }
        return iToString;
    }
    
}

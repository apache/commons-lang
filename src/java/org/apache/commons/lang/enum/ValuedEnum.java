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

import java.util.Iterator;
import java.util.List;

/**
 * <p>Abstract superclass for type-safe enums with integer values suitable
 * for use in <code>switch</code> statements.</p>
 *
 * <p><em>NOTE:</em>Due to the way in which Java ClassLoaders work, comparing
 * <code>Enum</code> objects should always be done using the equals() method,
 * not <code>==</code>. The equals() method will try <code>==</code> first so
 * in most cases the effect is the same.</p>
 *
 * <p>To use this class, it must be subclassed. For example:</p>
 *
 * <pre>
 * public final class JavaVersionEnum extends ValuedEnum {
 *   //standard enums for version of JVM
 *   public static final int  JAVA1_0_VALUE  = 100;
 *   public static final int  JAVA1_1_VALUE  = 110;
 *   public static final int  JAVA1_2_VALUE  = 120;
 *   public static final int  JAVA1_3_VALUE  = 130;
 *   public static final JavaVersionEnum  JAVA1_0  = new JavaVersionEnum( "Java 1.0", JAVA1_0_VALUE );
 *   public static final JavaVersionEnum  JAVA1_1  = new JavaVersionEnum( "Java 1.1", JAVA1_1_VALUE );
 *   public static final JavaVersionEnum  JAVA1_2  = new JavaVersionEnum( "Java 1.2", JAVA1_2_VALUE );
 *   public static final JavaVersionEnum  JAVA1_3  = new JavaVersionEnum( "Java 1.3", JAVA1_3_VALUE );
 *
 *   private JavaVersionEnum(String name, int value) {
 *     super( name, value );
 *   }
 * 
 *   public static JavaVersionEnum getEnum(String javaVersion) {
 *     return (JavaVersionEnum) getEnum(JavaVersionEnum.class, javaVersion);
 *   }
 * 
 *   public static JavaVersionEnum getEnum(int javaVersion) {
 *     return (JavaVersionEnum) getEnum(JavaVersionEnum.class, javaVersion);
 *   }
 * 
 *   public static Map getEnumMap() {
 *     return getEnumMap(JavaVersionEnum.class);
 *   }
 * 
 *   public static List getEnumList() {
 *     return getEnumList(JavaVersionEnum.class);
 *   }
 * 
 *   public static Iterator iterator() {
 *     return iterator(JavaVersionEnum.class);
 *   }
 * }
 * </pre>
 *
 * <p>The above class could then be used as follows:</p>
 *
 * <pre>
 * public void doSomething(JavaVersion ver) {
 *   switch (ver.getValue()) {
 *     case JAVA1_0_VALUE:
 *       // ...
 *       break;
 *     case JAVA1_1_VALUE:
 *       // ...
 *       break;
 *     //...
 *   }
 * }
 * </pre>
 *
 * <p>As shown, each enum has a name and a value. These can be accessed using
 * <code>getName</code> and <code>getValue</code>.</p>
 *
 * <p>The <code>getEnum</code> and <code>iterator</code> methods are recommended.
 * Unfortunately, Java restrictions require these to be coded as shown in each subclass.
 * An alternative choice is to use the {@link EnumUtils} class.</p>
 *
 * @author Apache Avalon project
 * @author Stephen Colebourne
 * @since 1.0
 * @version $Id: ValuedEnum.java,v 1.8 2003/07/20 15:49:58 scolebourne Exp $
 */
public abstract class ValuedEnum extends Enum {
    /**
     * The value contained in enum.
     */
    private final int iValue;

    /**
     * Constructor for enum item.
     *
     * @param name the name of enum item.
     * @param value the value of enum item.
     */
    protected ValuedEnum(String name, int value) {
        super(name);
        iValue = value;
    }

    /**
     * <p>Gets an <code>Enum</code> object by class and value.</p>
     *
     * <p>This method loops through the list of <code>Enum</code>,
     * thus if there are many <code>Enum</code>s this will be
     * slow.</p>
     * 
     * @param enumClass  the class of the <code>Enum</code> to get
     * @param value  the value of the <code>Enum</code> to get
     * @return the enum object, or null if the enum does not exist
     * @throws IllegalArgumentException if the enum class is <code>null</code>
     */
    protected static Enum getEnum(Class enumClass, int value) {
        if (enumClass == null) {
            throw new IllegalArgumentException("The Enum Class must not be null");
        }
        List list = Enum.getEnumList(enumClass);
        for (Iterator it = list.iterator(); it.hasNext();) {
            ValuedEnum enum = (ValuedEnum) it.next();
            if (enum.getValue() == value) {
                return enum;
            }
        }
        return null;
    }

    /**
     * <p>Get value of enum item.</p>
     *
     * @return the enum item's value.
     */
    public final int getValue() {
        return iValue;
    }

    /**
     * <p>Tests for order.</p>
     *
     * <p>The default ordering is numeric by value, but this
     * can be overridden by subclasses.</p>
     * 
     * @see java.lang.Comparable#compareTo(Object)
     * @param other  the other object to compare to
     * @return -ve if this is less than the other object, +ve if greater than,
     *  <code>0</code> of equal
     * @throws ClassCastException if other is not an <code>Enum</code>
     * @throws NullPointerException if other is <code>null</code>
     */
    public int compareTo(Object other) {
        return iValue - ((ValuedEnum) other).iValue;
    }

    /**
     * <p>Human readable description of this <code>Enum</code> item.</p>
     *
     * <p>For use when debugging.</p>
     * 
     * @return String in the form <code>type[name=value]</code>, for example:
     *  <code>JavaVersion[Java 1.0=100]</code>. Note that the package name is
     *  stripped from the type name.
     */
    public String toString() {
        String shortName = Enum.getEnumClass(getClass()).getName();
        int pos = shortName.lastIndexOf('.');
        if (pos != -1) {
            shortName = shortName.substring(pos + 1);
        }
        shortName = shortName.replace('$', '.');
        return shortName + "[" + getName() + "=" + getValue() + "]";
    }
}

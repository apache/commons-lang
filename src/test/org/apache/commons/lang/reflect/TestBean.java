/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//lang/src/test/org/apache/commons/lang/reflect/Attic/TestBean.java,v 1.1 2002/11/14 18:53:36 rdonkin Exp $
 * $Revision: 1.1 $
 * $Date: 2002/11/14 18:53:36 $
 *
 * ====================================================================
 *
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999-2002 The Apache Software Foundation.  All rights
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
 *    permission of the Apache Group.
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
 *
 */


package org.apache.commons.lang.reflect;


import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


/**
 * General purpose test bean for JUnit tests for the "beanutils" component.
 *
 * @author Craig R. McClanahan
 * @version $Revision: 1.1 $ $Date: 2002/11/14 18:53:36 $
 */

public class TestBean {


    // ------------------------------------------------------------- Properties


    /**
     * A boolean property.
     */
    private boolean booleanProperty = true;

    public boolean getBooleanProperty() {
        return (booleanProperty);
    }

    public void setBooleanProperty(boolean booleanProperty) {
        this.booleanProperty = booleanProperty;
    }


    /**
     * A boolean property that uses an "is" method for the getter.
     */
    private boolean booleanSecond = true;

    public boolean isBooleanSecond() {
        return (booleanSecond);
    }

    public void setBooleanSecond(boolean booleanSecond) {
        this.booleanSecond = booleanSecond;
    }


    /**
     * A double property.
     */
    private double doubleProperty = 321.0;

    public double getDoubleProperty() {
        return (this.doubleProperty);
    }

    public void setDoubleProperty(double doubleProperty) {
        this.doubleProperty = doubleProperty;
    }


    /**
     * An "indexed property" accessible via both array and subscript
     * based getters and setters.
     */
    private String dupProperty[] =
    { "Dup 0", "Dup 1", "Dup 2", "Dup 3", "Dup 4" };

    public String[] getDupProperty() {
        return (this.dupProperty);
    }

    public String getDupProperty(int index) {
        return (this.dupProperty[index]);
    }

    public void setDupProperty(int index, String value) {
        this.dupProperty[index] = value;
    }

    public void setDupProperty(String dupProperty[]) {
        this.dupProperty = dupProperty;
    }


    /**
     * A float property.
     */
    private float floatProperty = (float) 123.0;

    public float getFloatProperty() {
        return (this.floatProperty);
    }

    public void setFloatProperty(float floatProperty) {
        this.floatProperty = floatProperty;
    }


    /**
     * An integer array property accessed as an array.
     */
    private int intArray[] = { 0, 10, 20, 30, 40 };

    public int[] getIntArray() {
        return (this.intArray);
    }

    public void setIntArray(int intArray[]) {
        this.intArray = intArray;
    }


    /**
     * An integer array property accessed as an indexed property.
     */
    private int intIndexed[] = { 0, 10, 20, 30, 40 };

    public int getIntIndexed(int index) {
        return (intIndexed[index]);
    }

    public void setIntIndexed(int index, int value) {
        intIndexed[index] = value;
    }


    /**
     * An integer property.
     */
    private int intProperty = 123;

    public int getIntProperty() {
        return (this.intProperty);
    }

    public void setIntProperty(int intProperty) {
        this.intProperty = intProperty;
    }


    /**
     * A List property accessed as an indexed property.
     */
    private static List listIndexed = new ArrayList();

    static {
        listIndexed.add("String 0");
        listIndexed.add("String 1");
        listIndexed.add("String 2");
        listIndexed.add("String 3");
        listIndexed.add("String 4");
    }

    public List getListIndexed() {
        return (listIndexed);
    }


    /**
     * A long property.
     */
    private long longProperty = 321;

    public long getLongProperty() {
        return (this.longProperty);
    }

    public void setLongProperty(long longProperty) {
        this.longProperty = longProperty;
    }


    /**
     * A mapped property with only a getter and setter for a Map.
     */
    private Map mapProperty = null;

    public Map getMapProperty() {
        // Create the map the very first time
        if (mapProperty == null) {
            mapProperty = new HashMap();
            mapProperty.put("First Key", "First Value");
            mapProperty.put("Second Key", "Second Value");
        }
        return (mapProperty);
    }

    public void setMapProperty(Map mapProperty) {
        // Create the map the very first time
        if (mapProperty == null) {
            mapProperty = new HashMap();
            mapProperty.put("First Key", "First Value");
            mapProperty.put("Second Key", "Second Value");
        }
        this.mapProperty = mapProperty;
    }


    /**
     * A mapped property that has String keys and Object values.
     */
    private HashMap mappedObjects = null;

    public Object getMappedObjects(String key) {
        // Create the map the very first time
        if (mappedObjects == null) {
            mappedObjects = new HashMap();
            mappedObjects.put("First Key", "First Value");
            mappedObjects.put("Second Key", "Second Value");
        }
        return (mappedObjects.get(key));
    }

    public void setMappedObjects(String key, Object value) {
        // Create the map the very first time
        if (mappedObjects == null) {
            mappedObjects = new HashMap();
            mappedObjects.put("First Key", "First Value");
            mappedObjects.put("Second Key", "Second Value");
        }
        mappedObjects.put(key, value);
    }


    /**
     * A mapped property that has String keys and String values.
     */
    private HashMap mappedProperty = null;

    public String getMappedProperty(String key) {
        // Create the map the very first time
        if (mappedProperty == null) {
            mappedProperty = new HashMap();
            mappedProperty.put("First Key", "First Value");
            mappedProperty.put("Second Key", "Second Value");
        }
        return ((String) mappedProperty.get(key));
    }

    public void setMappedProperty(String key, String value) {
        // Create the map the very first time
        if (mappedProperty == null) {
            mappedProperty = new HashMap();
            mappedProperty.put("First Key", "First Value");
            mappedProperty.put("Second Key", "Second Value");
        }
        mappedProperty.put(key, value);
    }


    /**
     * A mapped property that has String keys and int values.
     */
    private HashMap mappedIntProperty = null;

    public int getMappedIntProperty(String key) {
        // Create the map the very first time
        if (mappedProperty == null) {
            mappedProperty = new HashMap();
            mappedProperty.put("One", new Integer(1));
            mappedProperty.put("Two", new Integer(2));
        }
        Integer x = (Integer) mappedIntProperty.get(key);
        return ((x == null) ? 0 : x.intValue());
    }

    public void setMappedIntProperty(String key, int value) {
        mappedIntProperty.put(key, new Integer(value));
    }


    /**
     * A nested reference to another test bean (populated as needed).
     */
    private TestBean nested = null;

    public TestBean getNested() {
        if (nested == null)
            nested = new TestBean();
        return (nested);
    }


    /*
     * Another nested reference to a bean containing mapp properties
     */
    class MappedTestBean { 
        public void setValue(String key,String val) { }
        public String getValue(String key) { return "Mapped Value"; }
    }
    
    private MappedTestBean mappedNested = null;

    public MappedTestBean getMappedNested() { 
        if (mappedNested == null) 
        {
            mappedNested = new MappedTestBean();
        }
        return mappedNested;
    }

    /**
     * A String property with an initial value of null.
     */
    private String nullProperty = null;

    public String getNullProperty() {
        return (this.nullProperty);
    }

    public void setNullProperty(String nullProperty) {
        this.nullProperty = nullProperty;
    }


    /**
     * A read-only String property.
     */
    private String readOnlyProperty = "Read Only String Property";

    public String getReadOnlyProperty() {
        return (this.readOnlyProperty);
    }


    /**
     * A short property.
     */
    private short shortProperty = (short) 987;

    public short getShortProperty() {
        return (this.shortProperty);
    }

    public void setShortProperty(short shortProperty) {
        this.shortProperty = shortProperty;
    }


    /**
     * A String array property accessed as a String.
     */
    private String stringArray[] =
            { "String 0", "String 1", "String 2", "String 3", "String 4" };

    public String[] getStringArray() {
        return (this.stringArray);
    }

    public void setStringArray(String stringArray[]) {
        this.stringArray = stringArray;
    }


    /**
     * A String array property accessed as an indexed property.
     */
    private String stringIndexed[] =
            { "String 0", "String 1", "String 2", "String 3", "String 4" };

    public String getStringIndexed(int index) {
        return (stringIndexed[index]);
    }

    public void setStringIndexed(int index, String value) {
        stringIndexed[index] = value;
    }


    /**
     * A String property.
     */
    private String stringProperty = "This is a string";

    public String getStringProperty() {
        return (this.stringProperty);
    }

    public void setStringProperty(String stringProperty) {
        this.stringProperty = stringProperty;
    }


    /**
     * A write-only String property.
     */
    private String writeOnlyProperty = "Write Only String Property";

    public String getWriteOnlyPropertyValue() {
        return (this.writeOnlyProperty);
    }

    public void setWriteOnlyProperty(String writeOnlyProperty) {
        this.writeOnlyProperty = writeOnlyProperty;
    }

    
    // ------------------------------------------------------- Static Variables


    /**
     * A static variable that is accessed and updated via static methods
     * for MethodUtils testing.
     */
    private static int counter = 0;


    /**
     * Return the current value of the counter.
     */
    public static int currentCounter() {

        return (counter);

    }


    /**
     * Increment the current value of the counter by 1.
     */
    public static void incrementCounter() {

        incrementCounter(1);

    }


    /**
     * Increment the current value of the counter by the specified amount.
     *
     * @param amount Amount to be added to the current counter
     */
    public static void incrementCounter(int amount) {

        counter += amount;

    }


}

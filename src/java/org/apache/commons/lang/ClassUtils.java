package org.apache.commons.lang;

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

/**
 * A set of static utilities for use with ClassUtils.
 *
 * @author  bayard@generationjava.com
 * @version $Id: ClassUtils.java,v 1.1 2002/08/14 04:51:28 bayard Exp $
 */
final public class ClassUtils {

    /**
     * Create an object from the classname. Must have an empty constructor.
     *
     * @param classname String name of the class
     *
     * @return Object instance of the class or null
     */
    static public Object createObject(String classname) {
        Class tmpClass = null;

        tmpClass = getClass(classname);

        return createObject(tmpClass);
    }

    /**
     * Create an object from a class. 
     *
     * @param clss Class object to instantiate
     *
     * @return Object instance of the class or null
     */
    static public Object createObject(Class clss) {

        try {
            return clss.newInstance();
        } catch (IllegalAccessException  iae) {
            System.err.println("Cant instantiate " + clss.getName() + " because " +
                   iae.getMessage());
        } catch (InstantiationException  ie) {
            System.err.println("Cant instantiate " + clss.getName() + " because " +
                   ie.getMessage());
        }

        return null;
    }

    /**
     * Is this Class in the CLASSPATH
     *
     * @param classname String of the class
     *
     * @return boolean exists or not.
     */
    static public boolean classExists(String classname) {
        Class tmpClass = null;

        /* try and load class */
        try {
            tmpClass = Class.forName(classname);
        } catch (ClassNotFoundException cnfe) {
            return false;
        } catch (IllegalArgumentException iae) {
            return false;
        }
     
        return true;   
    }

    /**
     * Get the Class object for a classname.
     *
     * @param classname String of the class
     *
     * @return Class instance for the class.
     */
    static public Class getClass(String classname) {
        Class tmpClass = null;

        /* try an load class */
        try {
            tmpClass = Class.forName(classname);
        } catch (ClassNotFoundException cnfe) {
            System.out.println("Can't resolve classname " + classname);
        } catch (IllegalArgumentException iae) {
            System.err.println("Cant resolve " + tmpClass.getName() + " because " + iae.getMessage());
        }
     
        return tmpClass;   
    }

    /**
     * Is this Class object an instance of the class with this name.
     *
     * @param clss Class instance
     * @param inst String name of potential supertype
     *
     * @return boolean was it an instanceof
     */
    static public boolean classInstanceOf(Class clss, String inst) {
        if(classImplements(clss,inst)) {
            return true;
        } else
        if(classExtends(clss,inst)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Does this Class implement an interface with this name.
     *
     * @param clss Class instance
     * @param exts String name of potential interface
     *
     * @return boolean was it an implementor
     */
    static public boolean classImplements(Class clss, String exts) {

      Class sprcls = clss;
      Class excls  = getClass(exts);

      while(sprcls != null) {
        Class[] interfaces = sprcls.getInterfaces();

        for(int i=0;i<interfaces.length;i++) {
            if(interfaces[i].equals(excls)) {
                return true;
            }
        }

        sprcls = sprcls.getSuperclass();
      }

      return false;
    }

    /**
     * Does this Class extend a superclass with this name.
     *
     * @param clss Class instance
     * @param exts String name of potential superclass
     *
     * @return boolean was it a superclass
     */
    static public boolean classExtends(Class clss, String exts) {
        if(clss == null) {
            return false;
        }
        if(clss.getName().equals(exts)) {
            return true;
        }
        Class sprcls = clss.getSuperclass();
        Class excls = getClass(exts);

//        while(! sprcls.equals(sprcls.getSuperclass()) ) {
        while( sprcls != null ) {
            if(sprcls.equals(excls)) {
                return true;
            }
            sprcls = sprcls.getSuperclass();
        }
        return false;
    }

}

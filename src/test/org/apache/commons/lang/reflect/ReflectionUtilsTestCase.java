/*
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


import java.lang.reflect.Method;
import java.lang.reflect.Member;
import java.lang.reflect.Modifier;

import org.apache.commons.lang.reflect.priv.PrivateBeanFactory;
import org.apache.commons.lang.reflect.priv.PublicSubBean;

import junit.framework.TestCase;
import junit.framework.Test;
import junit.framework.TestSuite;


/**
 * <p> Test case for <code>ReflectionUtils</code> </p>
 *
 */
public class ReflectionUtilsTestCase extends TestCase {

    // ---------------------------------------------------- Instance Variables

    // ---------------------------------------------------------- Constructors

    /**
     * Construct a new instance of this test case.
     *
     * @param name Name of the test case
     */
    public ReflectionUtilsTestCase(String name) {
        super(name);
    }


    // -------------------------------------------------- Overall Test Methods


    /**
     * Set up instance variables required by this test case.
     */
    public void setUp() {
        // any set up goes here
    }


    /**
     * Return the tests included in this test suite.
     */
    public static Test suite() {
        return (new TestSuite(ReflectionUtilsTestCase.class));
    }

    /**
     * Tear down instance variables required by this test case.
     */
    public void tearDown() {
        // any tear down goes here
    }


    // ------------------------------------------------ Individual Test Methods
    
    
    public void testScopeUtils() throws Exception {
        ScopeBean bean = new ScopeBean();
        Member field = bean.getPrivateField();        
        assertEquals("Private scope (field) [isFinal]", false ,ReflectionUtils.isFinal(field));
        assertEquals("Private scope (field) [isPackageScope]", false, ReflectionUtils.isPackageScope(field));
        assertEquals("Private scope (field) [isPrivateScope]", true, ReflectionUtils.isPrivateScope(field));
        assertEquals("Private scope (field) [isProtectedScope]", false, ReflectionUtils.isProtectedScope(field));
        assertEquals("Private scope (field) [isPublicScope]", false, ReflectionUtils.isPublicScope(field));
        assertEquals("Private scope (field) [isStatic]", false, ReflectionUtils.isStatic(field));
        
        Member method = bean.getPrivateMethod();
        assertEquals("Private scope (method) [isFinal]", false, ReflectionUtils.isFinal(method));
        assertEquals("Private scope (method) [isPackageScope]", false, ReflectionUtils.isPackageScope(method));
        assertEquals("Private scope (method) [isPrivateScope]", true, ReflectionUtils.isPrivateScope(method));
        assertEquals("Private scope (method) [isProtectedScope]", false, ReflectionUtils.isProtectedScope(method));
        assertEquals("Private scope (method) [isPublicScope]", false, ReflectionUtils.isPublicScope(method));
        assertEquals("Private scope (method) [isStatic]", false, ReflectionUtils.isStatic(method));
        
        field = bean.getPackageField();        
        assertEquals("Package scope (field) [isFinal]", false, ReflectionUtils.isFinal(field));
        assertEquals("Package scope (field) [isPackageScope]", true, ReflectionUtils.isPackageScope(field));
        assertEquals("Package scope (field) [isPrivateScope]", false, ReflectionUtils.isPrivateScope(field));
        assertEquals("Package scope (field) [isProtectedScope]", false, ReflectionUtils.isProtectedScope(field));
        assertEquals("Package scope (field) [isPublicScope]", false, ReflectionUtils.isPublicScope(field));
        assertEquals("Package scope (field) [isStatic]", false, ReflectionUtils.isStatic(field));
        
        method = bean.getPackageMethod();
        assertEquals("Package scope (method) [isFinal]", false, ReflectionUtils.isFinal(method));
        assertEquals("Package scope (method) [isPackageScope]", true, ReflectionUtils.isPackageScope(method));
        assertEquals("Package scope (method) [isPrivateScope]", false, ReflectionUtils.isPrivateScope(method));
        assertEquals("Package scope (method) [isProtectedScope]", false, ReflectionUtils.isProtectedScope(method));
        assertEquals("Package scope (method) [isPublicScope]", false, ReflectionUtils.isPublicScope(method));
        assertEquals("Packages scope (method) [isStatic]", false, ReflectionUtils.isStatic(method));
        
        field = bean.getPublicField();        
        assertEquals("Public scope (field) [isFinal]", false, ReflectionUtils.isFinal(field));
        assertEquals("Public scope (field) [isPackageScope]", false, ReflectionUtils.isPackageScope(field));
        assertEquals("Public scope (field) [isPrivateScope]", false, ReflectionUtils.isPrivateScope(field));
        assertEquals("Public scope (field) [isProtectedScope]", false, ReflectionUtils.isProtectedScope(field));
        assertEquals("Public scope (field) [isPublicScope]", true, ReflectionUtils.isPublicScope(field));
        assertEquals("Public scope (field) [isStatic]", false, ReflectionUtils.isStatic(field));
        
        method = bean.getPublicMethod();
        assertEquals("Public scope (method) [isFinal]", false, ReflectionUtils.isFinal(method));
        assertEquals("Public scope (method) [isPackageScope]", false, ReflectionUtils.isPackageScope(method));
        assertEquals("Public scope (method) [isPrivateScope]", false, ReflectionUtils.isPrivateScope(method));
        assertEquals("Public scope (method) [isProtectedScope]", false, ReflectionUtils.isProtectedScope(method));
        assertEquals("Public scope (method) [isPublicScope]", true, ReflectionUtils.isPublicScope(method));
        assertEquals("Public scope (method) [isStatic]", false, ReflectionUtils.isStatic(method));
        
        field = bean.getFinalField();        
        assertEquals("Final scope (field) [isFinal]", true, ReflectionUtils.isFinal(field));
        assertEquals("Final scope (field) [isPackageScope]", false, ReflectionUtils.isPackageScope(field));
        assertEquals("Final scope (field) [isPrivateScope]", false, ReflectionUtils.isPrivateScope(field));
        assertEquals("Final scope (field) [isProtectedScope]", false, ReflectionUtils.isProtectedScope(field));
        assertEquals("Final scope (field) [isPublicScope]", true, ReflectionUtils.isPublicScope(field));
        assertEquals("Final scope (field) [isStatic]", false, ReflectionUtils.isStatic(field));
        
        method = bean.getFinalMethod();
        assertEquals("Final scope (method) [isFinal]", true, ReflectionUtils.isFinal(method));
        assertEquals("Final scope (method) [isPackageScope]", false, ReflectionUtils.isPackageScope(method));
        assertEquals("Final scope (method) [isPrivateScope]", false, ReflectionUtils.isPrivateScope(method));
        assertEquals("Final scope (method) [isProtectedScope]", false, ReflectionUtils.isProtectedScope(method));
        assertEquals("Final scope (method) [isPublicScope]", true, ReflectionUtils.isPublicScope(method));
        assertEquals("Final scope (method) [isStatic]", false, ReflectionUtils.isStatic(method));
        
        field = bean.getStaticField();        
        assertEquals("Static scope (field) [isFinal]", false, ReflectionUtils.isFinal(field));
        assertEquals("Static scope (field) [isPackageScope]", false, ReflectionUtils.isPackageScope(field));
        assertEquals("Static scope (field) [isPrivateScope]", false, ReflectionUtils.isPrivateScope(field));
        assertEquals("Static scope (field) [isProtectedScope]", false, ReflectionUtils.isProtectedScope(field));
        assertEquals("Static scope (field) [isPublicScope]", true, ReflectionUtils.isPublicScope(field));
        assertEquals("Static scope (field) [isStatic]", true, ReflectionUtils.isStatic(field));
        
        method = bean.getStaticMethod();
        assertEquals("Static scope (method) [isFinal]", false, ReflectionUtils.isFinal(method));
        assertEquals("Static scope (method) [isPackageScope]", false, ReflectionUtils.isPackageScope(method));
        assertEquals("Static scope (method) [isPrivateScope]", false, ReflectionUtils.isPrivateScope(method));
        assertEquals("Static scope (method) [isProtectedScope]", false, ReflectionUtils.isProtectedScope(method));
        assertEquals("Static scope (method) [isPublicScope]", true, ReflectionUtils.isPublicScope(method));
        assertEquals("Static scope (method) [isStatic]", true, ReflectionUtils.isStatic(method));
    }
}

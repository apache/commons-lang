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
package org.apache.commons.lang3.reflect;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.junit.Assume.assumeNotNull;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

import org.apache.commons.lang3.reflect.testbed.Ambig;
import org.apache.commons.lang3.reflect.testbed.Foo;
import org.apache.commons.lang3.reflect.testbed.PrivatelyShadowedChild;
import org.apache.commons.lang3.reflect.testbed.PublicChild;
import org.apache.commons.lang3.reflect.testbed.PubliclyShadowedChild;
import org.apache.commons.lang3.reflect.testbed.StaticContainer;
import org.apache.commons.lang3.reflect.testbed.StaticContainerChild;
import org.junit.Before;
import org.junit.Test;

/**
 * Unit tests FieldUtils
 * @version $Id$
 */
public class FieldUtilsTest {

    static final String S = "s";
    static final String SS = "ss";
    static final Integer I0 = Integer.valueOf(0);
    static final Integer I1 = Integer.valueOf(1);
    static final Double D0 = Double.valueOf(0.0);
    static final Double D1 = Double.valueOf(1.0);

    private PublicChild publicChild;
    private PubliclyShadowedChild publiclyShadowedChild;
    private PrivatelyShadowedChild privatelyShadowedChild;
    private Class<?> parentClass = PublicChild.class.getSuperclass();

    @Before
    public void setUp() {
        StaticContainer.reset();
        publicChild = new PublicChild();
        publiclyShadowedChild = new PubliclyShadowedChild();
        privatelyShadowedChild = new PrivatelyShadowedChild();
    }

    @Test
    public void testConstructor() {
        assertNotNull(new FieldUtils());
        Constructor<?>[] cons = FieldUtils.class.getDeclaredConstructors();
        assertEquals(1, cons.length);
        assertTrue(Modifier.isPublic(cons[0].getModifiers()));
        assertTrue(Modifier.isPublic(FieldUtils.class.getModifiers()));
        assertFalse(Modifier.isFinal(FieldUtils.class.getModifiers()));
    }
    
    @Test
    public void testGetField() {
        assertEquals(Foo.class, FieldUtils.getField(PublicChild.class, "VALUE").getDeclaringClass());
        assertEquals(parentClass, FieldUtils.getField(PublicChild.class, "s").getDeclaringClass());
        assertNull(FieldUtils.getField(PublicChild.class, "b"));
        assertNull(FieldUtils.getField(PublicChild.class, "i"));
        assertNull(FieldUtils.getField(PublicChild.class, "d"));
        assertEquals(Foo.class, FieldUtils.getField(PubliclyShadowedChild.class, "VALUE").getDeclaringClass());
        assertEquals(PubliclyShadowedChild.class, FieldUtils.getField(PubliclyShadowedChild.class, "s")
                .getDeclaringClass());
        assertEquals(PubliclyShadowedChild.class, FieldUtils.getField(PubliclyShadowedChild.class, "b")
                .getDeclaringClass());
        assertEquals(PubliclyShadowedChild.class, FieldUtils.getField(PubliclyShadowedChild.class, "i")
                .getDeclaringClass());
        assertEquals(PubliclyShadowedChild.class, FieldUtils.getField(PubliclyShadowedChild.class, "d")
                .getDeclaringClass());
        assertEquals(Foo.class, FieldUtils.getField(PrivatelyShadowedChild.class, "VALUE").getDeclaringClass());
        assertEquals(parentClass, FieldUtils.getField(PrivatelyShadowedChild.class, "s").getDeclaringClass());
        assertNull(FieldUtils.getField(PrivatelyShadowedChild.class, "b"));
        assertNull(FieldUtils.getField(PrivatelyShadowedChild.class, "i"));
        assertNull(FieldUtils.getField(PrivatelyShadowedChild.class, "d"));
    }

    @Test(expected=IllegalArgumentException.class)
    public void testGetFieldIllegalArgumentException1() {
        FieldUtils.getField(null, "none");
    }
    
    @Test(expected=IllegalArgumentException.class)
    public void testGetFieldIllegalArgumentException2() {
        FieldUtils.getField(PublicChild.class, null);
    }
    
    @Test
    public void testGetFieldForceAccess() {
        assertEquals(PublicChild.class, FieldUtils.getField(PublicChild.class, "VALUE", true).getDeclaringClass());
        assertEquals(parentClass, FieldUtils.getField(PublicChild.class, "s", true).getDeclaringClass());
        assertEquals(parentClass, FieldUtils.getField(PublicChild.class, "b", true).getDeclaringClass());
        assertEquals(parentClass, FieldUtils.getField(PublicChild.class, "i", true).getDeclaringClass());
        assertEquals(parentClass, FieldUtils.getField(PublicChild.class, "d", true).getDeclaringClass());
        assertEquals(Foo.class, FieldUtils.getField(PubliclyShadowedChild.class, "VALUE", true).getDeclaringClass());
        assertEquals(PubliclyShadowedChild.class, FieldUtils.getField(PubliclyShadowedChild.class, "s", true)
                .getDeclaringClass());
        assertEquals(PubliclyShadowedChild.class, FieldUtils.getField(PubliclyShadowedChild.class, "b", true)
                .getDeclaringClass());
        assertEquals(PubliclyShadowedChild.class, FieldUtils.getField(PubliclyShadowedChild.class, "i", true)
                .getDeclaringClass());
        assertEquals(PubliclyShadowedChild.class, FieldUtils.getField(PubliclyShadowedChild.class, "d", true)
                .getDeclaringClass());
        assertEquals(Foo.class, FieldUtils.getField(PrivatelyShadowedChild.class, "VALUE", true).getDeclaringClass());
        assertEquals(PrivatelyShadowedChild.class, FieldUtils.getField(PrivatelyShadowedChild.class, "s", true)
                .getDeclaringClass());
        assertEquals(PrivatelyShadowedChild.class, FieldUtils.getField(PrivatelyShadowedChild.class, "b", true)
                .getDeclaringClass());
        assertEquals(PrivatelyShadowedChild.class, FieldUtils.getField(PrivatelyShadowedChild.class, "i", true)
                .getDeclaringClass());
        assertEquals(PrivatelyShadowedChild.class, FieldUtils.getField(PrivatelyShadowedChild.class, "d", true)
                .getDeclaringClass());
    }

    @Test(expected=IllegalArgumentException.class)
    public void testGetFieldForceAccessIllegalArgumentException1() {
        FieldUtils.getField(null, "none", true);
    }
    
    @Test(expected=IllegalArgumentException.class)
    public void testGetFieldForceAccessIllegalArgumentException2() {
        FieldUtils.getField(PublicChild.class, null, true);
    }    

    @Test
    public void testGetDeclaredField() {
        assertNull(FieldUtils.getDeclaredField(PublicChild.class, "VALUE"));
        assertNull(FieldUtils.getDeclaredField(PublicChild.class, "s"));
        assertNull(FieldUtils.getDeclaredField(PublicChild.class, "b"));
        assertNull(FieldUtils.getDeclaredField(PublicChild.class, "i"));
        assertNull(FieldUtils.getDeclaredField(PublicChild.class, "d"));
        assertNull(FieldUtils.getDeclaredField(PubliclyShadowedChild.class, "VALUE"));
        assertEquals(PubliclyShadowedChild.class, FieldUtils.getDeclaredField(PubliclyShadowedChild.class, "s")
                .getDeclaringClass());
        assertEquals(PubliclyShadowedChild.class, FieldUtils.getDeclaredField(PubliclyShadowedChild.class, "b")
                .getDeclaringClass());
        assertEquals(PubliclyShadowedChild.class, FieldUtils.getDeclaredField(PubliclyShadowedChild.class, "i")
                .getDeclaringClass());
        assertEquals(PubliclyShadowedChild.class, FieldUtils.getDeclaredField(PubliclyShadowedChild.class, "d")
                .getDeclaringClass());
        assertNull(FieldUtils.getDeclaredField(PrivatelyShadowedChild.class, "VALUE"));
        assertNull(FieldUtils.getDeclaredField(PrivatelyShadowedChild.class, "s"));
        assertNull(FieldUtils.getDeclaredField(PrivatelyShadowedChild.class, "b"));
        assertNull(FieldUtils.getDeclaredField(PrivatelyShadowedChild.class, "i"));
        assertNull(FieldUtils.getDeclaredField(PrivatelyShadowedChild.class, "d"));
    }

    @Test(expected=IllegalArgumentException.class)
    public void testGetDeclaredFieldAccessIllegalArgumentException1() {
        FieldUtils.getDeclaredField(null, "none");
    }    

    @Test(expected=IllegalArgumentException.class)
    public void testGetDeclaredFieldAccessIllegalArgumentException2() {
        FieldUtils.getDeclaredField(PublicChild.class, null);
    }    

    @Test
    public void testGetDeclaredFieldForceAccess() {
        assertEquals(PublicChild.class, FieldUtils.getDeclaredField(PublicChild.class, "VALUE", true)
                .getDeclaringClass());
        assertNull(FieldUtils.getDeclaredField(PublicChild.class, "s", true));
        assertNull(FieldUtils.getDeclaredField(PublicChild.class, "b", true));
        assertNull(FieldUtils.getDeclaredField(PublicChild.class, "i", true));
        assertNull(FieldUtils.getDeclaredField(PublicChild.class, "d", true));
        assertNull(FieldUtils.getDeclaredField(PubliclyShadowedChild.class, "VALUE", true));
        assertEquals(PubliclyShadowedChild.class, FieldUtils.getDeclaredField(PubliclyShadowedChild.class, "s", true)
                .getDeclaringClass());
        assertEquals(PubliclyShadowedChild.class, FieldUtils.getDeclaredField(PubliclyShadowedChild.class, "b", true)
                .getDeclaringClass());
        assertEquals(PubliclyShadowedChild.class, FieldUtils.getDeclaredField(PubliclyShadowedChild.class, "i", true)
                .getDeclaringClass());
        assertEquals(PubliclyShadowedChild.class, FieldUtils.getDeclaredField(PubliclyShadowedChild.class, "d", true)
                .getDeclaringClass());
        assertNull(FieldUtils.getDeclaredField(PrivatelyShadowedChild.class, "VALUE", true));
        assertEquals(PrivatelyShadowedChild.class, FieldUtils.getDeclaredField(PrivatelyShadowedChild.class, "s", true)
                .getDeclaringClass());
        assertEquals(PrivatelyShadowedChild.class, FieldUtils.getDeclaredField(PrivatelyShadowedChild.class, "b", true)
                .getDeclaringClass());
        assertEquals(PrivatelyShadowedChild.class, FieldUtils.getDeclaredField(PrivatelyShadowedChild.class, "i", true)
                .getDeclaringClass());
        assertEquals(PrivatelyShadowedChild.class, FieldUtils.getDeclaredField(PrivatelyShadowedChild.class, "d", true)
                .getDeclaringClass());
    }

    @Test(expected=IllegalArgumentException.class)
    public void testGetDeclaredFieldForceAccessIllegalArgumentException1() {
        FieldUtils.getDeclaredField(null, "none", true);
    }    

    @Test(expected=IllegalArgumentException.class)
    public void testGetDeclaredFieldForceAccessIllegalArgumentException2() {
        FieldUtils.getDeclaredField(PublicChild.class, null, true);
    }    

    @Test
    public void testReadStaticField() throws Exception {
        assertEquals(Foo.VALUE, FieldUtils.readStaticField(FieldUtils.getField(Foo.class, "VALUE")));
    }

    @Test(expected=IllegalArgumentException.class)
    public void testReadStaticFieldIllegalArgumentException1() throws Exception {
         FieldUtils.readStaticField(null);
    }

    @Test(expected=IllegalArgumentException.class)
    public void testReadStaticFieldIllegalArgumentException2() throws Exception {
        assertEquals(Foo.VALUE, FieldUtils.readStaticField(FieldUtils.getField(Foo.class, "VALUE")));
        Field nonStaticField = FieldUtils.getField(PublicChild.class, "s");
        assumeNotNull(nonStaticField);
        FieldUtils.readStaticField(nonStaticField);
    }

    @Test
    public void testReadStaticFieldForceAccess() throws Exception {
        assertEquals(Foo.VALUE, FieldUtils.readStaticField(FieldUtils.getField(Foo.class, "VALUE")));
        assertEquals(Foo.VALUE, FieldUtils.readStaticField(FieldUtils.getField(PublicChild.class, "VALUE")));
    }

    @Test(expected=IllegalArgumentException.class)
    public void testReadStaticFieldForceAccessIllegalArgumentException1() throws Exception {
        FieldUtils.readStaticField(null, true);
    }

    @Test(expected=IllegalArgumentException.class)
    public void testReadStaticFieldForceAccessIllegalArgumentException2() throws Exception {
        Field nonStaticField = FieldUtils.getField(PublicChild.class, "s", true);
        assumeNotNull(nonStaticField);
        FieldUtils.readStaticField(nonStaticField);
    }

    @Test
    public void testReadNamedStaticField() throws Exception {
        assertEquals(Foo.VALUE, FieldUtils.readStaticField(Foo.class, "VALUE"));
        assertEquals(Foo.VALUE, FieldUtils.readStaticField(PubliclyShadowedChild.class, "VALUE"));
        assertEquals(Foo.VALUE, FieldUtils.readStaticField(PrivatelyShadowedChild.class, "VALUE"));
        assertEquals(Foo.VALUE, FieldUtils.readStaticField(PublicChild.class, "VALUE"));
        
        try {
            FieldUtils.readStaticField(null, "none");
            fail("null class should cause an IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // expected
        }
        
        try {
            FieldUtils.readStaticField(Foo.class, null);
            fail("null field name should cause an IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // expected
        }
        
        try {
            FieldUtils.readStaticField(Foo.class, "does_not_exist");
            fail("a field that doesn't exist should cause an IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // expected
        }
        
        try {
            FieldUtils.readStaticField(PublicChild.class, "s");
            fail("non-static field should cause an IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // expected
        }
    }

    @Test
    public void testReadNamedStaticFieldForceAccess() throws Exception {
        assertEquals(Foo.VALUE, FieldUtils.readStaticField(Foo.class, "VALUE", true));
        assertEquals(Foo.VALUE, FieldUtils.readStaticField(PubliclyShadowedChild.class, "VALUE", true));
        assertEquals(Foo.VALUE, FieldUtils.readStaticField(PrivatelyShadowedChild.class, "VALUE", true));
        assertEquals("child", FieldUtils.readStaticField(PublicChild.class, "VALUE", true));
        
        try {
            FieldUtils.readStaticField(null, "none", true);
            fail("null class should cause an IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // expected
        }
        
        try {
            FieldUtils.readStaticField(Foo.class, null, true);
            fail("null field name should cause an IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // expected
        }
        
        try {
            FieldUtils.readStaticField(Foo.class, "does_not_exist", true);
            fail("a field that doesn't exist should cause an IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // expected
        }
        
        try {
            FieldUtils.readStaticField(PublicChild.class, "s", false);
            fail("non-static field should cause an IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // expected
        }
    }

    @Test
    public void testReadDeclaredNamedStaticField() throws Exception {
        assertEquals(Foo.VALUE, FieldUtils.readDeclaredStaticField(Foo.class, "VALUE"));
        try {
            assertEquals("child", FieldUtils.readDeclaredStaticField(PublicChild.class, "VALUE"));
            fail("expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            assertEquals(Foo.VALUE, FieldUtils.readDeclaredStaticField(PubliclyShadowedChild.class, "VALUE"));
            fail("expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            assertEquals(Foo.VALUE, FieldUtils.readDeclaredStaticField(PrivatelyShadowedChild.class, "VALUE"));
            fail("expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
    }

    @Test
    public void testReadDeclaredNamedStaticFieldForceAccess() throws Exception {
        assertEquals(Foo.VALUE, FieldUtils.readDeclaredStaticField(Foo.class, "VALUE", true));
        assertEquals("child", FieldUtils.readDeclaredStaticField(PublicChild.class, "VALUE", true));
        try {
            assertEquals(Foo.VALUE, FieldUtils.readDeclaredStaticField(PubliclyShadowedChild.class, "VALUE", true));
            fail("expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            assertEquals(Foo.VALUE, FieldUtils.readDeclaredStaticField(PrivatelyShadowedChild.class, "VALUE", true));
            fail("expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
    }

    @Test
    public void testReadField() throws Exception {
        Field parentS = FieldUtils.getDeclaredField(parentClass, "s");
        assertEquals("s", FieldUtils.readField(parentS, publicChild));
        assertEquals("s", FieldUtils.readField(parentS, publiclyShadowedChild));
        assertEquals("s", FieldUtils.readField(parentS, privatelyShadowedChild));
        Field parentB = FieldUtils.getDeclaredField(parentClass, "b", true);
        assertEquals(Boolean.FALSE, FieldUtils.readField(parentB, publicChild));
        assertEquals(Boolean.FALSE, FieldUtils.readField(parentB, publiclyShadowedChild));
        assertEquals(Boolean.FALSE, FieldUtils.readField(parentB, privatelyShadowedChild));
        Field parentI = FieldUtils.getDeclaredField(parentClass, "i", true);
        assertEquals(I0, FieldUtils.readField(parentI, publicChild));
        assertEquals(I0, FieldUtils.readField(parentI, publiclyShadowedChild));
        assertEquals(I0, FieldUtils.readField(parentI, privatelyShadowedChild));
        Field parentD = FieldUtils.getDeclaredField(parentClass, "d", true);
        assertEquals(D0, FieldUtils.readField(parentD, publicChild));
        assertEquals(D0, FieldUtils.readField(parentD, publiclyShadowedChild));
        assertEquals(D0, FieldUtils.readField(parentD, privatelyShadowedChild));
        
        try {
            FieldUtils.readField((Field)null, publicChild);
            fail("a null field should cause an IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // expected
        }
    }

    @Test
    public void testReadFieldForceAccess() throws Exception {
        Field parentS = FieldUtils.getDeclaredField(parentClass, "s");
        parentS.setAccessible(false);
        assertEquals("s", FieldUtils.readField(parentS, publicChild, true));
        assertEquals("s", FieldUtils.readField(parentS, publiclyShadowedChild, true));
        assertEquals("s", FieldUtils.readField(parentS, privatelyShadowedChild, true));
        Field parentB = FieldUtils.getDeclaredField(parentClass, "b", true);
        parentB.setAccessible(false);
        assertEquals(Boolean.FALSE, FieldUtils.readField(parentB, publicChild, true));
        assertEquals(Boolean.FALSE, FieldUtils.readField(parentB, publiclyShadowedChild, true));
        assertEquals(Boolean.FALSE, FieldUtils.readField(parentB, privatelyShadowedChild, true));
        Field parentI = FieldUtils.getDeclaredField(parentClass, "i", true);
        parentI.setAccessible(false);
        assertEquals(I0, FieldUtils.readField(parentI, publicChild, true));
        assertEquals(I0, FieldUtils.readField(parentI, publiclyShadowedChild, true));
        assertEquals(I0, FieldUtils.readField(parentI, privatelyShadowedChild, true));
        Field parentD = FieldUtils.getDeclaredField(parentClass, "d", true);
        parentD.setAccessible(false);
        assertEquals(D0, FieldUtils.readField(parentD, publicChild, true));
        assertEquals(D0, FieldUtils.readField(parentD, publiclyShadowedChild, true));
        assertEquals(D0, FieldUtils.readField(parentD, privatelyShadowedChild, true));
        
        try {
            FieldUtils.readField((Field)null, publicChild, true);
            fail("a null field should cause an IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // expected
        }
    }

    @Test
    public void testReadNamedField() throws Exception {
        assertEquals("s", FieldUtils.readField(publicChild, "s"));
        assertEquals("ss", FieldUtils.readField(publiclyShadowedChild, "s"));
        assertEquals("s", FieldUtils.readField(privatelyShadowedChild, "s"));
        
        try {
            FieldUtils.readField(publicChild, null);
            fail("a null field name should cause an IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // expected
        }
        
        try {
            FieldUtils.readField((Object)null, "none");
            fail("a null target should cause an IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // expected
        }
        
        try {
            assertEquals(Boolean.FALSE, FieldUtils.readField(publicChild, "b"));
            fail("expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        assertEquals(Boolean.TRUE, FieldUtils.readField(publiclyShadowedChild, "b"));
        try {
            assertEquals(Boolean.FALSE, FieldUtils.readField(privatelyShadowedChild, "b"));
            fail("expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            assertEquals(I0, FieldUtils.readField(publicChild, "i"));
            fail("expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        assertEquals(I1, FieldUtils.readField(publiclyShadowedChild, "i"));
        try {
            assertEquals(I0, FieldUtils.readField(privatelyShadowedChild, "i"));
            fail("expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            assertEquals(D0, FieldUtils.readField(publicChild, "d"));
            fail("expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        assertEquals(D1, FieldUtils.readField(publiclyShadowedChild, "d"));
        try {
            assertEquals(D0, FieldUtils.readField(privatelyShadowedChild, "d"));
            fail("expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
    }

    @Test
    public void testReadNamedFieldForceAccess() throws Exception {
        assertEquals("s", FieldUtils.readField(publicChild, "s", true));
        assertEquals("ss", FieldUtils.readField(publiclyShadowedChild, "s", true));
        assertEquals("ss", FieldUtils.readField(privatelyShadowedChild, "s", true));
        assertEquals(Boolean.FALSE, FieldUtils.readField(publicChild, "b", true));
        assertEquals(Boolean.TRUE, FieldUtils.readField(publiclyShadowedChild, "b", true));
        assertEquals(Boolean.TRUE, FieldUtils.readField(privatelyShadowedChild, "b", true));
        assertEquals(I0, FieldUtils.readField(publicChild, "i", true));
        assertEquals(I1, FieldUtils.readField(publiclyShadowedChild, "i", true));
        assertEquals(I1, FieldUtils.readField(privatelyShadowedChild, "i", true));
        assertEquals(D0, FieldUtils.readField(publicChild, "d", true));
        assertEquals(D1, FieldUtils.readField(publiclyShadowedChild, "d", true));
        assertEquals(D1, FieldUtils.readField(privatelyShadowedChild, "d", true));
        
        try {
            FieldUtils.readField(publicChild, null, true);
            fail("a null field name should cause an IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // expected
        }
        
        try {
            FieldUtils.readField((Object)null, "none", true);
            fail("a null target should cause an IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // expected
        }
    }

    @Test
    public void testReadDeclaredNamedField() throws Exception {
        try {
            FieldUtils.readDeclaredField(publicChild, null);
            fail("a null field name should cause an IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // expected
        }
        
        try {
            FieldUtils.readDeclaredField((Object)null, "none");
            fail("a null target should cause an IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // expected
        }
        
        try {
            assertEquals("s", FieldUtils.readDeclaredField(publicChild, "s"));
            fail("expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        assertEquals("ss", FieldUtils.readDeclaredField(publiclyShadowedChild, "s"));
        try {
            assertEquals("s", FieldUtils.readDeclaredField(privatelyShadowedChild, "s"));
            fail("expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            assertEquals(Boolean.FALSE, FieldUtils.readDeclaredField(publicChild, "b"));
            fail("expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        assertEquals(Boolean.TRUE, FieldUtils.readDeclaredField(publiclyShadowedChild, "b"));
        try {
            assertEquals(Boolean.FALSE, FieldUtils.readDeclaredField(privatelyShadowedChild, "b"));
            fail("expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            assertEquals(I0, FieldUtils.readDeclaredField(publicChild, "i"));
            fail("expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        assertEquals(I1, FieldUtils.readDeclaredField(publiclyShadowedChild, "i"));
        try {
            assertEquals(I0, FieldUtils.readDeclaredField(privatelyShadowedChild, "i"));
            fail("expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            assertEquals(D0, FieldUtils.readDeclaredField(publicChild, "d"));
            fail("expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        assertEquals(D1, FieldUtils.readDeclaredField(publiclyShadowedChild, "d"));
        try {
            assertEquals(D0, FieldUtils.readDeclaredField(privatelyShadowedChild, "d"));
            fail("expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
    }

    @Test
    public void testReadDeclaredNamedFieldForceAccess() throws Exception {
        try {
            FieldUtils.readDeclaredField(publicChild, null, true);
            fail("a null field name should cause an IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // expected
        }
        
        try {
            FieldUtils.readDeclaredField((Object)null, "none", true);
            fail("a null target should cause an IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // expected
        }
        
        try {
            assertEquals("s", FieldUtils.readDeclaredField(publicChild, "s", true));
            fail("expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        assertEquals("ss", FieldUtils.readDeclaredField(publiclyShadowedChild, "s", true));
        assertEquals("ss", FieldUtils.readDeclaredField(privatelyShadowedChild, "s", true));
        try {
            assertEquals(Boolean.FALSE, FieldUtils.readDeclaredField(publicChild, "b", true));
            fail("expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        assertEquals(Boolean.TRUE, FieldUtils.readDeclaredField(publiclyShadowedChild, "b", true));
        assertEquals(Boolean.TRUE, FieldUtils.readDeclaredField(privatelyShadowedChild, "b", true));
        try {
            assertEquals(I0, FieldUtils.readDeclaredField(publicChild, "i", true));
            fail("expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        assertEquals(I1, FieldUtils.readDeclaredField(publiclyShadowedChild, "i", true));
        assertEquals(I1, FieldUtils.readDeclaredField(privatelyShadowedChild, "i", true));
        try {
            assertEquals(D0, FieldUtils.readDeclaredField(publicChild, "d", true));
            fail("expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        assertEquals(D1, FieldUtils.readDeclaredField(publiclyShadowedChild, "d", true));
        assertEquals(D1, FieldUtils.readDeclaredField(privatelyShadowedChild, "d", true));
    }

    @Test
    public void testWriteStaticField() throws Exception {
        Field field = StaticContainer.class.getDeclaredField("mutablePublic");
        FieldUtils.writeStaticField(field, "new");
        assertEquals("new", StaticContainer.mutablePublic);
        field = StaticContainer.class.getDeclaredField("mutableProtected");
        try {
            FieldUtils.writeStaticField(field, "new");
            fail("Expected IllegalAccessException");
        } catch (IllegalAccessException e) {
            // pass
        }
        field = StaticContainer.class.getDeclaredField("mutablePackage");
        try {
            FieldUtils.writeStaticField(field, "new");
            fail("Expected IllegalAccessException");
        } catch (IllegalAccessException e) {
            // pass
        }
        field = StaticContainer.class.getDeclaredField("mutablePrivate");
        try {
            FieldUtils.writeStaticField(field, "new");
            fail("Expected IllegalAccessException");
        } catch (IllegalAccessException e) {
            // pass
        }
        field = StaticContainer.class.getDeclaredField("IMMUTABLE_PUBLIC");
        try {
            FieldUtils.writeStaticField(field, "new");
            fail("Expected IllegalAccessException");
        } catch (IllegalAccessException e) {
            // pass
        }
        field = StaticContainer.class.getDeclaredField("IMMUTABLE_PROTECTED");
        try {
            FieldUtils.writeStaticField(field, "new");
            fail("Expected IllegalAccessException");
        } catch (IllegalAccessException e) {
            // pass
        }
        field = StaticContainer.class.getDeclaredField("IMMUTABLE_PACKAGE");
        try {
            FieldUtils.writeStaticField(field, "new");
            fail("Expected IllegalAccessException");
        } catch (IllegalAccessException e) {
            // pass
        }
        field = StaticContainer.class.getDeclaredField("IMMUTABLE_PRIVATE");
        try {
            FieldUtils.writeStaticField(field, "new");
            fail("Expected IllegalAccessException");
        } catch (IllegalAccessException e) {
            // pass
        }
    }

    @Test
    public void testWriteStaticFieldForceAccess() throws Exception {
        Field field = StaticContainer.class.getDeclaredField("mutablePublic");
        FieldUtils.writeStaticField(field, "new", true);
        assertEquals("new", StaticContainer.mutablePublic);
        field = StaticContainer.class.getDeclaredField("mutableProtected");
        FieldUtils.writeStaticField(field, "new", true);
        assertEquals("new", StaticContainer.getMutableProtected());
        field = StaticContainer.class.getDeclaredField("mutablePackage");
        FieldUtils.writeStaticField(field, "new", true);
        assertEquals("new", StaticContainer.getMutablePackage());
        field = StaticContainer.class.getDeclaredField("mutablePrivate");
        FieldUtils.writeStaticField(field, "new", true);
        assertEquals("new", StaticContainer.getMutablePrivate());
        field = StaticContainer.class.getDeclaredField("IMMUTABLE_PUBLIC");
        try {
            FieldUtils.writeStaticField(field, "new", true);
            fail("Expected IllegalAccessException");
        } catch (IllegalAccessException e) {
            // pass
        }
        field = StaticContainer.class.getDeclaredField("IMMUTABLE_PROTECTED");
        try {
            FieldUtils.writeStaticField(field, "new", true);
            fail("Expected IllegalAccessException");
        } catch (IllegalAccessException e) {
            // pass
        }
        field = StaticContainer.class.getDeclaredField("IMMUTABLE_PACKAGE");
        try {
            FieldUtils.writeStaticField(field, "new", true);
            fail("Expected IllegalAccessException");
        } catch (IllegalAccessException e) {
            // pass
        }
        field = StaticContainer.class.getDeclaredField("IMMUTABLE_PRIVATE");
        try {
            FieldUtils.writeStaticField(field, "new", true);
            fail("Expected IllegalAccessException");
        } catch (IllegalAccessException e) {
            // pass
        }
    }

    @Test
    public void testWriteNamedStaticField() throws Exception {
        FieldUtils.writeStaticField(StaticContainerChild.class, "mutablePublic", "new");
        assertEquals("new", StaticContainer.mutablePublic);
        try {
            FieldUtils.writeStaticField(StaticContainerChild.class, "mutableProtected", "new");
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            FieldUtils.writeStaticField(StaticContainerChild.class, "mutablePackage", "new");
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            FieldUtils.writeStaticField(StaticContainerChild.class, "mutablePrivate", "new");
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            FieldUtils.writeStaticField(StaticContainerChild.class, "IMMUTABLE_PUBLIC", "new");
            fail("Expected IllegalAccessException");
        } catch (IllegalAccessException e) {
            // pass
        }
        try {
            FieldUtils.writeStaticField(StaticContainerChild.class, "IMMUTABLE_PROTECTED", "new");
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            FieldUtils.writeStaticField(StaticContainerChild.class, "IMMUTABLE_PACKAGE", "new");
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            FieldUtils.writeStaticField(StaticContainerChild.class, "IMMUTABLE_PRIVATE", "new");
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
    }

    @Test
    public void testWriteNamedStaticFieldForceAccess() throws Exception {
        FieldUtils.writeStaticField(StaticContainerChild.class, "mutablePublic", "new", true);
        assertEquals("new", StaticContainer.mutablePublic);
        FieldUtils.writeStaticField(StaticContainerChild.class, "mutableProtected", "new", true);
        assertEquals("new", StaticContainer.getMutableProtected());
        FieldUtils.writeStaticField(StaticContainerChild.class, "mutablePackage", "new", true);
        assertEquals("new", StaticContainer.getMutablePackage());
        FieldUtils.writeStaticField(StaticContainerChild.class, "mutablePrivate", "new", true);
        assertEquals("new", StaticContainer.getMutablePrivate());
        try {
            FieldUtils.writeStaticField(StaticContainerChild.class, "IMMUTABLE_PUBLIC", "new", true);
            fail("Expected IllegalAccessException");
        } catch (IllegalAccessException e) {
            // pass
        }
        try {
            FieldUtils.writeStaticField(StaticContainerChild.class, "IMMUTABLE_PROTECTED", "new", true);
            fail("Expected IllegalAccessException");
        } catch (IllegalAccessException e) {
            // pass
        }
        try {
            FieldUtils.writeStaticField(StaticContainerChild.class, "IMMUTABLE_PACKAGE", "new", true);
            fail("Expected IllegalAccessException");
        } catch (IllegalAccessException e) {
            // pass
        }
        try {
            FieldUtils.writeStaticField(StaticContainerChild.class, "IMMUTABLE_PRIVATE", "new", true);
            fail("Expected IllegalAccessException");
        } catch (IllegalAccessException e) {
            // pass
        }
    }

    @Test
    public void testWriteDeclaredNamedStaticField() throws Exception {
        FieldUtils.writeStaticField(StaticContainer.class, "mutablePublic", "new");
        assertEquals("new", StaticContainer.mutablePublic);
        try {
            FieldUtils.writeDeclaredStaticField(StaticContainer.class, "mutableProtected", "new");
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            FieldUtils.writeDeclaredStaticField(StaticContainer.class, "mutablePackage", "new");
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            FieldUtils.writeDeclaredStaticField(StaticContainer.class, "mutablePrivate", "new");
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            FieldUtils.writeDeclaredStaticField(StaticContainer.class, "IMMUTABLE_PUBLIC", "new");
            fail("Expected IllegalAccessException");
        } catch (IllegalAccessException e) {
            // pass
        }
        try {
            FieldUtils.writeDeclaredStaticField(StaticContainer.class, "IMMUTABLE_PROTECTED", "new");
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            FieldUtils.writeDeclaredStaticField(StaticContainer.class, "IMMUTABLE_PACKAGE", "new");
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            FieldUtils.writeDeclaredStaticField(StaticContainer.class, "IMMUTABLE_PRIVATE", "new");
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
    }

    @Test
    public void testWriteDeclaredNamedStaticFieldForceAccess() throws Exception {
        FieldUtils.writeDeclaredStaticField(StaticContainer.class, "mutablePublic", "new", true);
        assertEquals("new", StaticContainer.mutablePublic);
        FieldUtils.writeDeclaredStaticField(StaticContainer.class, "mutableProtected", "new", true);
        assertEquals("new", StaticContainer.getMutableProtected());
        FieldUtils.writeDeclaredStaticField(StaticContainer.class, "mutablePackage", "new", true);
        assertEquals("new", StaticContainer.getMutablePackage());
        FieldUtils.writeDeclaredStaticField(StaticContainer.class, "mutablePrivate", "new", true);
        assertEquals("new", StaticContainer.getMutablePrivate());
        try {
            FieldUtils.writeDeclaredStaticField(StaticContainer.class, "IMMUTABLE_PUBLIC", "new", true);
            fail("Expected IllegalAccessException");
        } catch (IllegalAccessException e) {
            // pass
        }
        try {
            FieldUtils.writeDeclaredStaticField(StaticContainer.class, "IMMUTABLE_PROTECTED", "new", true);
            fail("Expected IllegalAccessException");
        } catch (IllegalAccessException e) {
            // pass
        }
        try {
            FieldUtils.writeDeclaredStaticField(StaticContainer.class, "IMMUTABLE_PACKAGE", "new", true);
            fail("Expected IllegalAccessException");
        } catch (IllegalAccessException e) {
            // pass
        }
        try {
            FieldUtils.writeDeclaredStaticField(StaticContainer.class, "IMMUTABLE_PRIVATE", "new", true);
            fail("Expected IllegalAccessException");
        } catch (IllegalAccessException e) {
            // pass
        }
    }

    @Test
    public void testWriteField() throws Exception {
        Field field = parentClass.getDeclaredField("s");
        FieldUtils.writeField(field, publicChild, "S");
        assertEquals("S", field.get(publicChild));
        field = parentClass.getDeclaredField("b");
        try {
            FieldUtils.writeField(field, publicChild, Boolean.TRUE);
            fail("Expected IllegalAccessException");
        } catch (IllegalAccessException e) {
            // pass
        }
        field = parentClass.getDeclaredField("i");
        try {
            FieldUtils.writeField(field, publicChild, Integer.valueOf(Integer.MAX_VALUE));
        } catch (IllegalAccessException e) {
            // pass
        }
        field = parentClass.getDeclaredField("d");
        try {
            FieldUtils.writeField(field, publicChild, Double.valueOf(Double.MAX_VALUE));
        } catch (IllegalAccessException e) {
            // pass
        }
    }

    @Test
    public void testWriteFieldForceAccess() throws Exception {
        Field field = parentClass.getDeclaredField("s");
        FieldUtils.writeField(field, publicChild, "S", true);
        assertEquals("S", field.get(publicChild));
        field = parentClass.getDeclaredField("b");
        FieldUtils.writeField(field, publicChild, Boolean.TRUE, true);
        assertEquals(Boolean.TRUE, field.get(publicChild));
        field = parentClass.getDeclaredField("i");
        FieldUtils.writeField(field, publicChild, Integer.valueOf(Integer.MAX_VALUE), true);
        assertEquals(Integer.valueOf(Integer.MAX_VALUE), field.get(publicChild));
        field = parentClass.getDeclaredField("d");
        FieldUtils.writeField(field, publicChild, Double.valueOf(Double.MAX_VALUE), true);
        assertEquals(Double.valueOf(Double.MAX_VALUE), field.get(publicChild));
    }

    @Test
    public void testWriteNamedField() throws Exception {
        FieldUtils.writeField(publicChild, "s", "S");
        assertEquals("S", FieldUtils.readField(publicChild, "s"));
        try {
            FieldUtils.writeField(publicChild, "b", Boolean.TRUE);
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            FieldUtils.writeField(publicChild, "i", Integer.valueOf(1));
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            FieldUtils.writeField(publicChild, "d", Double.valueOf(1.0));
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }

        FieldUtils.writeField(publiclyShadowedChild, "s", "S");
        assertEquals("S", FieldUtils.readField(publiclyShadowedChild, "s"));
        FieldUtils.writeField(publiclyShadowedChild, "b", Boolean.FALSE);
        assertEquals(Boolean.FALSE, FieldUtils.readField(publiclyShadowedChild, "b"));
        FieldUtils.writeField(publiclyShadowedChild, "i", Integer.valueOf(0));
        assertEquals(Integer.valueOf(0), FieldUtils.readField(publiclyShadowedChild, "i"));
        FieldUtils.writeField(publiclyShadowedChild, "d", Double.valueOf(0.0));
        assertEquals(Double.valueOf(0.0), FieldUtils.readField(publiclyShadowedChild, "d"));

        FieldUtils.writeField(privatelyShadowedChild, "s", "S");
        assertEquals("S", FieldUtils.readField(privatelyShadowedChild, "s"));
        try {
            FieldUtils.writeField(privatelyShadowedChild, "b", Boolean.TRUE);
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            FieldUtils.writeField(privatelyShadowedChild, "i", Integer.valueOf(1));
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            FieldUtils.writeField(privatelyShadowedChild, "d", Double.valueOf(1.0));
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
    }

    @Test
    public void testWriteNamedFieldForceAccess() throws Exception {
        FieldUtils.writeField(publicChild, "s", "S", true);
        assertEquals("S", FieldUtils.readField(publicChild, "s", true));
        FieldUtils.writeField(publicChild, "b", Boolean.TRUE, true);
        assertEquals(Boolean.TRUE, FieldUtils.readField(publicChild, "b", true));
        FieldUtils.writeField(publicChild, "i", Integer.valueOf(1), true);
        assertEquals(Integer.valueOf(1), FieldUtils.readField(publicChild, "i", true));
        FieldUtils.writeField(publicChild, "d", Double.valueOf(1.0), true);
        assertEquals(Double.valueOf(1.0), FieldUtils.readField(publicChild, "d", true));

        FieldUtils.writeField(publiclyShadowedChild, "s", "S", true);
        assertEquals("S", FieldUtils.readField(publiclyShadowedChild, "s", true));
        FieldUtils.writeField(publiclyShadowedChild, "b", Boolean.FALSE, true);
        assertEquals(Boolean.FALSE, FieldUtils.readField(publiclyShadowedChild, "b", true));
        FieldUtils.writeField(publiclyShadowedChild, "i", Integer.valueOf(0), true);
        assertEquals(Integer.valueOf(0), FieldUtils.readField(publiclyShadowedChild, "i", true));
        FieldUtils.writeField(publiclyShadowedChild, "d", Double.valueOf(0.0), true);
        assertEquals(Double.valueOf(0.0), FieldUtils.readField(publiclyShadowedChild, "d", true));

        FieldUtils.writeField(privatelyShadowedChild, "s", "S", true);
        assertEquals("S", FieldUtils.readField(privatelyShadowedChild, "s", true));
        FieldUtils.writeField(privatelyShadowedChild, "b", Boolean.FALSE, true);
        assertEquals(Boolean.FALSE, FieldUtils.readField(privatelyShadowedChild, "b", true));
        FieldUtils.writeField(privatelyShadowedChild, "i", Integer.valueOf(0), true);
        assertEquals(Integer.valueOf(0), FieldUtils.readField(privatelyShadowedChild, "i", true));
        FieldUtils.writeField(privatelyShadowedChild, "d", Double.valueOf(0.0), true);
        assertEquals(Double.valueOf(0.0), FieldUtils.readField(privatelyShadowedChild, "d", true));
    }

    @Test
    public void testWriteDeclaredNamedField() throws Exception {
        try {
            FieldUtils.writeDeclaredField(publicChild, "s", "S");
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            FieldUtils.writeDeclaredField(publicChild, "b", Boolean.TRUE);
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            FieldUtils.writeDeclaredField(publicChild, "i", Integer.valueOf(1));
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            FieldUtils.writeDeclaredField(publicChild, "d", Double.valueOf(1.0));
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }

        FieldUtils.writeDeclaredField(publiclyShadowedChild, "s", "S");
        assertEquals("S", FieldUtils.readDeclaredField(publiclyShadowedChild, "s"));
        FieldUtils.writeDeclaredField(publiclyShadowedChild, "b", Boolean.FALSE);
        assertEquals(Boolean.FALSE, FieldUtils.readDeclaredField(publiclyShadowedChild, "b"));
        FieldUtils.writeDeclaredField(publiclyShadowedChild, "i", Integer.valueOf(0));
        assertEquals(Integer.valueOf(0), FieldUtils.readDeclaredField(publiclyShadowedChild, "i"));
        FieldUtils.writeDeclaredField(publiclyShadowedChild, "d", Double.valueOf(0.0));
        assertEquals(Double.valueOf(0.0), FieldUtils.readDeclaredField(publiclyShadowedChild, "d"));

        try {
            FieldUtils.writeDeclaredField(privatelyShadowedChild, "s", "S");
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            FieldUtils.writeDeclaredField(privatelyShadowedChild, "b", Boolean.TRUE);
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            FieldUtils.writeDeclaredField(privatelyShadowedChild, "i", Integer.valueOf(1));
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            FieldUtils.writeDeclaredField(privatelyShadowedChild, "d", Double.valueOf(1.0));
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
    }

    @Test
    public void testWriteDeclaredNamedFieldForceAccess() throws Exception {
        try {
            FieldUtils.writeDeclaredField(publicChild, "s", "S", true);
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            FieldUtils.writeDeclaredField(publicChild, "b", Boolean.TRUE, true);
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            FieldUtils.writeDeclaredField(publicChild, "i", Integer.valueOf(1), true);
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }
        try {
            FieldUtils.writeDeclaredField(publicChild, "d", Double.valueOf(1.0), true);
            fail("Expected IllegalArgumentException");
        } catch (IllegalArgumentException e) {
            // pass
        }

        FieldUtils.writeDeclaredField(publiclyShadowedChild, "s", "S", true);
        assertEquals("S", FieldUtils.readDeclaredField(publiclyShadowedChild, "s", true));
        FieldUtils.writeDeclaredField(publiclyShadowedChild, "b", Boolean.FALSE, true);
        assertEquals(Boolean.FALSE, FieldUtils.readDeclaredField(publiclyShadowedChild, "b", true));
        FieldUtils.writeDeclaredField(publiclyShadowedChild, "i", Integer.valueOf(0), true);
        assertEquals(Integer.valueOf(0), FieldUtils.readDeclaredField(publiclyShadowedChild, "i", true));
        FieldUtils.writeDeclaredField(publiclyShadowedChild, "d", Double.valueOf(0.0), true);
        assertEquals(Double.valueOf(0.0), FieldUtils.readDeclaredField(publiclyShadowedChild, "d", true));

        FieldUtils.writeDeclaredField(privatelyShadowedChild, "s", "S", true);
        assertEquals("S", FieldUtils.readDeclaredField(privatelyShadowedChild, "s", true));
        FieldUtils.writeDeclaredField(privatelyShadowedChild, "b", Boolean.FALSE, true);
        assertEquals(Boolean.FALSE, FieldUtils.readDeclaredField(privatelyShadowedChild, "b", true));
        FieldUtils.writeDeclaredField(privatelyShadowedChild, "i", Integer.valueOf(0), true);
        assertEquals(Integer.valueOf(0), FieldUtils.readDeclaredField(privatelyShadowedChild, "i", true));
        FieldUtils.writeDeclaredField(privatelyShadowedChild, "d", Double.valueOf(0.0), true);
        assertEquals(Double.valueOf(0.0), FieldUtils.readDeclaredField(privatelyShadowedChild, "d", true));
    }

    @Test(expected=IllegalArgumentException.class)
    public void testAmbig() {
        FieldUtils.getField(Ambig.class, "VALUE");
    }

}

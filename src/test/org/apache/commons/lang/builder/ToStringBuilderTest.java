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
// package org.apache.commons.lang.builder

package org.apache.commons.lang.builder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;
/**
 * Unit tests {@link org.apache.commons.lang.ToStringBuilder}.
 *
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id: ToStringBuilderTest.java,v 1.3 2002/12/31 20:17:53 scolebourne Exp $
 */
public class ToStringBuilderTest extends TestCase {

    private final Integer base = new Integer(5);
    private final String baseStr = base.getClass().getName() + "@" + Integer.toHexString(System.identityHashCode(base));
    
    public ToStringBuilderTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ToStringBuilderTest.class);
        suite.setName("ToStringBuilder Tests");
        return suite;
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    //-----------------------------------------------------------------------

    public void testConstructorEx1() {
        try {
            new ToStringBuilder(null);
            
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }

    public void testConstructorEx2() {
        try {
            new ToStringBuilder(null, null);
            
        } catch (IllegalArgumentException ex) {
            try {
                new ToStringBuilder(base, null);
                
            } catch (Exception ex2) {
                fail();
            }
            return;
        }
        fail();
    }

    public void testConstructorEx3() {
        try {
            new ToStringBuilder(null, null, null);
            
        } catch (IllegalArgumentException ex) {
            try {
                new ToStringBuilder(base, null, null);
                new ToStringBuilder(base, ToStringStyle.DEFAULT_STYLE, null);
                
            } catch (Exception ex2) {
                fail();
            }
            return;
        }
        fail();
    }

    public void testGetSetDefault() {
        try {
            ToStringBuilder.setDefaultStyle(ToStringStyle.NO_FIELD_NAMES_STYLE);
            assertSame(ToStringStyle.NO_FIELD_NAMES_STYLE, ToStringBuilder.getDefaultStyle());
        } finally {
            // reset for other tests
            ToStringBuilder.setDefaultStyle(ToStringStyle.DEFAULT_STYLE);
        }
    }

    public void testSetDefaultEx() {
        try {
            ToStringBuilder.setDefaultStyle(null);
            
        } catch (IllegalArgumentException ex) {
            return;
        }
        fail();
    }

    public void testBlank() {
        assertEquals(baseStr + "[]", new ToStringBuilder(base).toString());
    }
    
	public void testReflection() {
		assertEquals(baseStr + "[value=5]", ToStringBuilder.reflectionToString(base));
	}

    private String toBaseString(Object o) {
        return o.getClass().getName() + "@" + Integer.toHexString(System.identityHashCode(o));
    }

    public void testReflectionHierarchyArrayList() {
        List base = new ArrayList();
        String baseStr = this.toBaseString(base);
        assertEquals(baseStr + "[elementData={<null>,<null>,<null>,<null>,<null>,<null>,<null>,<null>,<null>,<null>},size=0,modCount=0]", ToStringBuilder.reflectionToString(base, null, true));
        assertEquals(baseStr + "[size=0]", ToStringBuilder.reflectionToString(base, null, false));
    }

    public void testReflectionHierarchy() {
        ReflectionTestFixtureA baseA = new ReflectionTestFixtureA();
        String baseStr = this.toBaseString(baseA);
        assertEquals(baseStr + "[a=a]", ToStringBuilder.reflectionToString(baseA));
        assertEquals(baseStr + "[a=a]", ToStringBuilder.reflectionToString(baseA, null));
        assertEquals(baseStr + "[a=a]", ToStringBuilder.reflectionToString(baseA, null, false));
        assertEquals(baseStr + "[a=a,transientA=t]", ToStringBuilder.reflectionToString(baseA, null, true));
        assertEquals(baseStr + "[a=a]", ToStringBuilder.reflectionToString(baseA, null, false, null));
        assertEquals(baseStr + "[a=a]", ToStringBuilder.reflectionToString(baseA, null, false, Object.class));
        assertEquals(baseStr + "[a=a]", ToStringBuilder.reflectionToString(baseA, null, false, List.class));
        assertEquals(baseStr + "[a=a]", ToStringBuilder.reflectionToString(baseA, null, false, ReflectionTestFixtureA.class));
        
        ReflectionTestFixtureB baseB = new ReflectionTestFixtureB();
        baseStr = this.toBaseString(baseB);
        assertEquals(baseStr + "[b=b,a=a]", ToStringBuilder.reflectionToString(baseB));
        assertEquals(baseStr + "[b=b,a=a]", ToStringBuilder.reflectionToString(baseB));
        assertEquals(baseStr + "[b=b,a=a]", ToStringBuilder.reflectionToString(baseB, null));
        assertEquals(baseStr + "[b=b,a=a]", ToStringBuilder.reflectionToString(baseB, null, false));
        assertEquals(baseStr + "[b=b,transientB=t,a=a,transientA=t]", ToStringBuilder.reflectionToString(baseB, null, true));
        assertEquals(baseStr + "[b=b,a=a]", ToStringBuilder.reflectionToString(baseB, null, false, null));
        assertEquals(baseStr + "[b=b,a=a]", ToStringBuilder.reflectionToString(baseB, null, false, Object.class));
        assertEquals(baseStr + "[b=b,a=a]", ToStringBuilder.reflectionToString(baseB, null, false, List.class));
        assertEquals(baseStr + "[b=b,a=a]", ToStringBuilder.reflectionToString(baseB, null, false, ReflectionTestFixtureA.class));
        assertEquals(baseStr + "[b=b]", ToStringBuilder.reflectionToString(baseB, null, false, ReflectionTestFixtureB.class));
    }

	static class ReflectionTestFixtureA {
		private char a='a';
        private transient char transientA='t';
	}

	static class ReflectionTestFixtureB extends ReflectionTestFixtureA {
		private char b='b';
        private transient char transientB='t';
	}

    public void testAppendSuper() {
        assertEquals(baseStr + "[]", new ToStringBuilder(base).appendSuper("Integer@8888[]").toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).appendSuper("Integer@8888[<null>]").toString());
        
        assertEquals(baseStr + "[a=hello]", new ToStringBuilder(base).appendSuper("Integer@8888[]").append("a", "hello").toString());
        assertEquals(baseStr + "[<null>,a=hello]", new ToStringBuilder(base).appendSuper("Integer@8888[<null>]").append("a", "hello").toString());
        assertEquals(baseStr + "[a=hello]", new ToStringBuilder(base).appendSuper(null).append("a", "hello").toString());
    }
    
    public void testAppendToString() {
        assertEquals(baseStr + "[]", new ToStringBuilder(base).appendToString("Integer@8888[]").toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).appendToString("Integer@8888[<null>]").toString());
        
        assertEquals(baseStr + "[a=hello]", new ToStringBuilder(base).appendToString("Integer@8888[]").append("a", "hello").toString());
        assertEquals(baseStr + "[<null>,a=hello]", new ToStringBuilder(base).appendToString("Integer@8888[<null>]").append("a", "hello").toString());
        assertEquals(baseStr + "[a=hello]", new ToStringBuilder(base).appendToString(null).append("a", "hello").toString());
    }
    
    public void testObject() {
        Integer i3 = new Integer(3);
        Integer i4 = new Integer(4);
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) null).toString());
        assertEquals(baseStr + "[3]", new ToStringBuilder(base).append(i3).toString());
        assertEquals(baseStr + "[a=<null>]", new ToStringBuilder(base).append("a", (Object) null).toString());
        assertEquals(baseStr + "[a=3]", new ToStringBuilder(base).append("a", i3).toString());
        assertEquals(baseStr + "[a=3,b=4]", new ToStringBuilder(base).append("a", i3).append("b", i4).toString());
        assertEquals(baseStr + "[a=<Integer>]", new ToStringBuilder(base).append("a", i3, false).toString());
        assertEquals(baseStr + "[a=<size=0>]", new ToStringBuilder(base).append("a", new ArrayList(), false).toString());
        assertEquals(baseStr + "[a=[]]", new ToStringBuilder(base).append("a", new ArrayList(), true).toString());
        assertEquals(baseStr + "[a=<size=0>]", new ToStringBuilder(base).append("a", new HashMap(), false).toString());
        assertEquals(baseStr + "[a={}]", new ToStringBuilder(base).append("a", new HashMap(), true).toString());
        assertEquals(baseStr + "[a=<size=0>]", new ToStringBuilder(base).append("a", (Object) new String[0], false).toString());
        assertEquals(baseStr + "[a={}]", new ToStringBuilder(base).append("a", (Object) new String[0], true).toString());
    }

    public void testLong() {
        assertEquals(baseStr + "[3]", new ToStringBuilder(base).append(3L).toString());
        assertEquals(baseStr + "[a=3]", new ToStringBuilder(base).append("a", 3L).toString());
        assertEquals(baseStr + "[a=3,b=4]", new ToStringBuilder(base).append("a", 3L).append("b", 4L).toString());
    }

    public void testInt() {
        assertEquals(baseStr + "[3]", new ToStringBuilder(base).append((int) 3).toString());
        assertEquals(baseStr + "[a=3]", new ToStringBuilder(base).append("a", (int) 3).toString());
        assertEquals(baseStr + "[a=3,b=4]", new ToStringBuilder(base).append("a", (int) 3).append("b", (int) 4).toString());
    }

    public void testShort() {
        assertEquals(baseStr + "[3]", new ToStringBuilder(base).append((short) 3).toString());
        assertEquals(baseStr + "[a=3]", new ToStringBuilder(base).append("a", (short) 3).toString());
        assertEquals(baseStr + "[a=3,b=4]", new ToStringBuilder(base).append("a", (short) 3).append("b", (short) 4).toString());
    }

    public void testChar() {
        assertEquals(baseStr + "[A]", new ToStringBuilder(base).append((char) 65).toString());
        assertEquals(baseStr + "[a=A]", new ToStringBuilder(base).append("a", (char) 65).toString());
        assertEquals(baseStr + "[a=A,b=B]", new ToStringBuilder(base).append("a", (char) 65).append("b", (char) 66).toString());
    }

    public void testByte() {
        assertEquals(baseStr + "[3]", new ToStringBuilder(base).append((byte) 3).toString());
        assertEquals(baseStr + "[a=3]", new ToStringBuilder(base).append("a", (byte) 3).toString());
        assertEquals(baseStr + "[a=3,b=4]", new ToStringBuilder(base).append("a", (byte) 3).append("b", (byte) 4).toString());
    }

    public void testDouble() {
        assertEquals(baseStr + "[3.2]", new ToStringBuilder(base).append((double) 3.2).toString());
        assertEquals(baseStr + "[a=3.2]", new ToStringBuilder(base).append("a", (double) 3.2).toString());
        assertEquals(baseStr + "[a=3.2,b=4.3]", new ToStringBuilder(base).append("a", (double) 3.2).append("b", (double) 4.3).toString());
    }

    public void testFloat() {
        assertEquals(baseStr + "[3.2]", new ToStringBuilder(base).append((float) 3.2).toString());
        assertEquals(baseStr + "[a=3.2]", new ToStringBuilder(base).append("a", (float) 3.2).toString());
        assertEquals(baseStr + "[a=3.2,b=4.3]", new ToStringBuilder(base).append("a", (float) 3.2).append("b", (float) 4.3).toString());
    }

    public void testBoolean() {
        assertEquals(baseStr + "[true]", new ToStringBuilder(base).append(true).toString());
        assertEquals(baseStr + "[a=true]", new ToStringBuilder(base).append("a", true).toString());
        assertEquals(baseStr + "[a=true,b=false]", new ToStringBuilder(base).append("a", true).append("b", false).toString());
    }


    public void testObjectArray() {
        Object[] array = new Object[] {null, base, new int[] {3, 6}};
        assertEquals(baseStr + "[{<null>,5,{3,6}}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{<null>,5,{3,6}}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    public void testLongArray() {
        long[] array = new long[] {1, 2, -3, 4};
        assertEquals(baseStr + "[{1,2,-3,4}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{1,2,-3,4}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    public void testIntArray() {
        int[] array = new int[] {1, 2, -3, 4};
        assertEquals(baseStr + "[{1,2,-3,4}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{1,2,-3,4}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    public void testShortArray() {
        short[] array = new short[] {1, 2, -3, 4};
        assertEquals(baseStr + "[{1,2,-3,4}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{1,2,-3,4}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }
    
    public void testByteArray() {
        byte[] array = new byte[] {1, 2, -3, 4};
        assertEquals(baseStr + "[{1,2,-3,4}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{1,2,-3,4}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    public void testCharArray() {
        char[] array = new char[] {'A', '2', '_', 'D'};
        assertEquals(baseStr + "[{A,2,_,D}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{A,2,_,D}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    public void testDoubleArray() {
        double[] array = new double[] {1.0, 2.9876, -3.00001, 4.3};
        assertEquals(baseStr + "[{1.0,2.9876,-3.00001,4.3}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{1.0,2.9876,-3.00001,4.3}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    public void testFloatArray() {
        float[] array = new float[] {1.0f, 2.9876f, -3.00001f, 4.3f};
        assertEquals(baseStr + "[{1.0,2.9876,-3.00001,4.3}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{1.0,2.9876,-3.00001,4.3}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }
    
    public void testBooleanArray() {
        boolean[] array = new boolean[] {true, false, false};
        assertEquals(baseStr + "[{true,false,false}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{true,false,false}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }


    public void testLongArrayArray() {
        long[][] array = new long[][] {{1, 2}, null, {5}};
        assertEquals(baseStr + "[{{1,2},<null>,{5}}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{{1,2},<null>,{5}}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    public void testIntArrayArray() {
        int[][] array = new int[][] {{1, 2}, null, {5}};
        assertEquals(baseStr + "[{{1,2},<null>,{5}}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{{1,2},<null>,{5}}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    public void testShortArrayArray() {
        short[][] array = new short[][] {{1, 2}, null, {5}};
        assertEquals(baseStr + "[{{1,2},<null>,{5}}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{{1,2},<null>,{5}}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    public void testByteArrayArray() {
        byte[][] array = new byte[][] {{1, 2}, null, {5}};
        assertEquals(baseStr + "[{{1,2},<null>,{5}}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{{1,2},<null>,{5}}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    public void testCharArrayArray() {
        char[][] array = new char[][] {{'A', 'B'}, null, {'p'}};
        assertEquals(baseStr + "[{{A,B},<null>,{p}}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{{A,B},<null>,{p}}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    public void testDoubleArrayArray() {
        double[][] array = new double[][] {{1.0, 2.29686}, null, {Double.NaN}};
        assertEquals(baseStr + "[{{1.0,2.29686},<null>,{NaN}}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{{1.0,2.29686},<null>,{NaN}}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    public void testFloatArrayArray() {
        float[][] array = new float[][] {{1.0f, 2.29686f}, null, {Float.NaN}};
        assertEquals(baseStr + "[{{1.0,2.29686},<null>,{NaN}}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{{1.0,2.29686},<null>,{NaN}}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

    public void testBooleanArrayArray() {
        boolean[][] array = new boolean[][] {{true, false}, null, {false}};
        assertEquals(baseStr + "[{{true,false},<null>,{false}}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{{true,false},<null>,{false}}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

}

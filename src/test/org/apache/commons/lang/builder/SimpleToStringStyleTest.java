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
// package org.apache.commons.lang.builder

package org.apache.commons.lang.builder;
import java.util.ArrayList;
import java.util.HashMap;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;
/**
 * Unit tests {@link org.apache.commons.lang.ToStringStyle}.
 *
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id: SimpleToStringStyleTest.java,v 1.3 2003/05/21 23:49:15 scolebourne Exp $
 */
public class SimpleToStringStyleTest extends TestCase {

    private final Integer base = new Integer(5);
    
    public SimpleToStringStyleTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(SimpleToStringStyleTest.class);
        suite.setName("DefaultToStringStyle Tests");
        return suite;
    }

    protected void setUp() throws Exception {
        super.setUp();
        ToStringBuilder.setDefaultStyle(ToStringStyle.SIMPLE_STYLE);
    }

    protected void tearDown() throws Exception {
        super.tearDown();
        ToStringBuilder.setDefaultStyle(ToStringStyle.DEFAULT_STYLE);
    }

    //----------------------------------------------------------------
    
    public void testBlank() {
        assertEquals("", new ToStringBuilder(base).toString());
    }

    public void testAppendSuper() {
        assertEquals("", new ToStringBuilder(base).appendSuper("").toString());
        assertEquals("<null>", new ToStringBuilder(base).appendSuper("<null>").toString());
        
        assertEquals("hello", new ToStringBuilder(base).appendSuper("").append("a", "hello").toString());
        assertEquals("<null>,hello", new ToStringBuilder(base).appendSuper("<null>").append("a", "hello").toString());
        assertEquals("hello", new ToStringBuilder(base).appendSuper(null).append("a", "hello").toString());
    }
    
    public void testObject() {
        Integer i3 = new Integer(3);
        Integer i4 = new Integer(4);
        assertEquals("<null>", new ToStringBuilder(base).append((Object) null).toString());
        assertEquals("3", new ToStringBuilder(base).append(i3).toString());
        assertEquals("<null>", new ToStringBuilder(base).append("a", (Object) null).toString());
        assertEquals("3", new ToStringBuilder(base).append("a", i3).toString());
        assertEquals("3,4", new ToStringBuilder(base).append("a", i3).append("b", i4).toString());
        assertEquals("<Integer>", new ToStringBuilder(base).append("a", i3, false).toString());
        assertEquals("<size=0>", new ToStringBuilder(base).append("a", new ArrayList(), false).toString());
        assertEquals("[]", new ToStringBuilder(base).append("a", new ArrayList(), true).toString());
        assertEquals("<size=0>", new ToStringBuilder(base).append("a", new HashMap(), false).toString());
        assertEquals("{}", new ToStringBuilder(base).append("a", new HashMap(), true).toString());
        assertEquals("<size=0>", new ToStringBuilder(base).append("a", (Object) new String[0], false).toString());
        assertEquals("{}", new ToStringBuilder(base).append("a", (Object) new String[0], true).toString());
    }

    public void testLong() {
        assertEquals("3", new ToStringBuilder(base).append(3L).toString());
        assertEquals("3", new ToStringBuilder(base).append("a", 3L).toString());
        assertEquals("3,4", new ToStringBuilder(base).append("a", 3L).append("b", 4L).toString());
    }

    public void testObjectArray() {
        Object[] array = new Object[] {null, base, new int[] {3, 6}};
        assertEquals("{<null>,5,{3,6}}", new ToStringBuilder(base).append(array).toString());
        assertEquals("{<null>,5,{3,6}}", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals("<null>", new ToStringBuilder(base).append(array).toString());
        assertEquals("<null>", new ToStringBuilder(base).append((Object) array).toString());
    }

    public void testLongArray() {
        long[] array = new long[] {1, 2, -3, 4};
        assertEquals("{1,2,-3,4}", new ToStringBuilder(base).append(array).toString());
        assertEquals("{1,2,-3,4}", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals("<null>", new ToStringBuilder(base).append(array).toString());
        assertEquals("<null>", new ToStringBuilder(base).append((Object) array).toString());
    }

    public void testLongArrayArray() {
        long[][] array = new long[][] {{1, 2}, null, {5}};
        assertEquals("{{1,2},<null>,{5}}", new ToStringBuilder(base).append(array).toString());
        assertEquals("{{1,2},<null>,{5}}", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals("<null>", new ToStringBuilder(base).append(array).toString());
        assertEquals("<null>", new ToStringBuilder(base).append((Object) array).toString());
    }

}

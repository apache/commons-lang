/*
 * Copyright 2002-2005 The Apache Software Foundation.
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
package org.apache.commons.lang.builder;

import java.util.ArrayList;
import java.util.HashMap;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Unit tests {@link org.apache.commons.lang.builder.NoFieldNamesToStringStyleTest}.
 *
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id$
 */
public class NoFieldNamesToStringStyleTest extends TestCase {

    private final Integer base = new Integer(5);
    private final String baseStr = base.getClass().getName() + "@" + Integer.toHexString(System.identityHashCode(base));
    
    public NoFieldNamesToStringStyleTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(NoFieldNamesToStringStyleTest.class);
        suite.setName("NoFieldNamesToStringStyle Tests");
        return suite;
    }

    protected void setUp() throws Exception {
        super.setUp();
        ToStringBuilder.setDefaultStyle(ToStringStyle.NO_FIELD_NAMES_STYLE);
    }

    protected void tearDown() throws Exception {
        super.tearDown();
        ToStringBuilder.setDefaultStyle(ToStringStyle.DEFAULT_STYLE);
    }

    //----------------------------------------------------------------
    
    public void testBlank() {
        assertEquals(baseStr + "[]", new ToStringBuilder(base).toString());
    }

    public void testAppendSuper() {
        assertEquals(baseStr + "[]", new ToStringBuilder(base).appendSuper("Integer@8888[]").toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).appendSuper("Integer@8888[<null>]").toString());
        
        assertEquals(baseStr + "[hello]", new ToStringBuilder(base).appendSuper("Integer@8888[]").append("a", "hello").toString());
        assertEquals(baseStr + "[<null>,hello]", new ToStringBuilder(base).appendSuper("Integer@8888[<null>]").append("a", "hello").toString());
        assertEquals(baseStr + "[hello]", new ToStringBuilder(base).appendSuper(null).append("a", "hello").toString());
    }
    
    public void testObject() {
        Integer i3 = new Integer(3);
        Integer i4 = new Integer(4);
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) null).toString());
        assertEquals(baseStr + "[3]", new ToStringBuilder(base).append(i3).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append("a", (Object) null).toString());
        assertEquals(baseStr + "[3]", new ToStringBuilder(base).append("a", i3).toString());
        assertEquals(baseStr + "[3,4]", new ToStringBuilder(base).append("a", i3).append("b", i4).toString());
        assertEquals(baseStr + "[<Integer>]", new ToStringBuilder(base).append("a", i3, false).toString());
        assertEquals(baseStr + "[<size=0>]", new ToStringBuilder(base).append("a", new ArrayList(), false).toString());
        assertEquals(baseStr + "[[]]", new ToStringBuilder(base).append("a", new ArrayList(), true).toString());
        assertEquals(baseStr + "[<size=0>]", new ToStringBuilder(base).append("a", new HashMap(), false).toString());
        assertEquals(baseStr + "[{}]", new ToStringBuilder(base).append("a", new HashMap(), true).toString());
        assertEquals(baseStr + "[<size=0>]", new ToStringBuilder(base).append("a", (Object) new String[0], false).toString());
        assertEquals(baseStr + "[{}]", new ToStringBuilder(base).append("a", (Object) new String[0], true).toString());
    }

    public void testLong() {
        assertEquals(baseStr + "[3]", new ToStringBuilder(base).append(3L).toString());
        assertEquals(baseStr + "[3]", new ToStringBuilder(base).append("a", 3L).toString());
        assertEquals(baseStr + "[3,4]", new ToStringBuilder(base).append("a", 3L).append("b", 4L).toString());
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

    public void testLongArrayArray() {
        long[][] array = new long[][] {{1, 2}, null, {5}};
        assertEquals(baseStr + "[{{1,2},<null>,{5}}]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[{{1,2},<null>,{5}}]", new ToStringBuilder(base).append((Object) array).toString());
        array = null;
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append(array).toString());
        assertEquals(baseStr + "[<null>]", new ToStringBuilder(base).append((Object) array).toString());
    }

}

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
package org.apache.commons.lang;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Unit tests {@link org.apache.commons.lang.CharSetUtils}.
 *
 * @author <a href="mailto:bayard@generationjava.com">Henri Yandell</a>
 * @author <a href="mailto:ridesmet@users.sourceforge.net">Ringo De Smet</a>
 * @version $Id: CharSetUtilsTest.java,v 1.7 2003/03/23 21:47:30 scolebourne Exp $
 */
public class CharSetUtilsTest extends TestCase
{
    public CharSetUtilsTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
    	TestSuite suite = new TestSuite(CharSetUtilsTest.class);
    	suite.setName("CharSetUtils Tests");
        return suite;
    }

    protected void setUp() throws Exception {
        super.setUp();
    }

    protected void tearDown() throws Exception {
        super.tearDown();
    }

    //-----------------------------------------------------------------------

    public void testSqueeze()
    {
        assertEquals("squeeze(String,String[]) failed",
                     "helo", CharSetUtils.squeeze("hello", new String[] {"el"}));
        assertEquals("squeeze(String,String[]) failed",
                     "", CharSetUtils.squeeze("", new String[] {"el"}));
        assertEquals("squeeze(String,String[]) failed",
                     "hello", CharSetUtils.squeeze("hello", new String[] {"e"}));
        assertEquals("squeeze(String,String[]) failed",
                     "fofof", CharSetUtils.squeeze("fooffooff", new String[] {"of"}));
        assertEquals("squeeze(String,String[]) failed",
                     "fof", CharSetUtils.squeeze("fooooff", new String[] {"fo"}));
    }

    public void testCount()
    {
        assertEquals("count(String,String[]) failed",
                     3, CharSetUtils.count("hello", new String[] {"el"}));
        assertEquals("count(String,String[]) failed",
                     0, CharSetUtils.count("", new String[] {"el"}));
        assertEquals("count(String,String[]) failed",
                     0, CharSetUtils.count("hello", new String[] {"x"}));
        assertEquals("count(String,String[]) failed",
                     2, CharSetUtils.count("hello", new String[] {"e-i"}));
        assertEquals("count(String,String[]) failed",
                     5, CharSetUtils.count("hello", new String[] {"a-z"}));
        assertEquals("count(String,String[]) failed",
                     0, CharSetUtils.count("hello", new String[] {""}));
    }

    public void testKeep()
    {
        assertEquals("keep(String,String[]) failed",
                     "ell", CharSetUtils.keep("hello", new String[] {"el"}));
        assertEquals("keep(String,String[]) failed",
                     "hello", CharSetUtils.keep("hello", new String[] {"elho"}));
        assertEquals("keep(String,String[]) failed",
                     "", CharSetUtils.keep("hello", new String[] {""}));
        assertEquals("keep(String,String[]) failed",
                     "hello", CharSetUtils.keep("hello", new String[] {"a-z"}));
        assertEquals("keep(String,String[]) failed",
                     "----", CharSetUtils.keep("----", new String[] {"-"}));
        assertEquals("keep(String,String[]) failed",
                     "ll", CharSetUtils.keep("hello", new String[] {"l"}));
    }

    public void testDelete()
    {
        assertEquals("delete(String,String[]) failed",
                     "ho", CharSetUtils.delete("hello", new String[] {"el"}));
        assertEquals("delete(String,String[]) failed",
                     "", CharSetUtils.delete("hello", new String[] {"elho"}));
        assertEquals("delete(String,String[]) failed",
                     "hello", CharSetUtils.delete("hello", new String[] {""}));
        assertEquals("delete(String,String[]) failed",
                     "", CharSetUtils.delete("hello", new String[] {"a-z"}));
        assertEquals("delete(String,String[]) failed",
                     "", CharSetUtils.delete("----", new String[] {"-"}));
        assertEquals("delete(String,String[]) failed",
                     "heo", CharSetUtils.delete("hello", new String[] {"l"}));
    }
}


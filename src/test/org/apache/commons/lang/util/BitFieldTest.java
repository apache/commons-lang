/*
 * $Header: /home/jerenkrantz/tmp/commons/commons-convert/cvs/home/cvs/jakarta-commons//lang/src/test/org/apache/commons/lang/util/Attic/BitFieldTest.java,v 1.1 2002/12/18 02:50:36 bayard Exp $
 * $Revision: 1.1 $
 * $Date: 2002/12/18 02:50:36 $
 *
 * ====================================================================
 *
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

package org.apache.commons.lang.util;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * Class to test BitField functionality
 *
 * @author Scott Sanders (sanders at apache dot org)
 * @author Marc Johnson
 * @author Glen Stampoultzis (gstamp@iprimus.com.au)
 * @version $Id: BitFieldTest.java,v 1.1 2002/12/18 02:50:36 bayard Exp $
 */

public class BitFieldTest
    extends TestCase
{

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
    	TestSuite suite = new TestSuite(BitFieldTest.class);
    	suite.setName("BitField Tests");
        return suite;
    }

    private static BitField bf_multi  = new BitField(0x3F80);
    private static BitField bf_single = new BitField(0x4000);

    /**
     * Constructor BitFieldTest
     *
     * @param name
     */

    public BitFieldTest(String name)
    {
        super(name);
    }

    /**
     * test the getValue() method
     */

    public void testGetValue()
    {
        assertEquals(bf_multi.getValue(-1), 127);
        assertEquals(bf_multi.getValue(0), 0);
        assertEquals(bf_single.getValue(-1), 1);
        assertEquals(bf_single.getValue(0), 0);
    }

    /**
     * test the getShortValue() method
     */

    public void testGetShortValue()
    {
        assertEquals(bf_multi.getShortValue(( short ) -1), ( short ) 127);
        assertEquals(bf_multi.getShortValue(( short ) 0), ( short ) 0);
        assertEquals(bf_single.getShortValue(( short ) -1), ( short ) 1);
        assertEquals(bf_single.getShortValue(( short ) 0), ( short ) 0);
    }

    /**
     * test the getRawValue() method
     */

    public void testGetRawValue()
    {
        assertEquals(bf_multi.getRawValue(-1), 0x3F80);
        assertEquals(bf_multi.getRawValue(0), 0);
        assertEquals(bf_single.getRawValue(-1), 0x4000);
        assertEquals(bf_single.getRawValue(0), 0);
    }

    /**
     * test the getShortRawValue() method
     */

    public void testGetShortRawValue()
    {
        assertEquals(bf_multi.getShortRawValue(( short ) -1),
                     ( short ) 0x3F80);
        assertEquals(bf_multi.getShortRawValue(( short ) 0), ( short ) 0);
        assertEquals(bf_single.getShortRawValue(( short ) -1),
                     ( short ) 0x4000);
        assertEquals(bf_single.getShortRawValue(( short ) 0), ( short ) 0);
    }

    /**
     * test the isSet() method
     */

    public void testIsSet()
    {
        assertTrue(!bf_multi.isSet(0));
        for (int j = 0x80; j <= 0x3F80; j += 0x80)
        {
            assertTrue(bf_multi.isSet(j));
        }
        assertTrue(!bf_single.isSet(0));
        assertTrue(bf_single.isSet(0x4000));
    }

    /**
     * test the isAllSet() method
     */

    public void testIsAllSet()
    {
        for (int j = 0; j < 0x3F80; j += 0x80)
        {
            assertTrue(!bf_multi.isAllSet(j));
        }
        assertTrue(bf_multi.isAllSet(0x3F80));
        assertTrue(!bf_single.isAllSet(0));
        assertTrue(bf_single.isAllSet(0x4000));
    }

    /**
     * test the setValue() method
     */

    public void testSetValue()
    {
        for (int j = 0; j < 128; j++)
        {
            assertEquals(bf_multi.getValue(bf_multi.setValue(0, j)), j);
            assertEquals(bf_multi.setValue(0, j), j << 7);
        }

        // verify that excess bits are stripped off
        assertEquals(bf_multi.setValue(0x3f80, 128), 0);
        for (int j = 0; j < 2; j++)
        {
            assertEquals(bf_single.getValue(bf_single.setValue(0, j)), j);
            assertEquals(bf_single.setValue(0, j), j << 14);
        }

        // verify that excess bits are stripped off
        assertEquals(bf_single.setValue(0x4000, 2), 0);
    }

    /**
     * test the setShortValue() method
     */

    public void testSetShortValue()
    {
        for (int j = 0; j < 128; j++)
        {
            assertEquals(bf_multi
                .getShortValue(bf_multi
                    .setShortValue(( short ) 0, ( short ) j)), ( short ) j);
            assertEquals(bf_multi.setShortValue(( short ) 0, ( short ) j),
                         ( short ) (j << 7));
        }

        // verify that excess bits are stripped off
        assertEquals(bf_multi.setShortValue(( short ) 0x3f80, ( short ) 128),
                     ( short ) 0);
        for (int j = 0; j < 2; j++)
        {
            assertEquals(bf_single
                .getShortValue(bf_single
                    .setShortValue(( short ) 0, ( short ) j)), ( short ) j);
            assertEquals(bf_single.setShortValue(( short ) 0, ( short ) j),
                         ( short ) (j << 14));
        }

        // verify that excess bits are stripped off
        assertEquals(bf_single.setShortValue(( short ) 0x4000, ( short ) 2),
                     ( short ) 0);
    }

    public void testByte()
    {
        assertEquals(1, new BitField(1).setByteBoolean(( byte ) 0, true));
        assertEquals(2, new BitField(2).setByteBoolean(( byte ) 0, true));
        assertEquals(4, new BitField(4).setByteBoolean(( byte ) 0, true));
        assertEquals(8, new BitField(8).setByteBoolean(( byte ) 0, true));
        assertEquals(16, new BitField(16).setByteBoolean(( byte ) 0, true));
        assertEquals(32, new BitField(32).setByteBoolean(( byte ) 0, true));
        assertEquals(64, new BitField(64).setByteBoolean(( byte ) 0, true));
        assertEquals(-128,
                     new BitField(128).setByteBoolean(( byte ) 0, true));
        assertEquals(0, new BitField(1).setByteBoolean(( byte ) 1, false));
        assertEquals(0, new BitField(2).setByteBoolean(( byte ) 2, false));
        assertEquals(0, new BitField(4).setByteBoolean(( byte ) 4, false));
        assertEquals(0, new BitField(8).setByteBoolean(( byte ) 8, false));
        assertEquals(0, new BitField(16).setByteBoolean(( byte ) 16, false));
        assertEquals(0, new BitField(32).setByteBoolean(( byte ) 32, false));
        assertEquals(0, new BitField(64).setByteBoolean(( byte ) 64, false));
        assertEquals(0, new BitField(128).setByteBoolean(( byte ) 128,
                                     false));
        assertEquals(-2, new BitField(1).setByteBoolean(( byte ) 255, false));
        byte clearedBit = new BitField(0x40).setByteBoolean(( byte ) -63,
                                       false);

        assertEquals(false, new BitField(0x40).isSet(clearedBit));
    }

    /**
     * test the clear() method
     */

    public void testClear()
    {
        assertEquals(bf_multi.clear(-1), 0xFFFFC07F);
        assertEquals(bf_single.clear(-1), 0xFFFFBFFF);
    }

    /**
     * test the clearShort() method
     */

    public void testClearShort()
    {
        assertEquals(bf_multi.clearShort(( short ) -1), ( short ) 0xC07F);
        assertEquals(bf_single.clearShort(( short ) -1), ( short ) 0xBFFF);
    }

    /**
     * test the set() method
     */

    public void testSet()
    {
        assertEquals(bf_multi.set(0), 0x3F80);
        assertEquals(bf_single.set(0), 0x4000);
    }

    /**
     * test the setShort() method
     */

    public void testSetShort()
    {
        assertEquals(bf_multi.setShort(( short ) 0), ( short ) 0x3F80);
        assertEquals(bf_single.setShort(( short ) 0), ( short ) 0x4000);
    }

    /**
     * test the setBoolean() method
     */

    public void testSetBoolean()
    {
        assertEquals(bf_multi.set(0), bf_multi.setBoolean(0, true));
        assertEquals(bf_single.set(0), bf_single.setBoolean(0, true));
        assertEquals(bf_multi.clear(-1), bf_multi.setBoolean(-1, false));
        assertEquals(bf_single.clear(-1), bf_single.setBoolean(-1, false));
    }

    /**
     * test the setShortBoolean() method
     */

    public void testSetShortBoolean()
    {
        assertEquals(bf_multi.setShort(( short ) 0),
                     bf_multi.setShortBoolean(( short ) 0, true));
        assertEquals(bf_single.setShort(( short ) 0),
                     bf_single.setShortBoolean(( short ) 0, true));
        assertEquals(bf_multi.clearShort(( short ) -1),
                     bf_multi.setShortBoolean(( short ) -1, false));
        assertEquals(bf_single.clearShort(( short ) -1),
                     bf_single.setShortBoolean(( short ) -1, false));
    }

}

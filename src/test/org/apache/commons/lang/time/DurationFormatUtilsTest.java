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
package org.apache.commons.lang.time;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.textui.TestRunner;

/**
 * TestCase for DurationFormatUtils.
 *
 * @author Apache Ant - DateUtilsTest
 * @author <a href="mailto:sbailliez@apache.org">Stephane Bailliez</a>
 * @author <a href="mailto:stefan.bodewig@epost.de">Stefan Bodewig</a>
 * @author Stephen Colebourne
 */
public class DurationFormatUtilsTest extends TestCase {

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
    	TestSuite suite = new TestSuite(DurationFormatUtilsTest.class);
    	suite.setName("DurationFormatUtils Tests");
        return suite;
    }

    public DurationFormatUtilsTest(String s) {
        super(s);
    }

    public void testFormatWords(){
        String text = null;
        
        text = DurationFormatUtils.formatWords(50*1000, true, false);
        assertEquals("50 seconds", text);
        text = DurationFormatUtils.formatWords(65*1000, true, false);
        assertEquals("1 minute 5 seconds", text);
        text = DurationFormatUtils.formatWords(120*1000, true, false);
        assertEquals("2 minutes 0 seconds", text);
        text = DurationFormatUtils.formatWords(121*1000, true, false);
        assertEquals("2 minutes 1 second", text);
        text = DurationFormatUtils.formatWords(72*60*1000, true, false);
        assertEquals("1 hour 12 minutes 0 seconds", text);
        
        text = DurationFormatUtils.formatWords(50*1000, true, true);
        assertEquals("50 seconds", text);
        text = DurationFormatUtils.formatWords(65*1000, true, true);
        assertEquals("1 minute 5 seconds", text);
        text = DurationFormatUtils.formatWords(120*1000, true, true);
        assertEquals("2 minutes", text);
        text = DurationFormatUtils.formatWords(121*1000, true, true);
        assertEquals("2 minutes 1 second", text);
        text = DurationFormatUtils.formatWords(72*60*1000, true, true);
        assertEquals("1 hour 12 minutes", text);
        
        text = DurationFormatUtils.formatWords(50*1000, false, true);
        assertEquals("0 days 0 hours 0 minutes 50 seconds", text);
        text = DurationFormatUtils.formatWords(65*1000, false, true);
        assertEquals("0 days 0 hours 1 minute 5 seconds", text);
        text = DurationFormatUtils.formatWords(120*1000, false, true);
        assertEquals("0 days 0 hours 2 minutes", text);
        text = DurationFormatUtils.formatWords(121*1000, false, true);
        assertEquals("0 days 0 hours 2 minutes 1 second", text);
        text = DurationFormatUtils.formatWords(72*60*1000, false, true);
        assertEquals("0 days 1 hour 12 minutes", text);
        
        text = DurationFormatUtils.formatWords(50*1000, false, false);
        assertEquals("0 days 0 hours 0 minutes 50 seconds", text);
        text = DurationFormatUtils.formatWords(65*1000, false, false);
        assertEquals("0 days 0 hours 1 minute 5 seconds", text);
        text = DurationFormatUtils.formatWords(120*1000, false, false);
        assertEquals("0 days 0 hours 2 minutes 0 seconds", text);
        text = DurationFormatUtils.formatWords(121*1000, false, false);
        assertEquals("0 days 0 hours 2 minutes 1 second", text);
        text = DurationFormatUtils.formatWords(72*60*1000, false, false);
        assertEquals("0 days 1 hour 12 minutes 0 seconds", text);
    }

    public void testFormatISOStyle(){
        long time = 0;
        assertEquals("0:00:00.000", DurationFormatUtils.formatISO(time));
        
        time = 1;
        assertEquals("0:00:00.001", DurationFormatUtils.formatISO(time));
        
        time = 15;
        assertEquals("0:00:00.015", DurationFormatUtils.formatISO(time));
        
        time = 165;
        assertEquals("0:00:00.165", DurationFormatUtils.formatISO(time));
        
        time = 1675;
        assertEquals("0:00:01.675", DurationFormatUtils.formatISO(time));
        
        time = 13465;
        assertEquals("0:00:13.465", DurationFormatUtils.formatISO(time));
        
        time = 72789;
        assertEquals("0:01:12.789", DurationFormatUtils.formatISO(time));
        
        time = 12789 + 32 * 60000;
        assertEquals("0:32:12.789", DurationFormatUtils.formatISO(time));
        
        time = 12789 + 62 * 60000;
        assertEquals("1:02:12.789", DurationFormatUtils.formatISO(time));
    }
    
}

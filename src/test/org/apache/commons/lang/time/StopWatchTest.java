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
 * TestCase for StopWatch.
 *
 * @author Stephen Colebourne
 * @version $Id: StopWatchTest.java,v 1.3 2003/06/08 23:14:23 scolebourne Exp $
 */
public class StopWatchTest extends TestCase {

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
    	TestSuite suite = new TestSuite(StopWatchTest.class);
    	suite.setName("StopWatch Tests");
        return suite;
    }

    public StopWatchTest(String s) {
        super(s);
    }

    public void testStopWatchSimple(){
        StopWatch watch = new StopWatch();
        assertEquals(0, watch.getTime());
        
        watch.start();
            try {Thread.sleep(550);} catch (InterruptedException ex) {}
        watch.stop();
        long time = watch.getTime();
        assertEquals(time, watch.getTime());
        
        assertTrue(time >= 500);
        assertTrue(time < 650);
        
        watch.reset();
        assertEquals(0, watch.getTime());
    }
    
    public void testStopWatchSplit(){
        StopWatch watch = new StopWatch();
        watch.start();
            try {Thread.sleep(550);} catch (InterruptedException ex) {}
        watch.split();
        long splitTime = watch.getTime();
            try {Thread.sleep(550);} catch (InterruptedException ex) {}
        watch.unsplit();
            try {Thread.sleep(550);} catch (InterruptedException ex) {}
        watch.stop();
        long totalTime = watch.getTime();
        
//        System.err.println(splitTime +"  "+totalTime);
        assertTrue(splitTime >= 500);
        assertTrue(splitTime < 650);
        assertTrue(totalTime >= 1500);
        assertTrue(totalTime < 1800);
    }
    
    public void testStopWatchSuspend(){
        StopWatch watch = new StopWatch();
        watch.start();
            try {Thread.sleep(550);} catch (InterruptedException ex) {}
        watch.suspend();
        long suspendTime = watch.getTime();
            try {Thread.sleep(550);} catch (InterruptedException ex) {}
        watch.resume();
            try {Thread.sleep(550);} catch (InterruptedException ex) {}
        watch.stop();
        long totalTime = watch.getTime();
        
//        System.err.println(suspendTime +"  "+totalTime);
        assertTrue(suspendTime >= 500);
        assertTrue(suspendTime < 600);
        assertTrue(totalTime >= 1000);
        assertTrue(totalTime < 1200);
    }

}

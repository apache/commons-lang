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
 *    any, must include the following acknowledgement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowledgement may appear in the software itself,
 *    if and wherever such third-party acknowledgements normally appear.
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

public class EntitiesPerformanceTest extends TestCase {
    private int COUNT = 10000;
    private int STRING_LENGTH = 1000;

    private static String stringWithUnicode;
    private static String stringWithEntities;
    private static Entities treeEntities;
    private static Entities hashEntities;
    private static Entities arrayEntities;
    private static Entities binaryEntities;
    private static Entities primitiveEntities;
    private static Entities lookupEntities;

    public EntitiesPerformanceTest(String name) {
        super(name);
    }

    public static void main(String[] args) {
        TestRunner.run(suite());
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(EntitiesPerformanceTest.class);
        return suite;
    }

    public void setUp() {
        if (stringWithUnicode == null) {
            StringBuffer buf = new StringBuffer(STRING_LENGTH);
            for (int i = 0; i < STRING_LENGTH / 5; ++i) {
                buf.append("xxxx");
                char ch = isovalue(i);
                buf.append(ch);
            }
            stringWithUnicode = buf.toString();
            stringWithEntities = Entities.HTML40.unescape(stringWithUnicode);
        }
    }

    private char html40value(int i) {
        String entityValue = Entities.HTML40_ARRAY[i % Entities.HTML40_ARRAY.length][1];
        char ch = (char) Integer.parseInt(entityValue);
        return ch;
    }

    private char isovalue(int i) {
        String entityValue = Entities.ISO8859_1_ARRAY[i % Entities.ISO8859_1_ARRAY.length][1];
        char ch = (char) Integer.parseInt(entityValue);
        return ch;
    }

    public void testBuildHash() throws Exception {
        for (int i = 0; i < COUNT; ++i) {
            hashEntities = build(new Entities.HashEntityMap());
        }
    }


    public void testBuildTree() throws Exception {
        for (int i = 0; i < COUNT; ++i) {
            treeEntities = build(new Entities.TreeEntityMap());
        }
    }

    public void testBuildArray() throws Exception {
        for (int i = 0; i < COUNT; ++i) {
            arrayEntities = build(new Entities.ArrayEntityMap());
        }
    }

    public void testBuildBinary() throws Exception {
        for (int i = 0; i < COUNT; ++i) {
            binaryEntities = build(new Entities.BinaryEntityMap());
        }
    }

    public void testBuildPrimitive() throws Exception {
        for (int i = 0; i < COUNT; ++i) {
            buildPrimitive();
        }
    }

    private void buildPrimitive()
    {
        primitiveEntities = build(new Entities.PrimitiveEntityMap());
    }

    public void testBuildLookup() throws Exception {
        for (int i = 0; i < COUNT; ++i) {
            buildLookup();
        }
    }

    private void buildLookup()
    {
        lookupEntities = build(new Entities.LookupEntityMap());
    }

    private Entities build(Entities.EntityMap intMap) {
        Entities entities;
        entities = new Entities();
        entities.map = intMap;
        Entities.fillWithHtml40Entities(entities);
        return entities;
    }

    public void testLookupHash() throws Exception {
        lookup(hashEntities);
    }

    public void testLookupTree() throws Exception {
        lookup(treeEntities);
    }

    public void testLookupArray() throws Exception {
        lookup(arrayEntities);
    }

    public void testLookupBinary() throws Exception {
        lookup(binaryEntities);
    }

    public void testLookupPrimitive() throws Exception {
        if (primitiveEntities == null) buildPrimitive();
        lookup(primitiveEntities);
    }

    public void testLookupLookup() throws Exception {
        if (lookupEntities == null) buildLookup();
        lookup(lookupEntities);
    }

    public void testEscapeHash() throws Exception {
        escapeIt(hashEntities);
    }

    public void testEscapeTree() throws Exception {
        escapeIt(treeEntities);
    }

    public void testEscapeArray() throws Exception {
        escapeIt(arrayEntities);
    }

    public void testEscapeBinary() throws Exception {
        escapeIt(binaryEntities);
    }

    public void testEscapePrimitive() throws Exception {
        escapeIt(primitiveEntities);
    }

    public void testEscapeLookup() throws Exception {
        escapeIt(lookupEntities);
    }

    public void testUnescapeHash() throws Exception {
        unescapeIt(hashEntities);
    }

    public void testUnescapeTree() throws Exception {
        unescapeIt(treeEntities);
    }

    public void testUnescapeArray() throws Exception {
        unescapeIt(arrayEntities);
    }

    public void testUnescapeBinary() throws Exception {
        unescapeIt(binaryEntities);
    }

    private void lookup(Entities entities) {
        for (int i = 0; i < COUNT * 1000; ++i) {
            entities.entityName(isovalue(i));
        }
    }

    private void escapeIt(Entities entities) {
        for (int i = 0; i < COUNT; ++i) {
            String escaped = entities.escape(stringWithUnicode);
            assertEquals("xxxx&nbsp;", escaped.substring(0, 10));
        }
    }

    private void unescapeIt(Entities entities) {
        for (int i = 0; i < COUNT; ++i) {
            String unescaped = entities.unescape(stringWithEntities);
            assertEquals("xxxx\u00A0", unescaped.substring(0, 5));
        }
    }


}


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

package org.apache.commons.lang3;

import static org.apache.commons.lang3.DigitalBase10SizeUnit.BITS;
import static org.apache.commons.lang3.DigitalBase10SizeUnit.BYTES;
import static org.apache.commons.lang3.DigitalBase10SizeUnit.GIGABYTES;
import static org.apache.commons.lang3.DigitalBase10SizeUnit.KILOBYTES;
import static org.apache.commons.lang3.DigitalBase10SizeUnit.MEGABYTES;
import static org.apache.commons.lang3.DigitalBase10SizeUnit.TERABYTES;

import org.junit.Assert;
import org.junit.Test;

public class DigitalBase10SizeUnitTest {

    @Test
    public void testToBits() throws Exception {
        Assert.assertEquals(1, BITS.toBits(1));
        Assert.assertEquals(16, BYTES.toBits(2));
        Assert.assertEquals(2000, KILOBYTES.toBits(2));
        Assert.assertEquals(2000000, MEGABYTES.toBits(2));
        Assert.assertEquals(2000000000L, GIGABYTES.toBits(2));
        Assert.assertEquals(2000000000000L, TERABYTES.toBits(2));
        //
        Assert.assertEquals(2, BYTES.convert(2, BYTES));
        Assert.assertEquals(2000, BYTES.convert(2, KILOBYTES));
        Assert.assertEquals(2000000, BYTES.convert(2, MEGABYTES));
        Assert.assertEquals(2000000000L, BYTES.convert(2, GIGABYTES));
        Assert.assertEquals(2000000000000L, BYTES.convert(2, TERABYTES));
    }

    @Test
    public void testToBytes() throws Exception {
        Assert.assertEquals(0, BITS.toBytes(2));
        Assert.assertEquals(2, BYTES.toBytes(2));
        Assert.assertEquals(2000, KILOBYTES.toBytes(2));
        Assert.assertEquals(2000000, MEGABYTES.toBytes(2));
        Assert.assertEquals(2000000000L, GIGABYTES.toBytes(2));
        Assert.assertEquals(2000000000000L, TERABYTES.toBytes(2));
        //
        Assert.assertEquals(2, BYTES.convert(2, BYTES));
        Assert.assertEquals(2000, BYTES.convert(2, KILOBYTES));
        Assert.assertEquals(2000000, BYTES.convert(2, MEGABYTES));
        Assert.assertEquals(2000000000L, BYTES.convert(2, GIGABYTES));
        Assert.assertEquals(2000000000000L, BYTES.convert(2, TERABYTES));
    }

    @Test
    public void testToGigabytes() throws Exception {
        Assert.assertEquals(2, BYTES.toGigabytes(2147483648l));
        Assert.assertEquals(2, KILOBYTES.toGigabytes(2000000));
        Assert.assertEquals(2, MEGABYTES.toGigabytes(2000));
        Assert.assertEquals(2, GIGABYTES.toGigabytes(2));
        Assert.assertEquals(2000, TERABYTES.toGigabytes(2));
        //
        Assert.assertEquals(2, GIGABYTES.convert(2000000000L, BYTES));
        Assert.assertEquals(2, GIGABYTES.convert(2000000, KILOBYTES));
        Assert.assertEquals(2, GIGABYTES.convert(2000, MEGABYTES));
        Assert.assertEquals(2, GIGABYTES.convert(2, GIGABYTES));
        Assert.assertEquals(2000, GIGABYTES.convert(2, TERABYTES));
    }

    @Test
    public void testToKilobytes() throws Exception {
        Assert.assertEquals(2, BYTES.toKilobytes(2000));
        Assert.assertEquals(2, KILOBYTES.toKilobytes(2));
        Assert.assertEquals(2000, MEGABYTES.toKilobytes(2));
        Assert.assertEquals(2000000, GIGABYTES.toKilobytes(2));
        Assert.assertEquals(2000000000L, TERABYTES.toKilobytes(2));
        //
        Assert.assertEquals(2, KILOBYTES.convert(2000, BYTES));
        Assert.assertEquals(2, KILOBYTES.convert(2, KILOBYTES));
        Assert.assertEquals(2000, KILOBYTES.convert(2, MEGABYTES));
        Assert.assertEquals(2000000, KILOBYTES.convert(2, GIGABYTES));
        Assert.assertEquals(2000000000L, KILOBYTES.convert(2, TERABYTES));
    }

    @Test
    public void testToMegabytes() throws Exception {
        Assert.assertEquals(2, BYTES.toMegabytes(2000000));
        Assert.assertEquals(2, KILOBYTES.toMegabytes(2000));
        Assert.assertEquals(2, MEGABYTES.toMegabytes(2));
        Assert.assertEquals(2000, GIGABYTES.toMegabytes(2));
        Assert.assertEquals(2000000, TERABYTES.toMegabytes(2));
        //
        Assert.assertEquals(2, MEGABYTES.convert(2000000, BYTES));
        Assert.assertEquals(2, MEGABYTES.convert(2000, KILOBYTES));
        Assert.assertEquals(2, MEGABYTES.convert(2, MEGABYTES));
        Assert.assertEquals(2000, MEGABYTES.convert(2, GIGABYTES));
        Assert.assertEquals(2000000, MEGABYTES.convert(2, TERABYTES));
    }

    @Test
    public void testToTerabytes() throws Exception {
        Assert.assertEquals(2, BYTES.toTerabytes(2199023255552l));
        Assert.assertEquals(2, KILOBYTES.toTerabytes(2147483648l));
        Assert.assertEquals(2, MEGABYTES.toTerabytes(2000000));
        Assert.assertEquals(2, GIGABYTES.toTerabytes(2000));
        Assert.assertEquals(2, TERABYTES.toTerabytes(2));
        //
        Assert.assertEquals(2, TERABYTES.convert(2000000000000L, BYTES));
        Assert.assertEquals(2, TERABYTES.convert(2000000000L, KILOBYTES));
        Assert.assertEquals(2, TERABYTES.convert(2000000, MEGABYTES));
        Assert.assertEquals(2, TERABYTES.convert(2000, GIGABYTES));
        Assert.assertEquals(2, TERABYTES.convert(2, TERABYTES));
    }
}
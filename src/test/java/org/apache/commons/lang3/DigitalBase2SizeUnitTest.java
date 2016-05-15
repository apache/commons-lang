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

import static org.apache.commons.lang3.DigitalBase2SizeUnit.BITS;
import static org.apache.commons.lang3.DigitalBase2SizeUnit.BYTES;
import static org.apache.commons.lang3.DigitalBase2SizeUnit.GIGABYTES;
import static org.apache.commons.lang3.DigitalBase2SizeUnit.KILOBYTES;
import static org.apache.commons.lang3.DigitalBase2SizeUnit.MEGABYTES;
import static org.apache.commons.lang3.DigitalBase2SizeUnit.TERABYTES;

import org.junit.Assert;
import org.junit.Test;

public class DigitalBase2SizeUnitTest {

    @Test
    public void testToBits() throws Exception {
        Assert.assertEquals(1, BITS.toBits(1));
        Assert.assertEquals(16, BYTES.toBits(2));
        Assert.assertEquals(2048, KILOBYTES.toBits(2));
        Assert.assertEquals(2097152, MEGABYTES.toBits(2));
        Assert.assertEquals(2147483648L, GIGABYTES.toBits(2));
        Assert.assertEquals(2199023255552L, TERABYTES.toBits(2));
        //
        Assert.assertEquals(2, BYTES.convert(2, BYTES));
        Assert.assertEquals(2048, BYTES.convert(2, KILOBYTES));
        Assert.assertEquals(2097152, BYTES.convert(2, MEGABYTES));
        Assert.assertEquals(2147483648L, BYTES.convert(2, GIGABYTES));
        Assert.assertEquals(2199023255552L, BYTES.convert(2, TERABYTES));
    }

    @Test
    public void testToBytes() throws Exception {
        Assert.assertEquals(0, BITS.toBytes(2));
        Assert.assertEquals(2, BYTES.toBytes(2));
        Assert.assertEquals(2048, KILOBYTES.toBytes(2));
        Assert.assertEquals(2097152, MEGABYTES.toBytes(2));
        Assert.assertEquals(2147483648L, GIGABYTES.toBytes(2));
        Assert.assertEquals(2199023255552L, TERABYTES.toBytes(2));
        //
        Assert.assertEquals(2, BYTES.convert(2, BYTES));
        Assert.assertEquals(2048, BYTES.convert(2, KILOBYTES));
        Assert.assertEquals(2097152, BYTES.convert(2, MEGABYTES));
        Assert.assertEquals(2147483648L, BYTES.convert(2, GIGABYTES));
        Assert.assertEquals(2199023255552L, BYTES.convert(2, TERABYTES));
    }

    @Test
    public void testToGigabytes() throws Exception {
        Assert.assertEquals(2, BYTES.toGigabytes(2147483648l));
        Assert.assertEquals(2, KILOBYTES.toGigabytes(2097152));
        Assert.assertEquals(2, MEGABYTES.toGigabytes(2048));
        Assert.assertEquals(2, GIGABYTES.toGigabytes(2));
        Assert.assertEquals(2048, TERABYTES.toGigabytes(2));
        //
        Assert.assertEquals(2, GIGABYTES.convert(2147483648L, BYTES));
        Assert.assertEquals(2, GIGABYTES.convert(2097152, KILOBYTES));
        Assert.assertEquals(2, GIGABYTES.convert(2048, MEGABYTES));
        Assert.assertEquals(2, GIGABYTES.convert(2, GIGABYTES));
        Assert.assertEquals(2048, GIGABYTES.convert(2, TERABYTES));
    }

    @Test
    public void testToKilobytes() throws Exception {
        Assert.assertEquals(2, BYTES.toKilobytes(2048));
        Assert.assertEquals(2, KILOBYTES.toKilobytes(2));
        Assert.assertEquals(2048, MEGABYTES.toKilobytes(2));
        Assert.assertEquals(2097152, GIGABYTES.toKilobytes(2));
        Assert.assertEquals(2147483648L, TERABYTES.toKilobytes(2));
        //
        Assert.assertEquals(2, KILOBYTES.convert(2048, BYTES));
        Assert.assertEquals(2, KILOBYTES.convert(2, KILOBYTES));
        Assert.assertEquals(2048, KILOBYTES.convert(2, MEGABYTES));
        Assert.assertEquals(2097152, KILOBYTES.convert(2, GIGABYTES));
        Assert.assertEquals(2147483648L, KILOBYTES.convert(2, TERABYTES));
    }

    @Test
    public void testToMegabytes() throws Exception {
        Assert.assertEquals(2, BYTES.toMegabytes(2097152));
        Assert.assertEquals(2, KILOBYTES.toMegabytes(2048));
        Assert.assertEquals(2, MEGABYTES.toMegabytes(2));
        Assert.assertEquals(2048, GIGABYTES.toMegabytes(2));
        Assert.assertEquals(2097152, TERABYTES.toMegabytes(2));
        //
        Assert.assertEquals(2, MEGABYTES.convert(2097152, BYTES));
        Assert.assertEquals(2, MEGABYTES.convert(2048, KILOBYTES));
        Assert.assertEquals(2, MEGABYTES.convert(2, MEGABYTES));
        Assert.assertEquals(2048, MEGABYTES.convert(2, GIGABYTES));
        Assert.assertEquals(2097152, MEGABYTES.convert(2, TERABYTES));
    }

    @Test
    public void testToTerabytes() throws Exception {
        Assert.assertEquals(2, BYTES.toTerabytes(2199023255552l));
        Assert.assertEquals(2, KILOBYTES.toTerabytes(2147483648l));
        Assert.assertEquals(2, MEGABYTES.toTerabytes(2097152));
        Assert.assertEquals(2, GIGABYTES.toTerabytes(2048));
        Assert.assertEquals(2, TERABYTES.toTerabytes(2));
        //
        Assert.assertEquals(2, TERABYTES.convert(2199023255552L, BYTES));
        Assert.assertEquals(2, TERABYTES.convert(2147483648L, KILOBYTES));
        Assert.assertEquals(2, TERABYTES.convert(2097152, MEGABYTES));
        Assert.assertEquals(2, TERABYTES.convert(2048, GIGABYTES));
        Assert.assertEquals(2, TERABYTES.convert(2, TERABYTES));
    }
}
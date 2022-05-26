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

package org.apache.commons.lang3.builder;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;

/**
 */
public class MultilineRecursiveToStringStyleTest {

    private static final String BR = System.lineSeparator();
    private static final String BASE_WITH_ARRAYS_TO_STRING = "[" + BR
            + "  boolArray=#BOOLEAN#," + BR
            + "  byteArray=#BYTE#," + BR
            + "  charArray=#CHAR#," + BR
            + "  doubleArray=#DOUBLE#," + BR
            + "  floatArray=#FLOAT#," + BR
            + "  intArray=#INT#," + BR
            + "  longArray=#LONG#," + BR
            + "  shortArray=#SHORT#," + BR
            + "  stringArray=#STRING#" + BR
            + "]";

    @Test
    public void simpleObject() {
        final Transaction tx = new Transaction("2014.10.15", 100);
        final String expected = getClassPrefix(tx) + "[" + BR
                        + "  amount=100.0," + BR
                        + "  date=2014.10.15" + BR
                        + "]";
        assertEquals(expected, toString(tx));
    }

    @Test
    public void nestedElements() {
        final Customer customer = new Customer("Douglas Adams");
        final Bank bank = new Bank("ASF Bank");
        customer.bank = bank;
        final String exp = getClassPrefix(customer) + "[" + BR
                   + "  accounts=<null>," + BR
                   + "  bank=" + getClassPrefix(bank) + "[" + BR
                   + "    name=ASF Bank" + BR
                   + "  ]," + BR
                   + "  name=Douglas Adams" + BR
                + "]";
        assertEquals(exp, toString(customer));
    }

    @Test
    public void nestedAndArray() {
        final Account acc = new Account();
        final Transaction tx1 = new Transaction("2014.10.14", 100);
        final Transaction tx2 = new Transaction("2014.10.15", 50);
        acc.transactions.add(tx1);
        acc.transactions.add(tx2);
        final String expected = getClassPrefix(acc) + "[" + BR
                        + "  owner=<null>," + BR
                        + "  transactions=" + getClassPrefix(acc.transactions) + "{" + BR
                        + "    " + getClassPrefix(tx1) + "[" + BR
                        + "      amount=100.0," + BR
                        + "      date=2014.10.14" + BR
                        + "    ]," + BR
                        + "    " + getClassPrefix(tx2) + "[" + BR
                        + "      amount=50.0," + BR
                        + "      date=2014.10.15" + BR
                        + "    ]" + BR
                        + "  }" + BR
                        + "]";
        assertEquals(expected, toString(acc));
    }

    @Test
    public void noArray() {
        final WithArrays wa = new WithArrays();
        final String exp = getExpectedToString(wa, WithArraysTestType.NONE, "");
        assertEquals(exp, toString(wa));
    }

    @Test
    public void boolArray() {
        final WithArrays wa = new WithArrays();
        wa.boolArray = new boolean[] { true, false, true };
        final String exp = getExpectedToString(
                wa, WithArraysTestType.BOOLEAN,
                "{" + BR
                + "    true," + BR
                + "    false," + BR
                + "    true" + BR
                + "  }");
        assertEquals(exp, toString(wa));
    }

    @Test
    public void byteArray() {
        final WithArrays wa = new WithArrays();
        wa.byteArray = new byte[] { 1, 2 };
        final String exp = getExpectedToString(
                wa, WithArraysTestType.BYTE,
                "{" + BR
                + "    1," + BR
                + "    2" + BR
                + "  }");
        assertEquals(exp, toString(wa));
    }

    @Test
    public void charArray() {
        final WithArrays wa = new WithArrays();
        wa.charArray = new char[] { 'a', 'A' };
        final String exp = getExpectedToString(
                wa, WithArraysTestType.CHAR,
                "{" + BR
                + "    a," + BR
                + "    A" + BR
                + "  }");
        assertEquals(exp, toString(wa));
    }

    @Test
    public void intArray() {
        final WithArrays wa = new WithArrays();
        wa.intArray = new int[] { 1, 2 };
        final String exp = getExpectedToString(
                wa, WithArraysTestType.INT,
                "{" + BR
                + "    1," + BR
                + "    2" + BR
                + "  }");
        assertEquals(exp, toString(wa));
    }

    @Test
    public void doubleArray() {
        final WithArrays wa = new WithArrays();
        wa.doubleArray = new double[] { 1, 2 };
        final String exp = getExpectedToString(
                wa, WithArraysTestType.DOUBLE,
                "{" + BR
                + "    1.0," + BR
                + "    2.0" + BR
                + "  }");
        assertEquals(exp, toString(wa));
    }

    @Test
    public void floatArray() {
        final WithArrays wa = new WithArrays();
        wa.floatArray = new float[] { 1f, 2f };
        final String exp = getExpectedToString(
                wa, WithArraysTestType.FLOAT,
                "{" + BR
                + "    1.0," + BR
                + "    2.0" + BR
                + "  }");
        assertEquals(exp, toString(wa));
    }

    @Test
    public void longArray() {
        final WithArrays wa = new WithArrays();
        wa.longArray = new long[] { 1L, 2L };
        final String exp = getExpectedToString(
                wa, WithArraysTestType.LONG,
                "{" + BR
                + "    1," + BR
                + "    2" + BR
                + "  }");
        assertEquals(exp, toString(wa));
    }

    @Test
    public void stringArray() {
        final WithArrays wa = new WithArrays();
        wa.stringArray = new String[] { "a", "A" };
        final String exp = getExpectedToString(
                wa, WithArraysTestType.STRING,
                "{" + BR
                + "    a," + BR
                + "    A" + BR
                + "  }");
        assertEquals(exp, toString(wa));
    }

    @Test
    public void shortArray() {
        final WithArrays wa = new WithArrays();
        wa.shortArray = new short[] { 1, 2 };
        final String exp = getExpectedToString(
                wa, WithArraysTestType.SHORT,
                "{" + BR
                + "    1," + BR
                + "    2" + BR
                + "  }");
        assertEquals(exp, toString(wa));
    }

    @Test
    public void testLANG1319() {
        final String[] stringArray = {"1", "2"};

        final String exp = getClassPrefix(stringArray) + "[" + BR
                + "  {" + BR
                + "    1," + BR
                + "    2" + BR
                + "  }" + BR
                + "]";
        assertEquals(exp, toString(stringArray));
    }

    private String getClassPrefix(final Object object) {
        return object.getClass().getName() + "@" + Integer.toHexString(System.identityHashCode(object));
    }

    private String toString(final Object object) {
        return new ReflectionToStringBuilder(object, new MultilineRecursiveToStringStyle()).toString();
    }

    static class WithArrays {
        boolean[] boolArray;
        byte[] byteArray;
        char[] charArray;
        double[] doubleArray;
        float[] floatArray;
        int[] intArray;
        long[] longArray;
        short[] shortArray;
        String[] stringArray;
    }

    /**
     * Create an expected to String for the given WithArraysInstance
     * @param wa                 Instance
     * @param arrayType          Type - empty used to indicate expect all nulls
     * @param expectedArrayValue Expected value for the array type
     * @return expected toString
     */
    private String getExpectedToString(final WithArrays wa, final WithArraysTestType arrayType, final String expectedArrayValue) {
        return getClassPrefix(wa)
                + BASE_WITH_ARRAYS_TO_STRING
                .replace("#" + arrayType + "#", expectedArrayValue)
                .replaceAll("#[A-Z]+#", "<null>");
    }

    private enum WithArraysTestType {
        NONE, BOOLEAN, BYTE, CHAR, DOUBLE, FLOAT, INT, LONG, SHORT, STRING
    }

    static class Bank {
        String name;

        Bank(final String name) {
            this.name = name;
        }
    }

    static class Customer {
        String name;
        Bank bank;
        List<Account> accounts;

        Customer(final String name) {
            this.name = name;
        }
    }

    static class Account {
        Customer owner;
        List<Transaction> transactions = new ArrayList<>();

        public double getBalance() {
            double balance = 0;
            for (final Transaction tx : transactions) {
                balance += tx.amount;
            }
            return balance;
        }
    }

    static class Transaction {
        double amount;
        String date;

        Transaction(final String datum, final double betrag) {
            this.date = datum;
            this.amount = betrag;
        }
    }

}

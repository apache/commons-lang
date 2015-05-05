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

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.SystemUtils;
import org.junit.Test;

/**
 */
public class MultilineRecursiveToStringStyleTest {

    private final String BR = SystemUtils.LINE_SEPARATOR;

    @Test
    public void simpleObject() {
        Transaction tx = new Transaction("2014.10.15", 100);
        String expected = getClassPrefix(tx) + "[" + BR 
                        + "  amount=100.0," + BR 
                        + "  date=2014.10.15" + BR 
                        + "]";
        assertEquals(expected, toString(tx));
    }

    @Test
    public void nestedElements() {
        Customer customer = new Customer("Douglas Adams");
        Bank bank = new Bank("ASF Bank");
        customer.bank = bank;
        String exp = getClassPrefix(customer) + "[" + BR 
                   + "  name=Douglas Adams," + BR 
                   + "  bank=" + getClassPrefix(bank) + "[" + BR 
                   + "    name=ASF Bank" + BR 
                   + "  ]," + BR 
                   + "  accounts=<null>" + BR 
                   + "]";
        assertEquals(exp, toString(customer));
    }

    @Test
    public void nestedAndArray() {
        Account acc = new Account();
        Transaction tx1 = new Transaction("2014.10.14", 100);
        Transaction tx2 = new Transaction("2014.10.15", 50);
        acc.transactions.add(tx1);
        acc.transactions.add(tx2);
        String expected = getClassPrefix(acc) + "[" + BR 
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
        WithArrays wa = new WithArrays();
        String exp = getClassPrefix(wa) + "[" + BR 
                   + "  boolArray=<null>," + BR 
                   + "  charArray=<null>," + BR
                   + "  intArray=<null>," + BR 
                   + "  doubleArray=<null>," + BR 
                   + "  longArray=<null>," + BR 
                   + "  stringArray=<null>" + BR 
                   + "]";
        assertEquals(exp, toString(wa));
    }

    @Test
    public void boolArray() {
        WithArrays wa = new WithArrays();
        wa.boolArray = new boolean[] { true, false, true };
        String exp = getClassPrefix(wa) + "[" + BR 
                   + "  boolArray={" + BR 
                   + "    true," + BR 
                   + "    false," + BR 
                   + "    true" + BR 
                   + "  }," + BR 
                   + "  charArray=<null>," + BR 
                   + "  intArray=<null>," + BR 
                   + "  doubleArray=<null>," + BR
                   + "  longArray=<null>," + BR 
                   + "  stringArray=<null>" + BR 
                   + "]";
        assertEquals(exp, toString(wa));
    }

    @Test
    public void charArray() {
        WithArrays wa = new WithArrays();
        wa.charArray = new char[] { 'a', 'A' };
        String exp = getClassPrefix(wa) + "[" + BR 
                   + "  boolArray=<null>," + BR 
                   + "  charArray={" + BR 
                   + "    a," + BR 
                   + "    A" + BR 
                   + "  }," + BR 
                   + "  intArray=<null>," + BR 
                   + "  doubleArray=<null>," + BR 
                   + "  longArray=<null>," + BR
                   + "  stringArray=<null>" + BR 
                   + "]";
        assertEquals(exp, toString(wa));
    }

    @Test
    public void intArray() {
        WithArrays wa = new WithArrays();
        wa.intArray = new int[] { 1, 2 };
        String exp = getClassPrefix(wa) + "[" + BR 
                   + "  boolArray=<null>," + BR 
                   + "  charArray=<null>," + BR 
                   + "  intArray={" + BR 
                   + "    1," + BR 
                   + "    2" + BR 
                   + "  }," + BR 
                   + "  doubleArray=<null>," + BR 
                   + "  longArray=<null>," + BR
                   + "  stringArray=<null>" + BR 
                   + "]";
        assertEquals(exp, toString(wa));
    }

    @Test
    public void doubleArray() {
        WithArrays wa = new WithArrays();
        wa.doubleArray = new double[] { 1, 2 };
        String exp = getClassPrefix(wa) + "[" + BR 
                   + "  boolArray=<null>," + BR 
                   + "  charArray=<null>," + BR
                   + "  intArray=<null>," + BR 
                   + "  doubleArray={" + BR 
                   + "    1.0," + BR 
                   + "    2.0" + BR 
                   + "  }," + BR
                   + "  longArray=<null>," + BR 
                   + "  stringArray=<null>" + BR 
                   + "]";
        assertEquals(exp, toString(wa));
    }

    @Test
    public void longArray() {
        WithArrays wa = new WithArrays();
        wa.longArray = new long[] { 1L, 2L };
        String exp = getClassPrefix(wa) + "[" + BR 
                   + "  boolArray=<null>," + BR 
                   + "  charArray=<null>," + BR
                   + "  intArray=<null>," + BR 
                   + "  doubleArray=<null>," + BR 
                   + "  longArray={" + BR 
                   + "    1," + BR 
                   + "    2" + BR
                   + "  }," + BR 
                   + "  stringArray=<null>" + BR 
                   + "]";
        assertEquals(exp, toString(wa));
    }

    @Test
    public void stringArray() {
        WithArrays wa = new WithArrays();
        wa.stringArray = new String[] { "a", "A" };
        String exp = getClassPrefix(wa) + "[" + BR 
                   + "  boolArray=<null>," + BR 
                   + "  charArray=<null>," + BR
                   + "  intArray=<null>," + BR 
                   + "  doubleArray=<null>," + BR 
                   + "  longArray=<null>," + BR 
                   + "  stringArray={" + BR
                   + "    a," + BR 
                   + "    A" + BR 
                   + "  }" + BR 
                   + "]";
        assertEquals(exp, toString(wa));
    }

    private String getClassPrefix(Object object) {
        return object.getClass().getName() + "@" + Integer.toHexString(System.identityHashCode(object));
    }

    private String toString(Object object) {
        return new ReflectionToStringBuilder(object, new MultilineRecursiveToStringStyle()).toString();
    }

    static class WithArrays {
        boolean[] boolArray;
        char[] charArray;
        int[] intArray;
        double[] doubleArray;
        long[] longArray;
        String[] stringArray;
    }

    static class Bank {
        String name;

        public Bank(String name) {
            this.name = name;
        }
    }

    static class Customer {
        String name;
        Bank bank;
        List<Account> accounts;

        public Customer(String name) {
            this.name = name;
        }
    }

    static class Account {
        Customer owner;
        List<Transaction> transactions = new ArrayList<Transaction>();

        public double getBalance() {
            double balance = 0;
            for (Transaction tx : transactions) {
                balance += tx.amount;
            }
            return balance;
        }
    }

    static class Transaction {
        double amount;
        String date;

        public Transaction(String datum, double betrag) {
            this.date = datum;
            this.amount = betrag;
        }
    }

}

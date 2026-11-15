/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang3.concurrent.locks;

import static org.junit.jupiter.api.Assertions.*;

import java.util.concurrent.locks.ReentrantLock;
import org.junit.jupiter.api.Test;

class LockingVisitorsTV04Test {
    @Test
    void reentrantLockVisitorReadsAndWritesUnderLock() {
        StringBuilder value = new StringBuilder("A");
        LockingVisitors.ReentrantLockVisitor<StringBuilder> visitor = LockingVisitors.reentrantLockVisitor(value);
        assertEquals(1, (int) visitor.applyReadLocked(StringBuilder::length));
        visitor.acceptWriteLocked(text -> text.append("B"));
        assertEquals("AB", visitor.applyWriteLocked(StringBuilder::toString));
        assertSame(value, visitor.getObject());
        assertNotNull(visitor.getLock());
    }

    @Test
    void readWriteAndStampedLockVisitorsUseTheirLockTypes() {
        StringBuilder readWriteValue = new StringBuilder("R");
        LockingVisitors.ReadWriteLockVisitor<StringBuilder> readWrite = LockingVisitors.reentrantReadWriteLockVisitor(readWriteValue);
        readWrite.acceptWriteLocked(text -> text.append("W"));
        assertEquals("RW", readWrite.applyReadLocked(StringBuilder::toString));

        StringBuilder stampedValue = new StringBuilder("S");
        LockingVisitors.StampedLockVisitor<StringBuilder> stamped = LockingVisitors.stampedLockVisitor(stampedValue);
        stamped.acceptWriteLocked(text -> text.append("T"));
        assertEquals("ST", stamped.applyReadLocked(StringBuilder::toString));
    }

    @Test
    void explicitReentrantLockCanBeSuppliedAndNullObjectsAreRejected() {
        StringBuilder value = new StringBuilder("x");
        ReentrantLock lock = new ReentrantLock();
        LockingVisitors.ReentrantLockVisitor<StringBuilder> visitor = LockingVisitors.create(value, lock);
        assertSame(lock, visitor.getLock());
        assertThrows(NullPointerException.class, () -> LockingVisitors.reentrantLockVisitor(null));
    }
}

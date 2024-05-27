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

package org.apache.commons.lang3.function;

import java.util.function.Consumer;
import java.util.stream.Stream;

import org.apache.commons.lang3.AbstractLangTest;
import org.easymock.EasyMock;
import org.junit.jupiter.api.Test;

import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Tests {@link Consumers}.
 */
public class ConsumersTest extends AbstractLangTest {

    /**
     * Tests {@link Consumers#nop()}.
     */
    @Test
    public void testNop() {
        Stream.of("").forEach(Consumers.nop());
        //
        final Consumer<?> c1 = Consumers.nop();
        c1.accept(null);
        final Consumer<Object> c2 = Consumers.nop();
        c2.accept(null);
        final Consumer<String> c3 = Consumers.nop();
        c3.accept(null);
        //
        Consumers.nop().accept(null);
        Consumers.nop().accept("");
    }

    /**
     * Tests {@link Consumers#accept(Object, Consumer)}.
     */
    @Test
    public void testAccept() {
        final StringBuilder builder = new StringBuilder("foo");
        Consumers.accept(builder, sb -> sb.append("-bar"));
        assertEquals("foo-bar", builder.toString());

        final Consumer<String> mock = EasyMock.createMock(Consumer.class);
        mock.accept(null);
        replay(mock);
        Consumers.accept(null, mock);
        verify(mock);

        final StringBuilder builder2 = new StringBuilder("foo");
        Consumers.accept(builder2, null);
        assertEquals("foo", builder2.toString());
    }
}

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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;

import java.io.IOException;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.Objects;

import org.apache.commons.lang3.AppendableJoiner.Builder;
import org.apache.commons.lang3.text.StrBuilder;
import org.apache.commons.text.TextStringBuilder;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Tests {@link AppendableJoiner}.
 */
public class AppendableJoinerTest {

    static class Fixture {

        private final String name;

        Fixture(final String name) {
            this.name = name;
        }

        /**
         * Renders myself onto an Appendable to avoid creating intermediary strings.
         */
        void render(final Appendable appendable) throws IOException {
            appendable.append(name);
            appendable.append('!');
        }
    }

    @Test
    public void testAllBuilderPropertiesStringBuilder() {
        // @formatter:off
        final AppendableJoiner<Object> joiner = AppendableJoiner.builder()
                .setPrefix("<")
                .setDelimiter(".")
                .setSuffix(">")
                .setElementAppender((a, e) -> a.append(String.valueOf(e)))
                .get();
        // @formatter:on
        final StringBuilder sbuilder = new StringBuilder("A");
        assertEquals("A<B.C>", joiner.join(sbuilder, "B", "C").toString());
        sbuilder.append("1");
        assertEquals("A<B.C>1<D.E>", joiner.join(sbuilder, Arrays.asList("D", "E")).toString());
    }

    @Test
    public void testBuildDefaultStringBuilder() {
        final Builder<Object> builder = AppendableJoiner.builder();
        assertNotSame(builder.get(), builder.get());
        final AppendableJoiner<Object> joiner = builder.get();
        final StringBuilder sbuilder = new StringBuilder("A");
        assertEquals("ABC", joiner.join(sbuilder, "B", "C").toString());
        sbuilder.append("1");
        assertEquals("ABC1DE", joiner.join(sbuilder, "D", "E").toString());
    }

    @Test
    public void testBuilder() {
        assertNotSame(AppendableJoiner.builder(), AppendableJoiner.builder());
    }

    @SuppressWarnings("deprecation") // Test own StrBuilder
    @ParameterizedTest
    @ValueSource(classes = { StringBuilder.class, StringBuffer.class, StringWriter.class, StrBuilder.class, TextStringBuilder.class })
    public void testDelimiterAppendable(final Class<? extends Appendable> clazz) throws Exception {
        final AppendableJoiner<Object> joiner = AppendableJoiner.builder().setDelimiter(".").get();
        final Appendable sbuilder = clazz.newInstance();
        sbuilder.append("A");
        // throws IOException
        assertEquals("AB.C", joiner.joinA(sbuilder, "B", "C").toString());
        sbuilder.append("1");
        // throws IOException
        assertEquals("AB.C1D.E", joiner.joinA(sbuilder, Arrays.asList("D", "E")).toString());
    }

    @Test
    public void testDelimiterStringBuilder() {
        final AppendableJoiner<Object> joiner = AppendableJoiner.builder().setDelimiter(".").get();
        final StringBuilder sbuilder = new StringBuilder("A");
        // does not throw IOException
        assertEquals("AB.C", joiner.join(sbuilder, "B", "C").toString());
        sbuilder.append("1");
        // does not throw IOException
        assertEquals("AB.C1D.E", joiner.join(sbuilder, Arrays.asList("D", "E")).toString());
    }

    @Test
    public void testToCharSequenceStringBuilder1() {
        // @formatter:off
        final AppendableJoiner<Object> joiner = AppendableJoiner.builder()
                .setPrefix("<")
                .setDelimiter(".")
                .setSuffix(">")
                .setElementAppender((a, e) -> a.append("|").append(Objects.toString(e)))
                .get();
        // @formatter:on
        final StringBuilder sbuilder = new StringBuilder("A");
        assertEquals("A<|B.|C>", joiner.join(sbuilder, "B", "C").toString());
        sbuilder.append("1");
        assertEquals("A<|B.|C>1<|D.|E>", joiner.join(sbuilder, Arrays.asList("D", "E")).toString());
    }

    @Test
    public void testToCharSequenceStringBuilder2() {
        // @formatter:off
        final AppendableJoiner<Fixture> joiner = AppendableJoiner.<Fixture>builder()
                .setElementAppender((a, e) -> e.render(a))
                .get();
        // @formatter:on
        final StringBuilder sbuilder = new StringBuilder("[");
        assertEquals("[B!C!", joiner.join(sbuilder, new Fixture("B"), new Fixture("C")).toString());
        sbuilder.append("]");
        assertEquals("[B!C!]D!E!", joiner.join(sbuilder, Arrays.asList(new Fixture("D"), new Fixture("E"))).toString());
    }
}

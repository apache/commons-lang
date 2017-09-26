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
package org.apache.commons.lang3.test;

import java.util.Locale;
import java.util.TimeZone;

import org.apache.commons.lang3.LocaleUtils;
import org.junit.rules.TestRule;
import org.junit.runner.Description;
import org.junit.runners.model.Statement;

/**
 * Test Rule used with {@link SystemDefaults} annotation that sets and restores the system default Locale and TimeZone.
 *
 * <p>
 * Set up tests to use alternate system default Locale and/or TimeZone by creating an instance of this rule
 * and annotating the test method with {@link SystemDefaults}
 * </p>
 *
 * <pre>
 * public class SystemDefaultsDependentTest {
 *
 *     {@literal @}Rule
 *     public SystemDefaultsSwitch locale = new SystemDefaultsSwitch();
 *
 *     {@literal @}Test
 *     public void testThatWillExecuteWithTheDefaultLocaleAndTimeZone() {
 *         // nothing to do, just implement the test
 *     }
 *
 *     {@literal @}Test
 *     {@literal @}SystemDefaults(local="zh_CN")
 *     public void testWithSimplifiedChinaDefaultLocale() {
 *         // Locale.getDefault() will return Locale.CHINA until the end of this test method
 *     }
 *
 *     {@literal @}Test
 *     {@literal @}SystemDefaults(timezone="America/New_York")
 *     public void testWithNorthAmericaEasternTimeZone() {
 *         // TimeZone.getDefault() will equal TimeZone.getTimeZone("America/New_York") until the end of this method
 *     }
 * }
 * </pre>
 */
public class SystemDefaultsSwitch implements TestRule {

    @Override
    public Statement apply(final Statement stmt, final Description description) {
        final SystemDefaults defaults = description.getAnnotation(SystemDefaults.class);
        if (defaults == null) {
            return stmt;
        }
        return applyTimeZone(defaults, applyLocale(defaults, stmt));
    }

    private Statement applyTimeZone(final SystemDefaults defaults, final Statement stmt) {
        if (defaults.timezone().isEmpty()) {
            return stmt;
        }

        final TimeZone newTimeZone = TimeZone.getTimeZone(defaults.timezone());

        return new Statement() {
            @Override
            public void evaluate() throws Throwable {
                final TimeZone save = TimeZone.getDefault();
                try {
                    TimeZone.setDefault(newTimeZone);
                    stmt.evaluate();
                } finally {
                    TimeZone.setDefault(save);
                }
            }
        };
    }

    private Statement applyLocale(final SystemDefaults defaults, final Statement stmt) {
        if (defaults.locale().isEmpty()) {
            return stmt;
        }

        final Locale newLocale = LocaleUtils.toLocale(defaults.locale());

        return new Statement() {
            @Override
            public void evaluate() throws Throwable {
                final Locale save = Locale.getDefault();
                try {
                    Locale.setDefault(newLocale);
                    stmt.evaluate();
                } finally {
                    Locale.setDefault(save);
                }
            }
        };
    }

}

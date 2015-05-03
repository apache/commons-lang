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
package org.apache.commons.lang3.time;

import java.util.Locale;

import org.junit.rules.TestRule;
import org.junit.runner.Description;
import org.junit.runners.model.Statement;

/**
 * Rule implementation that sets and resets the default Locale.
 * 
 * <p>
 * Set up tests to use {@code Locale.getDefault()} by creating a TestLocale rule using {@link #usingDefaultLocale()}.
 * To override the default Locale for a single test method, use {@link #setLocale(Locale)}. The TestLocale rule will
 * make sure, that the default Locale is restored after each test.
 * </p>
 * 
 * <pre>
 * public class LocaleDependentTest {
 *
 *     {@literal@}Rule
 *     public TestLocale locale = TestLocale.usingDefaultLocale();
 *
 *     {@literal@}Test
 *     public void testThatWillExecuteWithTheDefaultLocale() {
 *         // nothing to do, just implement the test
 *     }
 *
 *     {@literal@}Test
 *     public void testWithDifferentDefaultLocale() {
 *         locale.setLocale(Locale.CHINA);
 *         // Locale.getDefault() will return Locale.CHINA until the end of this test method
 *     }
 * }
 * </pre>
 * 
 * <p>
 * If all tests should use a different default Locale, use {@link #using(Locale)}. All tests will then have the given
 * Locale set as default Locale. After each test method, the default Locale is restored to its initial value.
 * </p>
 * 
 * <pre>
 * public class LocaleDependentTest {
 *
 *     {@literal@}Rule
 *     public TestLocale locale = TestLocale.using(Locale.CHINA);
 *
 *     {@literal@}Test
 *     public void testThatWillExecuteWithLocaleChina() {
 *         // nothing to do, just implement the test
 *     }
 *
 *     {@literal@}Test
 *     public void testWithDifferentDefaultLocale() {
 *         locale.setLocale(Locale.US);
 *         // Locale.getDefault() will return Locale.US until the end of this test method
 *     }
 * }
 * </pre>
 *
 * @see TestTimeZone
 */
public class TestLocale implements TestRule {

    private static final Locale DEFAULT_LOCALE = Locale.getDefault();

    /**
     * Creates a new instance using the default locale as default for tests.
     *
     * <p>
     * The locale used for tests can be overridden using {@link #setLocale(Locale)}.
     * </p>
     */
    public static TestLocale usingDefaultLocale() {
        return new TestLocale(DEFAULT_LOCALE);
    }

    /**
     * Creates a new instance using the provided locale as default for tests.
     *
     * <p>
     * The locale used for tests can be overridden using {@link #setLocale(Locale)}.
     * </p>
     *
     * @param testLocale the locale to run tests with.
     */
    public static TestLocale using(final Locale testLocale) {
        return new TestLocale(testLocale);
    }

    private Locale testLocale;

    private TestLocale(final Locale testLocale) {
        this.testLocale = testLocale;
    }

    /**
     * Override the configured test locale for this test execution.
     *
     * @param testLocale the locale to run this test with.
     */
    public void setLocale(final Locale testLocale) {
        Locale.setDefault(testLocale);
    }

    @Override
    public Statement apply(final Statement base, final Description description) {
        return new Statement() {
            @Override
            public void evaluate() throws Throwable {
                Locale.setDefault(testLocale);
                try {
                    base.evaluate();
                } finally {
                    Locale.setDefault(DEFAULT_LOCALE);
                }
            }
        };
    }
}

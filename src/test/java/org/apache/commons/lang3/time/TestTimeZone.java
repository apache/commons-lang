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

import java.util.TimeZone;

import org.junit.rules.TestRule;
import org.junit.runner.Description;
import org.junit.runners.model.Statement;

/**
 * Rule implementation that sets and resets the default TimeZone.
 * 
 * <p>
 * Set up tests to use {@code TimeZone.getDefault()} by creating a TestTimeZone rule using {@link #usingDefaultTimeZone()}.
 * To override the default TimeZone for a single test method, use {@link #setTimeZone(TimeZone)} or
 * {@link #setTimeZone(String)}. The TestTimeZone rule will make sure, that the default TimeZone is restored after each
 * test.
 * </p>
 * 
 * <pre>
 * public class TimeZoneDependentTest {
 *
 *     {@literal@}Rule
 *     public TestTimeZone timeZone = TestTimeZone.usingDefaultTimeZone();
 *
 *     {@literal@}Test
 *     public void testThatWillExecuteWithTheDefaultTimeZone() {
 *         // nothing to do, just implement the test
 *     }
 *
 *     {@literal@}Test
 *     public void testWithDifferentDefaultTimeZone() {
 *         timeZone.setTimeZone("UTC");
 *         // TimeZone.getDefault() will return TimeZone.getTimeZone("UTC") until the end of this test method
 *     }
 * }
 * </pre>
 * 
 * <p>
 * If all tests should use a different default TimeZone, use {@link #using(TimeZone)} or {@link #using(String)}. All
 * tests will then have the given TimeZone set as default TimeZone. After each test method, the default TimeZone is
 * restored to its initial value.
 * </p>
 * 
 * <pre>
 * public class TimeZoneDependentTest {
 *
 *     {@literal@}Rule
 *     public TestTimeZone timeZone = TestTimeZone.using("UTC");
 *
 *     {@literal@}Test
 *     public void testThatWillExecuteWithTimeZoneUTC() {
 *         // nothing to do, just implement the test
 *     }
 *
 *     {@literal@}Test
 *     public void testWithDifferentDefaultTimeZone() {
 *         timeZone.setTimeZone("GMT")
 *         // TimeZone.getDefault() will return TimeZone.getTimeZone("GMT") until the end of this test method
 *     }
 * }
 * </pre>
 *
 * @see TestLocale
 */
public class TestTimeZone implements TestRule {

    private static final TimeZone DEFAULT_TIMEZONE = TimeZone.getDefault();

    /**
     * Creates a new instance using the default TimeZone for tests.
     *
     * <p>
     * The TimeZone used for tests can be overridden with {@link #setTimeZone(String)} and
     * {@link #setTimeZone(TimeZone)}.
     * </p>
     */
    public static TestTimeZone usingDefaultTimeZone() {
        return new TestTimeZone(DEFAULT_TIMEZONE);
    }

    /**
     * Creates a new instance using the provided TimeZone as default for tests.
     * 
     * <p>
     * The TimeZone used for tests can be overridden with {@link #setTimeZone(String)} and
     * {@link #setTimeZone(TimeZone)}.
     * </p>
     *
     * @param testTimeZone the TimeZone to run tests with.
     */
    public static TestTimeZone using(final TimeZone testTimeZone) {
        return new TestTimeZone(testTimeZone);
    }

    /**
     * Creates a new instance using the provided zone Id to set the default TimeZone for tests.
     * 
     * <p>
     * The TimeZone used for tests can be overridden with {@link #setTimeZone(String)} and
     * {@link #setTimeZone(TimeZone)}.
     * </p>
     *
     * @param testTimeZoneId the ID of the TimeZone to run tests with.
     */
    public static TestTimeZone using(final String testTimeZoneId) {
        return new TestTimeZone(TimeZone.getTimeZone(testTimeZoneId));
    }

    private TimeZone testTimeZone;

    private TestTimeZone(final TimeZone testTimeZone) {
        this.testTimeZone = testTimeZone;
    }

    /**
     * Override the configured test locale for this test excution.
     *
     * @param testTimeZone the TimeZone to run this test with.
     */
    public void setTimeZone(final TimeZone testTimeZone) {
        TimeZone.setDefault(testTimeZone);
    }

    /**
     * Override the configured test locale for this test excution.
     *
     * @param zoneId the ID of the TimeZone to run tests with.
     */
    public void setTimeZone(final String zoneId) {
        setTimeZone(TimeZone.getTimeZone(zoneId));
    }

    @Override
    public Statement apply(final Statement base, final Description description) {
        return new Statement() {
            @Override
            public void evaluate() throws Throwable {
                TimeZone.setDefault(testTimeZone);
                try {
                    base.evaluate();
                } finally {
                    TimeZone.setDefault(DEFAULT_TIMEZONE);
                }
            }
        };
    }
}

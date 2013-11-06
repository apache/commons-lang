package org.apache.commons.lang3.time;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

import org.apache.commons.lang3.JavaVersion;
import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.lang3.Validate;
import org.apache.commons.lang3.reflect.MethodUtils;

/**
 * A {@link TimeZone} implementation that just delegates all non final method
 * calls to an other {@link TimeZone} instance which was specified in the
 * constructor.
 * 
 * Convenient base class for other {@link TimeZone} related classes.
 * 
 * Implementation detail: {@link TimeZone} has some final methods (
 * {@link #getDisplayName()} and some of its variants) which of course can't be
 * overridden but they all delegate to
 * {@link #getDisplayName(boolean, int, Locale)} which is not final and so can
 * be redefined.
 */
public class DelegatingTimeZone extends TimeZone {
    private static final long serialVersionUID = 1910962594884138771L;
    private static final String observesDaylightTimeName = "observesDaylightTime";
 
    private enum JavaVersionBasedInvoker {
        Java6 {
            @Override
            public boolean observesDaylightTime(TimeZone tz) {
                throw new UnsupportedOperationException(observesDaylightTimeName + "() is not supported on pre Java7 JVM.");
            }
        },
        Java7 {
            private final Method observesDaylightTime = MethodUtils.getAccessibleMethod(TimeZone.class, observesDaylightTimeName);

            @Override
            public boolean observesDaylightTime(TimeZone tz) {
                try {
                    return (Boolean) observesDaylightTime.invoke(tz);
                } catch (IllegalAccessException e) {
                    throw new IllegalStateException("Had no access to invoke " + observesDaylightTimeName + "().", e);
                } catch (InvocationTargetException e) {
                    throw new IllegalStateException("Invoking " + observesDaylightTimeName + "() failed.", e);
                }
            }
        };

        public abstract boolean observesDaylightTime(TimeZone tz);
    }

    private static final JavaVersionBasedInvoker javaVersionBasedInvoker = SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_1_7) ? JavaVersionBasedInvoker.Java7 : JavaVersionBasedInvoker.Java6;

    private final TimeZone delegate;

    /**
     * @param delegate
     *            the non-null delegate, all method calls will be forwarded to
     *            this object
     */
    public DelegatingTimeZone(TimeZone delegate) {
        this.delegate = Validate.notNull(delegate);
    }

    @Override
    public int hashCode() {
        return delegate.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        return delegate.equals(obj);
    }

    @Override
    public int getOffset(long date) {
        return delegate.getOffset(date);
    }

    @Override
    public String toString() {
        return delegate.toString();
    }

    @Override
    public String getID() {
        return delegate.getID();
    }

    @Override
    public void setID(String ID) {
        delegate.setID(ID);
    }

    @Override
    public String getDisplayName(boolean daylight, int style, Locale locale) {
        return delegate.getDisplayName(daylight, style, locale);
    }

    @Override
    public int getDSTSavings() {
        return delegate.getDSTSavings();
    }

    /**
     * This method is only available in JDK7 and up. See {@link TimeZone#observesDaylightTime()}.
     * 
     * @return {@code true} if the wrapped {@code TimeZone} is currently in Daylight Saving Time,
     * or if a transition from Standard Time to Daylight Saving Time occurs at any future time;
     * {@code false} otherwise.
     * @since 1.7
     */
    public boolean observesDaylightTime() {
        return javaVersionBasedInvoker.observesDaylightTime(delegate);
    }

    @Override
    public boolean hasSameRules(TimeZone other) {
        return delegate.hasSameRules(other);
    }

    @Override
    public int getOffset(int era, int year, int month, int day, int dayOfWeek,
            int milliseconds) {
        return delegate.getOffset(era, year, month, day, dayOfWeek,
                milliseconds);
    }

    @Override
    public void setRawOffset(int offsetMillis) {
        delegate.setRawOffset(offsetMillis);
    }

    @Override
    public int getRawOffset() {
        return delegate.getRawOffset();
    }

    @Override
    public boolean useDaylightTime() {
        return delegate.useDaylightTime();
    }

    @Override
    public boolean inDaylightTime(Date date) {
        return delegate.inDaylightTime(date);
    }

    @Override
    public DelegatingTimeZone clone() {
        return (DelegatingTimeZone) super.clone();
    }
}

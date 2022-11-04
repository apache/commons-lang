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
package org.apache.commons.lang3.exception;

import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.UndeclaredThrowableException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.StringTokenizer;
import java.util.function.Consumer;
import java.util.stream.Stream;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.ClassUtils;
import org.apache.commons.lang3.StringUtils;

/**
 * Provides utilities for manipulating and examining
 * {@link Throwable} objects.
 *
 * @since 1.0
 */
public class ExceptionUtils {

    private static final int NOT_FOUND = -1;

    /**
     * The names of methods commonly used to access a wrapped exception.
     */
    // TODO: Remove in Lang 4.0
    private static final String[] CAUSE_METHOD_NAMES = {
        "getCause",
        "getNextException",
        "getTargetException",
        "getException",
        "getSourceException",
        "getRootCause",
        "getCausedByException",
        "getNested",
        "getLinkedException",
        "getNestedException",
        "getLinkedCause",
        "getThrowable",
    };

    /**
     * Used when printing stack frames to denote the start of a
     * wrapped exception.
     *
     * <p>Package private for accessibility by test suite.</p>
     */
    static final String WRAPPED_MARKER = " [wrapped] ";

    /**
     * Claims a Throwable is another Throwable type using type erasure. This
     * hides a checked exception from the Java compiler, allowing a checked
     * exception to be thrown without having the exception in the method's throw
     * clause.
     */
    @SuppressWarnings("unchecked")
    private static <R, T extends Throwable> R eraseType(final Throwable throwable) throws T {
        throw (T) throwable;
    }

    /**
     * Introspects the {@link Throwable} to obtain the cause.
     *
     * <p>The method searches for methods with specific names that return a
     * {@link Throwable} object. This will pick up most wrapping exceptions,
     * including those from JDK 1.4.
     * </p>
     *
     * <p>The default list searched for are:</p>
     * <ul>
     *  <li>{@code getCause()}</li>
     *  <li>{@code getNextException()}</li>
     *  <li>{@code getTargetException()}</li>
     *  <li>{@code getException()}</li>
     *  <li>{@code getSourceException()}</li>
     *  <li>{@code getRootCause()}</li>
     *  <li>{@code getCausedByException()}</li>
     *  <li>{@code getNested()}</li>
     * </ul>
     *
     * <p>If none of the above is found, returns {@code null}.</p>
     *
     * @param throwable  the throwable to introspect for a cause, may be null
     * @return the cause of the {@link Throwable},
     *  {@code null} if none found or null throwable input
     * @since 1.0
     * @deprecated This feature will be removed in Lang 4.0, use {@link Throwable#getCause} instead
     */
    @Deprecated
    public static Throwable getCause(final Throwable throwable) {
        return getCause(throwable, null);
    }

    /**
     * Introspects the {@link Throwable} to obtain the cause.
     *
     * <p>A {@code null} set of method names means use the default set.
     * A {@code null} in the set of method names will be ignored.</p>
     *
     * @param throwable  the throwable to introspect for a cause, may be null
     * @param methodNames  the method names, null treated as default set
     * @return the cause of the {@link Throwable},
     *  {@code null} if none found or null throwable input
     * @since 1.0
     * @deprecated This feature will be removed in Lang 4.0, use {@link Throwable#getCause} instead
     */
    @Deprecated
    public static Throwable getCause(final Throwable throwable, String[] methodNames) {
        if (throwable == null) {
            return null;
        }
        if (methodNames == null) {
            final Throwable cause = throwable.getCause();
            if (cause != null) {
                return cause;
            }
            methodNames = CAUSE_METHOD_NAMES;
        }
        return Stream.of(methodNames).map(m -> getCauseUsingMethodName(throwable, m)).filter(Objects::nonNull).findFirst().orElse(null);
    }

    /**
     * Gets a {@link Throwable} by method name.
     *
     * @param throwable  the exception to examine
     * @param methodName  the name of the method to find and invoke
     * @return the wrapped exception, or {@code null} if not found
     */
    // TODO: Remove in Lang 4.0
    private static Throwable getCauseUsingMethodName(final Throwable throwable, final String methodName) {
        if (methodName != null) {
            Method method = null;
            try {
                method = throwable.getClass().getMethod(methodName);
            } catch (final NoSuchMethodException | SecurityException ignored) {
                // exception ignored
            }

            if (method != null && Throwable.class.isAssignableFrom(method.getReturnType())) {
                try {
                    return (Throwable) method.invoke(throwable);
                } catch (final IllegalAccessException | IllegalArgumentException | InvocationTargetException ignored) {
                    // exception ignored
                }
            }
        }
        return null;
    }

    /**
     * Gets the default names used when searching for the cause of an exception.
     *
     * <p>This may be modified and used in the overloaded getCause(Throwable, String[]) method.</p>
     *
     * @return cloned array of the default method names
     * @since 3.0
     * @deprecated This feature will be removed in Lang 4.0
     */
    @Deprecated
    public static String[] getDefaultCauseMethodNames() {
        return ArrayUtils.clone(CAUSE_METHOD_NAMES);
    }

    /**
     * Gets a short message summarizing the exception.
     * <p>
     * The message returned is of the form
     * {ClassNameWithoutPackage}: {ThrowableMessage}
     * </p>
     *
     * @param th  the throwable to get a message for, null returns empty string
     * @return the message, non-null
     * @since 2.2
     */
    public static String getMessage(final Throwable th) {
        if (th == null) {
            return StringUtils.EMPTY;
        }
        final String clsName = ClassUtils.getShortClassName(th, null);
        return clsName + ": " + StringUtils.defaultString(th.getMessage());
    }

    /**
     * Introspects the {@link Throwable} to obtain the root cause.
     *
     * <p>This method walks through the exception chain to the last element,
     * "root" of the tree, using {@link Throwable#getCause()}, and
     * returns that exception.</p>
     *
     * <p>From version 2.2, this method handles recursive cause structures
     * that might otherwise cause infinite loops. If the throwable parameter
     * has a cause of itself, then null will be returned. If the throwable
     * parameter cause chain loops, the last element in the chain before the
     * loop is returned.</p>
     *
     * @param throwable  the throwable to get the root cause for, may be null
     * @return the root cause of the {@link Throwable},
     *  {@code null} if null throwable input
     */
    public static Throwable getRootCause(final Throwable throwable) {
        final List<Throwable> list = getThrowableList(throwable);
        return list.isEmpty() ? null : list.get(list.size() - 1);
    }

    /**
     * Gets a short message summarizing the root cause exception.
     * <p>
     * The message returned is of the form
     * {ClassNameWithoutPackage}: {ThrowableMessage}
     * </p>
     *
     * @param throwable  the throwable to get a message for, null returns empty string
     * @return the message, non-null
     * @since 2.2
     */
    public static String getRootCauseMessage(final Throwable throwable) {
        final Throwable root = getRootCause(throwable);
        return getMessage(root == null ? throwable : root);
    }

    /**
     * Gets a compact stack trace for the root cause of the supplied
     * {@link Throwable}.
     *
     * <p>The output of this method is consistent across JDK versions.
     * It consists of the root exception followed by each of its wrapping
     * exceptions separated by '[wrapped]'. Note that this is the opposite
     * order to the JDK1.4 display.</p>
     *
     * @param throwable  the throwable to examine, may be null
     * @return an array of stack trace frames, never null
     * @since 2.0
     */
    public static String[] getRootCauseStackTrace(final Throwable throwable) {
        return getRootCauseStackTraceList(throwable).toArray(ArrayUtils.EMPTY_STRING_ARRAY);
    }

    /**
     * Gets a compact stack trace for the root cause of the supplied {@link Throwable}.
     *
     * <p>
     * The output of this method is consistent across JDK versions. It consists of the root exception followed by each of
     * its wrapping exceptions separated by '[wrapped]'. Note that this is the opposite order to the JDK1.4 display.
     * </p>
     *
     * @param throwable the throwable to examine, may be null
     * @return a list of stack trace frames, never null
     * @since 3.13.0
     */
    public static List<String> getRootCauseStackTraceList(final Throwable throwable) {
        if (throwable == null) {
            return Collections.emptyList();
        }
        final Throwable[] throwables = getThrowables(throwable);
        final int count = throwables.length;
        final List<String> frames = new ArrayList<>();
        List<String> nextTrace = getStackFrameList(throwables[count - 1]);
        for (int i = count; --i >= 0;) {
            final List<String> trace = nextTrace;
            if (i != 0) {
                nextTrace = getStackFrameList(throwables[i - 1]);
                removeCommonFrames(trace, nextTrace);
            }
            if (i == count - 1) {
                frames.add(throwables[i].toString());
            } else {
                frames.add(WRAPPED_MARKER + throwables[i].toString());
            }
            frames.addAll(trace);
        }
        return frames;
    }

    /**
     * Gets a {@link List} of stack frames - the message
     * is not included. Only the trace of the specified exception is
     * returned, any caused by trace is stripped.
     *
     * <p>This works in most cases - it will only fail if the exception
     * message contains a line that starts with:
     * {@code &quot;&nbsp;&nbsp;&nbsp;at&quot;.}</p>
     *
     * @param throwable is any throwable
     * @return List of stack frames
     */
    static List<String> getStackFrameList(final Throwable throwable) {
        final String stackTrace = getStackTrace(throwable);
        final String linebreak = System.lineSeparator();
        final StringTokenizer frames = new StringTokenizer(stackTrace, linebreak);
        final List<String> list = new ArrayList<>();
        boolean traceStarted = false;
        while (frames.hasMoreTokens()) {
            final String token = frames.nextToken();
            // Determine if the line starts with <whitespace>at
            final int at = token.indexOf("at");
            if (at != NOT_FOUND && token.substring(0, at).trim().isEmpty()) {
                traceStarted = true;
                list.add(token);
            } else if (traceStarted) {
                break;
            }
        }
        return list;
    }

    /**
     * Gets an array where each element is a line from the argument.
     *
     * <p>The end of line is determined by the value of {@link System#lineSeparator()}.</p>
     *
     * @param stackTrace  a stack trace String
     * @return an array where each element is a line from the argument
     */
    static String[] getStackFrames(final String stackTrace) {
        final String linebreak = System.lineSeparator();
        final StringTokenizer frames = new StringTokenizer(stackTrace, linebreak);
        final List<String> list = new ArrayList<>();
        while (frames.hasMoreTokens()) {
            list.add(frames.nextToken());
        }
        return list.toArray(ArrayUtils.EMPTY_STRING_ARRAY);
    }

    /**
     * Gets the stack trace associated with the specified
     * {@link Throwable} object, decomposing it into a list of
     * stack frames.
     *
     * <p>The result of this method vary by JDK version as this method
     * uses {@link Throwable#printStackTrace(java.io.PrintWriter)}.
     * On JDK1.3 and earlier, the cause exception will not be shown
     * unless the specified throwable alters printStackTrace.</p>
     *
     * @param throwable  the {@link Throwable} to examine, may be null
     * @return an array of strings describing each stack frame, never null
     */
    public static String[] getStackFrames(final Throwable throwable) {
        if (throwable == null) {
            return ArrayUtils.EMPTY_STRING_ARRAY;
        }
        return getStackFrames(getStackTrace(throwable));
    }

    /**
     * Gets the stack trace from a Throwable as a String.
     *
     * <p>The result of this method vary by JDK version as this method
     * uses {@link Throwable#printStackTrace(java.io.PrintWriter)}.
     * On JDK1.3 and earlier, the cause exception will not be shown
     * unless the specified throwable alters printStackTrace.</p>
     *
     * @param throwable  the {@link Throwable} to be examined, may be null
     * @return the stack trace as generated by the exception's
     * {@code printStackTrace(PrintWriter)} method, or an empty String if {@code null} input
     */
    public static String getStackTrace(final Throwable throwable) {
        if (throwable == null) {
            return StringUtils.EMPTY;
        }
        final StringWriter sw = new StringWriter();
        throwable.printStackTrace(new PrintWriter(sw, true));
        return sw.toString();
    }

    /**
     * Gets a count of the number of {@link Throwable} objects in the
     * exception chain.
     *
     * <p>A throwable without cause will return {@code 1}.
     * A throwable with one cause will return {@code 2} and so on.
     * A {@code null} throwable will return {@code 0}.</p>
     *
     * <p>From version 2.2, this method handles recursive cause structures
     * that might otherwise cause infinite loops. The cause chain is
     * processed until the end is reached, or until the next item in the
     * chain is already in the result set.</p>
     *
     * @param throwable  the throwable to inspect, may be null
     * @return the count of throwables, zero if null input
     */
    public static int getThrowableCount(final Throwable throwable) {
        return getThrowableList(throwable).size();
    }

    /**
     * Gets the list of {@link Throwable} objects in the
     * exception chain.
     *
     * <p>A throwable without cause will return a list containing
     * one element - the input throwable.
     * A throwable with one cause will return a list containing
     * two elements. - the input throwable and the cause throwable.
     * A {@code null} throwable will return a list of size zero.</p>
     *
     * <p>This method handles recursive cause structures that might
     * otherwise cause infinite loops. The cause chain is processed until
     * the end is reached, or until the next item in the chain is already
     * in the result set.</p>
     *
     * @param throwable  the throwable to inspect, may be null
     * @return the list of throwables, never null
     * @since 2.2
     */
    public static List<Throwable> getThrowableList(Throwable throwable) {
        final List<Throwable> list = new ArrayList<>();
        while (throwable != null && !list.contains(throwable)) {
            list.add(throwable);
            throwable = throwable.getCause();
        }
        return list;
    }

    /**
     * Performs an action for each Throwable causes of the given Throwable.
     * <p>
     * A throwable without cause will return a stream containing one element - the input throwable. A throwable with one cause
     * will return a stream containing two elements. - the input throwable and the cause throwable. A {@code null} throwable
     * will return a stream of count zero.
     * </p>
     *
     * <p>
     * This method handles recursive cause structures that might otherwise cause infinite loops. The cause chain is
     * processed until the end is reached, or until the next item in the chain is already in the result set.
     * </p>
     * @param throwable The Throwable to traverse.
     * @param consumer a non-interfering action to perform on the elements.
     * @since 3.13.0
     */
    public static void forEach(final Throwable throwable, final Consumer<Throwable> consumer) {
        stream(throwable).forEach(consumer);
    }

    /**
     * Gets the list of {@link Throwable} objects in the
     * exception chain.
     *
     * <p>A throwable without cause will return an array containing
     * one element - the input throwable.
     * A throwable with one cause will return an array containing
     * two elements. - the input throwable and the cause throwable.
     * A {@code null} throwable will return an array of size zero.</p>
     *
     * <p>From version 2.2, this method handles recursive cause structures
     * that might otherwise cause infinite loops. The cause chain is
     * processed until the end is reached, or until the next item in the
     * chain is already in the result set.</p>
     *
     * @see #getThrowableList(Throwable)
     * @param throwable  the throwable to inspect, may be null
     * @return the array of throwables, never null
     */
    public static Throwable[] getThrowables(final Throwable throwable) {
        return getThrowableList(throwable).toArray(ArrayUtils.EMPTY_THROWABLE_ARRAY);
    }

    /**
     * Tests if the throwable's causal chain have an immediate or wrapped exception
     * of the given type?
     *
     * @param chain
     *            The root of a Throwable causal chain.
     * @param type
     *            The exception type to test.
     * @return true, if chain is an instance of type or is an
     *         UndeclaredThrowableException wrapping a cause.
     * @since 3.5
     * @see #wrapAndThrow(Throwable)
     */
    public static boolean hasCause(Throwable chain,
            final Class<? extends Throwable> type) {
        if (chain instanceof UndeclaredThrowableException) {
            chain = chain.getCause();
        }
        return type.isInstance(chain);
    }

    /**
     * Worker method for the {@code indexOfType} methods.
     *
     * @param throwable  the throwable to inspect, may be null
     * @param type  the type to search for, subclasses match, null returns -1
     * @param fromIndex  the (zero-based) index of the starting position,
     *  negative treated as zero, larger than chain size returns -1
     * @param subclass if {@code true}, compares with {@link Class#isAssignableFrom(Class)}, otherwise compares
     * using references
     * @return index of the {@code type} within throwables nested within the specified {@code throwable}
     */
    private static int indexOf(final Throwable throwable, final Class<? extends Throwable> type, int fromIndex, final boolean subclass) {
        if (throwable == null || type == null) {
            return NOT_FOUND;
        }
        if (fromIndex < 0) {
            fromIndex = 0;
        }
        final Throwable[] throwables = getThrowables(throwable);
        if (fromIndex >= throwables.length) {
            return NOT_FOUND;
        }
        if (subclass) {
            for (int i = fromIndex; i < throwables.length; i++) {
                if (type.isAssignableFrom(throwables[i].getClass())) {
                    return i;
                }
            }
        } else {
            for (int i = fromIndex; i < throwables.length; i++) {
                if (type.equals(throwables[i].getClass())) {
                    return i;
                }
            }
        }
        return NOT_FOUND;
    }

    /**
     * Returns the (zero-based) index of the first {@link Throwable}
     * that matches the specified class (exactly) in the exception chain.
     * Subclasses of the specified class do not match - see
     * {@link #indexOfType(Throwable, Class)} for the opposite.
     *
     * <p>A {@code null} throwable returns {@code -1}.
     * A {@code null} type returns {@code -1}.
     * No match in the chain returns {@code -1}.</p>
     *
     * @param throwable  the throwable to inspect, may be null
     * @param clazz  the class to search for, subclasses do not match, null returns -1
     * @return the index into the throwable chain, -1 if no match or null input
     */
    public static int indexOfThrowable(final Throwable throwable, final Class<? extends Throwable> clazz) {
        return indexOf(throwable, clazz, 0, false);
    }

    /**
     * Returns the (zero-based) index of the first {@link Throwable}
     * that matches the specified type in the exception chain from
     * a specified index.
     * Subclasses of the specified class do not match - see
     * {@link #indexOfType(Throwable, Class, int)} for the opposite.
     *
     * <p>A {@code null} throwable returns {@code -1}.
     * A {@code null} type returns {@code -1}.
     * No match in the chain returns {@code -1}.
     * A negative start index is treated as zero.
     * A start index greater than the number of throwables returns {@code -1}.</p>
     *
     * @param throwable  the throwable to inspect, may be null
     * @param clazz  the class to search for, subclasses do not match, null returns -1
     * @param fromIndex  the (zero-based) index of the starting position,
     *  negative treated as zero, larger than chain size returns -1
     * @return the index into the throwable chain, -1 if no match or null input
     */
    public static int indexOfThrowable(final Throwable throwable, final Class<? extends Throwable> clazz, final int fromIndex) {
        return indexOf(throwable, clazz, fromIndex, false);
    }

    /**
     * Returns the (zero-based) index of the first {@link Throwable}
     * that matches the specified class or subclass in the exception chain.
     * Subclasses of the specified class do match - see
     * {@link #indexOfThrowable(Throwable, Class)} for the opposite.
     *
     * <p>A {@code null} throwable returns {@code -1}.
     * A {@code null} type returns {@code -1}.
     * No match in the chain returns {@code -1}.</p>
     *
     * @param throwable  the throwable to inspect, may be null
     * @param type  the type to search for, subclasses match, null returns -1
     * @return the index into the throwable chain, -1 if no match or null input
     * @since 2.1
     */
    public static int indexOfType(final Throwable throwable, final Class<? extends Throwable> type) {
        return indexOf(throwable, type, 0, true);
    }

    /**
     * Returns the (zero-based) index of the first {@link Throwable}
     * that matches the specified type in the exception chain from
     * a specified index.
     * Subclasses of the specified class do match - see
     * {@link #indexOfThrowable(Throwable, Class)} for the opposite.
     *
     * <p>A {@code null} throwable returns {@code -1}.
     * A {@code null} type returns {@code -1}.
     * No match in the chain returns {@code -1}.
     * A negative start index is treated as zero.
     * A start index greater than the number of throwables returns {@code -1}.</p>
     *
     * @param throwable  the throwable to inspect, may be null
     * @param type  the type to search for, subclasses match, null returns -1
     * @param fromIndex  the (zero-based) index of the starting position,
     *  negative treated as zero, larger than chain size returns -1
     * @return the index into the throwable chain, -1 if no match or null input
     * @since 2.1
     */
    public static int indexOfType(final Throwable throwable, final Class<? extends Throwable> type, final int fromIndex) {
        return indexOf(throwable, type, fromIndex, true);
    }

    /**
     * Prints a compact stack trace for the root cause of a throwable
     * to {@code System.err}.
     *
     * <p>The compact stack trace starts with the root cause and prints
     * stack frames up to the place where it was caught and wrapped.
     * Then it prints the wrapped exception and continues with stack frames
     * until the wrapper exception is caught and wrapped again, etc.</p>
     *
     * <p>The output of this method is consistent across JDK versions.
     * Note that this is the opposite order to the JDK1.4 display.</p>
     *
     * <p>The method is equivalent to {@code printStackTrace} for throwables
     * that don't have nested causes.</p>
     *
     * @param throwable  the throwable to output
     * @since 2.0
     */
    public static void printRootCauseStackTrace(final Throwable throwable) {
        printRootCauseStackTrace(throwable, System.err);
    }

    /**
     * Prints a compact stack trace for the root cause of a throwable.
     *
     * <p>The compact stack trace starts with the root cause and prints
     * stack frames up to the place where it was caught and wrapped.
     * Then it prints the wrapped exception and continues with stack frames
     * until the wrapper exception is caught and wrapped again, etc.</p>
     *
     * <p>The output of this method is consistent across JDK versions.
     * Note that this is the opposite order to the JDK1.4 display.</p>
     *
     * <p>The method is equivalent to {@code printStackTrace} for throwables
     * that don't have nested causes.</p>
     *
     * @param throwable  the throwable to output, may be null
     * @param printStream  the stream to output to, may not be null
     * @throws NullPointerException if the printStream is {@code null}
     * @since 2.0
     */
    @SuppressWarnings("resource")
    public static void printRootCauseStackTrace(final Throwable throwable, final PrintStream printStream) {
        if (throwable == null) {
            return;
        }
        Objects.requireNonNull(printStream, "printStream");
        getRootCauseStackTraceList(throwable).forEach(printStream::println);
        printStream.flush();
    }

    /**
     * Prints a compact stack trace for the root cause of a throwable.
     *
     * <p>The compact stack trace starts with the root cause and prints
     * stack frames up to the place where it was caught and wrapped.
     * Then it prints the wrapped exception and continues with stack frames
     * until the wrapper exception is caught and wrapped again, etc.</p>
     *
     * <p>The output of this method is consistent across JDK versions.
     * Note that this is the opposite order to the JDK1.4 display.</p>
     *
     * <p>The method is equivalent to {@code printStackTrace} for throwables
     * that don't have nested causes.</p>
     *
     * @param throwable  the throwable to output, may be null
     * @param printWriter  the writer to output to, may not be null
     * @throws NullPointerException if the printWriter is {@code null}
     * @since 2.0
     */
    @SuppressWarnings("resource")
    public static void printRootCauseStackTrace(final Throwable throwable, final PrintWriter printWriter) {
        if (throwable == null) {
            return;
        }
        Objects.requireNonNull(printWriter, "printWriter");
        getRootCauseStackTraceList(throwable).forEach(printWriter::println);
        printWriter.flush();
    }

    /**
     * Removes common frames from the cause trace given the two stack traces.
     *
     * @param causeFrames  stack trace of a cause throwable
     * @param wrapperFrames  stack trace of a wrapper throwable
     * @throws NullPointerException if either argument is null
     * @since 2.0
     */
    public static void removeCommonFrames(final List<String> causeFrames, final List<String> wrapperFrames) {
        Objects.requireNonNull(causeFrames, "causeFrames");
        Objects.requireNonNull(wrapperFrames, "wrapperFrames");
        int causeFrameIndex = causeFrames.size() - 1;
        int wrapperFrameIndex = wrapperFrames.size() - 1;
        while (causeFrameIndex >= 0 && wrapperFrameIndex >= 0) {
            // Remove the frame from the cause trace if it is the same
            // as in the wrapper trace
            final String causeFrame = causeFrames.get(causeFrameIndex);
            final String wrapperFrame = wrapperFrames.get(wrapperFrameIndex);
            if (causeFrame.equals(wrapperFrame)) {
                causeFrames.remove(causeFrameIndex);
            }
            causeFrameIndex--;
            wrapperFrameIndex--;
        }
    }

    /**
     * Throws a checked exception without adding the exception to the throws
     * clause of the calling method. This method prevents throws clause
     * pollution and reduces the clutter of "Caused by" exceptions in the
     * stacktrace.
     * <p>
     * The use of this technique may be controversial, but exceedingly useful to
     * library developers.
     * </p>
     * <pre>
     *  public int propagateExample { // note that there is no throws clause
     *      try {
     *          return invocation(); // throws IOException
     *      } catch (Exception e) {
     *          return ExceptionUtils.rethrow(e);  // propagates a checked exception
     *      }
     *  }
     * </pre>
     * <p>
     * This is an alternative to the more conservative approach of wrapping the
     * checked exception in a RuntimeException:
     * </p>
     * <pre>
     *  public int wrapExample { // note that there is no throws clause
     *      try {
     *          return invocation(); // throws IOException
     *      } catch (Error e) {
     *          throw e;
     *      } catch (RuntimeException e) {
     *          throw e;  // wraps a checked exception
     *      } catch (Exception e) {
     *          throw new UndeclaredThrowableException(e);  // wraps a checked exception
     *      }
     *  }
     * </pre>
     * <p>
     * One downside to using this approach is that the java compiler will not
     * allow invoking code to specify a checked exception in a catch clause
     * unless there is some code path within the try block that has invoked a
     * method declared with that checked exception. If the invoking site wishes
     * to catch the shaded checked exception, it must either invoke the shaded
     * code through a method re-declaring the desired checked exception, or
     * catch Exception and use the instanceof operator. Either of these
     * techniques are required when interacting with non-java jvm code such as
     * Jython, Scala, or Groovy, since these languages do not consider any
     * exceptions as checked.
     * </p>
     *
     * @param throwable
     *            The throwable to rethrow.
     * @param <R> The type of the returned value.
     * @return Never actually returned, this generic type matches any type
     *         which the calling site requires. "Returning" the results of this
     *         method, as done in the propagateExample above, will satisfy the
     *         java compiler requirement that all code paths return a value.
     * @since 3.5
     * @see #wrapAndThrow(Throwable)
     */
    public static <R> R rethrow(final Throwable throwable) {
        // claim that the typeErasure invocation throws a RuntimeException
        return ExceptionUtils.<R, RuntimeException>eraseType(throwable);
    }

    /**
     * Streams causes of a Throwable.
     * <p>
     * A throwable without cause will return a stream containing one element - the input throwable. A throwable with one cause
     * will return a stream containing two elements. - the input throwable and the cause throwable. A {@code null} throwable
     * will return a stream of count zero.
     * </p>
     *
     * <p>
     * This method handles recursive cause structures that might otherwise cause infinite loops. The cause chain is
     * processed until the end is reached, or until the next item in the chain is already in the result set.
     * </p>
     *
     * @param throwable The Throwable to traverse
     * @return A new Stream of Throwable causes.
     * @since 3.13.0
     */
    public static Stream<Throwable> stream(final Throwable throwable) {
        // No point building a custom Iterable as it would keep track of visited elements to avoid infinite loops
        return getThrowableList(throwable).stream();
    }

    /**
     * Worker method for the {@code throwableOfType} methods.
     *
     * @param <T> the type of Throwable you are searching.
     * @param throwable  the throwable to inspect, may be null
     * @param type  the type to search, subclasses match, null returns null
     * @param fromIndex  the (zero-based) index of the starting position,
     *  negative treated as zero, larger than chain size returns null
     * @param subclass if {@code true}, compares with {@link Class#isAssignableFrom(Class)}, otherwise compares
     * using references
     * @return throwable of the {@code type} within throwables nested within the specified {@code throwable}
     */
    private static <T extends Throwable> T throwableOf(final Throwable throwable, final Class<T> type, int fromIndex, final boolean subclass) {
        if (throwable == null || type == null) {
            return null;
        }
        if (fromIndex < 0) {
            fromIndex = 0;
        }
        final Throwable[] throwables = getThrowables(throwable);
        if (fromIndex >= throwables.length) {
            return null;
        }
        if (subclass) {
            for (int i = fromIndex; i < throwables.length; i++) {
                if (type.isAssignableFrom(throwables[i].getClass())) {
                    return type.cast(throwables[i]);
                }
            }
        } else {
            for (int i = fromIndex; i < throwables.length; i++) {
                if (type.equals(throwables[i].getClass())) {
                    return type.cast(throwables[i]);
                }
            }
        }
        return null;
    }

    /**
     * Returns the first {@link Throwable}
     * that matches the specified class (exactly) in the exception chain.
     * Subclasses of the specified class do not match - see
     * {@link #throwableOfType(Throwable, Class)} for the opposite.
     *
     * <p>A {@code null} throwable returns {@code null}.
     * A {@code null} type returns {@code null}.
     * No match in the chain returns {@code null}.</p>
     *
     * @param <T> the type of Throwable you are searching.
     * @param throwable  the throwable to inspect, may be null
     * @param clazz  the class to search for, subclasses do not match, null returns null
     * @return the first matching throwable from the throwable chain, null if no match or null input
     * @since 3.10
     */
    public static <T extends Throwable> T throwableOfThrowable(final Throwable throwable, final Class<T> clazz) {
        return throwableOf(throwable, clazz, 0, false);
    }

    /**
     * Returns the first {@link Throwable}
     * that matches the specified type in the exception chain from
     * a specified index.
     * Subclasses of the specified class do not match - see
     * {@link #throwableOfType(Throwable, Class, int)} for the opposite.
     *
     * <p>A {@code null} throwable returns {@code null}.
     * A {@code null} type returns {@code null}.
     * No match in the chain returns {@code null}.
     * A negative start index is treated as zero.
     * A start index greater than the number of throwables returns {@code null}.</p>
     *
     * @param <T> the type of Throwable you are searching.
     * @param throwable  the throwable to inspect, may be null
     * @param clazz  the class to search for, subclasses do not match, null returns null
     * @param fromIndex  the (zero-based) index of the starting position,
     *  negative treated as zero, larger than chain size returns null
     * @return the first matching throwable from the throwable chain, null if no match or null input
     * @since 3.10
     */
    public static <T extends Throwable> T throwableOfThrowable(final Throwable throwable, final Class<T> clazz, final int fromIndex) {
        return throwableOf(throwable, clazz, fromIndex, false);
    }

    /**
     * Returns the throwable of the first {@link Throwable}
     * that matches the specified class or subclass in the exception chain.
     * Subclasses of the specified class do match - see
     * {@link #throwableOfThrowable(Throwable, Class)} for the opposite.
     *
     * <p>A {@code null} throwable returns {@code null}.
     * A {@code null} type returns {@code null}.
     * No match in the chain returns {@code null}.</p>
     *
     * @param <T> the type of Throwable you are searching.
     * @param throwable  the throwable to inspect, may be null
     * @param type  the type to search for, subclasses match, null returns null
     * @return the first matching throwable from the throwable chain, null if no match or null input
     * @since 3.10
     */
    public static <T extends Throwable> T throwableOfType(final Throwable throwable, final Class<T> type) {
        return throwableOf(throwable, type, 0, true);
    }

    /**
     * Returns the first {@link Throwable}
     * that matches the specified type in the exception chain from
     * a specified index.
     * Subclasses of the specified class do match - see
     * {@link #throwableOfThrowable(Throwable, Class)} for the opposite.
     *
     * <p>A {@code null} throwable returns {@code null}.
     * A {@code null} type returns {@code null}.
     * No match in the chain returns {@code null}.
     * A negative start index is treated as zero.
     * A start index greater than the number of throwables returns {@code null}.</p>
     *
     * @param <T> the type of Throwable you are searching.
     * @param throwable  the throwable to inspect, may be null
     * @param type  the type to search for, subclasses match, null returns null
     * @param fromIndex  the (zero-based) index of the starting position,
     *  negative treated as zero, larger than chain size returns null
     * @return the first matching throwable from the throwable chain, null if no match or null input
     * @since 3.10
     */
    public static <T extends Throwable> T throwableOfType(final Throwable throwable, final Class<T> type, final int fromIndex) {
        return throwableOf(throwable, type, fromIndex, true);
    }

    /**
     * Throws a checked exception without adding the exception to the throws
     * clause of the calling method. For checked exceptions, this method throws
     * an UndeclaredThrowableException wrapping the checked exception. For
     * Errors and RuntimeExceptions, the original exception is rethrown.
     * <p>
     * The downside to using this approach is that invoking code which needs to
     * handle specific checked exceptions must sniff up the exception chain to
     * determine if the caught exception was caused by the checked exception.
     * </p>
     *
     * @param throwable
     *            The throwable to rethrow.
     * @param <R> The type of the returned value.
     * @return Never actually returned, this generic type matches any type
     *         which the calling site requires. "Returning" the results of this
     *         method will satisfy the java compiler requirement that all code
     *         paths return a value.
     * @since 3.5
     * @see #rethrow(Throwable)
     * @see #hasCause(Throwable, Class)
     */
    public static <R> R wrapAndThrow(final Throwable throwable) {
        if (throwable instanceof RuntimeException) {
            throw (RuntimeException) throwable;
        }
        if (throwable instanceof Error) {
            throw (Error) throwable;
        }
        throw new UndeclaredThrowableException(throwable);
    }

    /**
     * Public constructor allows an instance of {@link ExceptionUtils} to be created, although that is not
     * normally necessary.
     */
    public ExceptionUtils() {
    }
}

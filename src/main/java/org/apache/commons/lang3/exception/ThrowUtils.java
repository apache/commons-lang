package org.apache.commons.lang3.exception;

/**
 * @author <a href="mailto:thegzak@gmail.com">Gregory Zak</a>
 *
 * <p>Utility class containing a method to allow checked exceptions to be rethrown without explicitly
 * declaring them.</p>
 *
 */
public final class ThrowUtils {

    private ThrowUtils() {
    }

    /**
     * <p>Any method with uninteresting checked exceptions in the body can simply be wrapped in a
     * try/catch block, and the exceptions can then be freely rethrown from the catch block using
     * {@code rethrow(t)}. If a method requires a return value, simply use {@code return rethrow(t)}
     * in the catch block and compiler type inference will do the rest (no need to manually capture
     * a return variable in the try block and return it after the catch block).</p>
     *
     * @param throwable The throwable to rethrow
     * @param <R> Used only for compiler type inference
     * @return Never returns, always throws
     */
    public static <R> R rethrow(Throwable throwable) {
        return ThrowUtils.<R, RuntimeException>rethrow0(throwable);
    }

    @SuppressWarnings("unchecked")
    private static <R, T extends Throwable> R rethrow0(Throwable throwable) throws T {
        throw (T) throwable;
    }

}

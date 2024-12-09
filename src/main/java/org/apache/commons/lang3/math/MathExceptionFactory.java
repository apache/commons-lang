package org.apache.commons.lang3.math;

public class MathExceptionFactory {
    private MathExceptionFactory() {}

    public static void throwXMustNotBeZero(String x) {
        throw new ArithmeticException("The " + x + " must not be zero");
    }

    public static void throwXMustBePositive(String x) {
        throw new ArithmeticException("The " + x + " must be positive");
    }

    public static void throwOverflow(String message) {
        throw new ArithmeticException("overflow : " + message);
    }

    public static void throwXIsTooLargeForInteger(String x) {
        throw new ArithmeticException("The " + x + " is too large to represent as an Integer");
    }

    public static void throwUnableToConvertXToFraction(String x) {
        throw new ArithmeticException("Unable to convert " + x + " to fraction");
    }
}

package org.apache.commons.lang3.test;

/**
 * Allows for testing an exception that is not visible to
 * {@link org.apache.commons.lang3.exception.ExceptionUtils}
 */
public class NotVisibleExceptionFactory {

  private NotVisibleExceptionFactory() {}

  /**
   * Create a new Exception whose getCause method returns the
   * provided cause.
   * @param cause the cause of the exception
   * @return a new {@link Exception}
   */
  public static Exception createException(final Throwable cause) {
    return new NotVisibleException(cause);
  }

  private static class NotVisibleException extends Exception {

    private static final long serialVersionUID = 1L; // avoid warning

    private final Throwable cause;

    private NotVisibleException(Throwable cause) {
      this.cause = cause;
    }

    @Override
    public Throwable getCause() {
      return cause;
    }
  }
}

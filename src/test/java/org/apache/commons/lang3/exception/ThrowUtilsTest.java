package org.apache.commons.lang3.exception;

import org.junit.Test;

import static org.apache.commons.lang3.exception.ThrowUtils.rethrow;

/**
 * @author <a href="mailto:thegzak@gmail.com">Gregory Zak</a>
 */
public class ThrowUtilsTest {

    @Test(expected = Exception.class)
    public void testVoid() {
        rethrow(new Exception());
    }

    @Test(expected = Exception.class)
    public void testReturning() {
        Object obj = helper();
    }

    private static Object helper() {
        try {
            throw new Exception();
        } catch (Exception e) {
            return rethrow(e);
        }
    }

}

package org.apache.commons.lang3.util;

import java.lang.reflect.Array;

// New helper for reflection-based operations (if still needed)
public class ArrayReflectionUtils {
    public static Object copyArrayGrow1(final Object array, Class<?> componentType) {
        // Retain reflective behavior if required
        int length = (array != null) ? Array.getLength(array) : 0;
        Object newArray = Array.newInstance(componentType, length + 1);
        if (array != null) {
            System.arraycopy(array, 0, newArray, 0, length);
        }
        return newArray;
    }
}
package org.apache.commons.lang3;

import org.junit.Assert;
import org.junit.Test;

import java.util.Arrays;

/**
 * Unit tests {@link RandomEnumUtils}
 */
public class RandomEnumUtilsTest {

    @Test
    public void testRandomEnum() {
        TestEnum randomEnum = RandomEnumUtils.randomEnum(TestEnum.class);
        Assert.assertTrue(Arrays.asList(TestEnum.values()).contains(randomEnum));
    }

    private enum TestEnum {
        VALUE1, VALUE2, VALUE3
    }
}

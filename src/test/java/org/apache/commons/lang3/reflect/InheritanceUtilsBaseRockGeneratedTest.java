package org.apache.commons.lang3.reflect;

import org.apache.commons.lang3.reflect.InheritanceUtils;

import org.apache.commons.lang3.BooleanUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.ParameterizedTest;
import static org.junit.jupiter.api.Assertions.*;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

class InheritanceUtilsBaseRockGeneratedTest {

    //BaseRock generated method id: ${testDistanceWithNullChild}, hash: A6E6C31AA9CC2FAD80B6785457708D82
    @Test
    void testDistanceWithNullChild() {
        assertEquals(-1, InheritanceUtils.distance(null, Object.class));
    }

    //BaseRock generated method id: ${testDistanceWithNullParent}, hash: C0D6103AA5642E3E4D9113DD58EF9309
    @Test
    void testDistanceWithNullParent() {
        assertEquals(-1, InheritanceUtils.distance(String.class, null));
    }

    //BaseRock generated method id: ${testDistanceWithBothNull}, hash: FA9263D4F94EC1B52E113DAB2F0EDFD5
    @Test
    void testDistanceWithBothNull() {
        assertEquals(-1, InheritanceUtils.distance(null, null));
    }

    //BaseRock generated method id: ${testDistanceWithSameClass}, hash: 44351A16A9149A8BC4F7063109907F9B
    @Test
    void testDistanceWithSameClass() {
        assertEquals(0, InheritanceUtils.distance(String.class, String.class));
    }

    //BaseRock generated method id: ${testDistanceWithValidInheritance}, hash: EA8983A7DDF92C6571AE2C7ED13EF13A
    @ParameterizedTest
    @CsvSource({ "java.lang.String, java.lang.Object, 1", "java.util.ArrayList, java.util.List, 1", "java.util.ArrayList, java.util.Collection, 2", "java.util.ArrayList, java.lang.Object, 3" })
    void testDistanceWithValidInheritance(String childClassName, String parentClassName, int expectedDistance) throws ClassNotFoundException {
        Class<?> childClass = Class.forName(childClassName);
        Class<?> parentClass = Class.forName(parentClassName);
        assertEquals(expectedDistance, InheritanceUtils.distance(childClass, parentClass));
    }

    //BaseRock generated method id: ${testDistanceWithUnrelatedClasses}, hash: FB2DD032B0375E39B2DE622FFD1BC41D
    @Test
    void testDistanceWithUnrelatedClasses() {
        assertEquals(-1, InheritanceUtils.distance(String.class, Integer.class));
    }

    //BaseRock generated method id: ${testDeprecatedConstructor}, hash: A6DC8D6E56C2C7E310D88915C0A0A379
    @Test
    void testDeprecatedConstructor() {
        assertDoesNotThrow(InheritanceUtils::new);
    }
}

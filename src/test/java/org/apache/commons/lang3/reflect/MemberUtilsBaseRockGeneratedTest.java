package org.apache.commons.lang3.reflect;

import org.apache.commons.lang3.reflect.MemberUtils;

import java.util.Arrays;
import java.lang.reflect.Modifier;
import java.lang.reflect.Method;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.provider.CsvSource;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.params.ParameterizedTest;
import java.lang.reflect.Constructor;
import org.apache.commons.lang3.ClassUtils;
import static org.mockito.Mockito.*;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

class MemberUtilsBaseRockGeneratedTest {

    //BaseRock generated method id: ${testCompareConstructorFit}, hash: 538351DA1B693C751E13EC1BA5269AEC
    @Test
    void testCompareConstructorFit() {
        Constructor<?> left = mock(Constructor.class);
        Constructor<?> right = mock(Constructor.class);
        Class<?>[] actual = new Class<?>[] { String.class, Integer.class };
        when(left.getParameterTypes()).thenReturn(new Class<?>[] { String.class, Integer.class });
        when(right.getParameterTypes()).thenReturn(new Class<?>[] { Object.class, Number.class });
        when(left.isVarArgs()).thenReturn(false);
        when(right.isVarArgs()).thenReturn(false);
        int result = MemberUtils.compareConstructorFit(left, right, actual);
        assertTrue(result < 0);
    }

    //BaseRock generated method id: ${testCompareMethodFit}, hash: C2937A89D40C7CD5224AF082923A56B4
    @Test
    void testCompareMethodFit() {
        Method left = mock(Method.class);
        Method right = mock(Method.class);
        Class<?>[] actual = new Class<?>[] { String.class, Integer.class };
        when(left.getParameterTypes()).thenReturn(new Class<?>[] { String.class, Integer.class });
        when(right.getParameterTypes()).thenReturn(new Class<?>[] { Object.class, Number.class });
        when(left.isVarArgs()).thenReturn(false);
        when(right.isVarArgs()).thenReturn(false);
        int result = MemberUtils.compareMethodFit(left, right, actual);
        assertTrue(result < 0);
    }

    //BaseRock generated method id: ${testIsAccessible}, hash: C5CED04166D4D5276A853167BBD4DA48
    @ParameterizedTest
    @CsvSource({ "true, false, false", "false, true, false", "true, true, true", "false, false, false" })
    void testIsAccessible(boolean isPublic, boolean isSynthetic, boolean expected) {
        MockMember member = new MockMember(isPublic, isSynthetic);
        assertEquals(expected, MemberUtils.isAccessible(member));
    }

    //BaseRock generated method id: ${testIsMatchingConstructor}, hash: 660D6313C632A4076F13EF81AB4CC5D0
    @Test
    void testIsMatchingConstructor() {
        Constructor<?> constructor = mock(Constructor.class);
        Class<?>[] parameterTypes = new Class<?>[] { String.class, Integer.class };
        when(constructor.getParameterTypes()).thenReturn(parameterTypes);
        when(constructor.isVarArgs()).thenReturn(false);
        assertTrue(MemberUtils.isMatchingConstructor(constructor, parameterTypes));
    }

    //BaseRock generated method id: ${testIsMatchingMethod}, hash: 87FB723A691EB1BF66B75495E19CCF42
    @Test
    void testIsMatchingMethod() {
        Method method = mock(Method.class);
        Class<?>[] parameterTypes = new Class<?>[] { String.class, Integer.class };
        when(method.getParameterTypes()).thenReturn(parameterTypes);
        when(method.isVarArgs()).thenReturn(false);
        assertTrue(MemberUtils.isMatchingMethod(method, parameterTypes));
    }

    //BaseRock generated method id: ${testIsPackageAccess}, hash: B35D015258B9D6B10D419A6B5492A206
    @Test
    void testIsPackageAccess() {
        assertTrue(MemberUtils.isPackageAccess(0));
        assertFalse(MemberUtils.isPackageAccess(Modifier.PUBLIC));
    }

    //BaseRock generated method id: ${testIsPublic}, hash: BBEA9DB51E3B5824534FFB6CB8F03A7E
    @Test
    void testIsPublic() {
        MockMember publicMember = new MockMember(true, false);
        MockMember nonPublicMember = new MockMember(false, false);
        assertTrue(MemberUtils.isPublic(publicMember));
        assertFalse(MemberUtils.isPublic(nonPublicMember));
        assertFalse(MemberUtils.isPublic(null));
    }

    //BaseRock generated method id: ${testIsStatic}, hash: 822888A903A4EE538A5D709331479039
    @Test
    void testIsStatic() {
        MockMember staticMember = new MockMember(true, false, true);
        MockMember nonStaticMember = new MockMember(true, false, false);
        assertTrue(MemberUtils.isStatic(staticMember));
        assertFalse(MemberUtils.isStatic(nonStaticMember));
        assertFalse(MemberUtils.isStatic(null));
    }

    //BaseRock generated method id: ${testSetAccessibleWorkaround}, hash: 622904F00591B2AD307354B86D267786
    @Test
    void testSetAccessibleWorkaround() {
        MockAccessibleObject accessibleObject = new MockAccessibleObject();
        MemberUtils.setAccessibleWorkaround(accessibleObject);
        assertTrue(accessibleObject.isAccessible());
    }

    private static class MockMember implements java.lang.reflect.Member {

        private final boolean isPublic;

        private final boolean isSynthetic;

        private final boolean isStatic;

        MockMember(boolean isPublic, boolean isSynthetic) {
            this(isPublic, isSynthetic, false);
        }

        MockMember(boolean isPublic, boolean isSynthetic, boolean isStatic) {
            this.isPublic = isPublic;
            this.isSynthetic = isSynthetic;
            this.isStatic = isStatic;
        }

        @Override
        public Class<?> getDeclaringClass() {
            return null;
        }

        @Override
        public String getName() {
            return null;
        }

        @Override
        public int getModifiers() {
            int modifiers = 0;
            if (isPublic) {
                modifiers |= Modifier.PUBLIC;
            }
            if (isStatic) {
                modifiers |= Modifier.STATIC;
            }
            return modifiers;
        }

        @Override
        public boolean isSynthetic() {
            return isSynthetic;
        }
    }

    private static class MockAccessibleObject extends java.lang.reflect.AccessibleObject implements java.lang.reflect.Member {

        @Override
        public Class<?> getDeclaringClass() {
            return null;
        }

        @Override
        public String getName() {
            return null;
        }

        @Override
        public int getModifiers() {
            return Modifier.PUBLIC;
        }

        @Override
        public boolean isSynthetic() {
            return false;
        }
    }
}

package org.apache.commons.lang3;

import org.apache.commons.lang3.ClassLoaderUtils;

import org.junit.jupiter.api.Timeout;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.hamcrest.MatcherAssert.assertThat;
import java.net.URLClassLoader;
import org.junit.jupiter.api.Test;
import static org.hamcrest.Matchers.equalTo;
import java.net.URL;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class ClassLoaderUtilsBaseRockGeneratedTest {

    //BaseRock generated method id: ${getSystemURLsWhenClNotInstanceOfURLClassLoader}, hash: 02695E934D8E3EF2D69256C1333EABD9
    @Test
    void getSystemURLsWhenClNotInstanceOfURLClassLoader() {
        URL[] result = ClassLoaderUtils.getSystemURLs();
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${getThreadURLsWhenClNotInstanceOfURLClassLoader}, hash: 195E39175A703410B920EA0D7D3FB3DA
    @Test
    void getThreadURLsWhenClNotInstanceOfURLClassLoader() {
        URL[] result = ClassLoaderUtils.getThreadURLs();
        assertAll("result", () -> assertThat(result.length, equalTo(0)));
    }

    //BaseRock generated method id: ${toStringWhenClassLoaderInstanceOfURLClassLoader}, hash: 3A395DB4EC88DC3C5579BFDA46238A4B
    @Test
    void toStringWhenClassLoaderInstanceOfURLClassLoader() {
        URL[] urlArray = new URL[] {};
        URLClassLoader urlClassLoader = new URLClassLoader(urlArray);
        String result = ClassLoaderUtils.toString((ClassLoader) urlClassLoader);
        assertAll("result", () -> assertThat(result, equalTo(urlClassLoader + java.util.Arrays.toString(urlArray))));
    }

    //BaseRock generated method id: ${toStringWhenClassLoaderNotInstanceOfURLClassLoader}, hash: 7104C6EADBB2DDB30075994CAAFA5308
    @Test
    void toStringWhenClassLoaderNotInstanceOfURLClassLoader() {
        ClassLoader classLoader = ClassLoader.getSystemClassLoader();
        String result = ClassLoaderUtils.toString(classLoader);
        assertAll("result", () -> assertThat(result, equalTo(classLoader.toString())));
    }

    //BaseRock generated method id: ${toString1WhenClassLoaderIsNotNull}, hash: 62FBED240C2641BEF30E7486FE30398D
    @Test
    void toString1WhenClassLoaderIsNotNull() {
        URL[] urlArray = new URL[] {};
        URLClassLoader urlClassLoader = new URLClassLoader(urlArray);
        String result = ClassLoaderUtils.toString(urlClassLoader);
        assertAll("result", () -> assertThat(result, equalTo(urlClassLoader + java.util.Arrays.toString(urlArray))));
    }

    //BaseRock generated method id: ${toString1WhenClassLoaderIsNull}, hash: 51081B62DE11EDCCAD0FB671F9E348D2
    @Test
    void toString1WhenClassLoaderIsNull() {
        URLClassLoader urlClassLoader = null;
        String result = ClassLoaderUtils.toString(urlClassLoader);
        assertAll("result", () -> assertThat(result, equalTo("null")));
    }
}

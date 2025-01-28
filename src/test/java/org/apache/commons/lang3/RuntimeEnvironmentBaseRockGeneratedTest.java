package org.apache.commons.lang3;

import org.apache.commons.lang3.RuntimeEnvironment;
import java.util.Arrays;
import java.nio.file.Files;
import org.junit.jupiter.api.BeforeEach;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import static org.mockito.ArgumentMatchers.eq;
import java.io.File;
import org.apache.commons.lang3.RuntimeEnvironment;
import java.nio.charset.Charset;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.provider.CsvSource;
import static org.junit.jupiter.api.Assertions.*;
import java.nio.file.Paths;
import static org.mockito.Mockito.*;
import java.io.IOException;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import org.junit.jupiter.api.Disabled;

class RuntimeEnvironmentBaseRockGeneratedTest {

    private RuntimeEnvironment runtimeEnvironment;

    @BeforeEach
    void setUp() {
        runtimeEnvironment = new RuntimeEnvironment();
    }

    //BaseRock generated method id: ${testInContainerWithNoArgument}, hash: 97883FF7D430422C491B0278D71CEB77
    @Test
    void testInContainerWithNoArgument() {
        assertNotNull(RuntimeEnvironment.inContainer());
    }

    //BaseRock generated method id: ${testInContainerWithDirPrefix}, hash: 1F0662924BCE35A89391CCF7F58E9FE5
    @ParameterizedTest
    @CsvSource({ "/proc/1/environ, container, true", "/proc/1/environ, , false", "/.dockerenv, , true", "/run/.containerenv, , true", "/nonexistent, , false" })
    void testInContainerWithDirPrefix(String filePath, String containerValue, boolean expected) throws IOException {
        File mockFile = mock(File.class);
        when(mockFile.exists()).thenReturn(!filePath.equals("/nonexistent"));
        try {
            if (!filePath.equals("/nonexistent")) {
                Files.write(Paths.get(filePath), (containerValue + "\0").getBytes());
            }
            boolean result = RuntimeEnvironment.inContainer(filePath);
            assertEquals(expected, result);
        } finally {
            if (!filePath.equals("/nonexistent")) {
                Files.deleteIfExists(Paths.get(filePath));
            }
        }
    }

    //BaseRock generated method id: ${testFileExistsMethod}, hash: DFE9752DE8F06A1A19299C6D1D3EEC78
    @Disabled()
    @Test
    void testFileExistsMethod() throws Exception {
        File mockFile = mock(File.class);
        when(mockFile.exists()).thenReturn(true);
        assertTrue((Boolean) RuntimeEnvironment.class.getDeclaredMethod("fileExists", String.class).invoke(null, mockFile.getPath()));
    }

    //BaseRock generated method id: ${testReadFileMethod}, hash: 3C599D4C67C5ADF821BA14C7B5EB1471
    @Disabled()
    @Test
    void testReadFileMethod() throws Exception {
        String content = "key=value\0otherkey=othervalue";
        File tempFile = File.createTempFile("test", ".tmp");
        tempFile.deleteOnExit();
        Files.write(tempFile.toPath(), content.getBytes());
        String result = (String) RuntimeEnvironment.class.getDeclaredMethod("readFile", String.class, String.class).invoke(null, tempFile.getPath(), "key");
        assertEquals("value", result);
    }

    //BaseRock generated method id: ${testReadFileMethodWithNonExistentFile}, hash: 4BBCDFD6A9EE5D2C7462E1041C57F3F1
    @Disabled()
    @Test
    void testReadFileMethodWithNonExistentFile() throws Exception {
        String result = (String) RuntimeEnvironment.class.getDeclaredMethod("readFile", String.class, String.class).invoke(null, "/nonexistent/file", "key");
        assertNull(result);
    }

    //BaseRock generated method id: ${testDeprecatedConstructor}, hash: 586A1F38CE15EECEEEDC908188A9898E
    @Test
    void testDeprecatedConstructor() {
        assertDoesNotThrow(() -> new RuntimeEnvironment());
    }
}

package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import java.io.File;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mockStatic;
import org.junit.jupiter.api.Disabled;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class SystemUtilsBaseRockGeneratedTest {

    //BaseRock generated method id: ${getEnvironmentVariableWhenValueIsNull}, hash: 22DEC0A57EF510D64F351C4B449C4832
    @Test()
    void getEnvironmentVariableWhenValueIsNull() {
        /* Branches:
         * (value == null) : true
         */
        //Act Statement(s)
        String result = SystemUtils.getEnvironmentVariable("A", "defaultValue1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("defaultValue1")));
    }

    //BaseRock generated method id: ${getEnvironmentVariableWhenValueIsNotNull}, hash: 457BE3D6044690CDB5884EC24FFC0BEC
    @Disabled()
    @Test()
    void getEnvironmentVariableWhenValueIsNotNull() {
        /* Branches:
         * (value == null) : false
         */
        //Act Statement(s)
        String result = SystemUtils.getEnvironmentVariable("A", "defaultValue1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${getHostNameWhenNotIS_OS_WINDOWS}, hash: 871FD71E8F90CDB7B6B4AE79E377D276
    @Disabled()
    @Test()
    void getHostNameWhenNotIS_OS_WINDOWS() {
        /* Branches:
         * (IS_OS_WINDOWS) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String result = SystemUtils.getHostName();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${getJavaHomeTest}, hash: AD210F4A993229CCDA9D765230B838E8
    @Disabled()
    @Test()
    void getJavaHomeTest() {
        //Act Statement(s)
        File result = SystemUtils.getJavaHome();
        File file = new File("/home/andre/idea/idea-IC-242.21829.142/jbr");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(file)));
    }

    //BaseRock generated method id: ${getJavaIoTmpDirTest}, hash: F9C2458D3A9BCAC0AA760AFA48F7A409
    @Test()
    void getJavaIoTmpDirTest() {
        //Act Statement(s)
        File result = SystemUtils.getJavaIoTmpDir();
        File file = new File("/tmp");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(file)));
    }

    //BaseRock generated method id: ${getUserDirTest}, hash: A6508D79396F2581D2C68F3E8C07C760
    @Disabled()
    @Test()
    void getUserDirTest() {
        //Act Statement(s)
        File result = SystemUtils.getUserDir();
        File file = new File("/home/andre/idea/idea-IC-242.21829.142/bin");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(file)));
    }

    //BaseRock generated method id: ${getUserHomeTest}, hash: 90D21E7B866BA4EB8414588D691E6F42
    @Test()
    void getUserHomeTest() {
        //Act Statement(s)
        File result = SystemUtils.getUserHome();
        File file = new File("/home/andre");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(file)));
    }

    //BaseRock generated method id: ${getUserNameTest}, hash: 31A854EEF8DE470CF73B9EA7F5061AF7
    @Test()
    void getUserNameTest() {
        //Act Statement(s)
        String result = SystemUtils.getUserName();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("andre")));
    }

    //BaseRock generated method id: ${getUserName1Test}, hash: 869B8129D2C160C367CB524324066122
    @Test()
    void getUserName1Test() {
        //Act Statement(s)
        String result = SystemUtils.getUserName("A");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("andre")));
    }

    //BaseRock generated method id: ${isJavaAwtHeadlessTest}, hash: B8F8D98A1927EA690E7EC812FDE2315F
    @Test()
    void isJavaAwtHeadlessTest() {
        //Act Statement(s)
        boolean result = SystemUtils.isJavaAwtHeadless();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isJavaVersionAtLeastWhenJAVA_SPECIFICATION_VERSION_AS_ENUMAtLeastRequiredVersion}, hash: 11AD875E49BC79E26D1E1012B306FD99
    @Test()
    void isJavaVersionAtLeastWhenJAVA_SPECIFICATION_VERSION_AS_ENUMAtLeastRequiredVersion() {
        /* Branches:
         * (JAVA_SPECIFICATION_VERSION_AS_ENUM.atLeast(requiredVersion)) : true
         */
        //Act Statement(s)
        boolean result = SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_0_9);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isJavaVersionAtLeastWhenJAVA_SPECIFICATION_VERSION_AS_ENUMNotAtLeastRequiredVersion}, hash: 9D9D9A0F46C4CFF8D9D64FA7944BE9DC
    @Disabled()
    @Test()
    void isJavaVersionAtLeastWhenJAVA_SPECIFICATION_VERSION_AS_ENUMNotAtLeastRequiredVersion() {
        /* Branches:
         * (JAVA_SPECIFICATION_VERSION_AS_ENUM.atLeast(requiredVersion)) : false
         */
        //Act Statement(s)
        boolean result = SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_0_9);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isJavaVersionAtMostWhenJAVA_SPECIFICATION_VERSION_AS_ENUMAtMostRequiredVersion}, hash: 34E6A59EC275195BCE161E0C96CADF53
    @Disabled()
    @Test()
    void isJavaVersionAtMostWhenJAVA_SPECIFICATION_VERSION_AS_ENUMAtMostRequiredVersion() {
        /* Branches:
         * (JAVA_SPECIFICATION_VERSION_AS_ENUM.atMost(requiredVersion)) : true
         */
        //Act Statement(s)
        boolean result = SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_0_9);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isJavaVersionAtMostWhenJAVA_SPECIFICATION_VERSION_AS_ENUMNotAtMostRequiredVersion}, hash: 287DC64B1EF0C4A9E8B84FEE117DA910
    @Test()
    void isJavaVersionAtMostWhenJAVA_SPECIFICATION_VERSION_AS_ENUMNotAtMostRequiredVersion() {
        /* Branches:
         * (JAVA_SPECIFICATION_VERSION_AS_ENUM.atMost(requiredVersion)) : false
         */
        //Act Statement(s)
        boolean result = SystemUtils.isJavaVersionAtMost(JavaVersion.JAVA_0_9);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isJavaVersionMatchWhenVersionIsNotNull}, hash: CDF9206B611202464C3EFD872A6099C5
    @Test()
    void isJavaVersionMatchWhenVersionIsNotNull() {
        /* Branches:
         * (version == null) : false
         */
        //Act Statement(s)
        boolean result = SystemUtils.isJavaVersionMatch("21", "23");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isOsMatchWhenIsOsVersionMatchOsVersionOsVersionPrefix}, hash: DED285AF24E00ECF4846E8CF9DB65A51
    @Test()
    void isOsMatchWhenIsOsVersionMatchOsVersionOsVersionPrefix() {
        /* Branches:
         * (osName == null) : false
         * (osVersion == null) : false
         * (isOsNameMatch(osName, osNamePrefix)) : true
         * (isOsVersionMatch(osVersion, osVersionPrefix)) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<SystemUtils> systemUtils = mockStatic(SystemUtils.class, CALLS_REAL_METHODS)) {
            systemUtils.when(() -> SystemUtils.isOsNameMatch("Linux", "Mac OS X")).thenReturn(true);
            systemUtils.when(() -> SystemUtils.isOsVersionMatch("6.8.0-51-generic", "15")).thenReturn(true);
            //Act Statement(s)
            boolean result = SystemUtils.isOsMatch("Linux", "6.8.0-51-generic", "Mac OS X", "15");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                systemUtils.verify(() -> SystemUtils.isOsNameMatch("Linux", "Mac OS X"), atLeast(1));
                systemUtils.verify(() -> SystemUtils.isOsVersionMatch("6.8.0-51-generic", "15"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isOsMatchWhenIsOsVersionMatchNotOsVersionOsVersionPrefix}, hash: F31311F320EBAEA5F910A68E8B21128C
    @Test()
    void isOsMatchWhenIsOsVersionMatchNotOsVersionOsVersionPrefix() {
        /* Branches:
         * (osName == null) : false
         * (osVersion == null) : false
         * (isOsNameMatch(osName, osNamePrefix)) : true
         * (isOsVersionMatch(osVersion, osVersionPrefix)) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<SystemUtils> systemUtils = mockStatic(SystemUtils.class, CALLS_REAL_METHODS)) {
            systemUtils.when(() -> SystemUtils.isOsNameMatch("Linux", "Mac OS X")).thenReturn(true);
            systemUtils.when(() -> SystemUtils.isOsVersionMatch("6.8.0-51-generic", "15")).thenReturn(false);
            //Act Statement(s)
            boolean result = SystemUtils.isOsMatch("Linux", "6.8.0-51-generic", "Mac OS X", "15");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                systemUtils.verify(() -> SystemUtils.isOsNameMatch("Linux", "Mac OS X"), atLeast(1));
                systemUtils.verify(() -> SystemUtils.isOsVersionMatch("6.8.0-51-generic", "15"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isOsNameMatchWhenOsNameIsNotNull}, hash: 7A99430C041A5AD48C22FD002375BAEF
    @Test()
    void isOsNameMatchWhenOsNameIsNotNull() {
        /* Branches:
         * (osName == null) : false
         */
        //Act Statement(s)
        boolean result = SystemUtils.isOsNameMatch("Linux", "z/OS");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isOsVersionMatchWhenStringUtilsIsEmptyOsVersion}, hash: 955D107B37459E548EED62E6D6AAC890
    @Test()
    void isOsVersionMatchWhenStringUtilsIsEmptyOsVersion() {
        /* Branches:
         * (StringUtils.isEmpty(osVersion)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        boolean result = SystemUtils.isOsVersionMatch("osVersion1", "osVersionPrefix1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isOsVersionMatchWhenIIndexOfVersionPrefixPartsNotEqualsIIndexOfVersionParts}, hash: 1AE9F992C6927EF19B2F7F0E6B80BCB6
    @Test()
    void isOsVersionMatchWhenIIndexOfVersionPrefixPartsNotEqualsIIndexOfVersionParts() {
        /* Branches:
         * (StringUtils.isEmpty(osVersion)) : false
         * (i < Math.min(versionPrefixParts.length, versionParts.length)) : true
         * (!versionPrefixParts[i].equals(versionParts[i])) : true
         */
        //Arrange Statement(s)
        try (MockedStatic<JavaVersion> javaVersion = mockStatic(JavaVersion.class, CALLS_REAL_METHODS)) {
            String[] stringArray = new String[] { "B" };
            javaVersion.when(() -> JavaVersion.split("C")).thenReturn(stringArray);
            String[] stringArray2 = new String[] { "D" };
            javaVersion.when(() -> JavaVersion.split("A")).thenReturn(stringArray2);
            //Act Statement(s)
            boolean result = SystemUtils.isOsVersionMatch("A", "C");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                javaVersion.verify(() -> JavaVersion.split("C"), atLeast(1));
                javaVersion.verify(() -> JavaVersion.split("A"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isOsVersionMatchWhenIIndexOfVersionPrefixPartsEqualsIIndexOfVersionParts}, hash: 8108B6007DC4537FDA9FE60C29206AC6
    @Test()
    void isOsVersionMatchWhenIIndexOfVersionPrefixPartsEqualsIIndexOfVersionParts() {
        /* Branches:
         * (StringUtils.isEmpty(osVersion)) : false
         * (i < Math.min(versionPrefixParts.length, versionParts.length)) : true
         * (!versionPrefixParts[i].equals(versionParts[i])) : false
         */
        //Arrange Statement(s)
        try (MockedStatic<JavaVersion> javaVersion = mockStatic(JavaVersion.class, CALLS_REAL_METHODS)) {
            String[] stringArray = new String[] { "A" };
            javaVersion.when(() -> JavaVersion.split("osVersionPrefix1")).thenReturn(stringArray);
            String[] stringArray2 = new String[] { "A" };
            javaVersion.when(() -> JavaVersion.split("osVersion1")).thenReturn(stringArray2);
            //Act Statement(s)
            boolean result = SystemUtils.isOsVersionMatch("osVersion1", "osVersionPrefix1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                javaVersion.verify(() -> JavaVersion.split("osVersionPrefix1"), atLeast(1));
                javaVersion.verify(() -> JavaVersion.split("osVersion1"), atLeast(1));
            });
        }
    }
}

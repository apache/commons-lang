package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mockStatic;
import org.junit.jupiter.api.Disabled;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class ClassPathUtilsBaseRockGeneratedTest {

    //BaseRock generated method id: ${packageToPathTest}, hash: EFD9242C76C707C8A8CEC3C578B48A22
    @Test()
    void packageToPathTest() {
        //Act Statement(s)
        String result = ClassPathUtils.packageToPath("/");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("/")));
    }

    //BaseRock generated method id: ${pathToPackageTest}, hash: 94B55B78AD1CB7F40DE5BD1E2293B632
    @Test()
    void pathToPackageTest() {
        //Act Statement(s)
        String result = ClassPathUtils.pathToPackage(".");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(".")));
    }

    //BaseRock generated method id: ${toFullyQualifiedNameTest}, hash: 68696CC91F555AA0E4BDCD6B0120AA6D
    @Test()
    void toFullyQualifiedNameTest() {
        //Arrange Statement(s)
        try (MockedStatic<ClassPathUtils> classPathUtils = mockStatic(ClassPathUtils.class, CALLS_REAL_METHODS)) {
            classPathUtils.when(() -> ClassPathUtils.toFullyQualifiedName((Package) any(), eq("resourceName1"))).thenReturn("return_of_toFullyQualifiedName1");
            //Act Statement(s)
            String result = ClassPathUtils.toFullyQualifiedName(Object.class, "resourceName1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_toFullyQualifiedName1"));
                classPathUtils.verify(() -> ClassPathUtils.toFullyQualifiedName((Package) any(), eq("resourceName1")), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toFullyQualifiedName1Test}, hash: 9840BE4C82EF5642D413B8230C9D6271
    @Disabled()
    @Test()
    void toFullyQualifiedName1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Package _package = Package.getPackage("name1");
        //Act Statement(s)
        String result = ClassPathUtils.toFullyQualifiedName(_package, "");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(".")));
    }

    //BaseRock generated method id: ${toFullyQualifiedPathTest}, hash: 5E59C0A5CEDB807C5060C5E6733F20F7
    @Test()
    void toFullyQualifiedPathTest() {
        //Arrange Statement(s)
        try (MockedStatic<ClassPathUtils> classPathUtils = mockStatic(ClassPathUtils.class, CALLS_REAL_METHODS)) {
            classPathUtils.when(() -> ClassPathUtils.toFullyQualifiedPath((Package) any(), eq("resourceName1"))).thenReturn("return_of_toFullyQualifiedPath1");
            //Act Statement(s)
            String result = ClassPathUtils.toFullyQualifiedPath(Object.class, "resourceName1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_toFullyQualifiedPath1"));
                classPathUtils.verify(() -> ClassPathUtils.toFullyQualifiedPath((Package) any(), eq("resourceName1")), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${toFullyQualifiedPath1Test}, hash: 18E5D7E92B0B9881B9F4E67073E3C304
    @Disabled()
    @Test()
    void toFullyQualifiedPath1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        Package _package = Package.getPackage("name1");
        //Act Statement(s)
        String result = ClassPathUtils.toFullyQualifiedPath(_package, "A");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("//A")));
    }
}

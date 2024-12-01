/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang3;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.DynamicTest.dynamicTest;

/**
 * Tests {@link RuntimeEnvironment}.
 */
public class RuntimeEnvironmentTest {

    private static final String simpleEnviron = "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin\u0000" +
            "HOSTNAME=d62718b69f37\u0000TERM=xterm\u0000HOME=/root\u0000";

    private static final String podmanEnviron = "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin\u0000" +
            "HOSTNAME=d62718b69f37\u0000TERM=xterm\u0000container=podman\u0000HOME=/root\u0000";

    @TempDir
    private Path tempDir;

    @AfterAll
    public static void tearDown() {
        RuntimeEnvironment.rootDir = "/";
    }

    @TestFactory
    public DynamicTest[] testIsContainerDocker() {
        return new DynamicTest[]{
                dynamicTest("in docker no file", () ->
                        assertFalse(doTestInContainer(simpleEnviron, null))),

                dynamicTest("in docker with file", () ->
                        assertTrue(doTestInContainer(simpleEnviron, ".dockerenv"))),

                dynamicTest("in podman no file", () ->
                        assertTrue(doTestInContainer(podmanEnviron, null))),

                dynamicTest("in podman with file", () ->
                        assertTrue(doTestInContainer(simpleEnviron, "run/.containerenv"))),

                dynamicTest("not in container", () ->
                        assertFalse(doTestInContainer(simpleEnviron, null))),

                dynamicTest("pid1 error no file", () ->
                        assertFalse(doTestInContainer(null, null))),

                dynamicTest("pid1 error docker file", () ->
                        assertTrue(doTestInContainer(null, ".dockerenv"))),

                dynamicTest("pid1 error podman file", () ->
                        assertTrue(doTestInContainer(null, ".dockerenv"))),
        };
    }

    private boolean doTestInContainer(String environ, String fileToCreate) throws IOException {
        Path testDir = tempDir.resolve(UUID.randomUUID().toString());
        RuntimeEnvironment.rootDir = testDir + "/";
        Path pid1EnvironFile = testDir.resolve("proc/1/environ");
        Files.createDirectories(pid1EnvironFile.getParent());

        if (fileToCreate != null) {
            Path file = testDir.resolve(fileToCreate);
            Files.createDirectories(file.getParent());
            Files.createFile(file);
        }

        if (environ != null) {
            Files.write(pid1EnvironFile, environ.getBytes(StandardCharsets.UTF_8));
        }

        return RuntimeEnvironment.inContainer();
    }
}

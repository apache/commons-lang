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
import java.io.InputStream;
import java.net.URL;
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
                        assertFalse(doTestInContainer("RuntimeEnvironmentTest.docker.txt", null))),

                dynamicTest("in docker with file", () ->
                        assertTrue(doTestInContainer("RuntimeEnvironmentTest.docker.txt", ".dockerenv"))),

                dynamicTest("in podman no file", () ->
                        assertTrue(doTestInContainer("RuntimeEnvironmentTest.podman.txt", null))),

                dynamicTest("in podman with file", () ->
                        assertTrue(doTestInContainer("RuntimeEnvironmentTest.none.txt", "run/.containerenv"))),

                dynamicTest("not in container", () ->
                        assertFalse(doTestInContainer("RuntimeEnvironmentTest.none.txt", null))),

                dynamicTest("pid1 error no file", () ->
                        assertFalse(doTestInContainer(null, null))),

                dynamicTest("pid1 error docker file", () ->
                        assertTrue(doTestInContainer(null, ".dockerenv"))),

                dynamicTest("pid1 error podman file", () ->
                        assertTrue(doTestInContainer(null, ".dockerenv"))),
        };
    }

    private boolean doTestInContainer(String envFile, String fileToCreate) throws IOException {
        Path testDir = tempDir.resolve(UUID.randomUUID().toString());
        RuntimeEnvironment.rootDir = testDir + "/";
        Path pid1EnvironFile = testDir.resolve("proc/1/environ");
        Files.createDirectories(pid1EnvironFile.getParent());

        if (fileToCreate != null) {
            Path file = testDir.resolve(fileToCreate);
            Files.createDirectories(file.getParent());
            Files.createFile(file);
        }

        if (envFile != null) {
            URL resource = RuntimeEnvironmentTest.class.getResource(envFile);
            try (InputStream in = resource.openStream()) {
                Files.copy(in, pid1EnvironFile);
            }
        }

        return RuntimeEnvironment.inContainer();
    }
}

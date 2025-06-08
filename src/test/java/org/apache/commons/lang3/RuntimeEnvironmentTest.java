/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang3;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.UUID;

import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Tests {@link RuntimeEnvironment}.
 */
class RuntimeEnvironmentTest {

    private static final String simpleEnviron = "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin\u0000" +
            "HOSTNAME=d62718b69f37\u0000TERM=xterm\u0000HOME=/root\u0000";

    private static final String podmanEnviron = "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin\u0000" +
            "HOSTNAME=d62718b69f37\u0000TERM=xterm\u0000container=podman\u0000HOME=/root\u0000";

    private static final String emptyContainer = "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin\u0000" +
            "HOSTNAME=d62718b69f37\u0000TERM=xterm\u0000container=\u0000HOME=/root\u0000";

    private static Arguments[] testIsContainer() {
        return new Arguments[]{
                Arguments.of("in docker no file", simpleEnviron, null, false),
                Arguments.of("in docker with file", simpleEnviron, ".dockerenv", true),
                Arguments.of("in podman no file", podmanEnviron, "run/.containerenv", true),
                Arguments.of("in podman with file", simpleEnviron, "run/.containerenv", true),
                Arguments.of("in podman empty env var no file", emptyContainer, null, false),
                Arguments.of("in podman empty env var with file", emptyContainer, "run/.containerenv", false),
                Arguments.of("not in container", simpleEnviron, null, false),
                Arguments.of("pid1 error no file", null, null, false),
                Arguments.of("pid1 error docker file", null, ".dockerenv", true),
                Arguments.of("pid1 error podman file", null, ".dockerenv", true),
        };
    }

    @TempDir
    private Path tempDir;

    private boolean doTestInContainer(final String environ, final String fileToCreate) throws IOException {
        final Path testDir = tempDir.resolve(UUID.randomUUID().toString());
        final Path pid1EnvironFile = testDir.resolve("proc/1/environ");
        Files.createDirectories(pid1EnvironFile.getParent());
        if (fileToCreate != null) {
            final Path file = testDir.resolve(fileToCreate);
            Files.createDirectories(file.getParent());
            Files.createFile(file);
        }
        if (environ != null) {
            Files.write(pid1EnvironFile, environ.getBytes(Charset.defaultCharset()));
        }
        return RuntimeEnvironment.inContainer(testDir.toString());
    }

    @ParameterizedTest
    @MethodSource
    void testIsContainer(final String label, final String environ, final String fileToCreate, final boolean expected) throws IOException {
        assertEquals(expected, doTestInContainer(environ, fileToCreate), label);
    }
}

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

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;

/**
 * Helps query the runtime environment.
 *
 * @since 3.15.0
 */
public class RuntimeEnvironment {

    private static boolean fileExists(final String path) {
        return Files.exists(Paths.get(path));
    }

    /**
     * Tests whether we are running in a container like Docker or Podman.
     * <p>
     * <em>The following may change if we find better detection logic.</em>
     * </p>
     * <p>
     * We roughly follow the logic in SystemD:
     * </p>
     * <p>
     * <a href=
     * "https://github.com/systemd/systemd/blob/0747e3b60eb4496ee122066c844210ce818d76d9/src/basic/virt.c#L692">https://github.com/systemd/systemd/blob/0747e3b60eb4496ee122066c844210ce818d76d9/src/basic/virt.c#L692</a>
     * </p>
     * <p>
     * We check the `container` environment variable of process 1:
     * </p>
     * <ol>
     * <li>If the variable is empty, we return false. This includes the case, where the container developer wants to hide the fact that the application runs in
     * a container.</li>
     * <li>If the variable is not empty, we return true.</li>
     * <li>If the variable is absent, we continue.</li>
     * <li>We check files in the container. According to SystemD:/
     * <ol>
     * <li>/.dockerenv is used by Docker.</li>
     * <li>/run/.containerenv is used by PodMan.</li>
     * </ol>
     * </li>
     * </ol>
     *
     * @return whether we are running in a container like Docker or Podman. Never null.
     * @see <a href="https://github.com/systemd/systemd/blob/0747e3b60eb4496ee122066c844210ce818d76d9/src/basic/virt.c#L692">SystemD virt.c</a>
     */
    public static Boolean inContainer() {
        return inContainer(StringUtils.EMPTY);
    }

    /**
     * Tests whether we are running in a container like Docker or Podman.
     * <p>
     * <em>The following may change if we find better detection logic.</em>
     * </p>
     * <p>
     * We roughly follow the logic in SystemD:
     * </p>
     * <p>
     * <a href=
     * "https://github.com/systemd/systemd/blob/0747e3b60eb4496ee122066c844210ce818d76d9/src/basic/virt.c#L692">https://github.com/systemd/systemd/blob/0747e3b60eb4496ee122066c844210ce818d76d9/src/basic/virt.c#L692</a>
     * </p>
     * <p>
     * We check the `container` environment variable of process 1:
     * </p>
     * <ol>
     * <li>If the variable is empty, we return false. This includes the case, where the container developer wants to hide the fact that the application runs in
     * a container.</li>
     * <li>If the variable is not empty, we return true.</li>
     * <li>If the variable is absent, we continue.</li>
     * <li>We check files in the container. According to SystemD:/
     * <ol>
     * <li>/.dockerenv is used by Docker.</li>
     * <li>/run/.containerenv is used by PodMan.</li>
     * </ol>
     * </li>
     * </ol>
     *
     * @return Whether we are running in a container like Docker or Podman.
     * @see <a href="https://github.com/systemd/systemd/blob/0747e3b60eb4496ee122066c844210ce818d76d9/src/basic/virt.c#L692">SystemD virt.c</a>
     */
    static boolean inContainer(final String dirPrefix) {
        final String value = readFile(dirPrefix + "/proc/1/environ", "container");
        if (value != null) {
            return !value.isEmpty();
        }
        return fileExists(dirPrefix + "/.dockerenv") || fileExists(dirPrefix + "/run/.containerenv");
    }

    /**
     * Tests whether the {@code /proc/N/environ} file at the given path string contains a specific line prefix.
     *
     * @param envVarFile The path to a /proc/N/environ file.
     * @param key        The env var key to find.
     * @return value The env var value or null.
     */
    private static String readFile(final String envVarFile, final String key) {
        try {
            final byte[] bytes = Files.readAllBytes(Paths.get(envVarFile));
            final String content = new String(bytes, Charset.defaultCharset());
            // Split by null byte character
            final String[] lines = content.split(String.valueOf(CharUtils.NUL));
            final String prefix = key + "=";
            // @formatter:off
            return Arrays.stream(lines)
                    .filter(line -> line.startsWith(prefix))
                    .map(line -> line.split("=", 2))
                    .map(keyValue -> keyValue[1])
                    .findFirst()
                    .orElse(null);
            // @formatter:on
        } catch (final IOException e) {
            return null;
        }
    }

    /**
     * Constructs a new instance.
     *
     * @deprecated Will be removed in 4.0.0.
     */
    @Deprecated
    public RuntimeEnvironment() {
        // empty
    }
}

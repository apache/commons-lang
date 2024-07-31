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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.stream.Stream;

/**
 * Helps query the runtime environment.
 *
 * @since 3.15.0
 */
public class RuntimeEnvironment {

    /**
     * Tests whether the file at the given path string contains a specific line.
     *
     * @param path The path to a file.
     * @param line The line to find.
     * @return whether the file at the given path string contains a specific line.
     */
    private static Boolean containsLine(final String path, final String line) {
        try (Stream<String> stream = Files.lines(Paths.get(path))) {
            return stream.anyMatch(test -> test.contains(line));
        } catch (final IOException e) {
            return false;
        }
    }

    /**
     * Tests whether we are running in a container like Docker or Podman.
     *
     * @return whether we are running in a container like Docker or Podman.
     */
    public static Boolean inContainer() {
        return inDocker() || inPodman();
    }

    /**
     * Tests whether we are running in a Docker container.
     * <p>
     * Package-private for testing.
     * </p>
     *
     * @return whether we are running in a Docker container.
     */
    // Could be public at a later time.
    static Boolean inDocker() {
        return containsLine("/proc/1/cgroup", "/docker");
    }

    /**
     * Tests whether we are running in a Podman container.
     * <p>
     * Package-private for testing.
     * </p>
     *
     * @return whether we are running in a Podman container.
     */
    // Could be public at a later time.
    static Boolean inPodman() {
        return containsLine("/proc/1/environ", "container=podman");
    }

    /**
     * Tests whether we are running in a Windows Subsystem for Linux (WSL).
     * <p>
     * Package-private for testing.
     * </p>
     *
     * @return whether we are running in a Windows Subsystem for Linux (WSL).
     */
    // Could be public at a later time.
    static Boolean inWsl() {
        return containsLine("/proc/1/environ", "container=wslcontainer_host_id");
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

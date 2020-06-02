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
package org.apache.commons.lang3.function;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.function.BiConsumer;

public class CheckedBiConsumerTest {

    @DisplayName("an unchecked BiConsumer should throw an exception")
    @Test
    void uncheckedConsumerExceptionRethrowingTest() {
        //given
        Path invalidSourcePath = Paths.get("sss 3ees");
        Path invalidDestinationPath = Paths.get("fdfd fefd");
        BiConsumer<Path, Path> uncheckedBiConsumer = CheckedBiConsumer.unchecked(this::copy);

        //then
        Assertions.assertThrows(RuntimeException.class, () -> uncheckedBiConsumer.accept(invalidSourcePath, invalidDestinationPath));
    }

    private void copy(Path src, Path dest) throws IOException {
        Files.copy(src, dest);
    }
}

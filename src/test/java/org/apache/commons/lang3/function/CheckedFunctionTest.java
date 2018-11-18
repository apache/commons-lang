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

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileTime;
import java.util.function.Function;


public class CheckedFunctionTest {

    @DisplayName("the method unchecked() should rethrown an exception")
    @Test
    void uncheckedFunctionExceptionRethrowingTest() {
        //given
        Path notExistingFile = Paths.get("not existing file");
        Function<Path, FileTime> function = CheckedFunction.unchecked(Files::getLastModifiedTime);

        //then
        Assertions.assertThrows(RuntimeException.class, () -> function.apply(notExistingFile));
    }



}

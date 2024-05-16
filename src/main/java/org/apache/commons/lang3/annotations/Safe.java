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
package org.apache.commons.lang3.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * This annotation is used to indicate, that a variable, field, or parameter
 * contains a safe value. If so, the annotated element may be used in an
 * invocation of a constructor, or method, which is annotated with
 * {@code @Trusted}.
 *
 * For example, suggest the following method declaration:
 * <pre>
 *   {@literal @Insecure}
 *   public void runCommand(String pCmdLine) {
 *   }
 * </pre>
 *
 * Based on the example, this piece of source code would be invalid:
 * <pre>{@code
 *   String cmdLine = "echo" + " " + "okay";
 *   // It is unknown, whether the {@code cmdLine} variable contains a safe value.
 *   // Thus, the following should be considered dangerous:
 *   runCommand(cmdLine);
 * }</pre>
 *
 * In the following example, however, the value of {@code cmdLine} is
 * supposed to be safe, so it may be used when invoking the {@code runCommand}
 * method.
 * <pre>
 *   {@literal @Safe} String cmdLine = "echo" + " " + "okay";
 *   // It is unknown, whether the {@code cmdLine} variable contains a safe value.
 *   // Thus, the following should be considered dangerous:
 *   runCommand(cmdLine);
 * </pre>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.LOCAL_VARIABLE, ElementType.FIELD, ElementType.PARAMETER})
@Documented
public @interface Safe {

}

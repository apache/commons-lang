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
 * This annotation is used to indicate, that a constructor, or method
 * is insecure to use, unless the input parameters contain safe ("trusted")
 * values.
 *
 * For example, consider a method like <pre>
 *   {@literal @Insecure}
 *   public void runCommand(String pCmdLine) {
 *   }
 * </pre>
 *
 * The example method would invoke {@code /bin/sh} (Linux, Unix, or MacOS), or
 * {@code cmd} (Windows) to run an external command, as given by the parameter
 * {@code pCmdLine}. Obviously, depending on the value of the parameter,
 * this can be dangerous, unless the API user (downstream developer)
 * <em>knows</em>, that the parameter value is safe (for example, because it
 * is hard coded, or because it has been compared to a white list of
 * permissible values).
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.CONSTRUCTOR, ElementType.METHOD})
@Documented
public @interface Insecure {
}

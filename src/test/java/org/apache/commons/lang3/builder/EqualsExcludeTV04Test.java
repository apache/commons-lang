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

package org.apache.commons.lang3.builder;
import static org.junit.jupiter.api.Assertions.*;
import java.lang.reflect.Field;
import org.junit.jupiter.api.Test;
class EqualsExcludeTV04Test {
 static class Model { @EqualsExclude private int ignored; private int included; }
 @Test void annotationIsRuntimeVisibleOnFields() throws Exception { Field f=Model.class.getDeclaredField("ignored"); assertTrue(f.isAnnotationPresent(EqualsExclude.class)); }
 @Test void reflectionEqualsIgnoresAnnotatedField() { Model a=new Model(),b=new Model(); a.ignored=1;b.ignored=9;a.included=b.included=5; assertTrue(EqualsBuilder.reflectionEquals(a,b)); }
}

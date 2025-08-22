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

package org.apache.commons.lang3.reflect.testbed;

/**
 * This class is designed to test the default access JVM problem workaround. The issue is that public methods of a public subclass contained in a default access
 * superclass are returned by reflection but an IllegalAccessException is thrown when they are invoked.
 * <p>
 * This is the default access superclass.
 * </p>
 */
public class PublicSubBeanOtherPackage extends PackageBeanOtherPackage {

    /**
     * A directly implemented property.
     */
    private String foo = "This is foo";

    /**
     * Package private constructor, can only use factory method to create beans.
     */
    public PublicSubBeanOtherPackage() {
    }

    public String getFoo() {
        return this.foo;
    }

    public void setFoo(final String foo) {
        this.foo = foo;
    }
}

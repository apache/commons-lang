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

/**
 * Apache Commons Lang provides utility classes for the core of Java.
 */
module org.apache.commons.lang3 {

    // see AbstractCircuitBreaker
    requires static java.desktop;

    // all packages are exported
    exports org.apache.commons.lang3;
    exports org.apache.commons.lang3.arch;
    exports org.apache.commons.lang3.builder;
    exports org.apache.commons.lang3.concurrent;
    exports org.apache.commons.lang3.event;
    exports org.apache.commons.lang3.exception;
    exports org.apache.commons.lang3.math;
    exports org.apache.commons.lang3.mutable;
    exports org.apache.commons.lang3.reflect;
    exports org.apache.commons.lang3.text;
    exports org.apache.commons.lang3.text.translate;
    exports org.apache.commons.lang3.time;
    exports org.apache.commons.lang3.tuple;

}

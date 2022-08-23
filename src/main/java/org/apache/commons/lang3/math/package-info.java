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
 * Extends {@link java.math} for business mathematical classes.
 * This package is intended for business mathematical use, not scientific use.
 * See <a href="https://commons.apache.org/proper/commons-math/">Commons Math</a> for a more complete set of mathematical classes.
 * These classes are immutable, and therefore thread-safe.
 *
 * <p>Although Commons Math also exists, some basic mathematical functions are contained within Lang.
 * These include classes to a {@link org.apache.commons.lang3.math.Fraction} class, various utilities for random numbers, and the flagship class, {@link org.apache.commons.lang3.math.NumberUtils} which contains a handful of classic number functions.</p>
 *
 * <p>There are two aspects of this package that should be highlighted.
 * The first is {@link org.apache.commons.lang3.math.NumberUtils#createNumber(String)}, a method which does its best to convert a String into a {@link java.lang.Number} object.
 * You have no idea what type of Number it will return, so you should call the relevant {@code xxxValue} method when you reach the point of needing a number.
 * NumberUtils also has a related {@link org.apache.commons.lang3.math.NumberUtils#isCreatable(String)} method.</p>
 *
 * @since 2.0
 */
package org.apache.commons.lang3.math;

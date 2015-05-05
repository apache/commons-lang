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
 * <p>Provides highly reusable static utility methods, chiefly concerned with adding value to the {@link java.lang} classes.
 * Most of these classes are immutable and thus thread-safe.
 * However {@link org.apache.commons.lang3.CharSet} is not currently guaranteed thread-safe under all circumstances.</p>
 *
 * <p>The top level package contains various Utils classes, whilst there are various subpackages including {@link org.apache.commons.lang3.math}, {@link org.apache.commons.lang3.concurrent} and {@link org.apache.commons.lang3.builder}.
 * Using the Utils classes is generally simplicity itself.
 * They are the equivalent of global functions in another language, a collection of stand-alone, thread-safe, static methods.
 * In contrast, subpackages may contain interfaces which may have to be implemented or classes which may need to be extended to get the full functionality from the code.
 * They may, however, contain more global-like functions.</p>
 *
 * <p>Lang 3.0 requires JDK 1.5+, since Lang 3.2 it requires JDK 6+; The legacy release 2.6 requires JDK 1.2+.
 * In both cases you can find features of later JDKs being maintained by us and likely to be removed or modified in favour of the JDK in the next major version.
 * Note that Lang 3.0 uses a different package than its predecessors, allowing it to be used at the same time as an earlier version.</p>
 *
 * <p>You will find deprecated methods as you stroll through the Lang documentation. These are removed in the next major version. </p>
 *
 * <p>All util classes contain empty public constructors with warnings not to use.
 * This may seem an odd thing to do, but it allows tools like Velocity to access the class as if it were a bean.
 * In other words, yes we know about private constructors and have chosen not to use them.</p>
 *
 * <h3>String manipulation - StringUtils, StringEscapeUtils, RandomStringUtils</h3>
 *
 * <p>Lang has a series of String utilities.
 * The first is {@link org.apache.commons.lang3.StringUtils}, oodles and oodles of functions which tweak, transform, squeeze and cuddle {@link java.lang.String java.lang.Strings}.
 * In addition to StringUtils, there are a series of other String manipulating classes; {@link org.apache.commons.lang3.RandomStringUtils} and {@link org.apache.commons.lang3.StringEscapeUtils StringEscapeUtils}.
 * RandomStringUtils speaks for itself.
 * It's provides ways in which to generate pieces of text, such as might be used for default passwords.
 * StringEscapeUtils contains methods to escape and unescape Java, JavaScript, HTML, XML and SQL.</p>
 *
 * <p>These are ideal classes to start using if you're looking to get into Lang.
 * StringUtils' {@link org.apache.commons.lang3.StringUtils#capitalize(String)}, {@link org.apache.commons.lang3.StringUtils#substringBetween(String, String)}/{@link org.apache.commons.lang3.StringUtils#substringBefore(String, String) Before}/{@link org.apache.commons.lang3.StringUtils#substringAfter(String, String) After}, {@link org.apache.commons.lang3.StringUtils#split(String)} and {@link org.apache.commons.lang3.StringUtils#join(Object[])} are good methods to begin with.
 * If you use java.sql.Statements a lot, StringEscapeUtils.escapeSql might be of interest.</p>
 *
 * <h3>Character handling - CharSetUtils, CharSet, CharRange, CharUtils</h3>
 *
 * <p>In addition to dealing with Strings, it's also important to deal with chars and Characters.
 * {@link org.apache.commons.lang3.CharUtils} exists for this purpose, while {@link org.apache.commons.lang3.CharSetUtils} exists for set-manipulation of Strings.
 * Be careful, although CharSetUtils takes an argument of type String, it is only as a set of characters.
 * For example, <code>CharSetUtils.delete("testtest", "tr")</code> will remove all t's and all r's from the String, not just the String "tr". </p>
 *
 * <p>{@link org.apache.commons.lang3.CharRange} and {@link org.apache.commons.lang3.CharSet} are both used internally by CharSetUtils, and will probably rarely be used.</p>
 *
 * <h3>JVM interaction - SystemUtils, CharEncoding</h3>
 *
 * <p>SystemUtils is a simple little class which makes it easy to find out information about which platform you are on.
 * For some, this is a necessary evil. It was never something I expected to use myself until I was trying to ensure that Commons Lang itself compiled under JDK 1.2.
 * Having pushed out a few JDK 1.3 bits that had slipped in (<code>Collections.EMPTY_MAP</code> is a classic offender), I then found that one of the Unit Tests was dying mysteriously under JDK 1.2, but ran fine under JDK 1.3.
 * There was no obvious solution and I needed to move onwards, so the simple solution was to wrap that particular test in a <code>if(SystemUtils.isJavaVersionAtLeast(1.3f)) {</code>, make a note and move on.</p>
 *
 * <p>The {@link org.apache.commons.lang3.CharEncoding} class is also used to interact with the Java environment and may be used to see which character encodings are supported in a particular environment. </p>
 *
 * <h3>Serialization - SerializationUtils, SerializationException</h3>
 *
 * <p>Serialization doesn't have to be that hard!
 * A simple util class can take away the pain, plus it provides a method to clone an object by unserializing and reserializing, an old Java trick.</p>
 *
 * <h3>Assorted functions - ObjectUtils, ClassUtils, ArrayUtils, BooleanUtils</h3>
 *
 * <p>Would you believe it, {@link org.apache.commons.lang3.ObjectUtils} contains handy functions for Objects, mainly null-safe implementations of the methods on {@link java.lang.Object}.</p>
 *
 * <p>{@link org.apache.commons.lang3.ClassUtils} is largely a set of helper methods for reflection.
 * Of special note are the comparators hidden away in ClassUtils, useful for sorting Class and Package objects by name; however they merely sort alphabetically and don't understand the common habit of sorting <code>java</code> and <code>javax</code> first.</p>
 *
 * <p>Next up, {@link org.apache.commons.lang3.ArrayUtils}.
 * This is a big one with many methods and many overloads of these methods so it is probably worth an in depth look here.
 * Before we begin, assume that every method mentioned is overloaded for all the primitives and for Object.
 * Also, the short-hand 'xxx' implies a generic primitive type, but usually also includes Object. </p>
 *
 * <ul>
 *  <li>ArrayUtils provides singleton empty arrays for all the basic types. These will largely be of use in the Collections API with its toArray methods, but also will be of use with methods which want to return an empty array on error.</li>
 *  <li><code>add(xxx[], xxx)</code> will add a primitive type to an array, resizing the array as you'd expect. Object is also supported. </li>
 *  <li><code>clone(xxx[])</code> clones a primitive or Object array. </li>
 *  <li><code>contains(xxx[], xxx)</code> searches for a primitive or Object in a primitive or Object array. </li>
 *  <li><code>getLength(Object)</code> returns the length of any array or an IllegalArgumentException if the parameter is not an array. <code>hashCode(Object)</code>, <code>equals(Object, Object)</code>, <code>toString(Object)</code> </li>
 *  <li><code>indexOf(xxx[], xxx)</code> and <code>indexOf(xxx[], xxx, int)</code> are copies of the classic String methods, but this time for primitive/Object arrays. In addition, a lastIndexOf set of methods exists. </li>
 *  <li><code>isEmpty(xxx[])</code> lets you know if an array is zero-sized or null. </li>
 *  <li><code>isSameLength(xxx[], xxx[])</code> returns true if the arrays are the same length. </li>
 *  <li>Along side the add methods, there are also remove methods of two types. The first type remove the value at an index, <code>remove(xxx[], int)</code>, while the second type remove the first value from the array, <code>remove(xxx[], xxx)</code>. </li>
 *  <li>Nearing the end now. The <code>reverse(xxx[])</code> method turns an array around. </li>
 *  <li>The <code>subarray(xxx[], int, int)</code> method splices an array out of a larger array. </li>
 *  <li>Primitive to primitive wrapper conversion is handled by the <code>toObject(xxx[])</code> and <code>toPrimitive(Xxx[])</code> methods. </li>
 * </ul>
 *
 * <p>Lastly, {@link org.apache.commons.lang3.ArrayUtils#toMap(Object[])} is worthy of special note.
 * It is not a heavily overloaded method for working with arrays, but a simple way to create Maps from literals. </p>
 *
 * <h4>Using toMap</h4>
 * <pre>
 * <code>
 * Map colorMap = ArrayUtils.toMap(new String[][] {{
 *   {"RED", "#FF0000"},
 *   {"GREEN", "#00FF00"},
 *   {"BLUE", "#0000FF"}
 * });
 * </code>
 * </pre>
 *
 * <p>Our final util class is {@link org.apache.commons.lang3.BooleanUtils}.
 * It contains various Boolean acting methods, probably of most interest is the {@link org.apache.commons.lang3.BooleanUtils#toBoolean(String)} method which turns various positive/negative Strings into a Boolean object, and not just true/false as with Boolean.valueOf.</p>
 *
 * <h3>Flotsam - BitField, Validate</h3>
 * <p>On reaching the end of our package, we are left with a couple of classes that haven't fit any of the topics so far. </p>
 * <p>The {@link org.apache.commons.lang3.BitField} class provides a wrapper class around the classic bitmask integer, whilst the {@link org.apache.commons.lang3.Validate} class may be used for assertions (remember, we support Java 1.2). </p>
 *
 * @since 1.0
 */
package org.apache.commons.lang3;

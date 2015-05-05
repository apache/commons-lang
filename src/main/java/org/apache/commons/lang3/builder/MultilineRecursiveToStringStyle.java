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

package org.apache.commons.lang3.builder;

import org.apache.commons.lang3.ClassUtils;
import org.apache.commons.lang3.SystemUtils;

/**
 * <p>Works with {@link ToStringBuilder} to create a "deep" <code>toString</code>.
 * But instead a single line like the {@link RecursiveToStringStyle} this creates a multiline String 
 * similar to the {@link ToStringStyle#MULTI_LINE_STYLE}.</p>
 * 
 * <p>To use this class write code as follows:</p>
 *
 * <pre>
 * public class Job {
 *   String title;
 *   ...
 * }
 * 
 * public class Person {
 *   String name;
 *   int age;
 *   boolean smoker;
 *   Job job;
 * 
 *   ...
 * 
 *   public String toString() {
 *     return new ReflectionToStringBuilder(this, new MultilineRecursiveToStringStyle()).toString();
 *   }
 * }
 * </pre>
 *
 * <p>
 * This will produce a toString of the format:<br>
 * <code>Person@7f54[ <br>
 * &nbsp; name=Stephen, <br>
 * &nbsp; age=29, <br>
 * &nbsp; smoker=false, <br>
 * &nbsp; job=Job@43cd2[ <br>
 * &nbsp; &nbsp; title=Manager <br>
 * &nbsp;  ] <br>
 * ]
 * </code>
 * </p>
 * 
 * @since 3.4
 */
public class MultilineRecursiveToStringStyle extends RecursiveToStringStyle {

    /**
     * Required for serialization support.
     * @see java.io.Serializable
     */
    private static final long serialVersionUID = 1L;

    /** Indenting of inner lines. */
    private int indent = 2;

    /** Current indenting. */
    private int spaces = 2;

    /**
     * Constructor.
     */
    public MultilineRecursiveToStringStyle() {
        super();
        resetIndent();
    }

    /**
     * Resets the fields responsible for the line breaks and indenting. 
     * Must be invoked after changing the {@link #spaces} value.
     */
    private void resetIndent() {
        setArrayStart("{" + SystemUtils.LINE_SEPARATOR + spacer(spaces));
        setArraySeparator("," + SystemUtils.LINE_SEPARATOR + spacer(spaces));
        setArrayEnd(SystemUtils.LINE_SEPARATOR + spacer(spaces - indent) + "}");

        setContentStart("[" + SystemUtils.LINE_SEPARATOR + spacer(spaces));
        setFieldSeparator("," + SystemUtils.LINE_SEPARATOR + spacer(spaces));
        setContentEnd(SystemUtils.LINE_SEPARATOR + spacer(spaces - indent) + "]");
    }

    /**
     * Creates a StringBuilder responsible for the indenting.
     * 
     * @param spaces how far to indent
     * @return a StringBuilder with {spaces} leading space characters.
     */
    private StringBuilder spacer(int spaces) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < spaces; i++) {
            sb.append(" ");
        }
        return sb;
    }

    @Override
    public void appendDetail(StringBuffer buffer, String fieldName, Object value) {
        if (!ClassUtils.isPrimitiveWrapper(value.getClass()) && !String.class.equals(value.getClass())
                && accept(value.getClass())) {
            spaces += indent;
            resetIndent();
            buffer.append(ReflectionToStringBuilder.toString(value, this));
            spaces -= indent;
            resetIndent();
        } else {
            super.appendDetail(buffer, fieldName, value);
        }
    }

    @Override
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final Object[] array) {
        spaces += indent;
        resetIndent();
        super.appendDetail(buffer, fieldName, array);
        spaces -= indent;
        resetIndent();
    }

    @Override
    protected void reflectionAppendArrayDetail(final StringBuffer buffer, final String fieldName, final Object array) {
        spaces += indent;
        resetIndent();
        super.appendDetail(buffer, fieldName, array);
        spaces -= indent;
        resetIndent();
    }

    @Override
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final long[] array) {
        spaces += indent;
        resetIndent();
        super.appendDetail(buffer, fieldName, array);
        spaces -= indent;
        resetIndent();
    }

    @Override
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final int[] array) {
        spaces += indent;
        resetIndent();
        super.appendDetail(buffer, fieldName, array);
        spaces -= indent;
        resetIndent();
    }

    @Override
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final short[] array) {
        spaces += indent;
        resetIndent();
        super.appendDetail(buffer, fieldName, array);
        spaces -= indent;
        resetIndent();
    }

    @Override
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final byte[] array) {
        spaces += indent;
        resetIndent();
        super.appendDetail(buffer, fieldName, array);
        spaces -= indent;
        resetIndent();
    }

    @Override
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final char[] array) {
        spaces += indent;
        resetIndent();
        super.appendDetail(buffer, fieldName, array);
        spaces -= indent;
        resetIndent();
    }

    @Override
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final double[] array) {
        spaces += indent;
        resetIndent();
        super.appendDetail(buffer, fieldName, array);
        spaces -= indent;
        resetIndent();
    }

    @Override
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final float[] array) {
        spaces += indent;
        resetIndent();
        super.appendDetail(buffer, fieldName, array);
        spaces -= indent;
        resetIndent();
    }

    @Override
    protected void appendDetail(final StringBuffer buffer, final String fieldName, final boolean[] array) {
        spaces += indent;
        resetIndent();
        super.appendDetail(buffer, fieldName, array);
        spaces -= indent;
        resetIndent();
    }

}

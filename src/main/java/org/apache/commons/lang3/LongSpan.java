/**
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.commons.lang3;

import org.apache.commons.lang3.math.NumberUtils;

/**
 * Describes a span of data. Starting point and ending point.
 * 
 * Start, length and end values are limited to long values.
 */
public interface LongSpan
{

	/**
	 * Starting position
	 * 
	 * @return start position
	 */
	long getStart();

	/**
	 * Length of the span.
	 * 
	 * @return the length of the span.
	 */
	long getLength();

	/**
	 * Ending position of span
	 * 
	 * @return end position
	 */
	long getEnd();

	/**
	 * Return true if the spans share any positions.
	 * 
	 * @param other
	 *            The other span
	 * @return true if overlap
	 */
	boolean overlaps(LongSpan other);

	/**
	 * Return true if this span contains the position.
	 * 
	 * @param pos
	 *            the position to check for.
	 * @return true if start &lt;= pos &lt;= end
	 */
	boolean contains(long pos);

	/**
	 * Return true if this span contains the span in its entirety.
	 * 
	 * @param other
	 *            the other to check for.
	 * @return true if start &lt;= other.start and other.end &lt;= end
	 */
	boolean contains(LongSpan other);

	/**
	 * Convenience methods for implementating IntSpan.
	 * 
	 * note: In java 8 thise will be come static members of IntSpan or default 
	 * implementations of IntSpan methods.
	 */
	public static class Util
	{
		/**
		 * Return true if one span shares any positions with the other span.
		 * 
		 * @param one the first span
		 * @param other
		 *            The other span
		 * @return true if overlap
		 */
		public static boolean overlaps(LongSpan one, LongSpan other)
		{
			if (one.getEnd() < other.getStart()
					|| one.getStart() > other.getEnd())
			{
				return false;
			}

			return true;
		}

		/**
		 * Return true if one span contains the position.
		 * 
		 * @param one The span to check.
		 * @param pos
		 *            the position to check for.
		 * @return true if start &lt;= pos &lt;= end
		 */
		public static boolean contains(LongSpan one, long pos)
		{
			return one.getStart() <= pos && one.getEnd() >= pos;
		}

		/**
		 * Return true if one span contains the other span in its entirety.
		 * 
		 * @param one The span to check in.
		 * @param other
		 *            the other to check for.
		 * @return true if start &lt;= other.start and other.end &lt;= end
		 */
		public static boolean contains(LongSpan one, LongSpan other)
		{
			return one.getStart() <= other.getStart()
					&& one.getEnd() >= other.getEnd();
		}

		/**
		 * A method to calculate the end of a span from the start and length.
		 * Intended to be used by span implementations that store start and
		 * length.
		 * 
		 * @param span
		 *            The span to calculate end for
		 * @return The end position of the span
		 */
		static public long calcEnd(LongSpan span)
		{
			return span.getStart() + span.getLength() - 1;
		}

		/**
		 * A method to calculate the length of a span from the start and end.
		 * Intended to be used by span implementations that stoare start and
		 * end.
		 * 
		 * @param span
		 *            The span to calculate end for
		 * @return The end position of the span
		 */
		public static long calcLength(LongSpan span)
		{
			return span.getEnd() - span.getStart() + 1;
		}

		/**
		 * Check for over flow when calculating end position.
		 * 
		 * @param start
		 *            The starting position
		 * @param length
		 *            the length
		 * @throws IllegalArgumentException
		 *             if the result if not within [Long.MIN_VALUE,
		 *             Long.MAX_VALUE]
		 */
		public static void checkStartAndLength(long start, long length)
		{
			if (length < 0)
			{
				throw new IndexOutOfBoundsException(
						"Length may not be less than zero: " + length);
			}
			if (NumberUtils.isOverflow(start, length))
			{

				throw new IllegalArgumentException(String.format(
						"length (%s) + start (%s) > Long.MAX_VALUE (%s)",
						length, start, Long.MAX_VALUE));

			}
		}

		/**
		 * extends Abstract A default implementation of to string for the span.
		 * 
		 * @param span
		 *            The span to get the string for
		 * @return The printable string
		 */
		public static String toString(LongSpan span)
		{
			return String.format("%s[%s,%s]", span.getClass().getName(),
					span.getStart(),
					span.getLength() > 0 ? span.getEnd() : "-empty-");

		}
	}

	/**
	 * The factory to create LongSpans.
	 *
	 */
	public static class Factory
	{
		/**
		 * Construct a span from a starting position and an end position.
		 * 
		 * @param start
		 *            The starting position.
		 * @param end
		 *            The end position
		 * @return the new Span.
		 */
		public static LongSpan fromEnd(long start, long end)
		{
			try
			{
				if (NumberUtils.isUnderflow(end, start))
				{
					return new Impl(start,
							Math.subtractExact(Math.addExact(end, 1), start));
				} else
				{
					return new Impl(start,
							Math.addExact(Math.subtractExact(end, start), 1));
				}
			} catch (ArithmeticException e)
			{
				throw new IllegalArgumentException(String.format(
						"end (%s) - start (%s) + 1  must fall between [Long.MIN_VALUE (%s), Long.MAX_VALUE (%s)]",
						end, start, Long.MIN_VALUE, Long.MAX_VALUE));
			}
		}

		/**
		 * Create a span from a starting position and a length.
		 * 
		 * @param start
		 *            the starting position.
		 * @param length
		 *            the length.
		 * @return the new Span.
		 */
		public static LongSpan fromLength(long start, long length)
		{
			return new Impl(start, length);
		}
	}

	/**
	 * An implementation of LongSpan for factory use.
	 *
	 */
	public static class Impl implements LongSpan
	{

		private final long start;
		private final long length;

		/**
		 * Constructor using a starting position and a length. To construct
		 * using a starting position and an endpoint use fromEnd().
		 * 
		 * @param start
		 *            The starting position.
		 * @param length
		 *            The length.
		 */
		protected Impl(long start, long length)
		{
			Util.checkStartAndLength(start, length);
			this.start = start;
			this.length = length;
		}

		@Override
		public final long getStart()
		{
			return start;
		}

		@Override
		public final long getLength()
		{
			return length;
		}

		@Override
		public final long getEnd()
		{
			return Util.calcEnd(this);
		}

		@Override
		public String toString()
		{
			return Util.toString(this);
		}

		@Override
		public boolean overlaps(LongSpan other)
		{
			return Util.overlaps(this, other);
		}

		@Override
		public boolean contains(long pos)
		{
			return Util.contains(this, pos);
		}

		@Override
		public boolean contains(LongSpan other)
		{
			return Util.contains(this, other);
		}

	}

}

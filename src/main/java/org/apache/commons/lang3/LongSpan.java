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

import java.math.BigInteger;

import org.apache.commons.lang3.math.NumberUtils;

/**
 * Describes a span of data.
 * 
 * Spans are immutable.
 *  
 * Implementations of span may record start and end positions or start position and length.
 *
 * Since spans are described by the first and last position in the span a zero length span
 * has a first position but the last position will be in the previous position, while a span
 * with a length of one will have the same value for starting and ending position.
 *
 * In this implementation start, length and end values are limited to long values.
 *
 */
public interface LongSpan
{

	/**
	 * The first position in  the span
	 * 
	 * @return first position in the span.
	 */
	long getStart();

	/**
	 * Length of the span.
	 * 
	 * @return the length of the span.
	 */
	long getLength();

	/**
	 * Ending (last) position in the span.
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
		 * Intended to be used by span implementations that store start and
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
			return new Impl( start, end );

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
			if (length < 0)
			{
				throw new IllegalArgumentException( "Length may not be less than 0");
			}
			// will handle Long.MIN_value + (-1)
			if (NumberUtils.isOverflow( start, length-1))
			{
				throw new IllegalArgumentException( String.format( 
					"The end positions is too large start+length+1>%s", Long.MAX_VALUE));
			}
			return new Impl(start, start + (length-1));
		}
	}

	/**
	 * An implementation of LongSpan for factory use.
	 *
	 */
	public static class Impl implements LongSpan
	{

		private final long start;
		private final long end;

		/**
		 * Constructor using a starting position and an ending position.
		 * 
		 * @param start
		 *            The starting position.
		 * @param end
		 *            The ending position.
		 */
		protected Impl(long start, long end)
		{
			if (end < start && end+1 != start)
			{
				throw new IllegalArgumentException( String.format( "The end position too small (%s) end+1 >= start (%s)", end, start));
			}
			if (NumberUtils.isUnderflow( end, start))
			{
				throw new IllegalArgumentException( String.format( 
						"The distance between start (%s) and end (%s) positions is too larg end-start>%s", start,end,Long.MAX_VALUE));
			}
			this.start = start;
			this.end = end;
		}

		@Override
		public final long getStart()
		{
			return start;
		}

		@Override
		public final long getLength()
		{
			return Util.calcLength( this );
		}

		@Override
		public final long getEnd()
		{
			return end;
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

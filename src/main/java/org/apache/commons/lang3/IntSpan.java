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
 * Start, length and end values are limited to integer values.
 *
 */
public interface IntSpan {
	
	
	/**
	 * Starting position
	 * 
	 * @return start position
	 */
	int getStart();
	
	/**
	 * Length of the span.
	 * 
	 * @return the length of the span.
	 */
	int getLength();
	
	/**
	 * Ending position of span
	 * 
	 * @return end position
	 */
	int getEnd();


	/**
	 * Return true if the spans share any positions.
	 * 
	 * @param other
	 *            The other span
	 * @return true if overlap
	 */
	boolean overlaps(IntSpan other);

	/**
	 * Return true if this span contains the position.
	 * 
	 * @param pos
	 *            the position to check for.
	 * @return true if start &lt;= pos &lt;= end
	 */
	boolean contains(int pos);
	
	/**
	 * Return true if this span contains the span in its entirety.
	 * 
	 * @param other
	 *            the other to check for.
	 * @return true if start &lt;= other.start and other.end &lt;= end
	 */
	boolean contains(IntSpan other);

	/**
	 * The factory to create IntSpans.
	 *
	 */
	public static class Factory {

		/**
		 * Construct a span from a starting position and an endpoint.
		 * 
		 * @param start
		 *            The starting position.
		 * @param end
		 *            The endpoint
		 * @return the new Span.
		 */
		public static IntSpan fromEnd(int start, int end) {
		    return new Impl( start, end - start + 1 );
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
		public static IntSpan fromLength(int start, int length) {
		    return new Impl( start, length );
		}
		
	}

	/**
	 * Convenience methods for implementating IntSpan.
	 * 
	 * note: In java 8 thise will be come static members of IntSpan or default 
	 * implementations of IntSpan methods.
	 */
	public static class Util {

		/**
		 * A method to calculate the end of a span from the start and length.
		 * Intended to be used by span implementations that store start and
		 * length.
		 * 
		 * @param span
		 *            The span to calculate end for
		 * @return The end position of the span
		 */
		static public int calcEnd(IntSpan span) {
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
		public static int calcLength(IntSpan span) {
			return span.getEnd() - span.getStart() + 1;
		}

		/**
		 * Check for over flow when calculating end position.
		 * 
		 * @param start
		 *            The starting position
		 * @param increment
		 *            the length
		 * @return the int value created by adding start and increment.
		 * @throws IllegalArgumentException
		 *             if the result if not within [Long.MIN_VALUE,
		 *             Long.MAX_VALUE]
		 */
		public static int checkIntAddLimit(int start, int increment) {
			if (increment == 0) {
				return start;
			}
			if (NumberUtils.isOverflow(start, increment)) {
			if (increment < 0) {
				// this really subtracts
					throw new IllegalArgumentException(String.format(
							"Start (%s) - length (%s) < Integer.MIN_VALUE (%s)",
							start, increment, Integer.MIN_VALUE));
		
			
			} else {
					throw new IllegalArgumentException(String.format(
							"Length (%s) + Start (%s) > Integer.MAX_VALUE (%s)",
							increment, start, Integer.MAX_VALUE));
			}
			}
			return start + increment;
		}

		/**
		 * A default implementation of to string for the span.
		 * @param span The span to get the string for
		 * @return The printable string
		 */
		public static String toString(IntSpan span)
		{
			return String.format("%s[%s,%s]", span.getClass().getName(), span.getStart(), span.getLength() > 0 ? span.getEnd()
						: "-empty-");
			
		}
				
		/**
		 * Return true if one span shares any positions with the other span.
		 * 
		 * @param one the first span
		 * @param other
		 *            The other span
		 * @return true if overlap
		 */

        public static boolean overlaps(IntSpan one, IntSpan other) {
    		if (one.getEnd() < other.getStart()
    				|| one.getStart() > other.getEnd()) {
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
        public static boolean contains(IntSpan one, int pos) {
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
    	public static boolean contains(IntSpan one, IntSpan other) {
    		return one.getStart() <= other.getStart() && one.getEnd() >= other.getEnd();
    	}
	}
	
	/**
     * An implementation of IntSpan for factory use.
     *
     */
    public static class Impl implements IntSpan {

        private final int start;
        private final int length;

        /**
         * Constructor using a starting position and a length. To construct
         * using a starting position and an endpoint use fromEnd().
         * 
         * @param start
         *            The starting position.
         * @param length
         *            The length.
         */
        Impl(int start, int length) {
            Util.checkIntAddLimit( start, length );
            if (length < 0) {
                throw new IndexOutOfBoundsException( "Length may not be less than zero: " + length );
            }
            this.start = start;
            this.length = length;
        }

        @Override
        public final int getStart() {
            return start;
        }

        @Override
        public final int getLength() {
            return length;
        }

        @Override
        public final int getEnd() {
            return Util.calcEnd( this );
        }
        
        @Override
        public final String toString() {
        	return Util.toString(this);
        }
        
        @Override
        public boolean overlaps(IntSpan other) {
        	return Util.overlaps( this, other );
    	}

        @Override
    	public boolean contains(int pos) {
        	return Util.contains( this, pos);
    	}
    	
    	@Override
    	public boolean contains(IntSpan other) {
    		return Util.contains( this, other);
    	}

    }

}

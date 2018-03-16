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

import static org.junit.Assert.*;

import org.junit.Test;

/**
 * Describes a span of data. Starting point and ending point.
 * 
 * Start, length and end values are limited to long values.
 */
public class LongSpanTest {
	
	@Test
	public void fromEndTest_ZeroStart()
	{
		LongSpan span = LongSpan.Factory.fromEnd( 0, 5 );
		assertEquals( 0, span.getStart());
		assertEquals( 6, span.getLength());
		assertEquals( 5, span.getEnd());
	}
	
	@Test
	public void fromEndTest_NegativeStart()
	{
		LongSpan span = LongSpan.Factory.fromEnd( -1, 5 );
		assertEquals( -1, span.getStart());
		assertEquals( 7, span.getLength());
		assertEquals( 5, span.getEnd());
	}

	@Test
	public void fromEndTest_PositiveStart()
	{
		LongSpan span = LongSpan.Factory.fromEnd( 2, 5 );
		assertEquals( 2, span.getStart());
		assertEquals( 4, span.getLength());
		assertEquals( 5, span.getEnd());
	}

	@Test
	public void fromEndTest_MinimumStart()
	{
		LongSpan span = LongSpan.Factory.fromEnd( Long.MIN_VALUE, -5 );
		assertEquals( Long.MIN_VALUE, span.getStart());
		assertEquals( Math.abs(Long.MIN_VALUE+4), span.getLength());
		assertEquals( -5, span.getEnd());
	}

	
	@Test
	public void fromEndTest_MaximumEnd()
	{
		LongSpan span = LongSpan.Factory.fromEnd( 0, Long.MAX_VALUE-1 );
		assertEquals( 0, span.getStart());
		assertEquals( Long.MAX_VALUE, span.getLength());
		assertEquals( Long.MAX_VALUE-1, span.getEnd());
	}

	
	@Test
	public void fromLengthTest_ZeroStart()
	{
		LongSpan span = LongSpan.Factory.fromLength( 0, 5 );
		assertEquals( 0, span.getStart());
		assertEquals( 5, span.getLength());
		assertEquals( 4, span.getEnd());
	}
	
	@Test
	public void fromLengthTest_NegativeStart()
	{
		LongSpan span = LongSpan.Factory.fromLength( -1, 5 );
		assertEquals( -1, span.getStart());
		assertEquals( 5, span.getLength());
		assertEquals( 3, span.getEnd());
	}

	@Test
	public void fromLengthTest_PositiveStart()
	{
		LongSpan span = LongSpan.Factory.fromLength( 2, 5 );
		assertEquals( 2, span.getStart());
		assertEquals( 5, span.getLength());
		assertEquals( 6, span.getEnd());
	}

	@Test
	public void fromLengthTest_MinimumStart()
	{
		LongSpan span = LongSpan.Factory.fromLength( Long.MIN_VALUE, 5 );
		assertEquals( Long.MIN_VALUE, span.getStart());
		assertEquals( 5, span.getLength());
		assertEquals( Long.MIN_VALUE+4, span.getEnd());
	}

	@Test
	public void fromLengthTest_MaxLength() {
		LongSpan span = LongSpan.Factory.fromLength(0, Long.MAX_VALUE);
		assertEquals( 0, span.getStart());
		assertEquals( Long.MAX_VALUE, span.getLength());
		assertEquals( Long.MAX_VALUE-1, span.getEnd());
	}
	
	@Test
	public void containsTest_Point() {
		LongSpan span = LongSpan.Factory.fromEnd(0, 5);
		assertFalse( "Should not contain -1", span.contains( -1 ));
		for (int i=0;i<5;i++)
		{
			assertTrue( "Should contain i", span.contains( i ));
		}
		assertFalse( "Should not contain 6", span.contains( 6 ));
		
	}
	
	@Test
	public void containsTest_Span() {
		LongSpan span = LongSpan.Factory.fromEnd(0, 5);
	
		assertTrue( "Span should contain itself", span.contains( span ));
		assertTrue( "Span should contain other at start", span.contains( LongSpan.Factory.fromEnd(0, 4) ));
		assertTrue( "Span should contain other at end", span.contains( LongSpan.Factory.fromEnd(1, 5) ));
		assertTrue( "Span should contain inner set", span.contains( LongSpan.Factory.fromEnd(1, 4) ));
		
		assertFalse( "Span should not contain larger", span.contains( LongSpan.Factory.fromEnd( -1, 6)));
		assertFalse( "Span should not contain larger sharing start", span.contains( LongSpan.Factory.fromEnd( 0, 6)));
		assertFalse( "Span should not contain larger sharing end", span.contains( LongSpan.Factory.fromEnd( -1, 5)));
		assertFalse( "Span should not contain larger crossing start", span.contains( LongSpan.Factory.fromEnd( -1, 4)));
		assertFalse( "Span should not contain larger crossing end", span.contains( LongSpan.Factory.fromEnd( 3, 7)));
	}
	
	@Test
	public void overlapTest() {
		LongSpan span = LongSpan.Factory.fromEnd(0, 5);
		
		assertTrue( "Span should overlap itself", span.overlaps( span ));
		assertTrue( "Span should overlap other at start", span.overlaps( LongSpan.Factory.fromEnd(0, 4) ));
		assertTrue( "Span should overlap other at end", span.overlaps( LongSpan.Factory.fromEnd(1, 5) ));
		assertTrue( "Span should overlap inner set", span.overlaps( LongSpan.Factory.fromEnd(1, 4) ));
		
		assertTrue( "Span should overlap larger", span.overlaps( LongSpan.Factory.fromEnd( -1, 6)));
		assertTrue( "Span should overlap larger sharing start", span.overlaps( LongSpan.Factory.fromEnd( 0, 6)));
		assertTrue( "Span should overlap larger sharing end", span.overlaps( LongSpan.Factory.fromEnd( -1, 5)));
		assertTrue( "Span should overlap larger crossing start", span.overlaps( LongSpan.Factory.fromEnd( -1, 4)));
		assertTrue( "Span should overlap larger crossing end", span.overlaps( LongSpan.Factory.fromEnd( 3, 7)));

		assertFalse( "Span should not overlap disjoint span", span.overlaps( LongSpan.Factory.fromEnd( 10, 20)));

	}
}

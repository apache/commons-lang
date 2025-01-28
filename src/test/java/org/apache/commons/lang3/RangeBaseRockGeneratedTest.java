package org.apache.commons.lang3;

import org.apache.commons.lang3.Range;
import java.util.Arrays;
import java.util.Calendar;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.provider.CsvSource;
import java.util.Date;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import org.junit.jupiter.api.Disabled;

class RangeBaseRockGeneratedTest {

    //BaseRock generated method id: ${testBetween}, hash: 27AD8286FBC305A3130FBA932AED39A7
    @Test
    void testBetween() {
        Range<Integer> range = Range.between(1, 10);
        assertEquals(1, range.getMinimum());
        assertEquals(10, range.getMaximum());
    }

    //BaseRock generated method id: ${testIs}, hash: E40D922BB5A38A658E728803B32D8202
    @Test
    void testIs() {
        Range<String> range = Range.is("test");
        assertEquals("test", range.getMinimum());
        assertEquals("test", range.getMaximum());
    }

    //BaseRock generated method id: ${testContains}, hash: 2D43DEC67F75380D0723067E8EB11288
    @ParameterizedTest
    @CsvSource({ "1, 10, 5, true", "1, 10, 0, false", "1, 10, 11, false" })
    void testContains(int min, int max, int value, boolean expected) {
        Range<Integer> range = Range.between(min, max);
        assertEquals(expected, range.contains(value));
    }

    //BaseRock generated method id: ${testContainsRange}, hash: C97FC35F59DDBCF958750BD1878B9C06
    @Test
    void testContainsRange() {
        Range<Integer> range1 = Range.between(1, 10);
        Range<Integer> range2 = Range.between(2, 9);
        Range<Integer> range3 = Range.between(0, 11);
        assertTrue(range1.containsRange(range2));
        assertFalse(range1.containsRange(range3));
    }

    //BaseRock generated method id: ${testElementCompareTo}, hash: F36AED289D99A1275E3DE646A41C5C1F
    @Disabled()
    @ParameterizedTest
    @ValueSource(ints = { 0, 1, 5, 10, 11 })
    void testElementCompareTo(int element) {
        Range<Integer> range = Range.between(1, 10);
        int expected = Integer.compare(1, element);
        if (element > 1 && element < 10) {
            expected = 0;
        } else if (element > 10) {
            expected = -1;
        }
        assertEquals(expected, range.elementCompareTo(element));
    }

    //BaseRock generated method id: ${testEquals}, hash: 046C9FD9F22C2BA35071F2E517D1D656
    @Test
    void testEquals() {
        Range<Integer> range1 = Range.between(1, 10);
        Range<Integer> range2 = Range.between(1, 10);
        Range<Integer> range3 = Range.between(2, 11);
        assertEquals(range1, range2);
        assertNotEquals(range1, range3);
    }

    //BaseRock generated method id: ${testFit}, hash: 07F0E59BDC42C16A61F20263DE6CE4CF
    @ParameterizedTest
    @ValueSource(ints = { 0, 1, 5, 10, 11 })
    void testFit(int element) {
        Range<Integer> range = Range.between(1, 10);
        int expected = Math.min(Math.max(element, 1), 10);
        assertEquals(expected, range.fit(element));
    }

    //BaseRock generated method id: ${testGetComparator}, hash: B45AF6FB114E158516CDBB13482CE3F2
    @Test
    void testGetComparator() {
        Range<Integer> range = Range.between(1, 10);
        assertNotNull(range.getComparator());
    }

    //BaseRock generated method id: ${testHashCode}, hash: C2C7C2D5E9E59C66159D7478D31DD594
    @Test
    void testHashCode() {
        Range<Integer> range1 = Range.between(1, 10);
        Range<Integer> range2 = Range.between(1, 10);
        assertEquals(range1.hashCode(), range2.hashCode());
    }

    //BaseRock generated method id: ${testIntersectionWith}, hash: 48E447F5B62E6182DD2A8C6B0F454E16
    @Test
    void testIntersectionWith() {
        Range<Integer> range1 = Range.between(1, 10);
        Range<Integer> range2 = Range.between(5, 15);
        Range<Integer> intersection = range1.intersectionWith(range2);
        assertEquals(5, intersection.getMinimum());
        assertEquals(10, intersection.getMaximum());
    }

    //BaseRock generated method id: ${testIsAfter}, hash: 2B45DACEAC1A66E988795696A56BEDED
    @ParameterizedTest
    @ValueSource(ints = { 0, 1, 5, 10, 11 })
    void testIsAfter(int element) {
        Range<Integer> range = Range.between(1, 10);
        assertEquals(element < 1, range.isAfter(element));
    }

    //BaseRock generated method id: ${testIsAfterRange}, hash: 9B18CB2BD775BE5BA211F6438478A7F8
    @Disabled()
    @Test
    void testIsAfterRange() {
        Range<Integer> range1 = Range.between(1, 10);
        Range<Integer> range2 = Range.between(11, 20);
        Range<Integer> range3 = Range.between(5, 15);
        assertTrue(range1.isAfterRange(range2));
        assertFalse(range1.isAfterRange(range3));
    }

    //BaseRock generated method id: ${testIsBefore}, hash: FEE826F898FF0E60843E625AB5744CB6
    @ParameterizedTest
    @ValueSource(ints = { 0, 1, 5, 10, 11 })
    void testIsBefore(int element) {
        Range<Integer> range = Range.between(1, 10);
        assertEquals(element > 10, range.isBefore(element));
    }

    //BaseRock generated method id: ${testIsBeforeRange}, hash: 9C4C6F715BD3B50B593B1E4585F3A640
    @Disabled()
    @Test
    void testIsBeforeRange() {
        Range<Integer> range1 = Range.between(1, 10);
        Range<Integer> range2 = Range.between(-5, 0);
        Range<Integer> range3 = Range.between(5, 15);
        assertTrue(range1.isBeforeRange(range2));
        assertFalse(range1.isBeforeRange(range3));
    }

    //BaseRock generated method id: ${testIsEndedBy}, hash: 5480C4728537EE52F27BBB9C48447F1A
    @Test
    void testIsEndedBy() {
        Range<Integer> range = Range.between(1, 10);
        assertTrue(range.isEndedBy(10));
        assertFalse(range.isEndedBy(9));
    }

    //BaseRock generated method id: ${testIsNaturalOrdering}, hash: 6E9E02E11008459F6E305E4D4188C898
    @Test
    void testIsNaturalOrdering() {
        Range<Integer> range1 = Range.between(1, 10);
        Range<String> range2 = Range.between("a", "z", String::compareTo);
        assertTrue(range1.isNaturalOrdering());
        assertFalse(range2.isNaturalOrdering());
    }

    //BaseRock generated method id: ${testIsOverlappedBy}, hash: B1D0B9EA27C3AFC569130BFE228E899E
    @Test
    void testIsOverlappedBy() {
        Range<Integer> range1 = Range.between(1, 10);
        Range<Integer> range2 = Range.between(5, 15);
        Range<Integer> range3 = Range.between(11, 20);
        assertTrue(range1.isOverlappedBy(range2));
        assertFalse(range1.isOverlappedBy(range3));
    }

    //BaseRock generated method id: ${testIsStartedBy}, hash: C9643D4397E6653536D7458A4874BA4A
    @Test
    void testIsStartedBy() {
        Range<Integer> range = Range.between(1, 10);
        assertTrue(range.isStartedBy(1));
        assertFalse(range.isStartedBy(2));
    }

    //BaseRock generated method id: ${testToString}, hash: CDA8389B67C697142357E478CEFBEFCC
    @Test
    void testToString() {
        Range<Integer> range = Range.between(1, 10);
        assertEquals("[1..10]", range.toString());
    }

    //BaseRock generated method id: ${testToStringWithFormat}, hash: 07135C6154C3C92F2D630974E4C54C40
    @Test
    void testToStringWithFormat() {
        Range<Integer> range = Range.between(1, 10);
        assertEquals("From 1 to 10", range.toString("From %s to %s"));
    }
}

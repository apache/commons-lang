package org.apache.commons.lang3;

import org.apache.commons.lang3.EnumUtils;
import java.util.Arrays;
import static org.junit.jupiter.api.Assertions.assertAll;
import java.util.List;
import java.util.Map;
import static org.mockito.ArgumentMatchers.any;
import org.junit.jupiter.api.Test;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.ArgumentMatchers.eq;
import static org.junit.jupiter.api.Assertions.assertThrows;
import java.time.DayOfWeek;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import org.junit.jupiter.api.Timeout;
import static org.mockito.Mockito.mock;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.Mockito.atLeast;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mockStatic;
import java.util.function.Function;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import java.util.ArrayList;
import java.util.EnumSet;
import org.mockito.MockedStatic;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import org.junit.jupiter.api.Disabled;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class EnumUtilsBaseRockGeneratedTest {

    private final Function<Enum<?>, String> functionMock = mock(Function.class);

    //BaseRock generated method id: ${generateBitVector1WhenConstantsLengthLessThanOrEqualsToLongSIZEThrowsIllegalArgumentException}, hash: C59D7FCB4F2BCD0EEFCD3A8F5D33B09F
    @Disabled()
    @Test
    void generateBitVector1WhenConstantsLengthLessThanOrEqualsToLongSIZEThrowsIllegalArgumentException() {
        Iterable<DayOfWeek> iterable = new ArrayList<>();
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            EnumUtils.generateBitVector(DayOfWeek.class, iterable);
        });
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${generateBitVectors1WhenCondensedIsNotEmpty}, hash: 31656E0D39C8BA69D3625EE18619D3D9
    @Disabled()
    @Test
    void generateBitVectors1WhenCondensedIsNotEmpty() {
        Iterable<DayOfWeek> iterable = new ArrayList<>(Arrays.asList(DayOfWeek.MONDAY));
        long[] result = EnumUtils.generateBitVectors(DayOfWeek.class, iterable);
        long[] longResultArray = new long[] { 2L };
        assertAll("result", () -> assertThat(result, equalTo(longResultArray)));
    }

    //BaseRock generated method id: ${getEnumTest}, hash: A206B187B8D29B959AC7416F3E335D27
    @Test
    void getEnumTest() {
        try (MockedStatic<EnumUtils> enumUtils = mockStatic(EnumUtils.class, CALLS_REAL_METHODS)) {
            enumUtils.when(() -> EnumUtils.getEnum(DayOfWeek.class, "MONDAY", null)).thenReturn(DayOfWeek.MONDAY);
            DayOfWeek result = EnumUtils.getEnum(DayOfWeek.class, "MONDAY");
            assertAll("result", () -> {
                assertThat(result, equalTo(DayOfWeek.MONDAY));
                enumUtils.verify(() -> EnumUtils.getEnum(DayOfWeek.class, "MONDAY", null), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getEnum1WhenEnumNameIsNull}, hash: 6F705386AD0348F3C07D9434A5273F00
    @Test
    void getEnum1WhenEnumNameIsNull() {
        DayOfWeek result = EnumUtils.getEnum(DayOfWeek.class, null, DayOfWeek.MONDAY);
        assertAll("result", () -> assertThat(result, equalTo(DayOfWeek.MONDAY)));
    }

    //BaseRock generated method id: ${getEnum1WhenEnumNameIsNotNull}, hash: DD0A71E6FA0BFC2FEF34579A186D807B
    @Test
    void getEnum1WhenEnumNameIsNotNull() {
        DayOfWeek result = EnumUtils.getEnum(DayOfWeek.class, "MONDAY", DayOfWeek.MONDAY);
        assertAll("result", () -> assertThat(result, equalTo(DayOfWeek.MONDAY)));
    }

    //BaseRock generated method id: ${getEnum1WhenCaughtIllegalArgumentException}, hash: E4E984AE3F14C40F3D71821D981A0E04
    @Test
    void getEnum1WhenCaughtIllegalArgumentException() {
        DayOfWeek result = EnumUtils.getEnum(DayOfWeek.class, "INVALID", DayOfWeek.MONDAY);
        assertAll("result", () -> assertThat(result, equalTo(DayOfWeek.MONDAY)));
    }

    //BaseRock generated method id: ${getEnumIgnoreCaseTest}, hash: 6441E68F9AC6AC2F74767E94F5BAEC74
    @Test
    void getEnumIgnoreCaseTest() {
        try (MockedStatic<EnumUtils> enumUtils = mockStatic(EnumUtils.class, CALLS_REAL_METHODS)) {
            enumUtils.when(() -> EnumUtils.getEnumIgnoreCase(DayOfWeek.class, "monday", null)).thenReturn(DayOfWeek.MONDAY);
            DayOfWeek result = EnumUtils.getEnumIgnoreCase(DayOfWeek.class, "monday");
            assertAll("result", () -> {
                assertThat(result, equalTo(DayOfWeek.MONDAY));
                enumUtils.verify(() -> EnumUtils.getEnumIgnoreCase(DayOfWeek.class, "monday", null), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getEnumIgnoreCase1Test}, hash: 9624B6EDEEA678CA7C8A0D0D249CBD94
    @Test
    void getEnumIgnoreCase1Test() {
        DayOfWeek result = EnumUtils.getEnumIgnoreCase(DayOfWeek.class, "monday", DayOfWeek.MONDAY);
        assertAll("result", () -> assertThat(result, equalTo(DayOfWeek.MONDAY)));
    }

    //BaseRock generated method id: ${getEnumListTest}, hash: 165B399EBF68BE4B32B6CD0082C6BA53
    @Test
    void getEnumListTest() {
        List<DayOfWeek> result = EnumUtils.getEnumList(DayOfWeek.class);
        assertAll("result", () -> {
            assertThat(result.size(), equalTo(7));
            assertThat(result.get(0), is(instanceOf(DayOfWeek.class)));
        });
    }

    //BaseRock generated method id: ${getEnumMapTest}, hash: 860AF75336F96A445BB1DA2EA93606CA
    @Test
    void getEnumMapTest() {
        Map<String, DayOfWeek> result = EnumUtils.getEnumMap(DayOfWeek.class);
        assertAll("result", () -> assertThat(result.size(), equalTo(7)));
    }

    //BaseRock generated method id: ${getEnumMap1Test}, hash: 448BBFFBE709B14DE767D55874194437
    @Disabled()
    @Test
    void getEnumMap1Test() {
        Function<DayOfWeek, String> functionMock = mock(Function.class);
        Map<String, DayOfWeek> result = EnumUtils.getEnumMap(DayOfWeek.class, functionMock);
        assertAll("result", () -> assertThat(result.size(), equalTo(7)));
    }

    //BaseRock generated method id: ${getEnumSystemPropertyWhenPropNameIsNull}, hash: EFEDBF10C111FDA9BB8DF0A876598BA4
    @Test
    void getEnumSystemPropertyWhenPropNameIsNull() {
        DayOfWeek result = EnumUtils.getEnumSystemProperty(DayOfWeek.class, null, DayOfWeek.MONDAY);
        assertAll("result", () -> assertThat(result, equalTo(DayOfWeek.MONDAY)));
    }

    //BaseRock generated method id: ${getEnumSystemPropertyWhenPropNameIsNotNull}, hash: 8CFF7865F9138FAF0322485D6DF6F6AE
    @Test
    void getEnumSystemPropertyWhenPropNameIsNotNull() {
        System.setProperty("test.day", "MONDAY");
        DayOfWeek result = EnumUtils.getEnumSystemProperty(DayOfWeek.class, "test.day", DayOfWeek.TUESDAY);
        assertAll("result", () -> assertThat(result, equalTo(DayOfWeek.MONDAY)));
        System.clearProperty("test.day");
    }

    //BaseRock generated method id: ${getFirstEnumIgnoreCaseWhenEnumClassNotIsEnum}, hash: B38952A040D272FDBAF228DDEEC3307C
    @Test
    void getFirstEnumIgnoreCaseWhenEnumClassNotIsEnum() {
        DayOfWeek result = EnumUtils.getFirstEnumIgnoreCase(DayOfWeek.class, "monday", DayOfWeek::name, DayOfWeek.MONDAY);
        assertAll("result", () -> assertThat(result, equalTo(DayOfWeek.MONDAY)));
    }

    //BaseRock generated method id: ${getFirstEnumIgnoreCaseWhenEnumClassIsEnum}, hash: 7FB3D89EC7D7E5A5FF3100D5B5CA6831
    @Test
    void getFirstEnumIgnoreCaseWhenEnumClassIsEnum() {
        DayOfWeek result = EnumUtils.getFirstEnumIgnoreCase(DayOfWeek.class, "monday", DayOfWeek::name, DayOfWeek.MONDAY);
        assertAll("result", () -> assertThat(result, equalTo(DayOfWeek.MONDAY)));
    }

    //BaseRock generated method id: ${isValidEnumWhenGetEnumEnumClassEnumNameIsNotNull}, hash: 8286C4AB1F6B1BFD86A0244A56898698
    @Test
    void isValidEnumWhenGetEnumEnumClassEnumNameIsNotNull() {
        boolean result = EnumUtils.isValidEnum(DayOfWeek.class, "MONDAY");
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isValidEnumWhenGetEnumEnumClassEnumNameIsNull}, hash: CA1E77B17E9E2E3E1C32F02962935666
    @Test
    void isValidEnumWhenGetEnumEnumClassEnumNameIsNull() {
        boolean result = EnumUtils.isValidEnum(DayOfWeek.class, "INVALID");
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isValidEnumIgnoreCaseWhenGetEnumIgnoreCaseEnumClassEnumNameIsNotNull}, hash: 6287C04AD2EA135522775CD2028CBFE0
    @Test
    void isValidEnumIgnoreCaseWhenGetEnumIgnoreCaseEnumClassEnumNameIsNotNull() {
        boolean result = EnumUtils.isValidEnumIgnoreCase(DayOfWeek.class, "monday");
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isValidEnumIgnoreCaseWhenGetEnumIgnoreCaseEnumClassEnumNameIsNull}, hash: C0949876555C05CCB0C9A37CD4615D3E
    @Test
    void isValidEnumIgnoreCaseWhenGetEnumIgnoreCaseEnumClassEnumNameIsNull() {
        boolean result = EnumUtils.isValidEnumIgnoreCase(DayOfWeek.class, "invalid");
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${processBitVectorWhenConstantsLengthLessThanOrEqualsToLongSIZEThrowsIllegalArgumentException}, hash: CCB9F0C9546331B9C51D38301A4819B4
    @Disabled()
    @Test
    void processBitVectorWhenConstantsLengthLessThanOrEqualsToLongSIZEThrowsIllegalArgumentException() {
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            EnumUtils.processBitVector(DayOfWeek.class, 0L);
        });
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${processBitVectorsWhenBlockIndexOfLvaluesAnd1LConstantOrdinalModulusLongSIZENotEquals0}, hash: 9D37BB2A63F23B21946B8C8F750608F4
    @Test
    void processBitVectorsWhenBlockIndexOfLvaluesAnd1LConstantOrdinalModulusLongSIZENotEquals0() {
        long[] longArray = new long[] { 2L };
        EnumSet<DayOfWeek> result = EnumUtils.processBitVectors(DayOfWeek.class, longArray);
        assertAll("result", () -> assertThat(result.size(), equalTo(1)));
    }
}

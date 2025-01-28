package org.apache.commons.lang3.time;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.time.LocalDateTime;
import java.util.Locale;
import java.util.Calendar;
import java.time.LocalDate;
import java.time.ZonedDateTime;
import org.mockito.MockedStatic;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.Instant;
import java.util.TimeZone;
import static org.mockito.ArgumentMatchers.any;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.verify;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.doReturn;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class CalendarUtilsBaseRockGeneratedTest {

    //BaseRock generated method id: ${getInstanceTest}, hash: ED755EBD1D0F492736E2D523504FD527
    @Test()
    void getInstanceTest() {
        
        //Act Statement(s)
        CalendarUtils result = CalendarUtils.getInstance();
        
        //Assert statement(s)
        //TODO: Please implement equals method in CalendarUtils for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getInstance1Test}, hash: D1ED19C057492E05970634588BE83907
    @Test()
    void getInstance1Test() {
        //Arrange Statement(s)
        Locale locale = new Locale("language1");
        
        //Act Statement(s)
        CalendarUtils result = CalendarUtils.getInstance(locale);
        
        //Assert statement(s)
        //TODO: Please implement equals method in CalendarUtils for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${toLocalDateTimeTest}, hash: F9E15865259C12AF57F913ACDC1A6D5B
    @Test()
    void toLocalDateTimeTest() {
        //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        LocalDateTime result = CalendarUtils.toLocalDateTime(calendar);
        Instant instant = calendar.toInstant();
        TimeZone timeZone = calendar.getTimeZone();
        ZoneId zoneId = timeZone.toZoneId();
        LocalDateTime localDateTime = LocalDateTime.ofInstant(instant, zoneId);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(localDateTime)));
    }

    //BaseRock generated method id: ${toOffsetDateTimeTest}, hash: E76F7108F9631E47F0AF82BA591223D5
    @Test()
    void toOffsetDateTimeTest() {
        //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        OffsetDateTime result = CalendarUtils.toOffsetDateTime(calendar);
        Instant instant = calendar.toInstant();
        TimeZone timeZone = calendar.getTimeZone();
        ZoneId zoneId = timeZone.toZoneId();
        OffsetDateTime offsetDateTime = OffsetDateTime.ofInstant(instant, zoneId);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(offsetDateTime)));
    }

    //BaseRock generated method id: ${toZonedDateTimeTest}, hash: 8CE92034E57597756CBA9A8E97467E68
    @Test()
    void toZonedDateTimeTest() {
        //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        ZonedDateTime result = CalendarUtils.toZonedDateTime(calendar);
        Instant instant = calendar.toInstant();
        TimeZone timeZone = calendar.getTimeZone();
        ZoneId zoneId = timeZone.toZoneId();
        ZonedDateTime zonedDateTime = ZonedDateTime.ofInstant(instant, zoneId);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(zonedDateTime)));
    }

    //BaseRock generated method id: ${getDayOfMonthTest}, hash: 2FA417DC67CE367A16518B13F7C804D9
    @Test()
    void getDayOfMonthTest() {
        //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        Locale locale = new Locale("language1");
        CalendarUtils target = new CalendarUtils(calendar, locale);
        
        //Act Statement(s)
        int result = target.getDayOfMonth();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(28)));
    }

    //BaseRock generated method id: ${getDayOfYearTest}, hash: 2D9466BFFEBFA43FEE078CEBBC3CFE6D
    @Test()
    void getDayOfYearTest() {
        //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        Locale locale = new Locale("language1");
        CalendarUtils target = new CalendarUtils(calendar, locale);
        
        //Act Statement(s)
        int result = target.getDayOfYear();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(28)));
    }

    //BaseRock generated method id: ${getMonthTest}, hash: DD3DB377CA42AC4E819C05975D3AF4C9
    @Test()
    void getMonthTest() {
        //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        Locale locale = new Locale("language1");
        CalendarUtils target = new CalendarUtils(calendar, locale);
        
        //Act Statement(s)
        int result = target.getMonth();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${getMonthDisplayNamesWhenDisplayNamesIsNull}, hash: 67976208505CD718CAAC8117AAC3136F
    @Test()
    void getMonthDisplayNamesWhenDisplayNamesIsNull() {
        /* Branches:
         * (displayNames == null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        Locale locale = new Locale("language1");
        CalendarUtils target = new CalendarUtils(calendar, locale);
        
        //Act Statement(s)
        String[] result = target.getMonthDisplayNames(0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getMonthDisplayNamesWhenDisplayNamesIsNotNull}, hash: 0841C4444D731026341117CCC7EB5B1A
    @Test()
    void getMonthDisplayNamesWhenDisplayNamesIsNotNull() {
        /* Branches:
         * (displayNames == null) : false
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        Locale locale = new Locale("language1");
        CalendarUtils target = new CalendarUtils(calendar, locale);
        
        //Act Statement(s)
        String[] result = target.getMonthDisplayNames(0);
        String[] stringResultArray = new String[] { "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", (String) null, (String) null, (String) null, (String) null, (String) null, (String) null, (String) null, (String) null, (String) null, (String) null, (String) null, (String) null };
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringResultArray)));
    }

    //BaseRock generated method id: ${getStandaloneLongMonthNamesTest}, hash: 63DDFBB6467ACCEA708364F29AEB7589
    @Test()
    void getStandaloneLongMonthNamesTest() {
        //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        Locale locale = new Locale("language1");
        CalendarUtils target = spy(new CalendarUtils(calendar, locale));
        String[] stringArray = new String[] {};
        doReturn(stringArray).when(target).getMonthDisplayNames(32770);
        
        //Act Statement(s)
        String[] result = target.getStandaloneLongMonthNames();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(stringArray));
            verify(target).getMonthDisplayNames(32770);
        });
    }

    //BaseRock generated method id: ${getStandaloneShortMonthNamesTest}, hash: 88572C5C1C30D3A361156CF725C2C5AB
    @Test()
    void getStandaloneShortMonthNamesTest() {
        //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        Locale locale = new Locale("language1");
        CalendarUtils target = spy(new CalendarUtils(calendar, locale));
        String[] stringArray = new String[] {};
        doReturn(stringArray).when(target).getMonthDisplayNames(32769);
        
        //Act Statement(s)
        String[] result = target.getStandaloneShortMonthNames();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(stringArray));
            verify(target).getMonthDisplayNames(32769);
        });
    }

    //BaseRock generated method id: ${getYearTest}, hash: 6C354A3EC5C879930F7A2EB236E05BE4
    @Test()
    void getYearTest() {
        //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        Locale locale = new Locale("language1");
        CalendarUtils target = new CalendarUtils(calendar, locale);
        
        //Act Statement(s)
        int result = target.getYear();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(2025)));
    }

    //BaseRock generated method id: ${toLocalDateTest}, hash: AAF18854FE07BA8E0D94E79E72C29EB6
    @Test()
    void toLocalDateTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        Locale locale = new Locale("language1");
        CalendarUtils target = spy(new CalendarUtils(calendar, locale));
        LocalDateTime localDateTime = LocalDateTime.parse("2024-01-01T10:15:30");
        doReturn(localDateTime).when(target).toLocalDateTime();
        
        //Act Statement(s)
        LocalDate result = target.toLocalDate();
        LocalDate localDate = LocalDate.now();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(localDate));
            verify(target).toLocalDateTime();
        });
    }

    //BaseRock generated method id: ${toLocalDateTime1Test}, hash: AB32A20EADEADF65077F132AA67E80C5
    @Test()
    void toLocalDateTime1Test() {
        //Arrange Statement(s)
        try (MockedStatic<CalendarUtils> calendarUtils = mockStatic(CalendarUtils.class)) {
            LocalDateTime localDateTime = LocalDateTime.parse("2024-01-01T10:15:30");
            calendarUtils.when(() -> CalendarUtils.toLocalDateTime((Calendar) any())).thenReturn(localDateTime);
            Calendar calendar = Calendar.getInstance();
            Locale locale = new Locale("language1");
            CalendarUtils target = new CalendarUtils(calendar, locale);
            //Act Statement(s)
            LocalDateTime result = target.toLocalDateTime();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(localDateTime));
                calendarUtils.verify(() -> CalendarUtils.toLocalDateTime((Calendar) any()));
            });
        }
    }

    //BaseRock generated method id: ${toOffsetDateTime1Test}, hash: 41EE08577214884710391414E3745EC3
    @Test()
    void toOffsetDateTime1Test() {
        //Arrange Statement(s)
        try (MockedStatic<CalendarUtils> calendarUtils = mockStatic(CalendarUtils.class)) {
            OffsetDateTime offsetDateTime = OffsetDateTime.now();
            calendarUtils.when(() -> CalendarUtils.toOffsetDateTime((Calendar) any())).thenReturn(offsetDateTime);
            Calendar calendar = Calendar.getInstance();
            Locale locale = new Locale("language1");
            CalendarUtils target = new CalendarUtils(calendar, locale);
            //Act Statement(s)
            OffsetDateTime result = target.toOffsetDateTime();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(offsetDateTime));
                calendarUtils.verify(() -> CalendarUtils.toOffsetDateTime((Calendar) any()));
            });
        }
    }

    //BaseRock generated method id: ${toZonedDateTime1Test}, hash: 8AC26146B4F4B5FAD6C9AEB2C8DF8234
    @Test()
    void toZonedDateTime1Test() {
        //Arrange Statement(s)
        try (MockedStatic<CalendarUtils> calendarUtils = mockStatic(CalendarUtils.class)) {
            ZonedDateTime zonedDateTime = ZonedDateTime.now();
            calendarUtils.when(() -> CalendarUtils.toZonedDateTime((Calendar) any())).thenReturn(zonedDateTime);
            Calendar calendar = Calendar.getInstance();
            Locale locale = new Locale("language1");
            CalendarUtils target = new CalendarUtils(calendar, locale);
            //Act Statement(s)
            ZonedDateTime result = target.toZonedDateTime();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(zonedDateTime));
                calendarUtils.verify(() -> CalendarUtils.toZonedDateTime((Calendar) any()));
            });
        }
    }
}

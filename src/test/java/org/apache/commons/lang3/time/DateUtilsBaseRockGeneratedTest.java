package org.apache.commons.lang3.time;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.text.ParseException;
import java.util.List;
import java.util.Locale;
import java.util.Iterator;
import java.util.Calendar;
import java.util.Date;
import org.mockito.MockedStatic;
import java.util.ArrayList;
import java.util.TimeZone;
import static org.mockito.Mockito.doNothing;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doReturn;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mockStatic;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class DateUtilsBaseRockGeneratedTest {

    private final Calendar calendarMock = mock(Calendar.class);

    private final Calendar calendarMock2 = mock(Calendar.class);

    private final Object objectMock = mock(Object.class, "date");

    //BaseRock generated method id: ${addDaysTest}, hash: 310C6A19B81797FE0C60FB76089DF3EE
    @Test()
    void addDaysTest() {
        //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        Date result = DateUtils.addDays(date, 0);
        Calendar calendar = Calendar.getInstance();
        Date date2 = calendar.getTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${addHoursTest}, hash: 7D66DDBB58D82B9987FE88B0C4C3427D
    @Test()
    void addHoursTest() {
        //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        Date result = DateUtils.addHours(date, 0);
        Calendar calendar = Calendar.getInstance();
        Date date2 = calendar.getTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${addMillisecondsTest}, hash: 67C61B316A01B09053ED7087D15E4786
    @Test()
    void addMillisecondsTest() {
        //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        Date result = DateUtils.addMilliseconds(date, 0);
        Calendar calendar = Calendar.getInstance();
        Date date2 = calendar.getTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${addMinutesTest}, hash: 2B3CE6464D924231B864DDD21DC135C0
    @Test()
    void addMinutesTest() {
        //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        Date result = DateUtils.addMinutes(date, 0);
        Calendar calendar = Calendar.getInstance();
        Date date2 = calendar.getTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${addMonthsTest}, hash: 5F25D56DFD741481CE128A9A4A3C5C0C
    @Test()
    void addMonthsTest() {
        //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        Date result = DateUtils.addMonths(date, 0);
        Calendar calendar = Calendar.getInstance();
        Date date2 = calendar.getTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${addSecondsTest}, hash: 6A443FE402676F52B6E826BFBD765172
    @Test()
    void addSecondsTest() {
        //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        Date result = DateUtils.addSeconds(date, 0);
        Calendar calendar = Calendar.getInstance();
        Date date2 = calendar.getTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${addWeeksTest}, hash: E145CE256757F60915D6E2FA102EFF27
    @Test()
    void addWeeksTest() {
        //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        Date result = DateUtils.addWeeks(date, 0);
        Calendar calendar = Calendar.getInstance();
        Date date2 = calendar.getTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${addYearsTest}, hash: C39D6AA27E388430E4DB03B569198385
    @Test()
    void addYearsTest() {
        //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        Date result = DateUtils.addYears(date, 0);
        Calendar calendar = Calendar.getInstance();
        Date date2 = calendar.getTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${ceilingWhenValGetCalendarYEARGreaterThan280000000ThrowsArithmeticException}, hash: 855624EECEEFA091F8FEE7F130A9A3A6
    @Test()
    void ceilingWhenValGetCalendarYEARGreaterThan280000000ThrowsArithmeticException() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : true  #  inside modify method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        ArithmeticException arithmeticException = new ArithmeticException("Calendar value too large for accurate calculations");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            DateUtils.ceiling(calendar, 0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${ceilingWhenFieldEqualsCalendarMILLISECOND}, hash: C4575CE2257AC544139DC9A5E8CDEA6F
    @Test()
    void ceilingWhenFieldEqualsCalendarMILLISECOND() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : true  #  inside modify method
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        Calendar result = DateUtils.ceiling(calendar, 14);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${ceilingWhenFieldNotEqualsCalendarAM_PM}, hash: 86DC29A51B367BCAA11A61A2CF1B118F
    @Test()
    void ceilingWhenFieldNotEqualsCalendarAM_PM() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : false  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (millisecs < 500) : true  #  inside modify method
         * (field == Calendar.SECOND) : true  #  inside modify method
         * (!done) : false  #  inside modify method
         * (field == Calendar.MINUTE) : false  #  inside modify method
         * (!done) : false  #  inside modify method
         * (date.getTime() != time) : true  #  inside modify method
         * (for-each(fields)) : true  #  inside modify method
         * (for-each(aField)) : true  #  inside modify method
         * (element == field) : false  #  inside modify method
         * (switch(field) = default) : true  #  inside modify method
         * (!offsetSet) : true  #  inside modify method
         * (offset > (max - min) / 2) : true  #  inside modify method
         * (offset != 0) : true  #  inside modify method
         * (modType == ModifyType.CEILING) : true  #  inside modify method
         * (field == SEMI_MONTH) : false  #  inside modify method
         * (field == Calendar.AM_PM) : false  #  inside modify method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        Calendar result = DateUtils.ceiling(calendar, 13);
        Calendar calendar2 = Calendar.getInstance();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(calendar2)));
    }

    //BaseRock generated method id: ${ceilingWhenFieldNotEqualsSEMI_MONTHAndFieldNotEqualsCalendarAM_PM}, hash: B55D18CCBBE518B2A3738CC937B879AA
    @Test()
    void ceilingWhenFieldNotEqualsSEMI_MONTHAndFieldNotEqualsCalendarAM_PM() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : false  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (millisecs < 500) : true  #  inside modify method
         * (field == Calendar.SECOND) : false  #  inside modify method
         * (!done) : true  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (seconds < 30) : true  #  inside modify method
         * (field == Calendar.MINUTE) : true  #  inside modify method
         * (!done) : false  #  inside modify method
         * (date.getTime() != time) : true  #  inside modify method
         * (for-each(fields)) : true  #  inside modify method
         * (for-each(aField)) : true  #  inside modify method
         * (element == field) : false  #  inside modify method
         * (switch(field) = default) : true  #  inside modify method
         * (!offsetSet) : true  #  inside modify method
         * (offset > (max - min) / 2) : true  #  inside modify method
         * (offset != 0) : true  #  inside modify method
         * (modType == ModifyType.CEILING) : true  #  inside modify method
         * (field == SEMI_MONTH) : false  #  inside modify method
         * (field == Calendar.AM_PM) : false  #  inside modify method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        Calendar result = DateUtils.ceiling(calendar, 12);
        Calendar calendar2 = Calendar.getInstance();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(calendar2)));
    }

    //BaseRock generated method id: ${ceiling1WhenFieldEqualsCalendarMILLISECOND}, hash: 84D634927DE6FA4D02A86E425AE6626B
    @Test()
    void ceiling1WhenFieldEqualsCalendarMILLISECOND() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : true  #  inside modify method
         */
         //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        Date result = DateUtils.ceiling(date, 14);
        Calendar calendar = Calendar.getInstance();
        Date date2 = calendar.getTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${ceiling1WhenFieldNotEqualsCalendarAM_PM}, hash: 8766C422C52308A3F3E62E6824CE4681
    @Test()
    void ceiling1WhenFieldNotEqualsCalendarAM_PM() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : false  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (millisecs < 500) : false  #  inside modify method
         * (field == Calendar.SECOND) : true  #  inside modify method
         * (!done) : false  #  inside modify method
         * (field == Calendar.MINUTE) : false  #  inside modify method
         * (!done) : false  #  inside modify method
         * (date.getTime() != time) : false  #  inside modify method
         * (for-each(fields)) : true  #  inside modify method
         * (for-each(aField)) : true  #  inside modify method
         * (element == field) : false  #  inside modify method
         * (switch(field) = default) : true  #  inside modify method
         * (!offsetSet) : true  #  inside modify method
         * (offset > (max - min) / 2) : true  #  inside modify method
         * (offset != 0) : true  #  inside modify method
         * (modType == ModifyType.CEILING) : true  #  inside modify method
         * (field == SEMI_MONTH) : false  #  inside modify method
         * (field == Calendar.AM_PM) : false  #  inside modify method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: c - Method: setTime
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        Date result = DateUtils.ceiling(date, 13);
        Calendar calendar = Calendar.getInstance();
        Date date2 = calendar.getTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${ceiling1WhenOffsetEquals0ThrowsIllegalArgumentException}, hash: A0BB61DBF8136C06CAF4F029BE01998D
    @Test()
    void ceiling1WhenOffsetEquals0ThrowsIllegalArgumentException() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : false  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (millisecs < 500) : true  #  inside modify method
         * (field == Calendar.SECOND) : false  #  inside modify method
         * (!done) : true  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (seconds < 30) : false  #  inside modify method
         * (field == Calendar.MINUTE) : false  #  inside modify method
         * (!done) : true  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (minutes < 30) : true  #  inside modify method
         * (date.getTime() != time) : true  #  inside modify method
         * (for-each(fields)) : true  #  inside modify method
         * (for-each(aField)) : true  #  inside modify method
         * (element == field) : false  #  inside modify method
         * (switch(field) = default) : true  #  inside modify method
         * (!offsetSet) : true  #  inside modify method
         * (offset > (max - min) / 2) : false  #  inside modify method
         * (offset != 0) : false  #  inside modify method
         */
         //Arrange Statement(s)
        Date date = new Date();
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The field 3 is not supported");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            DateUtils.ceiling(date, 3);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${ceiling1WhenFieldNotEqualsSEMI_MONTHAndFieldNotEqualsCalendarAM_PM}, hash: E489D6634586C3BBC9F10DB7B75F5EB5
    @Test()
    void ceiling1WhenFieldNotEqualsSEMI_MONTHAndFieldNotEqualsCalendarAM_PM() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : false  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (millisecs < 500) : false  #  inside modify method
         * (field == Calendar.SECOND) : false  #  inside modify method
         * (!done) : true  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (seconds < 30) : false  #  inside modify method
         * (field == Calendar.MINUTE) : true  #  inside modify method
         * (!done) : false  #  inside modify method
         * (date.getTime() != time) : false  #  inside modify method
         * (for-each(fields)) : true  #  inside modify method
         * (for-each(aField)) : true  #  inside modify method
         * (element == field) : false  #  inside modify method
         * (switch(field) = default) : true  #  inside modify method
         * (!offsetSet) : true  #  inside modify method
         * (offset > (max - min) / 2) : true  #  inside modify method
         * (offset != 0) : true  #  inside modify method
         * (modType == ModifyType.CEILING) : true  #  inside modify method
         * (field == SEMI_MONTH) : false  #  inside modify method
         * (field == Calendar.AM_PM) : false  #  inside modify method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: c - Method: setTime
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        Date result = DateUtils.ceiling(date, 12);
        Calendar calendar = Calendar.getInstance();
        Date date2 = calendar.getTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${ceiling1WhenOffsetNotEquals0ThrowsIllegalArgumentException}, hash: A8DAAC99218F3846C91A5997ED5CF0DA
    @Test()
    void ceiling1WhenOffsetNotEquals0ThrowsIllegalArgumentException() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : false  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (millisecs < 500) : false  #  inside modify method
         * (field == Calendar.SECOND) : false  #  inside modify method
         * (!done) : true  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (seconds < 30) : false  #  inside modify method
         * (field == Calendar.MINUTE) : false  #  inside modify method
         * (!done) : true  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (minutes < 30) : true  #  inside modify method
         * (date.getTime() != time) : true  #  inside modify method
         * (for-each(fields)) : true  #  inside modify method
         * (for-each(aField)) : true  #  inside modify method
         * (element == field) : false  #  inside modify method
         * (switch(field) = Calendar.AM_PM) : true  #  inside modify method
         * (aField[0] == Calendar.DATE) : false  #  inside modify method
         * (!offsetSet) : true  #  inside modify method
         * (offset > (max - min) / 2) : true  #  inside modify method
         * (offset != 0) : true  #  inside modify method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: c - Method: setTime
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Date date = new Date();
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The field 1001 is not supported");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            DateUtils.ceiling(date, 1001);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${ceiling1WhenOffsetGreaterThanMaxMinusMinDividedBy2AndOffsetNotEquals0ThrowsIllegalArgumentException}, hash: 479AC9C5B6FDCF09BD02F544DFE0B4ED
    @Test()
    void ceiling1WhenOffsetGreaterThanMaxMinusMinDividedBy2AndOffsetNotEquals0ThrowsIllegalArgumentException() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : false  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (millisecs < 500) : false  #  inside modify method
         * (field == Calendar.SECOND) : false  #  inside modify method
         * (!done) : true  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (seconds < 30) : false  #  inside modify method
         * (field == Calendar.MINUTE) : false  #  inside modify method
         * (!done) : true  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (minutes < 30) : true  #  inside modify method
         * (date.getTime() != time) : true  #  inside modify method
         * (for-each(fields)) : true  #  inside modify method
         * (for-each(aField)) : true  #  inside modify method
         * (element == field) : false  #  inside modify method
         * (switch(field) = SEMI_MONTH) : true  #  inside modify method
         * (aField[0] == Calendar.HOUR_OF_DAY) : false  #  inside modify method
         * (!offsetSet) : true  #  inside modify method
         * (offset > (max - min) / 2) : true  #  inside modify method
         * (offset != 0) : true  #  inside modify method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: c - Method: setTime
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Date date = new Date();
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The field 9 is not supported");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            DateUtils.ceiling(date, 9);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${ceiling1WhenModTypeEqualsModifyTypeCEILINGAndFieldNotEqualsSEMI_MONTHAndFieldNotEqualsCalendarAM_PM}, hash: 9426E5BDBF29205141D735261D46286A
    @Test()
    void ceiling1WhenModTypeEqualsModifyTypeCEILINGAndFieldNotEqualsSEMI_MONTHAndFieldNotEqualsCalendarAM_PM() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : false  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (millisecs < 500) : false  #  inside modify method
         * (field == Calendar.SECOND) : false  #  inside modify method
         * (!done) : true  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (seconds < 30) : false  #  inside modify method
         * (field == Calendar.MINUTE) : false  #  inside modify method
         * (!done) : true  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (minutes < 30) : true  #  inside modify method
         * (date.getTime() != time) : true  #  inside modify method
         * (for-each(fields)) : true  #  inside modify method
         * (for-each(aField)) : true  #  inside modify method
         * (element == field) : false  #  inside modify method
         * (switch(field) = default) : true  #  inside modify method
         * (!offsetSet) : true  #  inside modify method
         * (offset > (max - min) / 2) : true  #  inside modify method
         * (offset != 0) : true  #  inside modify method
         * (modType == ModifyType.CEILING) : true  #  inside modify method
         * (field == SEMI_MONTH) : false  #  inside modify method
         * (field == Calendar.AM_PM) : false  #  inside modify method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: c - Method: setTime
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        Date result = DateUtils.ceiling(date, 11);
        Calendar calendar = Calendar.getInstance();
        Date date2 = calendar.getTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${ceiling2WhenDateInstanceOfDate}, hash: B7AF00FE2DAACDC285065DF4A040BCF0
    @Test()
    void ceiling2WhenDateInstanceOfDate() {
        /* Branches:
         * (date instanceof Date) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<DateUtils> dateUtils = mockStatic(DateUtils.class, CALLS_REAL_METHODS)) {
            Date date = new Date();
            Date date2 = new Date();
            dateUtils.when(() -> DateUtils.ceiling(date2, 0)).thenReturn(date);
            //Act Statement(s)
            Date result = DateUtils.ceiling((Object) date2, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(date));
                dateUtils.verify(() -> DateUtils.ceiling(date2, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${ceiling2WhenDateInstanceOfCalendar}, hash: D10C3C8123CBCBC1E95A450C54263F39
    @Test()
    void ceiling2WhenDateInstanceOfCalendar() {
        /* Branches:
         * (date instanceof Date) : false
         * (date instanceof Calendar) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<DateUtils> dateUtils = mockStatic(DateUtils.class, CALLS_REAL_METHODS)) {
            Calendar calendar = Calendar.getInstance();
            dateUtils.when(() -> DateUtils.ceiling((Calendar) any(), eq(0))).thenReturn(calendar);
            Calendar calendar2 = Calendar.getInstance();
            //Act Statement(s)
            Date result = DateUtils.ceiling((Object) calendar2, 0);
            Date date = new Date();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(date));
                dateUtils.verify(() -> DateUtils.ceiling((Calendar) any(), eq(0)), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${ceiling2WhenDateNotInstanceOfCalendarThrowsClassCastException}, hash: D7BFC0DA96FCB082D675629327FED624
    @Test()
    void ceiling2WhenDateNotInstanceOfCalendarThrowsClassCastException() {
        /* Branches:
         * (date instanceof Date) : false
         * (date instanceof Calendar) : false
         */
         //Arrange Statement(s)
        Object object = new Object();
        ClassCastException classCastException = new ClassCastException("Could not find ceiling of for type: ");
        //Act Statement(s)
        final ClassCastException result = assertThrows(ClassCastException.class, () -> {
            DateUtils.ceiling(object, 0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(classCastException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getFragmentInDaysWhenSwitchFragmentCaseDefaultThrowsIllegalArgumentException}, hash: 1410DF05A397EF0EEF10CE9459F5F525
    @Test()
    void getFragmentInDaysWhenSwitchFragmentCaseDefaultThrowsIllegalArgumentException() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : true  #  inside getFragment method
         * (switch(fragment) = Calendar.MONTH) : true  #  inside getFragment method
         * (switch(fragment) = default) : true  #  inside getFragment method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The fragment 2 is not supported");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            DateUtils.getFragmentInDays(calendar, 2);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getFragmentInDaysWhenSwitchFragmentCaseCalendarYEARAndSwitchFragmentCaseCalendarYEAR}, hash: 6B0BF8645C51AF29D967AA92FD6C32CD
    @Test()
    void getFragmentInDaysWhenSwitchFragmentCaseCalendarYEARAndSwitchFragmentCaseCalendarYEAR() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : true  #  inside getFragment method
         * (switch(fragment) = Calendar.YEAR) : true  #  inside getFragment method
         * (switch(fragment) = Calendar.YEAR or switch(fragment) = Calendar.MONTH or switch(fragment) = Calendar.DAY_OF_YEAR or switch(fragment) = Calendar.DATE) : true  #  inside getFragment method
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        long result = DateUtils.getFragmentInDays(calendar, 1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(28L)));
    }

    //BaseRock generated method id: ${getFragmentInDaysWhenSwitchFragmentCaseCalendarMILLISECOND}, hash: 8EF745548D2070CE59F9E65763768BED
    @Test()
    void getFragmentInDaysWhenSwitchFragmentCaseCalendarMILLISECOND() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : true  #  inside getFragment method
         * (switch(fragment) = default) : true  #  inside getFragment method
         * (switch(fragment) = Calendar.MILLISECOND) : true  #  inside getFragment method
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        long result = DateUtils.getFragmentInDays(calendar, 14);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${getFragmentInDays1WhenSwitchFragmentCaseDefaultThrowsIllegalArgumentException}, hash: DA0F79467E7D8CD316A3E14AECEECE48
    @Test()
    void getFragmentInDays1WhenSwitchFragmentCaseDefaultThrowsIllegalArgumentException() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : true  #  inside getFragment method
         * (switch(fragment) = Calendar.MONTH) : true  #  inside getFragment method
         * (switch(fragment) = default) : true  #  inside getFragment method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: calendar - Method: setTime
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Date date = new Date();
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The fragment 2 is not supported");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            DateUtils.getFragmentInDays(date, 2);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getFragmentInDays1WhenSwitchFragmentCaseCalendarYEARAndSwitchFragmentCaseCalendarYEAR}, hash: 905EF35562EA8F832D1EAF7446E538A8
    @Test()
    void getFragmentInDays1WhenSwitchFragmentCaseCalendarYEARAndSwitchFragmentCaseCalendarYEAR() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : true  #  inside getFragment method
         * (switch(fragment) = Calendar.YEAR) : true  #  inside getFragment method
         * (switch(fragment) = Calendar.YEAR or switch(fragment) = Calendar.MONTH or switch(fragment) = Calendar.DAY_OF_YEAR or switch(fragment) = Calendar.DATE) : true  #  inside getFragment method
         */
         //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        long result = DateUtils.getFragmentInDays(date, 1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(28L)));
    }

    //BaseRock generated method id: ${getFragmentInDays1WhenSwitchFragmentCaseCalendarMILLISECOND}, hash: D2074511F7AD254C6A07EF5196460893
    @Test()
    void getFragmentInDays1WhenSwitchFragmentCaseCalendarMILLISECOND() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : true  #  inside getFragment method
         * (switch(fragment) = default) : true  #  inside getFragment method
         * (switch(fragment) = Calendar.MILLISECOND) : true  #  inside getFragment method
         */
         //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        long result = DateUtils.getFragmentInDays(date, 14);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${getFragmentInHoursWhenSwitchFragmentCaseDefaultThrowsIllegalArgumentException}, hash: 1783E8BCFD4EE3E7D76F1E5A4DE41F3C
    @Test()
    void getFragmentInHoursWhenSwitchFragmentCaseDefaultThrowsIllegalArgumentException() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : false  #  inside getFragment method
         * (switch(fragment) = Calendar.MONTH) : true  #  inside getFragment method
         * (switch(fragment) = default) : true  #  inside getFragment method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The fragment 2 is not supported");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            DateUtils.getFragmentInHours(calendar, 2);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getFragmentInHoursWhenSwitchFragmentCaseCalendarYEARAndSwitchFragmentCaseCalendarYEAR}, hash: 9B863C13B5207AA6A81616EAED572189
    @Test()
    void getFragmentInHoursWhenSwitchFragmentCaseCalendarYEARAndSwitchFragmentCaseCalendarYEAR() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : false  #  inside getFragment method
         * (switch(fragment) = Calendar.YEAR) : true  #  inside getFragment method
         * (switch(fragment) = Calendar.YEAR or switch(fragment) = Calendar.MONTH or switch(fragment) = Calendar.DAY_OF_YEAR or switch(fragment) = Calendar.DATE) : true  #  inside getFragment method
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        long result = DateUtils.getFragmentInHours(calendar, 1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(661L)));
    }

    //BaseRock generated method id: ${getFragmentInHoursWhenSwitchFragmentCaseCalendarMILLISECOND}, hash: 74D5E439CE6FF743B8679B21DB3B71D8
    @Test()
    void getFragmentInHoursWhenSwitchFragmentCaseCalendarMILLISECOND() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : false  #  inside getFragment method
         * (switch(fragment) = default) : true  #  inside getFragment method
         * (switch(fragment) = Calendar.MILLISECOND) : true  #  inside getFragment method
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        long result = DateUtils.getFragmentInHours(calendar, 14);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${getFragmentInHours1WhenSwitchFragmentCaseDefaultThrowsIllegalArgumentException}, hash: A4E209063E73B7268DBD72D75AC6E9B8
    @Test()
    void getFragmentInHours1WhenSwitchFragmentCaseDefaultThrowsIllegalArgumentException() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : false  #  inside getFragment method
         * (switch(fragment) = Calendar.MONTH) : true  #  inside getFragment method
         * (switch(fragment) = default) : true  #  inside getFragment method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: calendar - Method: setTime
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Date date = new Date();
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The fragment 2 is not supported");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            DateUtils.getFragmentInHours(date, 2);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getFragmentInHours1WhenSwitchFragmentCaseCalendarYEARAndSwitchFragmentCaseCalendarYEAR}, hash: 6FB4664CFAC0D26CF3743DFA406E45A0
    @Test()
    void getFragmentInHours1WhenSwitchFragmentCaseCalendarYEARAndSwitchFragmentCaseCalendarYEAR() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : false  #  inside getFragment method
         * (switch(fragment) = Calendar.YEAR) : true  #  inside getFragment method
         * (switch(fragment) = Calendar.YEAR or switch(fragment) = Calendar.MONTH or switch(fragment) = Calendar.DAY_OF_YEAR or switch(fragment) = Calendar.DATE) : true  #  inside getFragment method
         */
         //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        long result = DateUtils.getFragmentInHours(date, 1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(661L)));
    }

    //BaseRock generated method id: ${getFragmentInHours1WhenSwitchFragmentCaseCalendarMILLISECOND}, hash: 00323B3789457B77FE869AB70D4BDE6C
    @Test()
    void getFragmentInHours1WhenSwitchFragmentCaseCalendarMILLISECOND() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : false  #  inside getFragment method
         * (switch(fragment) = default) : true  #  inside getFragment method
         * (switch(fragment) = Calendar.MILLISECOND) : true  #  inside getFragment method
         */
         //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        long result = DateUtils.getFragmentInHours(date, 14);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${getFragmentInMillisecondsWhenSwitchFragmentCaseDefaultThrowsIllegalArgumentException}, hash: 6A1F3CCE6EBB3713A3D6B9F327063B84
    @Test()
    void getFragmentInMillisecondsWhenSwitchFragmentCaseDefaultThrowsIllegalArgumentException() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : false  #  inside getFragment method
         * (switch(fragment) = Calendar.MONTH) : true  #  inside getFragment method
         * (switch(fragment) = default) : true  #  inside getFragment method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The fragment 2 is not supported");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            DateUtils.getFragmentInMilliseconds(calendar, 2);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getFragmentInMillisecondsWhenSwitchFragmentCaseCalendarYEARAndSwitchFragmentCaseCalendarYEAR}, hash: BA4AC336853A1F15798DA049968D63F1
    @Test()
    void getFragmentInMillisecondsWhenSwitchFragmentCaseCalendarYEARAndSwitchFragmentCaseCalendarYEAR() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : false  #  inside getFragment method
         * (switch(fragment) = Calendar.YEAR) : true  #  inside getFragment method
         * (switch(fragment) = Calendar.YEAR or switch(fragment) = Calendar.MONTH or switch(fragment) = Calendar.DAY_OF_YEAR or switch(fragment) = Calendar.DATE) : true  #  inside getFragment method
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        long result = DateUtils.getFragmentInMilliseconds(calendar, 1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(2380130478L)));
    }

    //BaseRock generated method id: ${getFragmentInMillisecondsWhenSwitchFragmentCaseCalendarMILLISECOND}, hash: 045FB3BDC4543CFA88ED7190F3E054BF
    @Test()
    void getFragmentInMillisecondsWhenSwitchFragmentCaseCalendarMILLISECOND() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : false  #  inside getFragment method
         * (switch(fragment) = default) : true  #  inside getFragment method
         * (switch(fragment) = Calendar.MILLISECOND) : true  #  inside getFragment method
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        long result = DateUtils.getFragmentInMilliseconds(calendar, 14);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${getFragmentInMilliseconds1WhenSwitchFragmentCaseDefaultThrowsIllegalArgumentException}, hash: A47E780B96F643C3D6CD8821C734C2CB
    @Test()
    void getFragmentInMilliseconds1WhenSwitchFragmentCaseDefaultThrowsIllegalArgumentException() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : false  #  inside getFragment method
         * (switch(fragment) = Calendar.MONTH) : true  #  inside getFragment method
         * (switch(fragment) = default) : true  #  inside getFragment method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: calendar - Method: setTime
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Date date = new Date();
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The fragment 2 is not supported");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            DateUtils.getFragmentInMilliseconds(date, 2);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getFragmentInMilliseconds1WhenSwitchFragmentCaseCalendarYEARAndSwitchFragmentCaseCalendarYEAR}, hash: 9FC5652AB820192369CB0F99318C4E6F
    @Test()
    void getFragmentInMilliseconds1WhenSwitchFragmentCaseCalendarYEARAndSwitchFragmentCaseCalendarYEAR() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : false  #  inside getFragment method
         * (switch(fragment) = Calendar.YEAR) : true  #  inside getFragment method
         * (switch(fragment) = Calendar.YEAR or switch(fragment) = Calendar.MONTH or switch(fragment) = Calendar.DAY_OF_YEAR or switch(fragment) = Calendar.DATE) : true  #  inside getFragment method
         */
         //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        long result = DateUtils.getFragmentInMilliseconds(date, 1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(2380130553L)));
    }

    //BaseRock generated method id: ${getFragmentInMilliseconds1WhenSwitchFragmentCaseCalendarMILLISECOND}, hash: 4E7AA0D28D30346DBC37CF933D200FEC
    @Test()
    void getFragmentInMilliseconds1WhenSwitchFragmentCaseCalendarMILLISECOND() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : false  #  inside getFragment method
         * (switch(fragment) = default) : true  #  inside getFragment method
         * (switch(fragment) = Calendar.MILLISECOND) : true  #  inside getFragment method
         */
         //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        long result = DateUtils.getFragmentInMilliseconds(date, 14);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${getFragmentInMinutesWhenSwitchFragmentCaseDefaultThrowsIllegalArgumentException}, hash: 6F723B9CC00D94415CCF514767D54B28
    @Test()
    void getFragmentInMinutesWhenSwitchFragmentCaseDefaultThrowsIllegalArgumentException() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : false  #  inside getFragment method
         * (switch(fragment) = Calendar.MONTH) : true  #  inside getFragment method
         * (switch(fragment) = default) : true  #  inside getFragment method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The fragment 2 is not supported");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            DateUtils.getFragmentInMinutes(calendar, 2);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getFragmentInMinutesWhenSwitchFragmentCaseCalendarYEARAndSwitchFragmentCaseCalendarYEAR}, hash: D37D4CCD68BB0D5A6B9F13706ABD855E
    @Test()
    void getFragmentInMinutesWhenSwitchFragmentCaseCalendarYEARAndSwitchFragmentCaseCalendarYEAR() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : false  #  inside getFragment method
         * (switch(fragment) = Calendar.YEAR) : true  #  inside getFragment method
         * (switch(fragment) = Calendar.YEAR or switch(fragment) = Calendar.MONTH or switch(fragment) = Calendar.DAY_OF_YEAR or switch(fragment) = Calendar.DATE) : true  #  inside getFragment method
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        long result = DateUtils.getFragmentInMinutes(calendar, 1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(39668L)));
    }

    //BaseRock generated method id: ${getFragmentInMinutesWhenSwitchFragmentCaseCalendarMILLISECOND}, hash: 1BD217727DF2746AD1513FFAC2D5CA66
    @Test()
    void getFragmentInMinutesWhenSwitchFragmentCaseCalendarMILLISECOND() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : false  #  inside getFragment method
         * (switch(fragment) = default) : true  #  inside getFragment method
         * (switch(fragment) = Calendar.MILLISECOND) : true  #  inside getFragment method
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        long result = DateUtils.getFragmentInMinutes(calendar, 14);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${getFragmentInMinutes1WhenSwitchFragmentCaseDefaultThrowsIllegalArgumentException}, hash: DE7D80872579B49C39AF565B99FAE7BB
    @Test()
    void getFragmentInMinutes1WhenSwitchFragmentCaseDefaultThrowsIllegalArgumentException() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : false  #  inside getFragment method
         * (switch(fragment) = Calendar.MONTH) : true  #  inside getFragment method
         * (switch(fragment) = default) : true  #  inside getFragment method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: calendar - Method: setTime
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Date date = new Date();
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The fragment 2 is not supported");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            DateUtils.getFragmentInMinutes(date, 2);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getFragmentInMinutes1WhenSwitchFragmentCaseCalendarYEARAndSwitchFragmentCaseCalendarYEAR}, hash: 2DAE2EFA264895C298FAD3F52687905F
    @Test()
    void getFragmentInMinutes1WhenSwitchFragmentCaseCalendarYEARAndSwitchFragmentCaseCalendarYEAR() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : false  #  inside getFragment method
         * (switch(fragment) = Calendar.YEAR) : true  #  inside getFragment method
         * (switch(fragment) = Calendar.YEAR or switch(fragment) = Calendar.MONTH or switch(fragment) = Calendar.DAY_OF_YEAR or switch(fragment) = Calendar.DATE) : true  #  inside getFragment method
         */
         //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        long result = DateUtils.getFragmentInMinutes(date, 1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(39668L)));
    }

    //BaseRock generated method id: ${getFragmentInMinutes1WhenSwitchFragmentCaseCalendarMILLISECOND}, hash: 7C44FF39A9C4126DD6291BB248701985
    @Test()
    void getFragmentInMinutes1WhenSwitchFragmentCaseCalendarMILLISECOND() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : false  #  inside getFragment method
         * (switch(fragment) = default) : true  #  inside getFragment method
         * (switch(fragment) = Calendar.MILLISECOND) : true  #  inside getFragment method
         */
         //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        long result = DateUtils.getFragmentInMinutes(date, 14);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${getFragmentInSecondsWhenSwitchFragmentCaseDefaultThrowsIllegalArgumentException}, hash: DED6DD8EA6DE5F2796771731D4677FF8
    @Test()
    void getFragmentInSecondsWhenSwitchFragmentCaseDefaultThrowsIllegalArgumentException() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : false  #  inside getFragment method
         * (switch(fragment) = Calendar.MONTH) : true  #  inside getFragment method
         * (switch(fragment) = default) : true  #  inside getFragment method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The fragment 2 is not supported");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            DateUtils.getFragmentInSeconds(calendar, 2);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getFragmentInSecondsWhenSwitchFragmentCaseCalendarYEARAndSwitchFragmentCaseCalendarYEAR}, hash: 42D058B40231C75E9F20F9770FBF18EF
    @Test()
    void getFragmentInSecondsWhenSwitchFragmentCaseCalendarYEARAndSwitchFragmentCaseCalendarYEAR() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : false  #  inside getFragment method
         * (switch(fragment) = Calendar.YEAR) : true  #  inside getFragment method
         * (switch(fragment) = Calendar.YEAR or switch(fragment) = Calendar.MONTH or switch(fragment) = Calendar.DAY_OF_YEAR or switch(fragment) = Calendar.DATE) : true  #  inside getFragment method
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        long result = DateUtils.getFragmentInSeconds(calendar, 1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(2380130L)));
    }

    //BaseRock generated method id: ${getFragmentInSecondsWhenSwitchFragmentCaseCalendarMILLISECOND}, hash: 2F1A336DCFDECCEB714BAEB3A5947CC2
    @Test()
    void getFragmentInSecondsWhenSwitchFragmentCaseCalendarMILLISECOND() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : false  #  inside getFragment method
         * (switch(fragment) = default) : true  #  inside getFragment method
         * (switch(fragment) = Calendar.MILLISECOND) : true  #  inside getFragment method
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        long result = DateUtils.getFragmentInSeconds(calendar, 14);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${getFragmentInSeconds1WhenSwitchFragmentCaseDefaultThrowsIllegalArgumentException}, hash: 16B3D2C82D202982E79A042DF6C8C6A1
    @Test()
    void getFragmentInSeconds1WhenSwitchFragmentCaseDefaultThrowsIllegalArgumentException() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : false  #  inside getFragment method
         * (switch(fragment) = Calendar.MONTH) : true  #  inside getFragment method
         * (switch(fragment) = default) : true  #  inside getFragment method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: calendar - Method: setTime
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Date date = new Date();
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The fragment 2 is not supported");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            DateUtils.getFragmentInSeconds(date, 2);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getFragmentInSeconds1WhenSwitchFragmentCaseCalendarYEARAndSwitchFragmentCaseCalendarYEAR}, hash: EBA8BDD3BF8FCFABC424DBAF9581FD9E
    @Test()
    void getFragmentInSeconds1WhenSwitchFragmentCaseCalendarYEARAndSwitchFragmentCaseCalendarYEAR() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : false  #  inside getFragment method
         * (switch(fragment) = Calendar.YEAR) : true  #  inside getFragment method
         * (switch(fragment) = Calendar.YEAR or switch(fragment) = Calendar.MONTH or switch(fragment) = Calendar.DAY_OF_YEAR or switch(fragment) = Calendar.DATE) : true  #  inside getFragment method
         */
         //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        long result = DateUtils.getFragmentInSeconds(date, 1);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(2380130L)));
    }

    //BaseRock generated method id: ${getFragmentInSeconds1WhenSwitchFragmentCaseCalendarMILLISECOND}, hash: DF25DF8AA4DCA548FF5DC25AD527A5B8
    @Test()
    void getFragmentInSeconds1WhenSwitchFragmentCaseCalendarMILLISECOND() {
        /* Branches:
         * (unit == TimeUnit.DAYS) : false  #  inside getFragment method
         * (switch(fragment) = default) : true  #  inside getFragment method
         * (switch(fragment) = Calendar.MILLISECOND) : true  #  inside getFragment method
         */
         //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        long result = DateUtils.getFragmentInSeconds(date, 14);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${isSameDayWhenCal1GetCalendarDAY_OF_YEAREqualsCal2GetCalendarDAY_OF_YEAR}, hash: 6005B7062AC58752508D3A2707A23F06
    @Test()
    void isSameDayWhenCal1GetCalendarDAY_OF_YEAREqualsCal2GetCalendarDAY_OF_YEAR() {
        /* Branches:
         * (cal1.get(Calendar.ERA) == cal2.get(Calendar.ERA)) : true
         * (cal1.get(Calendar.YEAR) == cal2.get(Calendar.YEAR)) : true
         * (cal1.get(Calendar.DAY_OF_YEAR) == cal2.get(Calendar.DAY_OF_YEAR)) : true
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        Calendar calendar2 = Calendar.getInstance();
        
        //Act Statement(s)
        boolean result = DateUtils.isSameDay(calendar, calendar2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isSameDayWhenCal1GetCalendarDAY_OF_YEARNotEqualsCal2GetCalendarDAY_OF_YEAR}, hash: DEF79CD22797B5A5CC94ED34A27913C0
    @Test()
    void isSameDayWhenCal1GetCalendarDAY_OF_YEARNotEqualsCal2GetCalendarDAY_OF_YEAR() {
        /* Branches:
         * (cal1.get(Calendar.ERA) == cal2.get(Calendar.ERA)) : true
         * (cal1.get(Calendar.YEAR) == cal2.get(Calendar.YEAR)) : true
         * (cal1.get(Calendar.DAY_OF_YEAR) == cal2.get(Calendar.DAY_OF_YEAR)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        Calendar calendar2 = Calendar.getInstance();
        
        //Act Statement(s)
        boolean result = DateUtils.isSameDay(calendar, calendar2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isSameDay1WhenIsSameDayToCalendarDate1ToCalendarDate2}, hash: CA3F738FF3E7428EB4716CF378A18FA8
    @Test()
    void isSameDay1WhenIsSameDayToCalendarDate1ToCalendarDate2() {
        /* Branches:
         * (isSameDay(toCalendar(date1), toCalendar(date2))) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<DateUtils> dateUtils = mockStatic(DateUtils.class, CALLS_REAL_METHODS)) {
            dateUtils.when(() -> DateUtils.isSameDay((Calendar) any(), (Calendar) any())).thenReturn(true);
            Date date = new Date();
            Date date2 = new Date();
            //Act Statement(s)
            boolean result = DateUtils.isSameDay(date, date2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                dateUtils.verify(() -> DateUtils.isSameDay((Calendar) any(), (Calendar) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSameDay1WhenIsSameDayNotToCalendarDate1ToCalendarDate2}, hash: 05838538CC1013BE184F61149D06C676
    @Test()
    void isSameDay1WhenIsSameDayNotToCalendarDate1ToCalendarDate2() {
        /* Branches:
         * (isSameDay(toCalendar(date1), toCalendar(date2))) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<DateUtils> dateUtils = mockStatic(DateUtils.class, CALLS_REAL_METHODS)) {
            dateUtils.when(() -> DateUtils.isSameDay((Calendar) any(), (Calendar) any())).thenReturn(false);
            Date date = new Date();
            Date date2 = new Date();
            //Act Statement(s)
            boolean result = DateUtils.isSameDay(date, date2);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                dateUtils.verify(() -> DateUtils.isSameDay((Calendar) any(), (Calendar) any()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${isSameInstantWhenCal1GetTimeGetTimeEqualsCal2GetTimeGetTime}, hash: 9E616AD8A51CCB90059DB8C32A8AB883
    @Test()
    void isSameInstantWhenCal1GetTimeGetTimeEqualsCal2GetTimeGetTime() {
        /* Branches:
         * (cal1.getTime().getTime() == cal2.getTime().getTime()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        Calendar calendar2 = Calendar.getInstance();
        
        //Act Statement(s)
        boolean result = DateUtils.isSameInstant(calendar, calendar2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isSameInstantWhenCal1GetTimeGetTimeNotEqualsCal2GetTimeGetTime}, hash: E821F5200858A975EBA40FCD4453CE89
    @Test()
    void isSameInstantWhenCal1GetTimeGetTimeNotEqualsCal2GetTimeGetTime() {
        /* Branches:
         * (cal1.getTime().getTime() == cal2.getTime().getTime()) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        Calendar calendar2 = Calendar.getInstance();
        
        //Act Statement(s)
        boolean result = DateUtils.isSameInstant(calendar, calendar2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isSameInstant1WhenDate1GetTimeEqualsDate2GetTime}, hash: EC2617B46B90BC30A4E8CF5D160082A9
    @Test()
    void isSameInstant1WhenDate1GetTimeEqualsDate2GetTime() {
        /* Branches:
         * (date1.getTime() == date2.getTime()) : true
         */
         //Arrange Statement(s)
        Date date = new Date();
        Date date2 = new Date();
        
        //Act Statement(s)
        boolean result = DateUtils.isSameInstant(date, date2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isSameInstant1WhenDate1GetTimeNotEqualsDate2GetTime}, hash: EA7A1A7B63A4E91B58AB3DC7A406E7A6
    @Test()
    void isSameInstant1WhenDate1GetTimeNotEqualsDate2GetTime() {
        /* Branches:
         * (date1.getTime() == date2.getTime()) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Date date = new Date();
        Date date2 = new Date();
        
        //Act Statement(s)
        boolean result = DateUtils.isSameInstant(date, date2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isSameLocalTimeWhenCal1GetClassEqualsCal2GetClass}, hash: DAEF1A316F97A856145869D2665C8A6D
    @Test()
    void isSameLocalTimeWhenCal1GetClassEqualsCal2GetClass() {
        /* Branches:
         * (cal1.get(Calendar.MILLISECOND) == cal2.get(Calendar.MILLISECOND)) : true
         * (cal1.get(Calendar.SECOND) == cal2.get(Calendar.SECOND)) : true
         * (cal1.get(Calendar.MINUTE) == cal2.get(Calendar.MINUTE)) : true
         * (cal1.get(Calendar.HOUR_OF_DAY) == cal2.get(Calendar.HOUR_OF_DAY)) : true
         * (cal1.get(Calendar.DAY_OF_YEAR) == cal2.get(Calendar.DAY_OF_YEAR)) : true
         * (cal1.get(Calendar.YEAR) == cal2.get(Calendar.YEAR)) : true
         * (cal1.get(Calendar.ERA) == cal2.get(Calendar.ERA)) : true
         * (cal1.getClass() == cal2.getClass()) : true
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        Calendar calendar2 = Calendar.getInstance();
        
        //Act Statement(s)
        boolean result = DateUtils.isSameLocalTime(calendar, calendar2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${isSameLocalTimeWhenCal1GetClassNotEqualsCal2GetClass}, hash: 4D75BFF9B40F56423ED6DC5053F69730
    @Test()
    void isSameLocalTimeWhenCal1GetClassNotEqualsCal2GetClass() {
        /* Branches:
         * (cal1.get(Calendar.MILLISECOND) == cal2.get(Calendar.MILLISECOND)) : true
         * (cal1.get(Calendar.SECOND) == cal2.get(Calendar.SECOND)) : true
         * (cal1.get(Calendar.MINUTE) == cal2.get(Calendar.MINUTE)) : true
         * (cal1.get(Calendar.HOUR_OF_DAY) == cal2.get(Calendar.HOUR_OF_DAY)) : true
         * (cal1.get(Calendar.DAY_OF_YEAR) == cal2.get(Calendar.DAY_OF_YEAR)) : true
         * (cal1.get(Calendar.YEAR) == cal2.get(Calendar.YEAR)) : true
         * (cal1.get(Calendar.ERA) == cal2.get(Calendar.ERA)) : true
         * (cal1.getClass() == cal2.getClass()) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        Calendar calendar2 = Calendar.getInstance();
        
        //Act Statement(s)
        boolean result = DateUtils.isSameLocalTime(calendar, calendar2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${iteratorWhenSwitchRangeStyleCaseDefaultThrowsIllegalArgumentException}, hash: D16A4FFF395A50BD6AA788E169388B22
    @Test()
    void iteratorWhenSwitchRangeStyleCaseDefaultThrowsIllegalArgumentException() {
        /* Branches:
         * (switch(rangeStyle) = default) : true
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The range style 8 is not valid.");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            DateUtils.iterator(calendar, 8);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${iteratorWhenEndGetCalendarDAY_OF_WEEKNotEqualsEndCutoff}, hash: 0DA5319DCDFD7A54479A464ABEB68A8F
    @Test()
    void iteratorWhenEndGetCalendarDAY_OF_WEEKNotEqualsEndCutoff() {
        /* Branches:
         * (switch(rangeStyle) = RANGE_MONTH_SUNDAY or switch(rangeStyle) = RANGE_MONTH_MONDAY) : true
         * (switch(rangeStyle) = RANGE_WEEK_SUNDAY) : true
         * (startCutoff < Calendar.SUNDAY) : false
         * (startCutoff > Calendar.SATURDAY) : false
         * (endCutoff < Calendar.SUNDAY) : false
         * (endCutoff > Calendar.SATURDAY) : false
         * (start.get(Calendar.DAY_OF_WEEK) != startCutoff) : true
         * (end.get(Calendar.DAY_OF_WEEK) != endCutoff) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<DateUtils> dateUtils = mockStatic(DateUtils.class, CALLS_REAL_METHODS)) {
            doNothing().when(calendarMock).add(5, -1);
            doReturn(-1, 0).when(calendarMock).get(7);
            dateUtils.when(() -> DateUtils.truncate((Calendar) any(), eq(5))).thenReturn(calendarMock).thenReturn(calendarMock2);
            doNothing().when(calendarMock2).add(5, 1);
            doReturn(1, 0).when(calendarMock2).get(7);
            Calendar calendar = Calendar.getInstance();
            //Act Statement(s)
            Iterator<Calendar> result = DateUtils.iterator(calendar, 1);
            //Assert statement(s)
            //TODO: Please implement equals method in DateIterator for verification of the entire object or you need to adjust respective assertion statements
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                dateUtils.verify(() -> DateUtils.truncate((Calendar) any(), eq(5)), atLeast(2));
                verify(calendarMock, atLeast(2)).get(7);
                verify(calendarMock, atLeast(1)).add(5, -1);
                verify(calendarMock2, atLeast(2)).get(7);
                verify(calendarMock2, atLeast(1)).add(5, 1);
            });
        }
    }

    //BaseRock generated method id: ${iteratorWhenStartGetCalendarDAY_OF_WEEKNotEqualsStartCutoffAndEndGetCalendarDAY_OF_WEEKNotEqualsEndCutoff}, hash: 3107B9A27DB2055FFAEA99599E9FA084
    @Test()
    void iteratorWhenStartGetCalendarDAY_OF_WEEKNotEqualsStartCutoffAndEndGetCalendarDAY_OF_WEEKNotEqualsEndCutoff() {
        /* Branches:
         * (switch(rangeStyle) = RANGE_WEEK_SUNDAY or switch(rangeStyle) = RANGE_WEEK_MONDAY or switch(rangeStyle) = RANGE_WEEK_RELATIVE or switch(rangeStyle) = RANGE_WEEK_CENTER) : true
         * (rangeStyle == RANGE_MONTH_MONDAY) : false
         * (startCutoff < Calendar.SUNDAY) : false
         * (startCutoff > Calendar.SATURDAY) : false
         * (endCutoff < Calendar.SUNDAY) : false
         * (endCutoff > Calendar.SATURDAY) : false
         * (start.get(Calendar.DAY_OF_WEEK) != startCutoff) : true
         * (end.get(Calendar.DAY_OF_WEEK) != endCutoff) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<DateUtils> dateUtils = mockStatic(DateUtils.class, CALLS_REAL_METHODS)) {
            dateUtils.when(() -> DateUtils.truncate((Calendar) any(), eq(2))).thenReturn(calendarMock);
            doReturn(calendarMock2).when(calendarMock).clone();
            doNothing().when(calendarMock2).add(2, 1);
            doNothing().when(calendarMock2).add(5, -1);
            doNothing().when(calendarMock2).add(5, 1);
            doReturn(1, 0).when(calendarMock2).get(7);
            doNothing().when(calendarMock).add(5, -1);
            doReturn(-1, 0).when(calendarMock).get(7);
            Calendar calendar = Calendar.getInstance();
            //Act Statement(s)
            Iterator<Calendar> result = DateUtils.iterator(calendar, 5);
            //Assert statement(s)
            //TODO: Please implement equals method in DateIterator for verification of the entire object or you need to adjust respective assertion statements
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                dateUtils.verify(() -> DateUtils.truncate((Calendar) any(), eq(2)), atLeast(1));
                verify(calendarMock, atLeast(1)).clone();
                verify(calendarMock2, atLeast(1)).add(2, 1);
                verify(calendarMock2, atLeast(1)).add(5, -1);
                verify(calendarMock2, atLeast(2)).get(7);
                verify(calendarMock2, atLeast(1)).add(5, 1);
                verify(calendarMock, atLeast(2)).get(7);
                verify(calendarMock, atLeast(1)).add(5, -1);
            });
        }
    }

    //BaseRock generated method id: ${iterator1Test}, hash: D0D63B275DDC27EBBBD1DD1DAF00889E
    @Test()
    void iterator1Test() {
        //Arrange Statement(s)
        try (MockedStatic<DateUtils> dateUtils = mockStatic(DateUtils.class, CALLS_REAL_METHODS)) {
            List<Calendar> calendarList = new ArrayList<>();
            Iterator<Calendar> iterator = calendarList.iterator();
            dateUtils.when(() -> DateUtils.iterator((Calendar) any(), eq(0))).thenReturn(iterator);
            Date date = new Date();
            //Act Statement(s)
            Iterator<Calendar> result = DateUtils.iterator(date, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                dateUtils.verify(() -> DateUtils.iterator((Calendar) any(), eq(0)), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${iterator2WhenCalendarInstanceOfDate}, hash: 749F050D0541647F6D8D41413080615F
    @Test()
    void iterator2WhenCalendarInstanceOfDate() {
        /* Branches:
         * (calendar instanceof Date) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<DateUtils> dateUtils = mockStatic(DateUtils.class, CALLS_REAL_METHODS)) {
            List<Calendar> calendarList = new ArrayList<>();
            Iterator<Calendar> iterator = calendarList.iterator();
            Date date = new Date();
            dateUtils.when(() -> DateUtils.iterator(date, 0)).thenReturn(iterator);
            //Act Statement(s)
            Iterator<?> result = DateUtils.iterator((Object) date, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                dateUtils.verify(() -> DateUtils.iterator(date, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${iterator2WhenCalendarInstanceOfCalendar}, hash: F19981FF445E9D0449288230370AE1A2
    @Test()
    void iterator2WhenCalendarInstanceOfCalendar() {
        /* Branches:
         * (calendar instanceof Date) : false
         * (calendar instanceof Calendar) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<DateUtils> dateUtils = mockStatic(DateUtils.class, CALLS_REAL_METHODS)) {
            List<Calendar> calendarList = new ArrayList<>();
            Iterator<Calendar> iterator = calendarList.iterator();
            dateUtils.when(() -> DateUtils.iterator((Calendar) any(), eq(0))).thenReturn(iterator);
            Calendar calendar = Calendar.getInstance();
            //Act Statement(s)
            Iterator<?> result = DateUtils.iterator((Object) calendar, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                dateUtils.verify(() -> DateUtils.iterator((Calendar) any(), eq(0)), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${iterator2WhenCalendarNotInstanceOfCalendarThrowsClassCastException}, hash: DD4E2EE498F84B9747C2F7D8B481BFD2
    @Test()
    void iterator2WhenCalendarNotInstanceOfCalendarThrowsClassCastException() {
        /* Branches:
         * (calendar instanceof Date) : false
         * (calendar instanceof Calendar) : false
         */
         //Arrange Statement(s)
        Object objectMock = mock(Object.class, "calendar");
        ClassCastException classCastException = new ClassCastException("Could not iterate based on calendar");
        //Act Statement(s)
        final ClassCastException result = assertThrows(ClassCastException.class, () -> {
            DateUtils.iterator(objectMock, 0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(classCastException.getMessage()));
        });
    }

    //BaseRock generated method id: ${parseDateWhenPosGetIndexEqualsDateStrLength}, hash: 44D6236C273D29C267594796834AAB72
    @Test()
    void parseDateWhenPosGetIndexEqualsDateStrLength() throws ParseException {
        /* Branches:
         * (for-each(parsePatterns)) : true  #  inside parseDateWithLeniency method
         * (fdp.parse(dateStr, pos, calendar)) : true  #  inside parseDateWithLeniency method
         * (pos.getIndex() == dateStr.length()) : true  #  inside parseDateWithLeniency method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Locale locale = new Locale("language1");
        String[] stringArray = new String[] { "parsePatternsItem1" };
        
        //Act Statement(s)
        Date result = DateUtils.parseDate("", locale, stringArray);
        Date date = new Date();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date)));
    }

    //BaseRock generated method id: ${parseDateWhenPosGetIndexNotEqualsDateStrLengthThrowsParseException}, hash: 1021EEEFA175F5B291ADE08D6F4E5CB0
    @Test()
    void parseDateWhenPosGetIndexNotEqualsDateStrLengthThrowsParseException() throws ParseException {
        /* Branches:
         * (for-each(parsePatterns)) : true  #  inside parseDateWithLeniency method
         * (fdp.parse(dateStr, pos, calendar)) : true  #  inside parseDateWithLeniency method
         * (pos.getIndex() == dateStr.length()) : false  #  inside parseDateWithLeniency method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Locale locale = new Locale("language1");
        String[] stringArray = new String[] { "parsePatternsItem1" };
        ParseException parseException = new ParseException("Unable to parse the date: A", -1);
        //Act Statement(s)
        final ParseException result = assertThrows(ParseException.class, () -> {
            DateUtils.parseDate("A", locale, stringArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(parseException.getMessage()));
        });
    }

    //BaseRock generated method id: ${parseDateWhenCaughtIllegalArgumentExceptionThrowsParseException}, hash: D0CF8191B889F856534623CAA72B0CE1
    @Test()
    void parseDateWhenCaughtIllegalArgumentExceptionThrowsParseException() throws ParseException {
        /* Branches:
         * (for-each(parsePatterns)) : true  #  inside parseDateWithLeniency method
         * (fdp.parse(dateStr, pos, calendar)) : true  #  inside parseDateWithLeniency method
         * (pos.getIndex() == dateStr.length()) : true  #  inside parseDateWithLeniency method
         * (catch-exception (IllegalArgumentException)) : true  #  inside parseDateWithLeniency method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Locale locale = new Locale("language1");
        String[] stringArray = new String[] { "parsePatternsItem1" };
        ParseException parseException = new ParseException("Unable to parse the date: ", -1);
        //Act Statement(s)
        final ParseException result = assertThrows(ParseException.class, () -> {
            DateUtils.parseDate("", locale, stringArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(parseException.getMessage()));
        });
    }

    //BaseRock generated method id: ${parseDate1Test}, hash: 5E8DB17DB47CA3FFFFFD5FDA76F43BBC
    @Test()
    void parseDate1Test() throws ParseException {
        //Arrange Statement(s)
        try (MockedStatic<DateUtils> dateUtils = mockStatic(DateUtils.class, CALLS_REAL_METHODS)) {
            Date date = new Date();
            String[] stringArray = new String[] {};
            dateUtils.when(() -> DateUtils.parseDate("str1", (Locale) null, stringArray)).thenReturn(date);
            //Act Statement(s)
            Date result = DateUtils.parseDate("str1", stringArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(date));
                dateUtils.verify(() -> DateUtils.parseDate("str1", (Locale) null, stringArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${parseDateStrictlyWhenPosGetIndexEqualsDateStrLength}, hash: 5FFCE0139F2911E099E042DECB9E15AF
    @Test()
    void parseDateStrictlyWhenPosGetIndexEqualsDateStrLength() throws ParseException {
        /* Branches:
         * (for-each(parsePatterns)) : true  #  inside parseDateWithLeniency method
         * (fdp.parse(dateStr, pos, calendar)) : true  #  inside parseDateWithLeniency method
         * (pos.getIndex() == dateStr.length()) : true  #  inside parseDateWithLeniency method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Locale locale = new Locale("language1");
        String[] stringArray = new String[] { "parsePatternsItem1" };
        
        //Act Statement(s)
        Date result = DateUtils.parseDateStrictly("", locale, stringArray);
        Date date = new Date();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date)));
    }

    //BaseRock generated method id: ${parseDateStrictlyWhenPosGetIndexNotEqualsDateStrLengthThrowsParseException}, hash: 8805AA3022262897E2BA27FD94A48323
    @Test()
    void parseDateStrictlyWhenPosGetIndexNotEqualsDateStrLengthThrowsParseException() throws ParseException {
        /* Branches:
         * (for-each(parsePatterns)) : true  #  inside parseDateWithLeniency method
         * (fdp.parse(dateStr, pos, calendar)) : true  #  inside parseDateWithLeniency method
         * (pos.getIndex() == dateStr.length()) : false  #  inside parseDateWithLeniency method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Locale locale = new Locale("language1");
        String[] stringArray = new String[] { "parsePatternsItem1" };
        ParseException parseException = new ParseException("Unable to parse the date: A", -1);
        //Act Statement(s)
        final ParseException result = assertThrows(ParseException.class, () -> {
            DateUtils.parseDateStrictly("A", locale, stringArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(parseException.getMessage()));
        });
    }

    //BaseRock generated method id: ${parseDateStrictlyWhenCaughtIllegalArgumentExceptionThrowsParseException}, hash: 7CF033D1E12F1715D62B33980F9D03ED
    @Test()
    void parseDateStrictlyWhenCaughtIllegalArgumentExceptionThrowsParseException() throws ParseException {
        /* Branches:
         * (for-each(parsePatterns)) : true  #  inside parseDateWithLeniency method
         * (fdp.parse(dateStr, pos, calendar)) : true  #  inside parseDateWithLeniency method
         * (pos.getIndex() == dateStr.length()) : true  #  inside parseDateWithLeniency method
         * (catch-exception (IllegalArgumentException)) : true  #  inside parseDateWithLeniency method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Locale locale = new Locale("language1");
        String[] stringArray = new String[] { "parsePatternsItem1" };
        ParseException parseException = new ParseException("Unable to parse the date: ", -1);
        //Act Statement(s)
        final ParseException result = assertThrows(ParseException.class, () -> {
            DateUtils.parseDateStrictly("", locale, stringArray);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(parseException.getMessage()));
        });
    }

    //BaseRock generated method id: ${parseDateStrictly1Test}, hash: F5FC1338DD63A8B6381C43E8C27EBA87
    @Test()
    void parseDateStrictly1Test() throws ParseException {
        //Arrange Statement(s)
        try (MockedStatic<DateUtils> dateUtils = mockStatic(DateUtils.class, CALLS_REAL_METHODS)) {
            Date date = new Date();
            String[] stringArray = new String[] {};
            dateUtils.when(() -> DateUtils.parseDateStrictly("str1", (Locale) null, stringArray)).thenReturn(date);
            //Act Statement(s)
            Date result = DateUtils.parseDateStrictly("str1", stringArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(date));
                dateUtils.verify(() -> DateUtils.parseDateStrictly("str1", (Locale) null, stringArray), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${roundWhenValGetCalendarYEARGreaterThan280000000ThrowsArithmeticException}, hash: 78B9C780924CE73D579A79E8251028E3
    @Test()
    void roundWhenValGetCalendarYEARGreaterThan280000000ThrowsArithmeticException() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : true  #  inside modify method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        ArithmeticException arithmeticException = new ArithmeticException("Calendar value too large for accurate calculations");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            DateUtils.round(calendar, 0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${roundWhenFieldEqualsCalendarMILLISECOND}, hash: E8C69FFD6F56256EE9A87F3FFADCE6FE
    @Test()
    void roundWhenFieldEqualsCalendarMILLISECOND() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : true  #  inside modify method
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        Calendar result = DateUtils.round(calendar, 14);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${roundWhenFieldNotEqualsCalendarAM_PM}, hash: C03A30217ABC83365BCBBDDDE42C3E1E
    @Test()
    void roundWhenFieldNotEqualsCalendarAM_PM() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : false  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (millisecs < 500) : true  #  inside modify method
         * (field == Calendar.SECOND) : true  #  inside modify method
         * (!done) : false  #  inside modify method
         * (field == Calendar.MINUTE) : false  #  inside modify method
         * (!done) : false  #  inside modify method
         * (date.getTime() != time) : true  #  inside modify method
         * (for-each(fields)) : true  #  inside modify method
         * (for-each(aField)) : true  #  inside modify method
         * (element == field) : false  #  inside modify method
         * (switch(field) = default) : true  #  inside modify method
         * (!offsetSet) : true  #  inside modify method
         * (offset > (max - min) / 2) : true  #  inside modify method
         * (offset != 0) : true  #  inside modify method
         * (modType == ModifyType.CEILING) : false  #  inside modify method
         * (modType == ModifyType.ROUND) : true  #  inside modify method
         * (roundUp) : true  #  inside modify method
         * (field == SEMI_MONTH) : false  #  inside modify method
         * (field == Calendar.AM_PM) : false  #  inside modify method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        Calendar result = DateUtils.round(calendar, 13);
        Calendar calendar2 = Calendar.getInstance();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(calendar2)));
    }

    //BaseRock generated method id: ${roundWhenFieldNotEqualsSEMI_MONTHAndFieldNotEqualsCalendarAM_PM}, hash: C394F113E0046FEA8B651D0949F290BC
    @Test()
    void roundWhenFieldNotEqualsSEMI_MONTHAndFieldNotEqualsCalendarAM_PM() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : false  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (millisecs < 500) : true  #  inside modify method
         * (field == Calendar.SECOND) : false  #  inside modify method
         * (!done) : true  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (seconds < 30) : true  #  inside modify method
         * (field == Calendar.MINUTE) : true  #  inside modify method
         * (!done) : false  #  inside modify method
         * (date.getTime() != time) : true  #  inside modify method
         * (for-each(fields)) : true  #  inside modify method
         * (for-each(aField)) : true  #  inside modify method
         * (element == field) : false  #  inside modify method
         * (switch(field) = default) : true  #  inside modify method
         * (!offsetSet) : true  #  inside modify method
         * (offset > (max - min) / 2) : true  #  inside modify method
         * (offset != 0) : true  #  inside modify method
         * (modType == ModifyType.CEILING) : false  #  inside modify method
         * (modType == ModifyType.ROUND) : true  #  inside modify method
         * (roundUp) : true  #  inside modify method
         * (field == SEMI_MONTH) : false  #  inside modify method
         * (field == Calendar.AM_PM) : false  #  inside modify method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        Calendar result = DateUtils.round(calendar, 12);
        Calendar calendar2 = Calendar.getInstance();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(calendar2)));
    }

    //BaseRock generated method id: ${round1WhenFieldEqualsCalendarMILLISECOND}, hash: FA00B35A2343FCB514D0FB5B02973157
    @Test()
    void round1WhenFieldEqualsCalendarMILLISECOND() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : true  #  inside modify method
         */
         //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        Date result = DateUtils.round(date, 14);
        Calendar calendar = Calendar.getInstance();
        Date date2 = calendar.getTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${round1WhenNotRoundUp}, hash: 16A2E1C9DF04BD26210D2747131F1CFF
    @Test()
    void round1WhenNotRoundUp() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : false  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (millisecs < 500) : true  #  inside modify method
         * (field == Calendar.SECOND) : true  #  inside modify method
         * (!done) : false  #  inside modify method
         * (field == Calendar.MINUTE) : false  #  inside modify method
         * (!done) : false  #  inside modify method
         * (date.getTime() != time) : true  #  inside modify method
         * (for-each(fields)) : true  #  inside modify method
         * (for-each(aField)) : true  #  inside modify method
         * (element == field) : false  #  inside modify method
         * (switch(field) = default) : true  #  inside modify method
         * (!offsetSet) : true  #  inside modify method
         * (offset > (max - min) / 2) : false  #  inside modify method
         * (offset != 0) : false  #  inside modify method
         * (modType == ModifyType.CEILING) : false  #  inside modify method
         * (modType == ModifyType.ROUND) : true  #  inside modify method
         * (roundUp) : false  #  inside modify method
         */
         //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        Date result = DateUtils.round(date, 13);
        Calendar calendar = Calendar.getInstance();
        Date date2 = calendar.getTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${round1WhenOffsetEquals0ThrowsIllegalArgumentException}, hash: 0660E666D5E6EF788A966220DB34EF16
    @Test()
    void round1WhenOffsetEquals0ThrowsIllegalArgumentException() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : false  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (millisecs < 500) : true  #  inside modify method
         * (field == Calendar.SECOND) : false  #  inside modify method
         * (!done) : true  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (seconds < 30) : false  #  inside modify method
         * (field == Calendar.MINUTE) : false  #  inside modify method
         * (!done) : true  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (minutes < 30) : true  #  inside modify method
         * (date.getTime() != time) : true  #  inside modify method
         * (for-each(fields)) : true  #  inside modify method
         * (for-each(aField)) : true  #  inside modify method
         * (element == field) : false  #  inside modify method
         * (switch(field) = SEMI_MONTH) : true  #  inside modify method
         * (aField[0] == Calendar.HOUR_OF_DAY) : false  #  inside modify method
         * (!offsetSet) : true  #  inside modify method
         * (offset > (max - min) / 2) : false  #  inside modify method
         * (offset != 0) : false  #  inside modify method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: c - Method: setTime
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Date date = new Date();
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The field 9 is not supported");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            DateUtils.round(date, 9);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${round1WhenOffsetGreaterThanMaxMinusMinDividedBy2AndOffsetNotEquals0ThrowsIllegalArgumentException}, hash: 597EA4A1343F524E9E330289B66E9A29
    @Test()
    void round1WhenOffsetGreaterThanMaxMinusMinDividedBy2AndOffsetNotEquals0ThrowsIllegalArgumentException() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : false  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (millisecs < 500) : false  #  inside modify method
         * (field == Calendar.SECOND) : false  #  inside modify method
         * (!done) : true  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (seconds < 30) : false  #  inside modify method
         * (field == Calendar.MINUTE) : false  #  inside modify method
         * (!done) : true  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (minutes < 30) : true  #  inside modify method
         * (date.getTime() != time) : true  #  inside modify method
         * (for-each(fields)) : true  #  inside modify method
         * (for-each(aField)) : true  #  inside modify method
         * (element == field) : false  #  inside modify method
         * (switch(field) = Calendar.AM_PM) : true  #  inside modify method
         * (aField[0] == Calendar.DATE) : false  #  inside modify method
         * (!offsetSet) : true  #  inside modify method
         * (offset > (max - min) / 2) : true  #  inside modify method
         * (offset != 0) : true  #  inside modify method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: c - Method: setTime
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Date date = new Date();
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The field 1001 is not supported");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            DateUtils.round(date, 1001);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${round1WhenFieldNotEqualsCalendarAM_PM}, hash: 4F558ED6C5D9ADB04CD840E5BDB6A0D2
    @Test()
    void round1WhenFieldNotEqualsCalendarAM_PM() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : false  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (millisecs < 500) : true  #  inside modify method
         * (field == Calendar.SECOND) : false  #  inside modify method
         * (!done) : true  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : false  #  inside modify method
         * (seconds < 30) : false  #  inside modify method
         * (field == Calendar.MINUTE) : true  #  inside modify method
         * (!done) : false  #  inside modify method
         * (date.getTime() != time) : true  #  inside modify method
         * (for-each(fields)) : true  #  inside modify method
         * (for-each(aField)) : true  #  inside modify method
         * (element == field) : false  #  inside modify method
         * (switch(field) = default) : true  #  inside modify method
         * (!offsetSet) : true  #  inside modify method
         * (offset > (max - min) / 2) : false  #  inside modify method
         * (offset != 0) : false  #  inside modify method
         * (modType == ModifyType.CEILING) : false  #  inside modify method
         * (modType == ModifyType.ROUND) : true  #  inside modify method
         * (roundUp) : true  #  inside modify method
         * (field == SEMI_MONTH) : false  #  inside modify method
         * (field == Calendar.AM_PM) : false  #  inside modify method
         */
         //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        Date result = DateUtils.round(date, 12);
        Calendar calendar = Calendar.getInstance();
        Date date2 = calendar.getTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${round2WhenDateInstanceOfDate}, hash: D13F65F91318036737402D69B8C6A8CB
    @Test()
    void round2WhenDateInstanceOfDate() {
        /* Branches:
         * (date instanceof Date) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<DateUtils> dateUtils = mockStatic(DateUtils.class, CALLS_REAL_METHODS)) {
            Date date = new Date();
            Date date2 = new Date();
            dateUtils.when(() -> DateUtils.round(date2, 0)).thenReturn(date);
            //Act Statement(s)
            Date result = DateUtils.round((Object) date2, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(date));
                dateUtils.verify(() -> DateUtils.round(date2, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${round2WhenDateInstanceOfCalendar}, hash: BDA6A10CBB261938F97940FBEAB07CF0
    @Test()
    void round2WhenDateInstanceOfCalendar() {
        /* Branches:
         * (date instanceof Date) : false
         * (date instanceof Calendar) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<DateUtils> dateUtils = mockStatic(DateUtils.class, CALLS_REAL_METHODS)) {
            Calendar calendar = Calendar.getInstance();
            dateUtils.when(() -> DateUtils.round((Calendar) any(), eq(0))).thenReturn(calendar);
            Calendar calendar2 = Calendar.getInstance();
            //Act Statement(s)
            Date result = DateUtils.round((Object) calendar2, 0);
            Date date = new Date();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(date));
                dateUtils.verify(() -> DateUtils.round((Calendar) any(), eq(0)), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${round2WhenDateNotInstanceOfCalendarThrowsClassCastException}, hash: 31E4884685896D3DEE94B3A24A8DAF81
    @Test()
    void round2WhenDateNotInstanceOfCalendarThrowsClassCastException() {
        /* Branches:
         * (date instanceof Date) : false
         * (date instanceof Calendar) : false
         */
         //Arrange Statement(s)
        ClassCastException classCastException = new ClassCastException("Could not round date");
        //Act Statement(s)
        final ClassCastException result = assertThrows(ClassCastException.class, () -> {
            DateUtils.round(objectMock, 0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(classCastException.getMessage()));
        });
    }

    //BaseRock generated method id: ${setDaysTest}, hash: 4221667AE3956D4C1486AD3A657D4F93
    @Test()
    void setDaysTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: c - Method: setTime
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        Date result = DateUtils.setDays(date, 0);
        Calendar calendar = Calendar.getInstance();
        Date date2 = calendar.getTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${setHoursTest}, hash: E07A3C6801264120445E7590CF530880
    @Test()
    void setHoursTest() {
        //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        Date result = DateUtils.setHours(date, 0);
        Calendar calendar = Calendar.getInstance();
        Date date2 = calendar.getTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${setMillisecondsTest}, hash: 8162F29B1052074DDB28B56F37A8F94A
    @Test()
    void setMillisecondsTest() {
        //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        Date result = DateUtils.setMilliseconds(date, 0);
        Calendar calendar = Calendar.getInstance();
        Date date2 = calendar.getTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${setMinutesTest}, hash: FF1F3FE86ED7F233C7D7A05A87A5611B
    @Test()
    void setMinutesTest() {
        //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        Date result = DateUtils.setMinutes(date, 0);
        Calendar calendar = Calendar.getInstance();
        Date date2 = calendar.getTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${setMonthsTest}, hash: 009E7830ADE6A93BC1EAE22678656A40
    @Test()
    void setMonthsTest() {
        //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        Date result = DateUtils.setMonths(date, 0);
        Calendar calendar = Calendar.getInstance();
        Date date2 = calendar.getTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${setSecondsTest}, hash: 7A3C57D410296C1DDF49480A90C93AF0
    @Test()
    void setSecondsTest() {
        //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        Date result = DateUtils.setSeconds(date, 0);
        Calendar calendar = Calendar.getInstance();
        Date date2 = calendar.getTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${setYearsTest}, hash: C5910E7079D08CA878CBA489BA4F4359
    @Test()
    void setYearsTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: c - Method: setTime
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        Date result = DateUtils.setYears(date, 0);
        Calendar calendar = Calendar.getInstance();
        Date date2 = calendar.getTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${toCalendarTest}, hash: 34D30B5C274D749B8BEC5A0B34FAE9D0
    @Test()
    void toCalendarTest() {
        //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        Calendar result = DateUtils.toCalendar(date);
        Calendar calendar = Calendar.getInstance();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(calendar)));
    }

    //BaseRock generated method id: ${toCalendar1Test}, hash: B77215AD017A336D7C2AC45383604E9A
    @Test()
    void toCalendar1Test() {
        //Arrange Statement(s)
        Date date = new Date();
        TimeZone timeZone = TimeZone.getDefault();
        
        //Act Statement(s)
        Calendar result = DateUtils.toCalendar(date, timeZone);
        Calendar calendar = Calendar.getInstance(timeZone);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(calendar)));
    }

    //BaseRock generated method id: ${truncateWhenValGetCalendarYEARGreaterThan280000000ThrowsArithmeticException}, hash: 879374EC7B32A5B88F3C7C7D34C3E0FF
    @Test()
    void truncateWhenValGetCalendarYEARGreaterThan280000000ThrowsArithmeticException() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : true  #  inside modify method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        ArithmeticException arithmeticException = new ArithmeticException("Calendar value too large for accurate calculations");
        //Act Statement(s)
        final ArithmeticException result = assertThrows(ArithmeticException.class, () -> {
            DateUtils.truncate(calendar, 0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(arithmeticException.getMessage()));
        });
    }

    //BaseRock generated method id: ${truncateWhenFieldEqualsCalendarMILLISECOND}, hash: 0D4CD51AC26CDC79207E21CE5D220E42
    @Test()
    void truncateWhenFieldEqualsCalendarMILLISECOND() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : true  #  inside modify method
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        Calendar result = DateUtils.truncate(calendar, 14);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${truncateWhenModTypeNotEqualsModifyTypeROUND}, hash: D610331194A6B6E64C859E6A1FBBF0C8
    @Test()
    void truncateWhenModTypeNotEqualsModifyTypeROUND() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : false  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : true  #  inside modify method
         * (field == Calendar.SECOND) : true  #  inside modify method
         * (!done) : false  #  inside modify method
         * (field == Calendar.MINUTE) : false  #  inside modify method
         * (!done) : false  #  inside modify method
         * (date.getTime() != time) : true  #  inside modify method
         * (for-each(fields)) : true  #  inside modify method
         * (for-each(aField)) : true  #  inside modify method
         * (element == field) : false  #  inside modify method
         * (switch(field) = default) : true  #  inside modify method
         * (!offsetSet) : true  #  inside modify method
         * (offset > (max - min) / 2) : true  #  inside modify method
         * (offset != 0) : true  #  inside modify method
         * (modType == ModifyType.CEILING) : false  #  inside modify method
         * (modType == ModifyType.ROUND) : false  #  inside modify method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        Calendar result = DateUtils.truncate(calendar, 13);
        Calendar calendar2 = Calendar.getInstance();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(calendar2)));
    }

    //BaseRock generated method id: ${truncateWhenModTypeNotEqualsModifyTypeCEILINGAndModTypeNotEqualsModifyTypeROUND}, hash: 6B1EC8678A20F03C2098C1778103F0FA
    @Test()
    void truncateWhenModTypeNotEqualsModifyTypeCEILINGAndModTypeNotEqualsModifyTypeROUND() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : false  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : true  #  inside modify method
         * (field == Calendar.SECOND) : false  #  inside modify method
         * (!done) : true  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : true  #  inside modify method
         * (field == Calendar.MINUTE) : true  #  inside modify method
         * (!done) : false  #  inside modify method
         * (date.getTime() != time) : true  #  inside modify method
         * (for-each(fields)) : true  #  inside modify method
         * (for-each(aField)) : true  #  inside modify method
         * (element == field) : false  #  inside modify method
         * (switch(field) = default) : true  #  inside modify method
         * (!offsetSet) : true  #  inside modify method
         * (offset > (max - min) / 2) : true  #  inside modify method
         * (offset != 0) : true  #  inside modify method
         * (modType == ModifyType.CEILING) : false  #  inside modify method
         * (modType == ModifyType.ROUND) : false  #  inside modify method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        Calendar result = DateUtils.truncate(calendar, 12);
        Calendar calendar2 = Calendar.getInstance();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(calendar2)));
    }

    //BaseRock generated method id: ${truncate1WhenFieldEqualsCalendarMILLISECOND}, hash: 46386B7519C561F96217CB19357F9D97
    @Test()
    void truncate1WhenFieldEqualsCalendarMILLISECOND() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : true  #  inside modify method
         */
         //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        Date result = DateUtils.truncate(date, 14);
        Calendar calendar = Calendar.getInstance();
        Date date2 = calendar.getTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${truncate1WhenModTypeNotEqualsModifyTypeROUND}, hash: 2858BCF9CE7CB5F05884B02E1B223ADE
    @Test()
    void truncate1WhenModTypeNotEqualsModifyTypeROUND() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : false  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : true  #  inside modify method
         * (field == Calendar.SECOND) : true  #  inside modify method
         * (!done) : false  #  inside modify method
         * (field == Calendar.MINUTE) : false  #  inside modify method
         * (!done) : false  #  inside modify method
         * (date.getTime() != time) : true  #  inside modify method
         * (for-each(fields)) : true  #  inside modify method
         * (for-each(aField)) : true  #  inside modify method
         * (element == field) : false  #  inside modify method
         * (switch(field) = default) : true  #  inside modify method
         * (!offsetSet) : true  #  inside modify method
         * (offset > (max - min) / 2) : false  #  inside modify method
         * (offset != 0) : false  #  inside modify method
         * (modType == ModifyType.CEILING) : false  #  inside modify method
         * (modType == ModifyType.ROUND) : false  #  inside modify method
         */
         //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        Date result = DateUtils.truncate(date, 13);
        Calendar calendar = Calendar.getInstance();
        Date date2 = calendar.getTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${truncate1WhenOffsetEquals0ThrowsIllegalArgumentException}, hash: BEFE44382A16CB64B715362B961E52CD
    @Test()
    void truncate1WhenOffsetEquals0ThrowsIllegalArgumentException() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : false  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : true  #  inside modify method
         * (field == Calendar.SECOND) : false  #  inside modify method
         * (!done) : true  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : true  #  inside modify method
         * (field == Calendar.MINUTE) : false  #  inside modify method
         * (!done) : true  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : true  #  inside modify method
         * (date.getTime() != time) : true  #  inside modify method
         * (for-each(fields)) : true  #  inside modify method
         * (for-each(aField)) : true  #  inside modify method
         * (element == field) : false  #  inside modify method
         * (switch(field) = default) : true  #  inside modify method
         * (!offsetSet) : true  #  inside modify method
         * (offset > (max - min) / 2) : false  #  inside modify method
         * (offset != 0) : false  #  inside modify method
         */
         //Arrange Statement(s)
        Date date = new Date();
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The field 3 is not supported");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            DateUtils.truncate(date, 3);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${truncate1WhenModTypeNotEqualsModifyTypeCEILINGAndModTypeNotEqualsModifyTypeROUND}, hash: 84A3AAEE911B992631F7C97C98EB9046
    @Test()
    void truncate1WhenModTypeNotEqualsModifyTypeCEILINGAndModTypeNotEqualsModifyTypeROUND() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : false  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : true  #  inside modify method
         * (field == Calendar.SECOND) : false  #  inside modify method
         * (!done) : true  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : true  #  inside modify method
         * (field == Calendar.MINUTE) : true  #  inside modify method
         * (!done) : false  #  inside modify method
         * (date.getTime() != time) : true  #  inside modify method
         * (for-each(fields)) : true  #  inside modify method
         * (for-each(aField)) : true  #  inside modify method
         * (element == field) : false  #  inside modify method
         * (switch(field) = default) : true  #  inside modify method
         * (!offsetSet) : true  #  inside modify method
         * (offset > (max - min) / 2) : false  #  inside modify method
         * (offset != 0) : false  #  inside modify method
         * (modType == ModifyType.CEILING) : false  #  inside modify method
         * (modType == ModifyType.ROUND) : false  #  inside modify method
         */
         //Arrange Statement(s)
        Date date = new Date();
        
        //Act Statement(s)
        Date result = DateUtils.truncate(date, 12);
        Calendar calendar = Calendar.getInstance();
        Date date2 = calendar.getTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${truncate1WhenOffsetNotGreaterThanMaxMinusMinDividedBy2AndOffsetEquals0ThrowsIllegalArgumentException}, hash: 4DE8A2C184F13E10B449A25D16FF4759
    @Test()
    void truncate1WhenOffsetNotGreaterThanMaxMinusMinDividedBy2AndOffsetEquals0ThrowsIllegalArgumentException() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : false  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : true  #  inside modify method
         * (field == Calendar.SECOND) : false  #  inside modify method
         * (!done) : true  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : true  #  inside modify method
         * (field == Calendar.MINUTE) : false  #  inside modify method
         * (!done) : true  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : true  #  inside modify method
         * (date.getTime() != time) : true  #  inside modify method
         * (for-each(fields)) : true  #  inside modify method
         * (for-each(aField)) : true  #  inside modify method
         * (element == field) : false  #  inside modify method
         * (switch(field) = Calendar.AM_PM) : true  #  inside modify method
         * (aField[0] == Calendar.DATE) : false  #  inside modify method
         * (!offsetSet) : true  #  inside modify method
         * (offset > (max - min) / 2) : false  #  inside modify method
         * (offset != 0) : false  #  inside modify method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: c - Method: setTime
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Date date = new Date();
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The field 1001 is not supported");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            DateUtils.truncate(date, 1001);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${truncate1WhenNotOffsetSetAndOffsetNotGreaterThanMaxMinusMinDividedBy2AndOffsetEquals0ThrowsIllegalArgumentException}, hash: 0A6D3E6D4202D15623965060FF1649CD
    @Test()
    void truncate1WhenNotOffsetSetAndOffsetNotGreaterThanMaxMinusMinDividedBy2AndOffsetEquals0ThrowsIllegalArgumentException() {
        /* Branches:
         * (val.get(Calendar.YEAR) > 280000000) : false  #  inside modify method
         * (field == Calendar.MILLISECOND) : false  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : true  #  inside modify method
         * (field == Calendar.SECOND) : false  #  inside modify method
         * (!done) : true  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : true  #  inside modify method
         * (field == Calendar.MINUTE) : false  #  inside modify method
         * (!done) : true  #  inside modify method
         * (ModifyType.TRUNCATE == modType) : true  #  inside modify method
         * (date.getTime() != time) : true  #  inside modify method
         * (for-each(fields)) : true  #  inside modify method
         * (for-each(aField)) : true  #  inside modify method
         * (element == field) : false  #  inside modify method
         * (switch(field) = SEMI_MONTH) : true  #  inside modify method
         * (aField[0] == Calendar.HOUR_OF_DAY) : false  #  inside modify method
         * (!offsetSet) : true  #  inside modify method
         * (offset > (max - min) / 2) : false  #  inside modify method
         * (offset != 0) : false  #  inside modify method
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: c - Method: setTime
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Date date = new Date();
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The field 9 is not supported");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            DateUtils.truncate(date, 9);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${truncate2WhenDateInstanceOfDate}, hash: AE76104307C4479516DF0D68517CEDD2
    @Test()
    void truncate2WhenDateInstanceOfDate() {
        /* Branches:
         * (date instanceof Date) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<DateUtils> dateUtils = mockStatic(DateUtils.class, CALLS_REAL_METHODS)) {
            Date date = new Date();
            Date date2 = new Date();
            dateUtils.when(() -> DateUtils.truncate(date2, 0)).thenReturn(date);
            //Act Statement(s)
            Date result = DateUtils.truncate((Object) date2, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(date));
                dateUtils.verify(() -> DateUtils.truncate(date2, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${truncate2WhenDateInstanceOfCalendar}, hash: C76D1665A04702A72AFDF40D7E89F6FA
    @Test()
    void truncate2WhenDateInstanceOfCalendar() {
        /* Branches:
         * (date instanceof Date) : false
         * (date instanceof Calendar) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<DateUtils> dateUtils = mockStatic(DateUtils.class, CALLS_REAL_METHODS)) {
            Calendar calendar = Calendar.getInstance();
            dateUtils.when(() -> DateUtils.truncate((Calendar) any(), eq(0))).thenReturn(calendar);
            Calendar calendar2 = Calendar.getInstance();
            //Act Statement(s)
            Date result = DateUtils.truncate((Object) calendar2, 0);
            Date date = new Date();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(date));
                dateUtils.verify(() -> DateUtils.truncate((Calendar) any(), eq(0)), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${truncate2WhenDateNotInstanceOfCalendarThrowsClassCastException}, hash: 05657260FCCC4F2B40DF053A741ADCE0
    @Test()
    void truncate2WhenDateNotInstanceOfCalendarThrowsClassCastException() {
        /* Branches:
         * (date instanceof Date) : false
         * (date instanceof Calendar) : false
         */
         //Arrange Statement(s)
        ClassCastException classCastException = new ClassCastException("Could not truncate date");
        //Act Statement(s)
        final ClassCastException result = assertThrows(ClassCastException.class, () -> {
            DateUtils.truncate(objectMock, 0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(classCastException.getMessage()));
        });
    }

    //BaseRock generated method id: ${truncatedCompareToTest}, hash: 4B4692605219DA852B4A9D8F5E3C9E0E
    @Test()
    void truncatedCompareToTest() {
        //Arrange Statement(s)
        try (MockedStatic<DateUtils> dateUtils = mockStatic(DateUtils.class, CALLS_REAL_METHODS)) {
            doReturn(0).when(calendarMock).compareTo((Calendar) any());
            Calendar calendar = Calendar.getInstance();
            dateUtils.when(() -> DateUtils.truncate((Calendar) any(), eq(0))).thenReturn(calendarMock).thenReturn(calendar);
            Calendar calendar2 = Calendar.getInstance();
            Calendar calendar3 = Calendar.getInstance();
            //Act Statement(s)
            int result = DateUtils.truncatedCompareTo(calendar2, calendar3, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                dateUtils.verify(() -> DateUtils.truncate((Calendar) any(), eq(0)), atLeast(2));
                verify(calendarMock, atLeast(1)).compareTo((Calendar) any());
            });
        }
    }

    //BaseRock generated method id: ${truncatedCompareTo1Test}, hash: 848EE034A6FB8FE644DE78393BEF89B2
    @Test()
    void truncatedCompareTo1Test() {
        //Arrange Statement(s)
        Date dateMock = mock(Date.class);
        try (MockedStatic<DateUtils> dateUtils = mockStatic(DateUtils.class, CALLS_REAL_METHODS)) {
            Date date = new Date();
            dateUtils.when(() -> DateUtils.truncate(date, 0)).thenReturn(dateMock);
            Date date2 = new Date();
            doReturn(0).when(dateMock).compareTo(date2);
            Date date3 = new Date();
            dateUtils.when(() -> DateUtils.truncate(date3, 0)).thenReturn(date2);
            //Act Statement(s)
            int result = DateUtils.truncatedCompareTo(date, date3, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                dateUtils.verify(() -> DateUtils.truncate(date, 0), atLeast(1));
                verify(dateMock, atLeast(1)).compareTo(date2);
                dateUtils.verify(() -> DateUtils.truncate(date3, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${truncatedEqualsWhenTruncatedCompareToCal1Cal2FieldEquals0}, hash: 9942C31E24D57E48607381480E7E5743
    @Test()
    void truncatedEqualsWhenTruncatedCompareToCal1Cal2FieldEquals0() {
        /* Branches:
         * (truncatedCompareTo(cal1, cal2, field) == 0) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<DateUtils> dateUtils = mockStatic(DateUtils.class, CALLS_REAL_METHODS)) {
            dateUtils.when(() -> DateUtils.truncatedCompareTo((Calendar) any(), (Calendar) any(), eq(0))).thenReturn(0);
            Calendar calendar = Calendar.getInstance();
            Calendar calendar2 = Calendar.getInstance();
            //Act Statement(s)
            boolean result = DateUtils.truncatedEquals(calendar, calendar2, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                dateUtils.verify(() -> DateUtils.truncatedCompareTo((Calendar) any(), (Calendar) any(), eq(0)), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${truncatedEqualsWhenTruncatedCompareToCal1Cal2FieldNotEquals0}, hash: 59D55CB6705F8EFA33A172029951E3CD
    @Test()
    void truncatedEqualsWhenTruncatedCompareToCal1Cal2FieldNotEquals0() {
        /* Branches:
         * (truncatedCompareTo(cal1, cal2, field) == 0) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<DateUtils> dateUtils = mockStatic(DateUtils.class, CALLS_REAL_METHODS)) {
            dateUtils.when(() -> DateUtils.truncatedCompareTo((Calendar) any(), (Calendar) any(), eq(0))).thenReturn(-1);
            Calendar calendar = Calendar.getInstance();
            Calendar calendar2 = Calendar.getInstance();
            //Act Statement(s)
            boolean result = DateUtils.truncatedEquals(calendar, calendar2, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                dateUtils.verify(() -> DateUtils.truncatedCompareTo((Calendar) any(), (Calendar) any(), eq(0)), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${truncatedEquals1WhenTruncatedCompareToDate1Date2FieldEquals0}, hash: E6847055D0CB495D00F357C7E28F75A8
    @Test()
    void truncatedEquals1WhenTruncatedCompareToDate1Date2FieldEquals0() {
        /* Branches:
         * (truncatedCompareTo(date1, date2, field) == 0) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<DateUtils> dateUtils = mockStatic(DateUtils.class, CALLS_REAL_METHODS)) {
            Date date = new Date();
            Date date2 = new Date();
            dateUtils.when(() -> DateUtils.truncatedCompareTo(date, date2, 0)).thenReturn(0);
            //Act Statement(s)
            boolean result = DateUtils.truncatedEquals(date, date2, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                dateUtils.verify(() -> DateUtils.truncatedCompareTo(date, date2, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${truncatedEquals1WhenTruncatedCompareToDate1Date2FieldNotEquals0}, hash: 8C0A7A8E90E03F9F66096D6BF2BB606A
    @Test()
    void truncatedEquals1WhenTruncatedCompareToDate1Date2FieldNotEquals0() {
        /* Branches:
         * (truncatedCompareTo(date1, date2, field) == 0) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<DateUtils> dateUtils = mockStatic(DateUtils.class, CALLS_REAL_METHODS)) {
            Date date = new Date();
            Date date2 = new Date();
            dateUtils.when(() -> DateUtils.truncatedCompareTo(date, date2, 0)).thenReturn(-1);
            //Act Statement(s)
            boolean result = DateUtils.truncatedEquals(date, date2, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                dateUtils.verify(() -> DateUtils.truncatedCompareTo(date, date2, 0), atLeast(1));
            });
        }
    }
}

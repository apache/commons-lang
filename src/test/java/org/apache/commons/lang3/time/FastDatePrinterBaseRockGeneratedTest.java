package org.apache.commons.lang3.time;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Disabled;
import java.util.List;
import java.util.Locale;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;
import java.text.FieldPosition;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.doReturn;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.hamcrest.core.IsInstanceOf.instanceOf;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class FastDatePrinterBaseRockGeneratedTest {

    private final Appendable appendableMock = mock(Appendable.class);

    private final FastDatePrinter fastDatePrinterMock = mock(FastDatePrinter.class);

    //BaseRock generated method id: ${getTimeZoneDisplayTest}, hash: 625A8DA67B08297D5251E1A2BA257DF2
    @Test()
    void getTimeZoneDisplayTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        
        //Act Statement(s)
        String result = FastDatePrinter.getTimeZoneDisplay(timeZone, false, 0, locale);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${applyRules1WhenRulesIsEmpty}, hash: 943FA8AA18F9B3A6E9E7DC585BEB8116
    @Test()
    void applyRules1WhenRulesIsEmpty() {
        /* Branches:
         * (for-each(rules)) : false  #  inside applyRules method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = new FastDatePrinter("pattern1", timeZone, locale);
        Calendar calendar = Calendar.getInstance();
        StringBuffer stringBuffer = new StringBuffer();
        
        //Act Statement(s)
        StringBuffer result = target.applyRules(calendar, stringBuffer);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringBuffer)));
    }

    //BaseRock generated method id: ${equalsWhenObjNotInstanceOfFastDatePrinter}, hash: 6F6186B1AC0E15EF211C609A194831BD
    @Test()
    void equalsWhenObjNotInstanceOfFastDatePrinter() {
        /* Branches:
         * (!(obj instanceof FastDatePrinter)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = new FastDatePrinter("pattern1", timeZone, locale);
        Object object = new Object();
        
        //Act Statement(s)
        boolean result = target.equals(object);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equalsWhenLocaleEqualsOtherLocale}, hash: F25B1731ED6C70C2D61079DF7DA5B950
    @Test()
    void equalsWhenLocaleEqualsOtherLocale() {
        /* Branches:
         * (!(obj instanceof FastDatePrinter)) : false
         * (pattern.equals(other.pattern)) : true
         * (timeZone.equals(other.timeZone)) : true
         * (locale.equals(other.locale)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Locale locale = new Locale("language1");
        FastDatePrinter target = new FastDatePrinter("pattern1", (TimeZone) null, locale);
        
        //Act Statement(s)
        boolean result = target.equals(fastDatePrinterMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsWhenLocaleNotEqualsOtherLocale}, hash: CAE05B244EA21BBDA1114C33621268F1
    @Test()
    void equalsWhenLocaleNotEqualsOtherLocale() {
        /* Branches:
         * (!(obj instanceof FastDatePrinter)) : false
         * (pattern.equals(other.pattern)) : true
         * (timeZone.equals(other.timeZone)) : true
         * (locale.equals(other.locale)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Locale locale = new Locale("language1");
        FastDatePrinter target = new FastDatePrinter("pattern1", (TimeZone) null, locale);
        
        //Act Statement(s)
        boolean result = target.equals(fastDatePrinterMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${formatTest}, hash: F26E95B63DABEDF176D15C8F836472AD
    @Test()
    void formatTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        StringBuilder stringBuilder = new StringBuilder("str1");
        StringBuilder stringBuilder2 = new StringBuilder();
        doReturn(stringBuilder).when(target).format((Calendar) any(), eq(stringBuilder2));
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        String result = target.format(calendar);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("result1"));
            verify(target).format((Calendar) any(), eq(stringBuilder2));
        });
    }

    //BaseRock generated method id: ${format1WhenRulesIsEmpty}, hash: 6ACD3976BC3934A917B4F1DC639E4F3D
    @Test()
    void format1WhenRulesIsEmpty() {
        /* Branches:
         * (!calendar.getTimeZone().equals(timeZone)) : true
         * (for-each(rules)) : false  #  inside applyRules method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = new FastDatePrinter("pattern1", timeZone, locale);
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        Appendable result = target.format(calendar, appendableMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(appendableMock)));
    }

    //BaseRock generated method id: ${format2Test}, hash: F68FD7CC4D1E7BE6767FDFBE8699E84F
    @Test()
    void format2Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        StringBuffer stringBuffer = new StringBuffer();
        Date date = new Date();
        StringBuffer stringBuffer2 = new StringBuffer();
        doReturn(stringBuffer).when(target).format(date, stringBuffer2);
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        StringBuffer result = target.format(calendar, stringBuffer2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(stringBuffer));
            verify(target).format(date, stringBuffer2);
        });
    }

    //BaseRock generated method id: ${format3WhenRulesIsEmpty}, hash: 267C6A82AF74C5A6B7ABEC3BA02103C1
    @Test()
    void format3WhenRulesIsEmpty() {
        /* Branches:
         * (for-each(rules)) : false  #  inside applyRules method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = new FastDatePrinter("pattern1", timeZone, locale);
        Date date = new Date();
        
        //Act Statement(s)
        String result = target.format(date);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${format4WhenRulesIsEmpty}, hash: 80472A92A5033138A5BB95F437257B4E
    @Test()
    void format4WhenRulesIsEmpty() {
        /* Branches:
         * (for-each(rules)) : false  #  inside applyRules method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = new FastDatePrinter("pattern1", timeZone, locale);
        Date date = new Date();
        
        //Act Statement(s)
        Appendable result = target.format(date, appendableMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(appendableMock)));
    }

    //BaseRock generated method id: ${format5WhenRulesIsEmpty}, hash: 0735E671ACF8A7C3E8FBF1F9A7C6D09A
    @Test()
    void format5WhenRulesIsEmpty() {
        /* Branches:
         * (for-each(rules)) : false  #  inside applyRules method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = new FastDatePrinter("pattern1", timeZone, locale);
        Date date = new Date();
        StringBuffer stringBuffer = new StringBuffer();
        
        //Act Statement(s)
        StringBuffer result = target.format(date, stringBuffer);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringBuffer)));
    }

    //BaseRock generated method id: ${format6WhenRulesIsEmpty}, hash: 0027F48CCF4A0CFBAAB3F4A520FE2E54
    @Test()
    void format6WhenRulesIsEmpty() {
        /* Branches:
         * (for-each(rules)) : false  #  inside applyRules method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = new FastDatePrinter("pattern1", timeZone, locale);
        
        //Act Statement(s)
        String result = target.format(0L);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${format7WhenRulesIsEmpty}, hash: A19711E953043ACE7A63A141E7342FA7
    @Test()
    void format7WhenRulesIsEmpty() {
        /* Branches:
         * (for-each(rules)) : false  #  inside applyRules method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = new FastDatePrinter("pattern1", timeZone, locale);
        
        //Act Statement(s)
        Appendable result = target.format(0L, appendableMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(appendableMock)));
    }

    //BaseRock generated method id: ${format8WhenRulesIsEmpty}, hash: D998E6D36D2AC0DA90C9A9423DB7F4D0
    @Test()
    void format8WhenRulesIsEmpty() {
        /* Branches:
         * (for-each(rules)) : false  #  inside applyRules method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = new FastDatePrinter("pattern1", timeZone, locale);
        StringBuffer stringBuffer = new StringBuffer();
        
        //Act Statement(s)
        StringBuffer result = target.format(0L, stringBuffer);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(stringBuffer)));
    }

    //BaseRock generated method id: ${format9WhenObjInstanceOfDate}, hash: E52DDD6F1A6F3BB2B8D68FF103A643C4
    @Test()
    void format9WhenObjInstanceOfDate() {
        /* Branches:
         * (obj instanceof Date) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        Date date = new Date();
        doReturn("return_of_format1").when(target).format(date);
        
        //Act Statement(s)
        String result = target.format((Object) date);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_format1"));
            verify(target).format(date);
        });
    }

    //BaseRock generated method id: ${format9WhenObjInstanceOfCalendar}, hash: 48183FEC2E8786F144B29DAD11E01952
    @Test()
    void format9WhenObjInstanceOfCalendar() {
        /* Branches:
         * (obj instanceof Date) : false
         * (obj instanceof Calendar) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        doReturn("return_of_format1").when(target).format((Calendar) any());
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        String result = target.format((Object) calendar);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_format1"));
            verify(target).format((Calendar) any());
        });
    }

    //BaseRock generated method id: ${format9WhenObjInstanceOfLong}, hash: DD220A79A65E841B95657D222F38104E
    @Test()
    void format9WhenObjInstanceOfLong() {
        /* Branches:
         * (obj instanceof Date) : false
         * (obj instanceof Calendar) : false
         * (obj instanceof Long) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        doReturn("return_of_format1").when(target).format(0L);
        
        //Act Statement(s)
        String result = target.format((Object) 0L);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_format1"));
            verify(target).format(0L);
        });
    }

    //BaseRock generated method id: ${format9WhenObjNotInstanceOfLongThrowsIllegalArgumentException}, hash: D59AD67F504E8DCFABB8102E2DDE2647
    @Test()
    void format9WhenObjNotInstanceOfLongThrowsIllegalArgumentException() {
        /* Branches:
         * (obj instanceof Date) : false
         * (obj instanceof Calendar) : false
         * (obj instanceof Long) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = new FastDatePrinter("pattern1", timeZone, locale);
        Object object = new Object();
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("s1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            target.format(object);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${format10WhenObjInstanceOfDate}, hash: FA901A04C49DC8D58636A1FEB3532BD0
    @Test()
    void format10WhenObjInstanceOfDate() {
        /* Branches:
         * (obj instanceof Date) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        StringBuffer stringBuffer = new StringBuffer();
        Date date = new Date();
        StringBuffer stringBuffer2 = new StringBuffer();
        doReturn(stringBuffer).when(target).format(date, stringBuffer2);
        FieldPosition fieldPosition = new FieldPosition(0);
        
        //Act Statement(s)
        StringBuffer result = target.format(date, stringBuffer2, fieldPosition);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(stringBuffer));
            verify(target).format(date, stringBuffer2);
        });
    }

    //BaseRock generated method id: ${format10WhenObjInstanceOfCalendar}, hash: 5777110718384FD50E2BDCA70B8E7347
    @Test()
    void format10WhenObjInstanceOfCalendar() {
        /* Branches:
         * (obj instanceof Date) : false
         * (obj instanceof Calendar) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        StringBuffer stringBuffer = new StringBuffer();
        StringBuffer stringBuffer2 = new StringBuffer();
        doReturn(stringBuffer).when(target).format((Calendar) any(), eq(stringBuffer2));
        Calendar calendar = Calendar.getInstance();
        FieldPosition fieldPosition = new FieldPosition(0);
        
        //Act Statement(s)
        StringBuffer result = target.format(calendar, stringBuffer2, fieldPosition);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(stringBuffer));
            verify(target).format((Calendar) any(), eq(stringBuffer2));
        });
    }

    //BaseRock generated method id: ${format10WhenObjInstanceOfLong}, hash: 89C2C96409476CD468202BA2E41D92CE
    @Test()
    void format10WhenObjInstanceOfLong() {
        /* Branches:
         * (obj instanceof Date) : false
         * (obj instanceof Calendar) : false
         * (obj instanceof Long) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        StringBuffer stringBuffer = new StringBuffer();
        StringBuffer stringBuffer2 = new StringBuffer();
        doReturn(stringBuffer).when(target).format(0L, stringBuffer2);
        FieldPosition fieldPosition = new FieldPosition(0);
        
        //Act Statement(s)
        StringBuffer result = target.format(0L, stringBuffer2, fieldPosition);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(stringBuffer));
            verify(target).format(0L, stringBuffer2);
        });
    }

    //BaseRock generated method id: ${format10WhenObjNotInstanceOfLongThrowsIllegalArgumentException}, hash: E187170DCDD725A7B2EF19F6C1903B7A
    @Test()
    void format10WhenObjNotInstanceOfLongThrowsIllegalArgumentException() {
        /* Branches:
         * (obj instanceof Date) : false
         * (obj instanceof Calendar) : false
         * (obj instanceof Long) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = new FastDatePrinter("pattern1", timeZone, locale);
        Object object = new Object();
        StringBuffer stringBuffer = new StringBuffer();
        FieldPosition fieldPosition = new FieldPosition(0);
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("s1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            target.format(object, stringBuffer, fieldPosition);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${getLocaleTest}, hash: 007009C86D6D423450A75B9207F36D5A
    @Test()
    void getLocaleTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = new FastDatePrinter("pattern1", timeZone, locale);
        
        //Act Statement(s)
        Locale result = target.getLocale();
        Locale locale2 = new Locale("language1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(locale2)));
    }

    //BaseRock generated method id: ${getMaxLengthEstimateTest}, hash: F8FD4F042C0F4BA390862320948B9079
    @Test()
    void getMaxLengthEstimateTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = new FastDatePrinter("pattern1", timeZone, locale);
        
        //Act Statement(s)
        int result = target.getMaxLengthEstimate();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${getPatternTest}, hash: 64BD21B323D3640F66389E81676676E3
    @Test()
    void getPatternTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = new FastDatePrinter("pattern1", timeZone, locale);
        
        //Act Statement(s)
        String result = target.getPattern();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("pattern1")));
    }

    //BaseRock generated method id: ${getTimeZoneTest}, hash: ACF0F21E85F83530053A36DA2E0EE105
    @Test()
    void getTimeZoneTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = new FastDatePrinter("pattern1", timeZone, locale);
        
        //Act Statement(s)
        TimeZone result = target.getTimeZone();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(timeZone)));
    }

    //BaseRock generated method id: ${parsePatternWhenTokenLenEquals0}, hash: 315E9A27B75ACFD32047CABBE1EE861E
    @Disabled()
    @Test()
    void parsePatternWhenTokenLenEquals0() {
        /* Branches:
         * (i < length) : true
         * (tokenLen == 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        //TimeZone timeZone = TimeZone.getDefault();
        //Locale locale = new Locale("language1");
        //FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        //int[] intArray = new int[] { 0 };
        //doReturn("return_of_parseToken1").when(target).parseToken("pattern1", intArray);
        
        //Act Statement(s)
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //List<FastDatePrinter.Rule> result = target.parsePattern();
        
        //Assert statement(s)
        //assertAll("result", () -> {    assertThat(result.size(), equalTo(0));    verify(target).parseToken("pattern1", intArray);});
    }

    //BaseRock generated method id: ${parsePatternWhenSwitchCCase_E_}, hash: 7A4799F2E131BB5E864013D3019E9439
    @Disabled()
    @Test()
    void parsePatternWhenSwitchCCase_E_() {
        /* Branches:
         * (i < length) : true
         * (tokenLen == 0) : false
         * (switch(c) = 'E') : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        //TimeZone timeZone = TimeZone.getDefault();
        //Locale locale = new Locale("language1");
        //FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        //int[] intArray = new int[] { 0 };
        //doReturn("return_of_parseToken1").when(target).parseToken("pattern1", intArray);
        
        //Act Statement(s)
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //List<FastDatePrinter.Rule> result = target.parsePattern();
        
        //Assert statement(s)
        //assertAll("result", () -> {    assertThat(result.size(), equalTo(1));    assertThat(result.get(0), is(instanceOf(FastDatePrinter.Rule.class)));    verify(target).parseToken("pattern1", intArray);});
    }

    //BaseRock generated method id: ${parsePatternWhenSwitchCCase_K_}, hash: 046A34010B5380414185D606962F09DE
    @Disabled()
    @Test()
    void parsePatternWhenSwitchCCase_K_() {
        /* Branches:
         * (i < length) : true
         * (tokenLen == 0) : false
         * (switch(c) = 'K') : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        //TimeZone timeZone = TimeZone.getDefault();
        //Locale locale = new Locale("language1");
        //FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        //int[] intArray = new int[] { 0 };
        //doReturn("return_of_parseToken1").when(target).parseToken("pattern1", intArray);
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //FastDatePrinter.NumberRule fastDatePrinterNumberRuleMock = mock(FastDatePrinter.NumberRule.class);
        //doReturn(fastDatePrinterNumberRuleMock).when(target).selectNumberRule(13, 0);
        
        //Act Statement(s)
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //List<FastDatePrinter.Rule> result = target.parsePattern();
        
        //Assert statement(s)
        //assertAll("result", () -> {    assertThat(result.size(), equalTo(1));    assertThat(result.get(0), is(instanceOf(FastDatePrinter.Rule.class)));    verify(target).parseToken("pattern1", intArray);    verify(target).selectNumberRule(13, 0);});
    }

    //BaseRock generated method id: ${parsePatternWhenSwitchCCase_M_}, hash: A3E829819F14AA8DCD103E8F01436492
    @Disabled()
    @Test()
    void parsePatternWhenSwitchCCase_M_() {
        /* Branches:
         * (i < length) : true
         * (tokenLen == 0) : false
         * (switch(c) = 'M') : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        //TimeZone timeZone = TimeZone.getDefault();
        //Locale locale = new Locale("language1");
        //FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        //int[] intArray = new int[] { 0 };
        //doReturn("return_of_parseToken1").when(target).parseToken("pattern1", intArray);
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //FastDatePrinter.NumberRule fastDatePrinterNumberRuleMock = mock(FastDatePrinter.NumberRule.class);
        //doReturn(fastDatePrinterNumberRuleMock).when(target).selectNumberRule(8, 0);
        
        //Act Statement(s)
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //List<FastDatePrinter.Rule> result = target.parsePattern();
        
        //Assert statement(s)
        //assertAll("result", () -> {    assertThat(result.size(), equalTo(1));    assertThat(result.get(0), is(instanceOf(FastDatePrinter.Rule.class)));    verify(target).parseToken("pattern1", intArray);    verify(target).selectNumberRule(8, 0);});
    }

    //BaseRock generated method id: ${parsePatternWhenSwitchCCase_S_}, hash: 0FCCFA259436BD0377624E63C2C881CD
    @Disabled()
    @Test()
    void parsePatternWhenSwitchCCase_S_() {
        /* Branches:
         * (i < length) : true
         * (tokenLen == 0) : false
         * (switch(c) = 'S') : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        //TimeZone timeZone = TimeZone.getDefault();
        //Locale locale = new Locale("language1");
        //FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        //int[] intArray = new int[] { 0 };
        //doReturn("return_of_parseToken1").when(target).parseToken("pattern1", intArray);
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //FastDatePrinter.NumberRule fastDatePrinterNumberRuleMock = mock(FastDatePrinter.NumberRule.class);
        //doReturn(fastDatePrinterNumberRuleMock).when(target).selectNumberRule(4, 0);
        
        //Act Statement(s)
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //List<FastDatePrinter.Rule> result = target.parsePattern();
        
        //Assert statement(s)
        //assertAll("result", () -> {    assertThat(result.size(), equalTo(1));    assertThat(result.get(0), is(instanceOf(FastDatePrinter.Rule.class)));    verify(target).parseToken("pattern1", intArray);    verify(target).selectNumberRule(4, 0);});
    }

    //BaseRock generated method id: ${parsePatternWhenSwitchCCase_W_}, hash: 97C7A6E23C8D319F00ABBF0DDCB0A110
    @Disabled()
    @Test()
    void parsePatternWhenSwitchCCase_W_() {
        /* Branches:
         * (i < length) : true
         * (tokenLen == 0) : false
         * (switch(c) = 'W') : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        //TimeZone timeZone = TimeZone.getDefault();
        //Locale locale = new Locale("language1");
        //FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        //int[] intArray = new int[] { 0 };
        //doReturn("return_of_parseToken1").when(target).parseToken("pattern1", intArray);
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //FastDatePrinter.NumberRule fastDatePrinterNumberRuleMock = mock(FastDatePrinter.NumberRule.class);
        //doReturn(fastDatePrinterNumberRuleMock).when(target).selectNumberRule(10, 0);
        
        //Act Statement(s)
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //List<FastDatePrinter.Rule> result = target.parsePattern();
        
        //Assert statement(s)
        //assertAll("result", () -> {    assertThat(result, is(notNullValue()));    verify(target).parseToken("pattern1", intArray);    verify(target).selectNumberRule(10, 0);});
    }

    //BaseRock generated method id: ${parsePatternWhenSwitchCCase_X_}, hash: CDEE89C109EF403431010858A1D98409
    @Disabled()
    @Test()
    void parsePatternWhenSwitchCCase_X_() {
        /* Branches:
         * (i < length) : true
         * (tokenLen == 0) : false
         * (switch(c) = 'X') : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        //TimeZone timeZone = TimeZone.getDefault();
        //Locale locale = new Locale("language1");
        //FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        //int[] intArray = new int[] { 0 };
        //doReturn("return_of_parseToken1").when(target).parseToken("pattern1", intArray);
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //FastDatePrinter.NumberRule fastDatePrinterNumberRuleMock = mock(FastDatePrinter.NumberRule.class);
        //doReturn(fastDatePrinterNumberRuleMock).when(target).selectNumberRule(7, 0);
        
        //Act Statement(s)
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //List<FastDatePrinter.Rule> result = target.parsePattern();
        
        //Assert statement(s)
        //assertAll("result", () -> {    assertThat(result, is(notNullValue()));    verify(target).parseToken("pattern1", intArray);    verify(target).selectNumberRule(7, 0);});
    }

    //BaseRock generated method id: ${parsePatternWhenSwitchCCase_a_}, hash: 96E4143C9745A3E073AED70F02928EBB
    @Disabled()
    @Test()
    void parsePatternWhenSwitchCCase_a_() {
        /* Branches:
         * (i < length) : true
         * (tokenLen == 0) : false
         * (switch(c) = 'a') : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        //TimeZone timeZone = TimeZone.getDefault();
        //Locale locale = new Locale("language1");
        //FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        //int[] intArray = new int[] { 0 };
        //doReturn("return_of_parseToken1").when(target).parseToken("pattern1", intArray);
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //FastDatePrinter.NumberRule fastDatePrinterNumberRuleMock = mock(FastDatePrinter.NumberRule.class);
        //doReturn(fastDatePrinterNumberRuleMock).when(target).selectNumberRule(11, 0);
        
        //Act Statement(s)
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //List<FastDatePrinter.Rule> result = target.parsePattern();
        
        //Assert statement(s)
        //assertAll("result", () -> {    assertThat(result, is(notNullValue()));    verify(target).parseToken("pattern1", intArray);    verify(target).selectNumberRule(11, 0);});
    }

    //BaseRock generated method id: ${parsePatternWhenSwitchCCase_d_}, hash: C8537250BD7C7DE3E53CFA49F6606B9A
    @Disabled()
    @Test()
    void parsePatternWhenSwitchCCase_d_() {
        /* Branches:
         * (i < length) : true
         * (tokenLen == 0) : false
         * (switch(c) = 'd') : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        //TimeZone timeZone = TimeZone.getDefault();
        //Locale locale = new Locale("language1");
        //FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        //int[] intArray = new int[] { 0 };
        //doReturn("return_of_parseToken1").when(target).parseToken("pattern1", intArray);
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //FastDatePrinter.NumberRule fastDatePrinterNumberRuleMock = mock(FastDatePrinter.NumberRule.class);
        //doReturn(fastDatePrinterNumberRuleMock).when(target).selectNumberRule(11, 0);
        
        //Act Statement(s)
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //List<FastDatePrinter.Rule> result = target.parsePattern();
        
        //Assert statement(s)
        //assertAll("result", () -> {    assertThat(result.size(), equalTo(1));    assertThat(result.get(0), is(instanceOf(FastDatePrinter.Rule.class)));    verify(target).parseToken("pattern1", intArray);    verify(target).selectNumberRule(11, 0);});
    }

    //BaseRock generated method id: ${parsePatternWhenSwitchCCase_h_}, hash: 9371FAD456575FC7FC6F1BCB5C25D031
    @Disabled()
    @Test()
    void parsePatternWhenSwitchCCase_h_() {
        /* Branches:
         * (i < length) : true
         * (tokenLen == 0) : false
         * (switch(c) = 'h') : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        //TimeZone timeZone = TimeZone.getDefault();
        //Locale locale = new Locale("language1");
        //FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        //int[] intArray = new int[] { 0 };
        //doReturn("return_of_parseToken1").when(target).parseToken("pattern1", intArray);
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //FastDatePrinter.NumberRule fastDatePrinterNumberRuleMock = mock(FastDatePrinter.NumberRule.class);
        //doReturn(fastDatePrinterNumberRuleMock).when(target).selectNumberRule(10, 0);
        
        //Act Statement(s)
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //List<FastDatePrinter.Rule> result = target.parsePattern();
        
        //Assert statement(s)
        //assertAll("result", () -> {    assertThat(result.size(), equalTo(1));    assertThat(result.get(0), is(instanceOf(FastDatePrinter.Rule.class)));    verify(target).parseToken("pattern1", intArray);    verify(target).selectNumberRule(10, 0);});
    }

    //BaseRock generated method id: ${parsePatternWhenSwitchCCase_k_}, hash: 7544FE35CDA64503CF7FBF1B14D38BCF
    @Disabled()
    @Test()
    void parsePatternWhenSwitchCCase_k_() {
        /* Branches:
         * (i < length) : true
         * (tokenLen == 0) : false
         * (switch(c) = 'k') : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        //TimeZone timeZone = TimeZone.getDefault();
        //Locale locale = new Locale("language1");
        //FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        //int[] intArray = new int[] { 0 };
        //doReturn("return_of_parseToken1").when(target).parseToken("pattern1", intArray);
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //FastDatePrinter.NumberRule fastDatePrinterNumberRuleMock = mock(FastDatePrinter.NumberRule.class);
        //doReturn(fastDatePrinterNumberRuleMock).when(target).selectNumberRule(12, 0);
        
        //Act Statement(s)
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //List<FastDatePrinter.Rule> result = target.parsePattern();
        
        //Assert statement(s)
        //assertAll("result", () -> {    assertThat(result.size(), equalTo(1));    assertThat(result.get(0), is(instanceOf(FastDatePrinter.Rule.class)));    verify(target).parseToken("pattern1", intArray);    verify(target).selectNumberRule(12, 0);});
    }

    //BaseRock generated method id: ${parsePatternWhenSwitchCCase_s_}, hash: 31B7E9888C045E571777BE391C871D50
    @Disabled()
    @Test()
    void parsePatternWhenSwitchCCase_s_() {
        /* Branches:
         * (i < length) : true
         * (tokenLen == 0) : false
         * (switch(c) = 's') : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        //TimeZone timeZone = TimeZone.getDefault();
        //Locale locale = new Locale("language1");
        //FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        //int[] intArray = new int[] { 0 };
        //doReturn("return_of_parseToken1").when(target).parseToken("pattern1", intArray);
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //FastDatePrinter.NumberRule fastDatePrinterNumberRuleMock = mock(FastDatePrinter.NumberRule.class);
        //doReturn(fastDatePrinterNumberRuleMock).when(target).selectNumberRule(14, 0);
        
        //Act Statement(s)
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //List<FastDatePrinter.Rule> result = target.parsePattern();
        
        //Assert statement(s)
        //assertAll("result", () -> {    assertThat(result.size(), equalTo(1));    assertThat(result.get(0), is(instanceOf(FastDatePrinter.Rule.class)));    verify(target).parseToken("pattern1", intArray);    verify(target).selectNumberRule(14, 0);});
    }

    //BaseRock generated method id: ${parsePatternWhenSwitchCCase_w_}, hash: C6F23F2963B1E9819683AA154A2C38E0
    @Disabled()
    @Test()
    void parsePatternWhenSwitchCCase_w_() {
        /* Branches:
         * (i < length) : true
         * (tokenLen == 0) : false
         * (switch(c) = 'w') : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        //TimeZone timeZone = TimeZone.getDefault();
        //Locale locale = new Locale("language1");
        //FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        //int[] intArray = new int[] { 0 };
        //doReturn("return_of_parseToken1").when(target).parseToken("pattern1", intArray);
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //FastDatePrinter.NumberRule fastDatePrinterNumberRuleMock = mock(FastDatePrinter.NumberRule.class);
        //doReturn(fastDatePrinterNumberRuleMock).when(target).selectNumberRule(5, 0);
        
        //Act Statement(s)
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //List<FastDatePrinter.Rule> result = target.parsePattern();
        
        //Assert statement(s)
        //assertAll("result", () -> {    assertThat(result.size(), equalTo(1));    assertThat(result.get(0), is(instanceOf(FastDatePrinter.Rule.class)));    verify(target).parseToken("pattern1", intArray);    verify(target).selectNumberRule(5, 0);});
    }

    //BaseRock generated method id: ${parsePatternWhenSwitchCCase_y_}, hash: A3CB7645B9FD0BFE4B10F548B5E41116
    @Disabled()
    @Test()
    void parsePatternWhenSwitchCCase_y_() {
        /* Branches:
         * (i < length) : true
         * (tokenLen == 0) : false
         * (switch(c) = 'y' or switch(c) = 'Y') : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        //TimeZone timeZone = TimeZone.getDefault();
        //Locale locale = new Locale("language1");
        //FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        //int[] intArray = new int[] { 0 };
        //doReturn("return_of_parseToken1").when(target).parseToken("pattern1", intArray);
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //FastDatePrinter.NumberRule fastDatePrinterNumberRuleMock = mock(FastDatePrinter.NumberRule.class);
        //doReturn(fastDatePrinterNumberRuleMock).when(target).selectNumberRule(6, 0);
        
        //Act Statement(s)
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //List<FastDatePrinter.Rule> result = target.parsePattern();
        
        //Assert statement(s)
        //assertAll("result", () -> {    assertThat(result.size(), equalTo(1));    assertThat(result.get(0), is(instanceOf(FastDatePrinter.Rule.class)));    verify(target).parseToken("pattern1", intArray);    verify(target).selectNumberRule(6, 0);});
    }

    //BaseRock generated method id: ${parsePatternWhenSwitchCCase_z_}, hash: 90A49913F7132F2AD18F5F2CCB1D8507
    @Disabled()
    @Test()
    void parsePatternWhenSwitchCCase_z_() {
        /* Branches:
         * (i < length) : true
         * (tokenLen == 0) : false
         * (switch(c) = 'z') : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        //TimeZone timeZone = TimeZone.getDefault();
        //Locale locale = new Locale("language1");
        //FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        //int[] intArray = new int[] { 0 };
        //doReturn("return_of_parseToken1").when(target).parseToken("pattern1", intArray);
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //FastDatePrinter.NumberRule fastDatePrinterNumberRuleMock = mock(FastDatePrinter.NumberRule.class);
        //doReturn(fastDatePrinterNumberRuleMock).when(target).selectNumberRule(3, 0);
        
        //Act Statement(s)
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //List<FastDatePrinter.Rule> result = target.parsePattern();
        
        //Assert statement(s)
        //assertAll("result", () -> {    assertThat(result.size(), equalTo(1));    assertThat(result.get(0), is(instanceOf(FastDatePrinter.Rule.class)));    verify(target).parseToken("pattern1", intArray);    verify(target).selectNumberRule(3, 0);});
    }

    //BaseRock generated method id: ${parsePatternWhenSwitchCCaseDefaultThrowsIllegalArgumentException}, hash: CD4C574D5E1B444FC3E3203E30BC2DF1
    @Disabled()
    @Test()
    void parsePatternWhenSwitchCCaseDefaultThrowsIllegalArgumentException() {
        /* Branches:
         * (i < length) : true
         * (tokenLen == 0) : false
         * (switch(c) = default) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        //TimeZone timeZone = TimeZone.getDefault();
        //Locale locale = new Locale("language1");
        //FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        //int[] intArray = new int[] { 0 };
        //doReturn("return_of_parseToken1").when(target).parseToken("pattern1", intArray);
        //IllegalArgumentException illegalArgumentException = new IllegalArgumentException("s1");
        ////Act Statement(s)//TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {    target.parsePattern();});
        
        //Assert statement(s)
        //assertAll("result", () -> {    assertThat(result, is(notNullValue()));    assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));    verify(target).parseToken("pattern1", intArray);});
    }

    //BaseRock generated method id: ${parsePatternWhenTokenLenEquals1}, hash: 479FBC8DACBDC719FF69CE8777422DFA
    @Disabled()
    @Test()
    void parsePatternWhenTokenLenEquals1() {
        /* Branches:
         * (i < length) : true
         * (tokenLen == 0) : false
         * (switch(c) = 'D') : true
         * (tokenLen == 1) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        //TimeZone timeZone = TimeZone.getDefault();
        //Locale locale = new Locale("language1");
        //FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        //int[] intArray = new int[] { 0 };
        //doReturn("return_of_parseToken1").when(target).parseToken("pattern1", intArray);
        
        //Act Statement(s)
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //List<FastDatePrinter.Rule> result = target.parsePattern();
        
        //Assert statement(s)
        //TODO: Please implement equals method in FastDatePrinter.TimeZoneNumberRule for verification of the entire object or you need to adjust respective assertion statements
        //assertAll("result", () -> {    assertThat(result.size(), equalTo(1));    verify(target).parseToken("pattern1", intArray);});
    }

    //BaseRock generated method id: ${parsePatternWhenSubLengthEquals1}, hash: 448D66630B4F3F346F5875966CE42B43
    @Disabled()
    @Test()
    void parsePatternWhenSubLengthEquals1() {
        /* Branches:
         * (i < length) : true
         * (tokenLen == 0) : false
         * (switch(c) = 'G') : true
         * (sub.length() == 1) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        //TimeZone timeZone = TimeZone.getDefault();
        //Locale locale = new Locale("language1");
        //FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        //int[] intArray = new int[] { 0 };
        //doReturn("return_of_parseToken1").when(target).parseToken("pattern1", intArray);
        
        //Act Statement(s)
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //List<FastDatePrinter.Rule> result = target.parsePattern();
        
        //Assert statement(s)
        //assertAll("result", () -> {    assertThat(result, is(notNullValue()));    verify(target).parseToken("pattern1", intArray);});
    }

    //BaseRock generated method id: ${parsePatternWhenSubLengthNotEquals1}, hash: 574E90F168C4C1496734639FD5979FE9
    @Disabled()
    @Test()
    void parsePatternWhenSubLengthNotEquals1() {
        /* Branches:
         * (i < length) : true
         * (tokenLen == 0) : false
         * (switch(c) = 'G') : true
         * (sub.length() == 1) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        //TimeZone timeZone = TimeZone.getDefault();
        //Locale locale = new Locale("language1");
        //FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        //int[] intArray = new int[] { 0 };
        //doReturn("return_of_parseToken1").when(target).parseToken("pattern1", intArray);
        
        //Act Statement(s)
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //List<FastDatePrinter.Rule> result = target.parsePattern();
        
        //Assert statement(s)
        //assertAll("result", () -> {    assertThat(result, is(notNullValue()));    verify(target).parseToken("pattern1", intArray);});
    }

    //BaseRock generated method id: ${parsePatternWhenTokenLenGreaterThanOrEqualsTo4}, hash: ADB4B8B56CF5B6E923A146B73A9A6EE7
    @Disabled()
    @Test()
    void parsePatternWhenTokenLenGreaterThanOrEqualsTo4() {
        /* Branches:
         * (i < length) : true
         * (tokenLen == 0) : false
         * (switch(c) = '\'') : true
         * (tokenLen >= 4) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        //TimeZone timeZone = TimeZone.getDefault();
        //Locale locale = new Locale("language1");
        //FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        //int[] intArray = new int[] { 0 };
        //doReturn("return_of_parseToken1").when(target).parseToken("pattern1", intArray);
        
        //Act Statement(s)
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //List<FastDatePrinter.Rule> result = target.parsePattern();
        
        //Assert statement(s)
        //assertAll("result", () -> {    assertThat(result, is(notNullValue()));    verify(target).parseToken("pattern1", intArray);});
    }

    //BaseRock generated method id: ${parsePatternWhenTokenLenLessThan4}, hash: 88E1AD71E62A993110F4B03C2077D0B9
    @Disabled()
    @Test()
    void parsePatternWhenTokenLenLessThan4() {
        /* Branches:
         * (i < length) : true
         * (tokenLen == 0) : false
         * (switch(c) = 'y' or switch(c) = 'Y') : true
         * (tokenLen < 4) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        //TimeZone timeZone = TimeZone.getDefault();
        //Locale locale = new Locale("language1");
        //FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        //int[] intArray = new int[] { 0 };
        //doReturn("return_of_parseToken1").when(target).parseToken("pattern1", intArray);
        
        //Act Statement(s)
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //List<FastDatePrinter.Rule> result = target.parsePattern();
        
        //Assert statement(s)
        //assertAll("result", () -> {    assertThat(result, is(notNullValue()));    verify(target).parseToken("pattern1", intArray);});
    }

    //BaseRock generated method id: ${parsePatternWhenCEquals_Y_}, hash: B7E722B5401A096CE78F28E8ACABC039
    @Disabled()
    @Test()
    void parsePatternWhenCEquals_Y_() {
        /* Branches:
         * (i < length) : true
         * (tokenLen == 0) : false
         * (switch(c) = 'u') : true
         * (tokenLen == 2) : true
         * (c == 'Y') : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        //TimeZone timeZone = TimeZone.getDefault();
        //Locale locale = new Locale("language1");
        //FastDatePrinter target = spy(new FastDatePrinter("pattern1", timeZone, locale));
        //int[] intArray = new int[] { 0 };
        //doReturn("return_of_parseToken1").when(target).parseToken("pattern1", intArray);
        
        //Act Statement(s)
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //List<FastDatePrinter.Rule> result = target.parsePattern();
        
        //Assert statement(s)
        //assertAll("result", () -> {    assertThat(result, is(notNullValue()));    verify(target).parseToken("pattern1", intArray);});
    }

    //BaseRock generated method id: ${parseTokenWhenPeekNotEqualsC}, hash: DC07EFE1B37156BF6A72FD4685DB410A
    @Test()
    void parseTokenWhenPeekNotEqualsC() {
        /* Branches:
         * (c >= 'A') : true
         * (c <= 'Z') : false
         * (c >= 'a') : true
         * (c <= 'z') : true
         * (i + 1 < length) : true
         * (peek != c) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = new FastDatePrinter("pattern1", timeZone, locale);
        int[] intArray = new int[] { 0 };
        
        //Act Statement(s)
        String result = target.parseToken("pattern1", intArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${parseTokenWhenIPlus1LessThanLengthAndPatternCharAtIPlus1Equals____}, hash: 44D1DCCA45BBA6B75872A4C5A7F7A6C3
    @Test()
    void parseTokenWhenIPlus1LessThanLengthAndPatternCharAtIPlus1Equals____() {
        /* Branches:
         * (c >= 'A') : true
         * (c <= 'Z') : false
         * (c >= 'a') : true
         * (c <= 'z') : true
         * (i + 1 < length) : true
         * (peek != c) : false
         * (i < length) : true
         * (c == '\'') : true
         * (i + 1 < length) : true
         * (pattern.charAt(i + 1) == '\'') : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = new FastDatePrinter("pattern1", timeZone, locale);
        int[] intArray = new int[] { 3 };
        
        //Act Statement(s)
        String result = target.parseToken("pattern1", intArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${parseTokenWhenIPlus1LessThanLengthAndPatternCharAtIPlus1NotEquals____AndNotInLiteral}, hash: 3069E8ADF1E7EB07BE5E159C6A37BEB6
    @Test()
    void parseTokenWhenIPlus1LessThanLengthAndPatternCharAtIPlus1NotEquals____AndNotInLiteral() {
        /* Branches:
         * (c >= 'A') : true
         * (c <= 'Z') : false
         * (c >= 'a') : true
         * (c <= 'z') : true
         * (i + 1 < length) : true
         * (peek != c) : false
         * (i < length) : true
         * (c == '\'') : true
         * (i + 1 < length) : true
         * (pattern.charAt(i + 1) == '\'') : false
         * (!inLiteral) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = new FastDatePrinter("pattern1", timeZone, locale);
        int[] intArray = new int[] { 2 };
        
        //Act Statement(s)
        String result = target.parseToken("pattern1", intArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${parseTokenWhenCGreaterThanOrEqualsTo_a_AndCLessThanOrEqualsTo_z_}, hash: 94CA0A87515301E952CBDFE8391E88D4
    @Test()
    void parseTokenWhenCGreaterThanOrEqualsTo_a_AndCLessThanOrEqualsTo_z_() {
        /* Branches:
         * (c >= 'A') : true
         * (c <= 'Z') : false
         * (c >= 'a') : true
         * (c <= 'z') : true
         * (i + 1 < length) : true
         * (peek != c) : false
         * (i < length) : true
         * (c == '\'') : false
         * (!inLiteral) : true
         * (c >= 'A') : true
         * (c <= 'Z') : false
         * (c >= 'a') : true
         * (c <= 'z') : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = new FastDatePrinter("pattern1", timeZone, locale);
        int[] intArray = new int[] { 0 };
        
        //Act Statement(s)
        String result = target.parseToken("pattern1", intArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${parseTokenWhenCGreaterThanOrEqualsTo_a_AndCGreaterThan_z_}, hash: 69B9EAE0F88E6FD744055776C0E27908
    @Test()
    void parseTokenWhenCGreaterThanOrEqualsTo_a_AndCGreaterThan_z_() {
        /* Branches:
         * (c >= 'A') : true
         * (c <= 'Z') : false
         * (c >= 'a') : true
         * (c <= 'z') : true
         * (i + 1 < length) : true
         * (peek != c) : false
         * (i < length) : true
         * (c == '\'') : false
         * (!inLiteral) : true
         * (c >= 'A') : true
         * (c <= 'Z') : false
         * (c >= 'a') : true
         * (c <= 'z') : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = new FastDatePrinter("pattern1", timeZone, locale);
        int[] intArray = new int[] { 2 };
        
        //Act Statement(s)
        String result = target.parseToken("pattern1", intArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${selectNumberRuleWhenSwitchPaddingCase1}, hash: B963647047C87C7684361F6513552EAC
    @Disabled()
    @Test()
    void selectNumberRuleWhenSwitchPaddingCase1() {
        /* Branches:
         * (switch(padding) = 1) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        //TimeZone timeZone = TimeZone.getDefault();
        //Locale locale = new Locale("language1");
        //FastDatePrinter target = new FastDatePrinter("pattern1", timeZone, locale);
        
        //Act Statement(s)
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //FastDatePrinter.NumberRule result = target.selectNumberRule(0, 0);
        
        //Assert statement(s)
        //assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${selectNumberRuleWhenSwitchPaddingCase2}, hash: 92E62FF25FF801AA31E9DE76F92FA68C
    @Disabled()
    @Test()
    void selectNumberRuleWhenSwitchPaddingCase2() {
        /* Branches:
         * (switch(padding) = 2) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        //TimeZone timeZone = TimeZone.getDefault();
        //Locale locale = new Locale("language1");
        //FastDatePrinter target = new FastDatePrinter("pattern1", timeZone, locale);
        
        //Act Statement(s)
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //FastDatePrinter.NumberRule result = target.selectNumberRule(0, 0);
        
        //Assert statement(s)
        //assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${selectNumberRuleWhenSwitchPaddingCaseDefault}, hash: 4413E959C6DCC20186B3309319483366
    @Disabled()
    @Test()
    void selectNumberRuleWhenSwitchPaddingCaseDefault() {
        /* Branches:
         * (switch(padding) = default) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        //TimeZone timeZone = TimeZone.getDefault();
        //Locale locale = new Locale("language1");
        //FastDatePrinter target = new FastDatePrinter("pattern1", timeZone, locale);
        
        //Act Statement(s)
        //TODO: Please change the modifier of the below class from private to public to isolate the test case scenario.
        //FastDatePrinter.NumberRule result = target.selectNumberRule(0, 0);
        
        //Assert statement(s)
        //assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${toStringTest}, hash: D80CDF7AC2C0807610CC565D58059312
    @Test()
    void toStringTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        FastDatePrinter target = new FastDatePrinter("pattern1", timeZone, locale);
        
        //Act Statement(s)
        String result = target.toString();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }
}

package org.apache.commons.lang3.time;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.text.ParseException;
import java.text.ParsePosition;
import java.util.Locale;
import java.util.Calendar;
import java.util.TimeZone;
import java.util.Date;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.hamcrest.Matchers.nullValue;
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

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class FastDateParserBaseRockGeneratedTest {

    private final FastDateParser fastDateParserMock = mock(FastDateParser.class);

    //BaseRock generated method id: ${equalsWhenObjNotInstanceOfFastDateParser}, hash: 94111AC6DD451DB3228CE5061C1C4DBD
    @Test()
    void equalsWhenObjNotInstanceOfFastDateParser() {
        /* Branches:
         * (!(obj instanceof FastDateParser)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateParser target = new FastDateParser("pattern1", timeZone, locale, date);
        Object object = new Object();
        
        //Act Statement(s)
        boolean result = target.equals(object);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equalsWhenLocaleEqualsOtherLocale}, hash: FCA79A7F935DEB88B6AC7F29362452B9
    @Test()
    void equalsWhenLocaleEqualsOtherLocale() {
        /* Branches:
         * (!(obj instanceof FastDateParser)) : false
         * (pattern.equals(other.pattern)) : true
         * (timeZone.equals(other.timeZone)) : true
         * (locale.equals(other.locale)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateParser target = new FastDateParser("null", (TimeZone) null, locale, date);
        
        //Act Statement(s)
        boolean result = target.equals(fastDateParserMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsWhenLocaleNotEqualsOtherLocale}, hash: 2B36D6B9AC770323CB68876F03B4ABBD
    @Test()
    void equalsWhenLocaleNotEqualsOtherLocale() {
        /* Branches:
         * (!(obj instanceof FastDateParser)) : false
         * (pattern.equals(other.pattern)) : true
         * (timeZone.equals(other.timeZone)) : true
         * (locale.equals(other.locale)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateParser target = new FastDateParser("null", (TimeZone) null, locale, date);
        
        //Act Statement(s)
        boolean result = target.equals(fastDateParserMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${getLocaleTest}, hash: AE99E75D30A3AE9DCD8CF41941CE8099
    @Test()
    void getLocaleTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateParser target = new FastDateParser("pattern1", timeZone, locale, date);
        
        //Act Statement(s)
        Locale result = target.getLocale();
        Locale locale2 = new Locale("language1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(locale2)));
    }

    //BaseRock generated method id: ${getPatternTest}, hash: F94367A52E5457345ABBDE46B220E78D
    @Test()
    void getPatternTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateParser target = new FastDateParser("pattern1", timeZone, locale, date);
        
        //Act Statement(s)
        String result = target.getPattern();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("pattern1")));
    }

    //BaseRock generated method id: ${getTimeZoneTest}, hash: DA264CEE2EE1B4E072C5E0D20057145D
    @Test()
    void getTimeZoneTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateParser target = new FastDateParser("pattern1", timeZone, locale, date);
        
        //Act Statement(s)
        TimeZone result = target.getTimeZone();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(timeZone)));
    }

    //BaseRock generated method id: ${parseWhenDateIsNotNull}, hash: AFDAEC131093B1D6E21E9D9A6011AEF4
    @Test()
    void parseWhenDateIsNotNull() throws ParseException {
        /* Branches:
         * (date == null) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateParser target = spy(new FastDateParser("pattern1", timeZone, locale, date));
        Date date2 = new Date();
        doReturn(date2).when(target).parse(eq("source1"), (ParsePosition) any());
        
        //Act Statement(s)
        Date result = target.parse("source1");
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(date2));
            verify(target).parse(eq("source1"), (ParsePosition) any());
        });
    }

    //BaseRock generated method id: ${parseWhenLocaleEqualsJAPANESE_IMPERIALThrowsParseException}, hash: 7CFC4FB27051818274F07ED937091EF6
    @Test()
    void parseWhenLocaleEqualsJAPANESE_IMPERIALThrowsParseException() throws ParseException {
        /* Branches:
         * (date == null) : true
         * (locale.equals(JAPANESE_IMPERIAL)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateParser target = spy(new FastDateParser("pattern1", timeZone, locale, date));
        doReturn(null).when(target).parse(eq("B"), (ParsePosition) any());
        ParseException parseException = new ParseException("(The ja_JP_JP_#u-ca-japanese locale does not support dates before 1868 AD)\nUnparseable date: \"B", -1);
        //Act Statement(s)
        final ParseException result = assertThrows(ParseException.class, () -> {
            target.parse("B");
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(parseException.getMessage()));
            verify(target).parse(eq("B"), (ParsePosition) any());
        });
    }

    //BaseRock generated method id: ${parseWhenLocaleNotEqualsJAPANESE_IMPERIALThrowsParseException}, hash: F2927361A02D8255367FD9A6C1F80D57
    @Test()
    void parseWhenLocaleNotEqualsJAPANESE_IMPERIALThrowsParseException() throws ParseException {
        /* Branches:
         * (date == null) : true
         * (locale.equals(JAPANESE_IMPERIAL)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateParser target = spy(new FastDateParser("pattern1", timeZone, locale, date));
        doReturn(null).when(target).parse(eq("A"), (ParsePosition) any());
        ParseException parseException = new ParseException("Unparseable date: A", -1);
        //Act Statement(s)
        final ParseException result = assertThrows(ParseException.class, () -> {
            target.parse("A");
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(parseException.getMessage()));
            verify(target).parse(eq("A"), (ParsePosition) any());
        });
    }

    //BaseRock generated method id: ${parse1WhenParseSourcePosCal}, hash: 7493140051FCBEB2BB96A14EC213898F
    @Test()
    void parse1WhenParseSourcePosCal() {
        /* Branches:
         * (parse(source, pos, cal)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateParser target = spy(new FastDateParser("pattern1", timeZone, locale, date));
        ParsePosition parsePosition = new ParsePosition(0);
        doReturn(true).when(target).parse(eq("source1"), eq(parsePosition), (Calendar) any());
        
        //Act Statement(s)
        Date result = target.parse("source1", parsePosition);
        Date date2 = new Date();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(date2));
            verify(target).parse(eq("source1"), eq(parsePosition), (Calendar) any());
        });
    }

    //BaseRock generated method id: ${parse1WhenParseNotSourcePosCal}, hash: EC5A576B1916FCBB5523A2C2C2F1F886
    @Test()
    void parse1WhenParseNotSourcePosCal() {
        /* Branches:
         * (parse(source, pos, cal)) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateParser target = spy(new FastDateParser("pattern1", timeZone, locale, date));
        ParsePosition parsePosition = new ParsePosition(0);
        doReturn(false).when(target).parse(eq("source1"), eq(parsePosition), (Calendar) any());
        
        //Act Statement(s)
        Date result = target.parse("source1", parsePosition);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(nullValue()));
            verify(target).parse(eq("source1"), eq(parsePosition), (Calendar) any());
        });
    }

    //BaseRock generated method id: ${parse2WhenLtHasNextThrowsNullPointerException}, hash: 17EC1321F781050EEFA1904585A1B603
    @Test()
    void parse2WhenLtHasNextThrowsNullPointerException() {
        /* Branches:
         * (lt.hasNext()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateParser target = new FastDateParser("pattern1", timeZone, locale, date);
        ParsePosition parsePosition = new ParsePosition(0);
        Calendar calendar = Calendar.getInstance();
        //Act Statement(s)
        final NullPointerException result = assertThrows(NullPointerException.class, () -> {
            target.parse("source1", parsePosition, calendar);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${parse2WhenLtNotHasNext}, hash: 797390C31D58CE42187817129FE58849
    @Test()
    void parse2WhenLtNotHasNext() {
        /* Branches:
         * (lt.hasNext()) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateParser target = new FastDateParser("pattern1", timeZone, locale, date);
        ParsePosition parsePosition = new ParsePosition(0);
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        boolean result = target.parse("source1", parsePosition, calendar);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${parseObjectTest}, hash: 50F5092FEAEC1E39F8008E6BD2EDF1EA
    @Test()
    void parseObjectTest() throws ParseException {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateParser target = spy(new FastDateParser("pattern1", timeZone, locale, date));
        Date date2 = new Date();
        doReturn(date2).when(target).parse("source1");
        
        //Act Statement(s)
        Object result = target.parseObject("source1");
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(date2));
            verify(target).parse("source1");
        });
    }

    //BaseRock generated method id: ${parseObject1Test}, hash: 3EF56A9574A8C353D30C90A2BB0C0E0A
    @Test()
    void parseObject1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateParser target = spy(new FastDateParser("pattern1", timeZone, locale, date));
        Date date2 = new Date();
        ParsePosition parsePosition = new ParsePosition(0);
        doReturn(date2).when(target).parse("source1", parsePosition);
        
        //Act Statement(s)
        Object result = target.parseObject("source1", parsePosition);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(date2));
            verify(target).parse("source1", parsePosition);
        });
    }

    //BaseRock generated method id: ${toStringTest}, hash: 0E47320C12ECE23BF500B6267CE413E9
    @Test()
    void toStringTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getTimeZone("B");
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateParser target = new FastDateParser("A", timeZone, locale, date);
        
        //Act Statement(s)
        String result = target.toString();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("FastDateParser[A, locale3, B]")));
    }

    //BaseRock generated method id: ${toStringAllTest}, hash: 68C6990E8A2823034677A9923EFB678F
    @Test()
    void toStringAllTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateParser target = new FastDateParser("A", timeZone, locale, date);
        
        //Act Statement(s)
        String result = target.toStringAll();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("FastDateParser [pattern=A, timeZone=timeZone, locale=locale3, century=0, startYear=8, patterns=[]]")));
    }
}

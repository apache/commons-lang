package org.apache.commons.lang3.time;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.text.ParseException;
import java.text.ParsePosition;
import java.util.Locale;
import java.util.Calendar;
import java.util.Date;
import org.mockito.MockedStatic;
import java.util.TimeZone;
import java.text.FieldPosition;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class FastDateFormatBaseRockGeneratedTest {

    private final Appendable appendableMock = mock(Appendable.class);

    //BaseRock generated method id: ${getDateInstanceTest}, hash: 814D62E9FA05962D351DAC3382F2F7DD
    @Test()
    void getDateInstanceTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: cache - Method: getDateInstance
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        FastDateFormat result = FastDateFormat.getDateInstance(0);
        FastDateFormat fastDateFormat = FastDateFormat.getInstance();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(fastDateFormat)));
    }

    //BaseRock generated method id: ${getDateInstance1Test}, hash: 74DB2182184734D8B8F74F5EF35ACAFC
    @Test()
    void getDateInstance1Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: cache - Method: getDateInstance
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Locale locale = new Locale("language1");
        
        //Act Statement(s)
        FastDateFormat result = FastDateFormat.getDateInstance(0, locale);
        FastDateFormat fastDateFormat = FastDateFormat.getInstance();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(fastDateFormat)));
    }

    //BaseRock generated method id: ${getDateInstance2Test}, hash: 4E0A292A5B2E3AE8CB957B05419CF5AB
    @Test()
    void getDateInstance2Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: cache - Method: getDateInstance
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        
        //Act Statement(s)
        FastDateFormat result = FastDateFormat.getDateInstance(0, timeZone);
        FastDateFormat fastDateFormat = FastDateFormat.getInstance();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(fastDateFormat)));
    }

    //BaseRock generated method id: ${getDateInstance3Test}, hash: 9BCD20E8FCA4106D34D16F25BDEDD6DA
    @Test()
    void getDateInstance3Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: cache - Method: getDateInstance
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        
        //Act Statement(s)
        FastDateFormat result = FastDateFormat.getDateInstance(0, timeZone, locale);
        FastDateFormat fastDateFormat = FastDateFormat.getInstance();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(fastDateFormat)));
    }

    //BaseRock generated method id: ${getDateTimeInstanceTest}, hash: BC5C61392E8A867E7DE6E48E2535B51E
    @Test()
    void getDateTimeInstanceTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: cache - Method: getDateTimeInstance
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        FastDateFormat result = FastDateFormat.getDateTimeInstance(0, 0);
        FastDateFormat fastDateFormat = FastDateFormat.getInstance();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(fastDateFormat)));
    }

    //BaseRock generated method id: ${getDateTimeInstance1Test}, hash: D9F06DFAA7D9DF83FBA5D870E8C7A6CA
    @Test()
    void getDateTimeInstance1Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: cache - Method: getDateTimeInstance
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Locale locale = new Locale("language1");
        
        //Act Statement(s)
        FastDateFormat result = FastDateFormat.getDateTimeInstance(0, 0, locale);
        FastDateFormat fastDateFormat = FastDateFormat.getInstance();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(fastDateFormat)));
    }

    //BaseRock generated method id: ${getDateTimeInstance2Test}, hash: 301172E971F5CB5D87CE0440E9A956EB
    @Test()
    void getDateTimeInstance2Test() {
        //Arrange Statement(s)
        try (MockedStatic<FastDateFormat> fastDateFormat = mockStatic(FastDateFormat.class, CALLS_REAL_METHODS)) {
            FastDateFormat fastDateFormat2 = FastDateFormat.getInstance();
            fastDateFormat.when(() -> FastDateFormat.getDateTimeInstance(eq(0), eq(0), (TimeZone) any(), eq((Locale) null))).thenReturn(fastDateFormat2);
            TimeZone timeZone = TimeZone.getDefault();
            //Act Statement(s)
            FastDateFormat result = FastDateFormat.getDateTimeInstance(0, 0, timeZone);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(fastDateFormat2));
                fastDateFormat.verify(() -> FastDateFormat.getDateTimeInstance(eq(0), eq(0), (TimeZone) any(), eq((Locale) null)), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getDateTimeInstance3Test}, hash: 2DD9C17AAA62A29740C6D041D8F51874
    @Test()
    void getDateTimeInstance3Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: cache - Method: getDateTimeInstance
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        
        //Act Statement(s)
        FastDateFormat result = FastDateFormat.getDateTimeInstance(0, 0, timeZone, locale);
        FastDateFormat fastDateFormat = FastDateFormat.getInstance();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(fastDateFormat)));
    }

    //BaseRock generated method id: ${getInstanceTest}, hash: 6E280F67BF804722BF08A61EE1B0B4B5
    @Test()
    void getInstanceTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: cache - Method: getInstance
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        FastDateFormat result = FastDateFormat.getInstance();
        FastDateFormat fastDateFormat = FastDateFormat.getInstance();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(fastDateFormat)));
    }

    //BaseRock generated method id: ${getInstance1Test}, hash: 968DFA2AC426D814934CE5A1710C9004
    @Test()
    void getInstance1Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: cache - Method: getInstance
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        FastDateFormat result = FastDateFormat.getInstance("pattern1");
        FastDateFormat fastDateFormat = FastDateFormat.getInstance();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(fastDateFormat)));
    }

    //BaseRock generated method id: ${getInstance2Test}, hash: BECC2402942C790FD3EA69B89FED50A7
    @Test()
    void getInstance2Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: cache - Method: getInstance
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Locale locale = new Locale("language1");
        
        //Act Statement(s)
        FastDateFormat result = FastDateFormat.getInstance("pattern1", locale);
        FastDateFormat fastDateFormat = FastDateFormat.getInstance();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(fastDateFormat)));
    }

    //BaseRock generated method id: ${getInstance3Test}, hash: 761F9D3EF66B037991E235C0F98C2673
    @Test()
    void getInstance3Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: cache - Method: getInstance
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        
        //Act Statement(s)
        FastDateFormat result = FastDateFormat.getInstance("pattern1", timeZone);
        FastDateFormat fastDateFormat = FastDateFormat.getInstance();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(fastDateFormat)));
    }

    //BaseRock generated method id: ${getInstance4Test}, hash: F8CD33DF50BC5AC47E77AC99EBA0848F
    @Test()
    void getInstance4Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: cache - Method: getInstance
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        
        //Act Statement(s)
        FastDateFormat result = FastDateFormat.getInstance("pattern1", timeZone, locale);
        FastDateFormat fastDateFormat = FastDateFormat.getInstance();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(fastDateFormat)));
    }

    //BaseRock generated method id: ${getTimeInstanceTest}, hash: FC6690883B20C6D3E545A356D2F1D050
    @Test()
    void getTimeInstanceTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: cache - Method: getTimeInstance
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        FastDateFormat result = FastDateFormat.getTimeInstance(0);
        FastDateFormat fastDateFormat = FastDateFormat.getInstance();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(fastDateFormat)));
    }

    //BaseRock generated method id: ${getTimeInstance1Test}, hash: 3B29D78E8FDAF8068B24BDC8A6CC67E7
    @Test()
    void getTimeInstance1Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: cache - Method: getTimeInstance
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Locale locale = new Locale("language1");
        
        //Act Statement(s)
        FastDateFormat result = FastDateFormat.getTimeInstance(0, locale);
        FastDateFormat fastDateFormat = FastDateFormat.getInstance();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(fastDateFormat)));
    }

    //BaseRock generated method id: ${getTimeInstance2Test}, hash: FB5E2951A644A7838AF3BDD4344FCA82
    @Test()
    void getTimeInstance2Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: cache - Method: getTimeInstance
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        
        //Act Statement(s)
        FastDateFormat result = FastDateFormat.getTimeInstance(0, timeZone);
        FastDateFormat fastDateFormat = FastDateFormat.getInstance();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(fastDateFormat)));
    }

    //BaseRock generated method id: ${getTimeInstance3Test}, hash: 4E7EF4071450D64E3CABE65C42571A3B
    @Test()
    void getTimeInstance3Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: cache - Method: getTimeInstance
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        
        //Act Statement(s)
        FastDateFormat result = FastDateFormat.getTimeInstance(0, timeZone, locale);
        FastDateFormat fastDateFormat = FastDateFormat.getInstance();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(fastDateFormat)));
    }

    //BaseRock generated method id: ${applyRulesTest}, hash: DFBD02D6D0EB97A208093304C70A016B
    @Test()
    void applyRulesTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  No constructor found to create an object without any exception for class org.apache.commons.lang3.time.FastDatePrinter
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateFormat target = new FastDateFormat("pattern1", timeZone, locale, date);
        Calendar calendar = Calendar.getInstance();
        StringBuffer stringBuffer = new StringBuffer();
        
        //Act Statement(s)
        StringBuffer result = target.applyRules(calendar, stringBuffer);
        
        //Assert statement(s)
        //TODO: Please implement equals method in StringBuffer for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${equalsWhenObjNotInstanceOfFastDateFormat}, hash: D1AE42C10A8C0AFB907DC40F2DAB526B
    @Test()
    void equalsWhenObjNotInstanceOfFastDateFormat() {
        /* Branches:
         * (!(obj instanceof FastDateFormat)) : true
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateFormat target = new FastDateFormat("pattern1", timeZone, locale, date);
        Object object = new Object();
        
        //Act Statement(s)
        boolean result = target.equals(object);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equalsWhenPrinterEqualsOtherPrinter}, hash: 605166C0A75485756B5D442C14ADEDDB
    @Test()
    void equalsWhenPrinterEqualsOtherPrinter() {
        /* Branches:
         * (!(obj instanceof FastDateFormat)) : false
         * (printer.equals(other.printer)) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  No constructor found to create an object without any exception for class org.apache.commons.lang3.time.FastDatePrinter
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateFormat target = new FastDateFormat("pattern1", timeZone, locale, date);
        FastDateFormat fastDateFormat = FastDateFormat.getInstance();
        
        //Act Statement(s)
        boolean result = target.equals(fastDateFormat);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsWhenPrinterNotEqualsOtherPrinter}, hash: 533ABFD70791D2CAAE897829D42E0A2D
    @Test()
    void equalsWhenPrinterNotEqualsOtherPrinter() {
        /* Branches:
         * (!(obj instanceof FastDateFormat)) : false
         * (printer.equals(other.printer)) : false
         *
         * TODO: Help needed! This method is not unit testable!
         *  No constructor found to create an object without any exception for class org.apache.commons.lang3.time.FastDatePrinter
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateFormat target = new FastDateFormat("pattern1", timeZone, locale, date);
        FastDateFormat fastDateFormat = FastDateFormat.getInstance();
        
        //Act Statement(s)
        boolean result = target.equals(fastDateFormat);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${formatTest}, hash: B949A15E3D1D11A71627B7C893099483
    @Test()
    void formatTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  No constructor found to create an object without any exception for class org.apache.commons.lang3.time.FastDatePrinter
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateFormat target = new FastDateFormat("pattern1", timeZone, locale, date);
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        String result = target.format(calendar);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${format1Test}, hash: 55CCFCAD7B5E8E0CFA49CD97598BF3B4
    @Test()
    void format1Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  No constructor found to create an object without any exception for class org.apache.commons.lang3.time.FastDatePrinter
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateFormat target = new FastDateFormat("pattern1", timeZone, locale, date);
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        Appendable result = target.format(calendar, appendableMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${format2Test}, hash: A8D57DED4DD2E524F0477E1D8ECC4C9D
    @Test()
    void format2Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  No constructor found to create an object without any exception for class org.apache.commons.lang3.time.FastDatePrinter
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateFormat target = new FastDateFormat("pattern1", timeZone, locale, date);
        Calendar calendar = Calendar.getInstance();
        StringBuffer stringBuffer = new StringBuffer();
        
        //Act Statement(s)
        StringBuffer result = target.format(calendar, stringBuffer);
        
        //Assert statement(s)
        //TODO: Please implement equals method in StringBuffer for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${format3Test}, hash: 06EB8FCBC4B3260593BE495554F9FF17
    @Test()
    void format3Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  No constructor found to create an object without any exception for class org.apache.commons.lang3.time.FastDatePrinter
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateFormat target = new FastDateFormat("pattern1", timeZone, locale, date);
        Date date2 = new Date();
        
        //Act Statement(s)
        String result = target.format(date2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${format4Test}, hash: 45AC6E79B0CBB22BEA7BF145DCA3E052
    @Test()
    void format4Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  No constructor found to create an object without any exception for class org.apache.commons.lang3.time.FastDatePrinter
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateFormat target = new FastDateFormat("pattern1", timeZone, locale, date);
        Date date2 = new Date();
        
        //Act Statement(s)
        Appendable result = target.format(date2, appendableMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${format5Test}, hash: 7A225FEC8E16F608831D4CD64811A18C
    @Test()
    void format5Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  No constructor found to create an object without any exception for class org.apache.commons.lang3.time.FastDatePrinter
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateFormat target = new FastDateFormat("pattern1", timeZone, locale, date);
        Date date2 = new Date();
        StringBuffer stringBuffer = new StringBuffer();
        
        //Act Statement(s)
        StringBuffer result = target.format(date2, stringBuffer);
        
        //Assert statement(s)
        //TODO: Please implement equals method in StringBuffer for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${format6Test}, hash: 08F01A60D25EF02C44C607FD27FD5B57
    @Test()
    void format6Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  No constructor found to create an object without any exception for class org.apache.commons.lang3.time.FastDatePrinter
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateFormat target = new FastDateFormat("pattern1", timeZone, locale, date);
        
        //Act Statement(s)
        String result = target.format(0L);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${format7Test}, hash: 2A3F0A6EE86970432935F5D5A6C2C979
    @Test()
    void format7Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  No constructor found to create an object without any exception for class org.apache.commons.lang3.time.FastDatePrinter
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateFormat target = new FastDateFormat("pattern1", timeZone, locale, date);
        
        //Act Statement(s)
        Appendable result = target.format(0L, appendableMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${format8Test}, hash: A6C51596826BCE56E348622229807E03
    @Test()
    void format8Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  No constructor found to create an object without any exception for class org.apache.commons.lang3.time.FastDatePrinter
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateFormat target = new FastDateFormat("pattern1", timeZone, locale, date);
        StringBuffer stringBuffer = new StringBuffer();
        
        //Act Statement(s)
        StringBuffer result = target.format(0L, stringBuffer);
        
        //Assert statement(s)
        //TODO: Please implement equals method in StringBuffer for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${format9Test}, hash: 1A84C603595DA63341D1ED6BE4446472
    @Test()
    void format9Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  No constructor found to create an object without any exception for class org.apache.commons.lang3.time.FastDatePrinter
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateFormat target = new FastDateFormat("pattern1", timeZone, locale, date);
        Object object = new Object();
        StringBuffer stringBuffer = new StringBuffer();
        FieldPosition fieldPosition = new FieldPosition(0);
        
        //Act Statement(s)
        StringBuffer result = target.format(object, stringBuffer, fieldPosition);
        
        //Assert statement(s)
        //TODO: Please implement equals method in StringBuffer for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getLocaleTest}, hash: 19FD644684679D4EE810396A09A1E043
    @Test()
    void getLocaleTest() {
        //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateFormat target = new FastDateFormat("pattern1", timeZone, locale, date);
        
        //Act Statement(s)
        Locale result = target.getLocale();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(locale)));
    }

    //BaseRock generated method id: ${getMaxLengthEstimateTest}, hash: A0CEF23BD598630F9E8BEB87B3745B7E
    @Test()
    void getMaxLengthEstimateTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  No constructor found to create an object without any exception for class org.apache.commons.lang3.time.FastDatePrinter
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateFormat target = new FastDateFormat("pattern1", timeZone, locale, date);
        
        //Act Statement(s)
        int result = target.getMaxLengthEstimate();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${getPatternTest}, hash: 9EBA595FF950ED87AC16F9AD74319821
    @Test()
    void getPatternTest() {
        //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateFormat target = new FastDateFormat("pattern1", timeZone, locale, date);
        
        //Act Statement(s)
        String result = target.getPattern();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("pattern1")));
    }

    //BaseRock generated method id: ${getTimeZoneTest}, hash: E4461191722154300E4AD087C9D5A50D
    @Test()
    void getTimeZoneTest() {
        //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateFormat target = new FastDateFormat("pattern1", timeZone, locale, date);
        
        //Act Statement(s)
        TimeZone result = target.getTimeZone();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(timeZone)));
    }

    //BaseRock generated method id: ${parseTest}, hash: 2629E155ED0110E90C03F0AF186A3A28
    @Test()
    void parseTest() throws ParseException {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  No constructor found to create an object without any exception for class org.apache.commons.lang3.time.FastDatePrinter
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateFormat target = new FastDateFormat("pattern1", timeZone, locale, date);
        
        //Act Statement(s)
        Date result = target.parse("source1");
        Date date2 = new Date();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${parse1Test}, hash: D3A70FFB09460F7F8B71F91790132E59
    @Test()
    void parse1Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  No constructor found to create an object without any exception for class org.apache.commons.lang3.time.FastDatePrinter
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateFormat target = new FastDateFormat("pattern1", timeZone, locale, date);
        ParsePosition parsePosition = new ParsePosition(0);
        
        //Act Statement(s)
        Date result = target.parse("source1", parsePosition);
        Date date2 = new Date();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(date2)));
    }

    //BaseRock generated method id: ${parse2WhenParserParseSourcePosCalendar}, hash: A540F987C5156DAF2255FDADE2640CDD
    @Test()
    void parse2WhenParserParseSourcePosCalendar() {
        /* Branches:
         * (parser.parse(source, pos, calendar)) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  No constructor found to create an object without any exception for class org.apache.commons.lang3.time.FastDatePrinter
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateFormat target = new FastDateFormat("pattern1", timeZone, locale, date);
        ParsePosition parsePosition = new ParsePosition(0);
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        boolean result = target.parse("source1", parsePosition, calendar);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${parse2WhenParserNotParseSourcePosCalendar}, hash: 7F302A28E8A88AA32BC065BD73D99D30
    @Test()
    void parse2WhenParserNotParseSourcePosCalendar() {
        /* Branches:
         * (parser.parse(source, pos, calendar)) : false
         *
         * TODO: Help needed! This method is not unit testable!
         *  No constructor found to create an object without any exception for class org.apache.commons.lang3.time.FastDatePrinter
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateFormat target = new FastDateFormat("pattern1", timeZone, locale, date);
        ParsePosition parsePosition = new ParsePosition(0);
        Calendar calendar = Calendar.getInstance();
        
        //Act Statement(s)
        boolean result = target.parse("source1", parsePosition, calendar);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${parseObjectTest}, hash: 32B58038258956748CD660AF9118C65E
    @Test()
    void parseObjectTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  No constructor found to create an object without any exception for class org.apache.commons.lang3.time.FastDatePrinter
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateFormat target = new FastDateFormat("pattern1", timeZone, locale, date);
        ParsePosition parsePosition = new ParsePosition(0);
        
        //Act Statement(s)
        Object result = target.parseObject("source1", parsePosition);
        
        //Assert statement(s)
        //TODO: Please implement equals method in Object for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${toStringTest}, hash: 63BB2EAEDFF305871B19FE0295D34641
    @Test()
    void toStringTest() {
        //Arrange Statement(s)
        TimeZone timeZone = TimeZone.getDefault();
        Locale locale = new Locale("language1");
        Date date = new Date();
        FastDateFormat target = new FastDateFormat("A", timeZone, locale, date);
        
        //Act Statement(s)
        String result = target.toString();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("FastDateFormat[A,language1,America/Sao_Paulo]")));
    }
}

package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertAll;
import org.junit.jupiter.api.Disabled;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class StringEscapeUtilsBaseRockGeneratedTest {

    //BaseRock generated method id: ${escapeCsvTest}, hash: 9CD179AAB3C5754B2B04A2D454DC55AC
    @Test()
    void escapeCsvTest() {
        //Act Statement(s)
        String result = StringEscapeUtils.escapeCsv("A");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${escapeEcmaScriptTest}, hash: BF532FB2CD23A5BD5252B6142AC5B6AC
    @Disabled()
    @Test()
    void escapeEcmaScriptTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: ESCAPE_ECMASCRIPT - Method: translate
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String result = StringEscapeUtils.escapeEcmaScript("input1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${escapeHtml3Test}, hash: 363A6BCC36378C9448F4B33B5063B361
    @Test()
    void escapeHtml3Test() {
        //Act Statement(s)
        String result = StringEscapeUtils.escapeHtml3("A");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${escapeHtml4Test}, hash: 6B6FBC597113EF87B2535071C6987CC4
    @Test()
    void escapeHtml4Test() {
        //Act Statement(s)
        String result = StringEscapeUtils.escapeHtml4("A");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${escapeJavaTest}, hash: 9A19C9D3EB844C21B4AE0F1E92B82EC5
    @Test()
    void escapeJavaTest() {
        //Act Statement(s)
        String result = StringEscapeUtils.escapeJava("A");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${escapeJsonTest}, hash: A41A833235FB8C013C1570ADB5D544C0
    @Disabled()
    @Test()
    void escapeJsonTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: ESCAPE_JSON - Method: translate
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String result = StringEscapeUtils.escapeJson("input1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${escapeXmlTest}, hash: B091004E80AA62647365DA4319C29785
    @Test()
    void escapeXmlTest() {
        //Act Statement(s)
        String result = StringEscapeUtils.escapeXml("A");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${escapeXml10Test}, hash: 1F95815CF11606E86344968E3561AFDD
    @Disabled()
    @Test()
    void escapeXml10Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: ESCAPE_XML10 - Method: translate
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String result = StringEscapeUtils.escapeXml10("input1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${escapeXml11Test}, hash: 101C77C61184866065EDA4610C1442E9
    @Disabled()
    @Test()
    void escapeXml11Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: ESCAPE_XML11 - Method: translate
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String result = StringEscapeUtils.escapeXml11("input1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${unescapeCsvTest}, hash: 92F0BBB8008B6A369B8962043B104940
    @Test()
    void unescapeCsvTest() {
        //Act Statement(s)
        String result = StringEscapeUtils.unescapeCsv("A");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }

    //BaseRock generated method id: ${unescapeEcmaScriptTest}, hash: D4CC2621CC007BD14A11F360F25DB752
    @Disabled()
    @Test()
    void unescapeEcmaScriptTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: UNESCAPE_ECMASCRIPT - Method: translate
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String result = StringEscapeUtils.unescapeEcmaScript("input1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${unescapeHtml3Test}, hash: 40EFC4F716C5821B42931490B3355E1D
    @Disabled()
    @Test()
    void unescapeHtml3Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: UNESCAPE_HTML3 - Method: translate
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String result = StringEscapeUtils.unescapeHtml3("input1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${unescapeHtml4Test}, hash: AC107860765FF6364C7CA6CCD08BE1ED
    @Disabled()
    @Test()
    void unescapeHtml4Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: UNESCAPE_HTML4 - Method: translate
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String result = StringEscapeUtils.unescapeHtml4("input1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${unescapeJavaTest}, hash: AE0498B6BBD91F6C595959F8F94466DF
    @Disabled()
    @Test()
    void unescapeJavaTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: UNESCAPE_JAVA - Method: translate
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String result = StringEscapeUtils.unescapeJava("input1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${unescapeJsonTest}, hash: 1EEC22B054AE91979DF3F8197656BDA9
    @Disabled()
    @Test()
    void unescapeJsonTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: UNESCAPE_JSON - Method: translate
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String result = StringEscapeUtils.unescapeJson("input1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${unescapeXmlTest}, hash: 158070E513C832717AD088FB0B5E3610
    @Disabled()
    @Test()
    void unescapeXmlTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: UNESCAPE_XML - Method: translate
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        String result = StringEscapeUtils.unescapeXml("input1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }
}

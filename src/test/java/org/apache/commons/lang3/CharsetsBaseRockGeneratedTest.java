package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.nio.charset.Charset;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.jupiter.api.Assertions.assertAll;
import org.junit.jupiter.api.Disabled;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class CharsetsBaseRockGeneratedTest {

    //BaseRock generated method id: ${toCharsetWhenCharsetIsNull}, hash: 252C1A41CF2243B2BEEAC487701B66FE
    @Test()
    void toCharsetWhenCharsetIsNull() {
        /* Branches:
         * (charset == null) : true
         */
        //Arrange Statement(s)
        Charset charset = null;
        //Act Statement(s)
        Charset result = Charsets.toCharset(charset);
        Charset charset2 = Charset.defaultCharset();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(charset2)));
    }

    //BaseRock generated method id: ${toCharsetWhenCharsetIsNotNull}, hash: 13DB22E13D1F45EEB651E95D032FE616
    @Test()
    void toCharsetWhenCharsetIsNotNull() {
        /* Branches:
         * (charset == null) : false
         */
        //Arrange Statement(s)
        Charset charset = Charset.defaultCharset();
        //Act Statement(s)
        Charset result = Charsets.toCharset(charset);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(charset)));
    }

    //BaseRock generated method id: ${toCharset1WhenCharsetNameIsNull}, hash: A7AABD364D1156D9212E300BFC1CEAE6
    @Test()
    void toCharset1WhenCharsetNameIsNull() {
        /* Branches:
         * (charsetName == null) : true
         */
        //Act Statement(s)
        Charset result = Charsets.toCharset((String) null);
        Charset charset = Charset.defaultCharset();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(charset)));
    }

    //BaseRock generated method id: ${toCharset1WhenCharsetNameIsNotNull}, hash: B4ABA9D813B69548BEF690CC53F0C789
    @Disabled()
    @Test()
    void toCharset1WhenCharsetNameIsNotNull() {
        /* Branches:
         * (charsetName == null) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Act Statement(s)
        Charset result = Charsets.toCharset("A");
        Charset charset = Charset.defaultCharset();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(charset)));
    }

    //BaseRock generated method id: ${toCharsetNameWhenCharsetNameIsNull}, hash: E58374A40A9E1FE405DB532E60B48AD9
    @Test()
    void toCharsetNameWhenCharsetNameIsNull() {
        /* Branches:
         * (charsetName == null) : true
         */
        //Act Statement(s)
        String result = Charsets.toCharsetName((String) null);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("UTF-8")));
    }

    //BaseRock generated method id: ${toCharsetNameWhenCharsetNameIsNotNull}, hash: 39F41F4B72462AA8F2C9F63A1BAF92D4
    @Test()
    void toCharsetNameWhenCharsetNameIsNotNull() {
        /* Branches:
         * (charsetName == null) : false
         */
        //Act Statement(s)
        String result = Charsets.toCharsetName("charsetName1");
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("charsetName1")));
    }
}

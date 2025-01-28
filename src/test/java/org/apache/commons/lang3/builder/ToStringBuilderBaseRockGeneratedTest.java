package org.apache.commons.lang3.builder;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.Mockito.verify;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class ToStringBuilderBaseRockGeneratedTest {

    //BaseRock generated method id: ${getDefaultStyleTest}, hash: EFB9E65963D287CDECD29B75D52050C3
    @Test()
    void getDefaultStyleTest() {
        
        //Act Statement(s)
        ToStringStyle result = ToStringBuilder.getDefaultStyle();
        
        //Assert statement(s)
        //TODO: Please implement equals method in ToStringStyle for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${reflectionToStringTest}, hash: 6B28D9A30C719861F76A3F61DFB07386
    @Test()
    void reflectionToStringTest() {
        //Arrange Statement(s)
        Object object = new Object();
        
        //Act Statement(s)
        String result = ToStringBuilder.reflectionToString(object);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("java.lang.Object@44c8f892[]")));
    }

    //BaseRock generated method id: ${reflectionToString1Test}, hash: 5F4715873CA7A214525B295007D58F5E
    @Test()
    void reflectionToString1Test() {
        //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        
        //Act Statement(s)
        String result = ToStringBuilder.reflectionToString(object, standardToStringStyle);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("java.lang.Object@59844035[]")));
    }

    //BaseRock generated method id: ${reflectionToString2Test}, hash: 0AC4A88708C187CF8644791261063DD2
    @Test()
    void reflectionToString2Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        
        //Act Statement(s)
        String result = ToStringBuilder.reflectionToString(object, standardToStringStyle, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${reflectionToString3Test}, hash: 663810D6C8D60FCD34C4AD40ACE7765E
    @Test()
    void reflectionToString3Test() {
        //Arrange Statement(s)
        Object object = new Object();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        
        //Act Statement(s)
        String result = ToStringBuilder.reflectionToString(object, standardToStringStyle, false, Object.class);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("java.lang.Object@16eaece7[]")));
    }

    //BaseRock generated method id: ${setDefaultStyleTest}, hash: EDB08F52016F5A727AA0A150D95A469E
    @Test()
    void setDefaultStyleTest() {
        //Arrange Statement(s)
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        
        //Act Statement(s)
        ToStringBuilder.setDefaultStyle(standardToStringStyle);
        
        //Assert statement(s)
        //TODO: Please implement equals method in ToStringStyle for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(ToStringBuilder.getDefaultStyle(), is(notNullValue())));
    }

    //BaseRock generated method id: ${appendTest}, hash: BF478056E40AFD58D7DB7D68D5486F44
    @Test()
    void appendTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        
        //Act Statement(s)
        ToStringBuilder result = target.append(false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append1Test}, hash: E57DBE2C9734E1FFEF186B9DA288C052
    @Test()
    void append1Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        boolean[] booleanArray = new boolean[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append(booleanArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append2Test}, hash: F14788096D99655FEC004A39A0A8A22F
    @Test()
    void append2Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        
        //Act Statement(s)
        ToStringBuilder result = target.append((byte) 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append3Test}, hash: 78CD6E158F6D31F3422C4304B16F4E6B
    @Test()
    void append3Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        byte[] byteArray = new byte[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append(byteArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append4Test}, hash: 908AC61DD3DD0616C475B53AFED78C38
    @Test()
    void append4Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        
        //Act Statement(s)
        ToStringBuilder result = target.append('A');
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append5Test}, hash: 706C10D226557C27DA963595641B1675
    @Test()
    void append5Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        char[] charArray = new char[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append(charArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append6Test}, hash: 3C5CF0360D0C8CDD73F48DA34429F93C
    @Test()
    void append6Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        
        //Act Statement(s)
        ToStringBuilder result = target.append(Double.parseDouble("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append7Test}, hash: 254EC52CF54763949A00D692AE81BAC9
    @Test()
    void append7Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        double[] doubleArray = new double[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append(doubleArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append8Test}, hash: 4917F6C10D8429DEE2F89240E41828D8
    @Test()
    void append8Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        
        //Act Statement(s)
        ToStringBuilder result = target.append(Float.parseFloat("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append9Test}, hash: 387840B4F47693FFC25B457956F91BEF
    @Test()
    void append9Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        float[] floatArray = new float[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append(floatArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append10Test}, hash: 7CC87B9E95CFA146B46EBA7745350D13
    @Test()
    void append10Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        
        //Act Statement(s)
        ToStringBuilder result = target.append(0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append11Test}, hash: 43C64981A795545E8289717C8D194AEE
    @Test()
    void append11Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        int[] intArray = new int[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append(intArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append12Test}, hash: 3225BC1933FA7EDDA364305816E25B51
    @Test()
    void append12Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        
        //Act Statement(s)
        ToStringBuilder result = target.append(0L);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append13Test}, hash: 229FE48FA63B55623A0F8AC73811D111
    @Test()
    void append13Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        long[] longArray = new long[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append(longArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append14Test}, hash: F03444EE277F809410AA8612DDAC8282
    @Test()
    void append14Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        Object object2 = new Object();
        
        //Act Statement(s)
        ToStringBuilder result = target.append(object2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append15Test}, hash: BE27B6B94AE83B1F4D767F9D4343EA9B
    @Test()
    void append15Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        Object[] objectArray = new Object[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append(objectArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append16Test}, hash: 060B8486D9ED34DA51920184C7AE1601
    @Test()
    void append16Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        
        //Act Statement(s)
        ToStringBuilder result = target.append((short) 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append17Test}, hash: 68F54B2EA505934CDCF481617FE1B4C0
    @Test()
    void append17Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        short[] shortArray = new short[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append(shortArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append18Test}, hash: 4D267CC298EA05884D16D3394845CE72
    @Test()
    void append18Test() {
        //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append19Test}, hash: C769D701BEE3C6F6C9940EE7DC1B7C4A
    @Test()
    void append19Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        boolean[] booleanArray = new boolean[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", booleanArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append20Test}, hash: F268777DD172DF48DFCC341CBF40DAE4
    @Test()
    void append20Test() {
        //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        boolean[] booleanArray = new boolean[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", booleanArray, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append21Test}, hash: 6B99A1F2E6A4AFECC0FCE512686B67E7
    @Test()
    void append21Test() {
        //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", (byte) 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append22Test}, hash: 02A7E4E87D28547E4DB84D3D6C88F730
    @Test()
    void append22Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        byte[] byteArray = new byte[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", byteArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append23Test}, hash: 272938FC071539B669796E984D6EF51E
    @Test()
    void append23Test() {
        //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        byte[] byteArray = new byte[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", byteArray, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append24Test}, hash: CE36BD9A6D82C44D3A04EEC2FE04644A
    @Test()
    void append24Test() {
        //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", 'A');
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append25Test}, hash: 32AB7E8CFA6FCA12B55A60690B6A38BB
    @Test()
    void append25Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        char[] charArray = new char[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", charArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append26Test}, hash: 2D253A314ACD4718139401A7E33698F4
    @Test()
    void append26Test() {
        //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        char[] charArray = new char[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", charArray, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append27Test}, hash: D9DD1BD2AFF0010448D501B6E0EE1C82
    @Test()
    void append27Test() {
        //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", Double.parseDouble("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append28Test}, hash: E72D72BF95C4477A277B85A93E8EE431
    @Test()
    void append28Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        double[] doubleArray = new double[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", doubleArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append29Test}, hash: DA5D5329821E383F1BBAB81A70BCF52A
    @Test()
    void append29Test() {
        //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        double[] doubleArray = new double[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", doubleArray, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append30Test}, hash: C59BF61894E04FC5BA03684E2FE78F06
    @Test()
    void append30Test() {
        //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", Float.parseFloat("0.0"));
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append31Test}, hash: 740217D67B9A773150FF4167B13B1056
    @Test()
    void append31Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        float[] floatArray = new float[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", floatArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append32Test}, hash: 2E9964B74BC9E2AB3911A9BA37FF92CC
    @Test()
    void append32Test() {
        //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        float[] floatArray = new float[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", floatArray, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append33Test}, hash: 74B42AA4E14A858FE41114D2A6E4D1DF
    @Test()
    void append33Test() {
        //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append34Test}, hash: 60BF992D7E34F10211D0564119C9C909
    @Test()
    void append34Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        int[] intArray = new int[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", intArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append35Test}, hash: 41AF3CC19E7155E98CB8848D2046A053
    @Test()
    void append35Test() {
        //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        int[] intArray = new int[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", intArray, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append36Test}, hash: 8A81E23B233BA6CC7B63F0D67044721C
    @Test()
    void append36Test() {
        //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", 0L);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append37Test}, hash: D46185B77CBC7B3986B36781A58AF6E3
    @Test()
    void append37Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        long[] longArray = new long[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", longArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append38Test}, hash: 9154215D47D5C88168CFBACB5951F378
    @Test()
    void append38Test() {
        //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        long[] longArray = new long[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", longArray, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append39Test}, hash: 1596486A5A8615C15FF61D30D6E2FF45
    @Test()
    void append39Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        Object object2 = new Object();
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", object2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append40Test}, hash: 7A254F7498DD66A5F8C2095EC951079F
    @Test()
    void append40Test() {
        //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        Object object2 = new Object();
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", object2, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append41Test}, hash: C58BAABD4BA2D359F14917C53E22F642
    @Test()
    void append41Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        Object[] objectArray = new Object[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", objectArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append42Test}, hash: 1D3F59DCEC3A104F476A0E0AEE053157
    @Test()
    void append42Test() {
        //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        Object[] objectArray = new Object[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", objectArray, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append43Test}, hash: 820695B8891484438F4F26B59A34997D
    @Test()
    void append43Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", (short) 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append44Test}, hash: 56FB866EF2737ED92AF9A20A12AA8F67
    @Test()
    void append44Test() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        short[] shortArray = new short[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", shortArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${append45Test}, hash: 4C2826160C765160B77EE83CD8151F57
    @Test()
    void append45Test() {
        //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        short[] shortArray = new short[] {};
        
        //Act Statement(s)
        ToStringBuilder result = target.append("fieldName1", shortArray, false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${appendAsObjectToStringTest}, hash: 9BDA76AFD369B21719F170A6DA413614
    @Test()
    void appendAsObjectToStringTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = spy(new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null));
        StringBuffer stringBuffer = new StringBuffer();
        doReturn(stringBuffer).when(target).getStringBuffer();
        Object object2 = new Object();
        
        //Act Statement(s)
        ToStringBuilder result = target.appendAsObjectToString(object2);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            verify(target).getStringBuffer();
        });
    }

    //BaseRock generated method id: ${appendSuperWhenSuperToStringIsNotNull}, hash: A0EDAD461F53BB8E0D2905300EDCCD46
    @Test()
    void appendSuperWhenSuperToStringIsNotNull() {
        /* Branches:
         * (superToString != null) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        
        //Act Statement(s)
        ToStringBuilder result = target.appendSuper("superToString1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${appendToStringWhenToStringIsNotNull}, hash: B60142454A5FFC5285D0DC2D643CE6DD
    @Test()
    void appendToStringWhenToStringIsNotNull() {
        /* Branches:
         * (toString != null) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        
        //Act Statement(s)
        ToStringBuilder result = target.appendToString("toString1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${buildTest}, hash: 33A98F442A4C738A1D167AC96AA5E301
    @Test()
    void buildTest() {
        /*
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        
        //Act Statement(s)
        String result = target.build();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("result1")));
    }

    //BaseRock generated method id: ${getObjectTest}, hash: BCA30912C6E3D501C2F3E94EEEFF5491
    @Test()
    void getObjectTest() {
        //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        
        //Act Statement(s)
        Object result = target.getObject();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(object)));
    }

    //BaseRock generated method id: ${getStringBufferTest}, hash: 8077B3DE37A53D87A345B55974067E3A
    @Test()
    void getStringBufferTest() {
        //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        
        //Act Statement(s)
        StringBuffer result = target.getStringBuffer();
        
        //Assert statement(s)
        //TODO: Please implement equals method in StringBuffer for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getStyleTest}, hash: 064AE447570884B340CC718D2DAEF429
    @Test()
    void getStyleTest() {
        //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null);
        
        //Act Statement(s)
        ToStringStyle result = target.getStyle();
        
        //Assert statement(s)
        //TODO: Please implement equals method in ToStringStyle for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${toStringWhenGetObjectIsNull}, hash: D6B06ED38B2E46B32610424B1B4DB902
    @Test()
    void toStringWhenGetObjectIsNull() {
        /* Branches:
         * (getObject() == null) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        ToStringBuilder target = spy(new ToStringBuilder((Object) null, (ToStringStyle) null, (StringBuffer) null));
        StringBuffer stringBuffer = new StringBuffer();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        doReturn(standardToStringStyle).when(target).getStyle();
        StringBuffer stringBuffer2 = new StringBuffer();
        doReturn(stringBuffer, stringBuffer2).when(target).getStringBuffer();
        
        //Act Statement(s)
        String result = target.toString();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("stringBuffer4"));
            verify(target, times(2)).getStringBuffer();
            verify(target).getStyle();
        });
    }

    //BaseRock generated method id: ${toStringWhenGetObjectIsNotNull}, hash: 086DB5E8905FEEF1891D6071703A053A
    @Test()
    void toStringWhenGetObjectIsNotNull() {
        /* Branches:
         * (getObject() == null) : false
         *
         * TODO: Help needed! This method is not unit testable!
         *  A variable could not be isolated/mocked when calling a method - Variable name: style - Method: appendStart
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        ToStringBuilder target = spy(new ToStringBuilder(object, (ToStringStyle) null, (StringBuffer) null));
        StringBuffer stringBuffer = new StringBuffer();
        StringBuffer stringBuffer2 = new StringBuffer();
        doReturn(stringBuffer, stringBuffer2).when(target).getStringBuffer();
        
        //Act Statement(s)
        String result = target.toString();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("stringBuffer3"));
            verify(target, times(2)).getStringBuffer();
        });
    }
}

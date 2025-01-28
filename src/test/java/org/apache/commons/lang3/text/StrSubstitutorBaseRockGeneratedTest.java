package org.apache.commons.lang3.text;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.Map;
import java.util.HashMap;
import org.mockito.MockedStatic;
import java.util.Properties;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.verify;
import static org.mockito.ArgumentMatchers.anyMap;
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
import static org.mockito.Mockito.mockStatic;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class StrSubstitutorBaseRockGeneratedTest {

    //BaseRock generated method id: ${replaceTest}, hash: A62A198BA92733AA694E69FD3EA8ED4D
    @Test()
    void replaceTest() {
        //Arrange Statement(s)
        Object object = new Object();
        Map<String, Object> stringObjectMap = new HashMap<>();
        
        //Act Statement(s)
        String result = StrSubstitutor.replace(object, stringObjectMap);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("java.lang.Object@51c45863")));
    }

    //BaseRock generated method id: ${replace1Test}, hash: 13A065B1844E1E7082C229B70AB627A5
    @Test()
    void replace1Test() {
        //Arrange Statement(s)
        Object object = new Object();
        Map<String, Object> stringObjectMap = new HashMap<>();
        
        //Act Statement(s)
        String result = StrSubstitutor.replace(object, stringObjectMap, "B", "C");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("java.lang.Object@3430d3d0")));
    }

    //BaseRock generated method id: ${replace2WhenValuePropertiesIsNull}, hash: 5D73C3A7500DC636EF3EF7D33ED88630
    @Test()
    void replace2WhenValuePropertiesIsNull() {
        /* Branches:
         * (valueProperties == null) : true
         */
         //Arrange Statement(s)
        Object objectMock = mock(Object.class, "source");
        Properties properties = null;
        
        //Act Statement(s)
        String result = StrSubstitutor.replace(objectMock, properties);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("source")));
    }

    //BaseRock generated method id: ${replace2WhenPropNamesHasMoreElements}, hash: 10AA716EF80D945FA4C2C0AFD4D34984
    @Test()
    void replace2WhenPropNamesHasMoreElements() {
        /* Branches:
         * (valueProperties == null) : false
         * (propNames.hasMoreElements()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        try (MockedStatic<StrSubstitutor> strSubstitutor = mockStatic(StrSubstitutor.class, CALLS_REAL_METHODS)) {
            Object object = new Object();
            strSubstitutor.when(() -> StrSubstitutor.replace(eq(object), anyMap())).thenReturn("return_of_replace1");
            Properties properties = new Properties();
            properties.put("object", "valuePropertiesItem1Value1");
            //Act Statement(s)
            String result = StrSubstitutor.replace(object, properties);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_replace1"));
                strSubstitutor.verify(() -> StrSubstitutor.replace(eq(object), anyMap()), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${replaceSystemPropertiesTest}, hash: A3132E68A2D9DE8896D3C2D9534F8B99
    @Test()
    void replaceSystemPropertiesTest() {
        //Arrange Statement(s)
        Object object = new Object();
        
        //Act Statement(s)
        String result = StrSubstitutor.replaceSystemProperties(object);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("java.lang.Object@67d164aa")));
    }

    //BaseRock generated method id: ${getEscapeCharTest}, hash: 509E57181BF51A2D9A771B8B8BF95E4B
    @Test()
    void getEscapeCharTest() {
        //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        
        //Act Statement(s)
        char result = target.getEscapeChar();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo('A')));
    }

    //BaseRock generated method id: ${getValueDelimiterMatcherTest}, hash: A10AF355DB5C245EB7613D58D5E6EE5C
    @Test()
    void getValueDelimiterMatcherTest() {
        //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        
        //Act Statement(s)
        StrMatcher result = target.getValueDelimiterMatcher();
        
        //Assert statement(s)
        //TODO: Please implement equals method in StrMatcher for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getVariablePrefixMatcherTest}, hash: CA6C48CAA0CB5FA680C37B93E2BFF3B5
    @Test()
    void getVariablePrefixMatcherTest() {
        //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        
        //Act Statement(s)
        StrMatcher result = target.getVariablePrefixMatcher();
        
        //Assert statement(s)
        //TODO: Please implement equals method in StrMatcher for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getVariableResolverTest}, hash: 5C2CBCFD33F868756A67046E7461AB35
    @Test()
    void getVariableResolverTest() {
        //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        
        //Act Statement(s)
        StrLookup<?> result = target.getVariableResolver();
        
        //Assert statement(s)
        //TODO: Please implement equals method in StrLookup for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getVariableSuffixMatcherTest}, hash: 85BBB05494BB880DEBBBB518015D8522
    @Test()
    void getVariableSuffixMatcherTest() {
        //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        
        //Act Statement(s)
        StrMatcher result = target.getVariableSuffixMatcher();
        
        //Assert statement(s)
        //TODO: Please implement equals method in StrMatcher for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${isEnableSubstitutionInVariablesTest}, hash: 364C84335FE6AB27419DF30BB9BA7084
    @Test()
    void isEnableSubstitutionInVariablesTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        
        //Act Statement(s)
        boolean result = target.isEnableSubstitutionInVariables();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isPreserveEscapesTest}, hash: CC90373C66BFA7B97A578526DDF4B6E5
    @Test()
    void isPreserveEscapesTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        
        //Act Statement(s)
        boolean result = target.isPreserveEscapes();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${replace3WhenSourceIsNull}, hash: 3B91D4FA4C9304B780947E563BE601AD
    @Test()
    void replace3WhenSourceIsNull() {
        /* Branches:
         * (source == null) : true
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        char[] _char = null;
        
        //Act Statement(s)
        String result = target.replace(_char);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${replace3WhenSourceIsNotNull}, hash: E66E67180539F70F3E60E9D699064A30
    @Test()
    void replace3WhenSourceIsNotNull() {
        /* Branches:
         * (source == null) : false
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        doReturn(false).when(target).substitute((StrBuilder) any(), eq(0), eq(0));
        char[] charArray = new char[] {};
        
        //Act Statement(s)
        String result = target.replace(charArray);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(""));
            verify(target).substitute((StrBuilder) any(), eq(0), eq(0));
        });
    }

    //BaseRock generated method id: ${replace4WhenSourceIsNull}, hash: C241C26F8B720EF9359C4E0E26EFB9F6
    @Test()
    void replace4WhenSourceIsNull() {
        /* Branches:
         * (source == null) : true
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        char[] _char = null;
        
        //Act Statement(s)
        String result = target.replace(_char, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${replace4WhenSourceIsNotNull}, hash: 7AC8585750523599C10737A934D4D7B5
    @Test()
    void replace4WhenSourceIsNotNull() {
        /* Branches:
         * (source == null) : false
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        doReturn(false).when(target).substitute((StrBuilder) any(), eq(0), eq(0));
        char[] charArray = new char[] {};
        
        //Act Statement(s)
        String result = target.replace(charArray, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(""));
            verify(target).substitute((StrBuilder) any(), eq(0), eq(0));
        });
    }

    //BaseRock generated method id: ${replace5WhenSourceIsNull}, hash: 1308CC3132268BE50BF8A7422AF1B279
    @Test()
    void replace5WhenSourceIsNull() {
        /* Branches:
         * (source == null) : true
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        CharSequence charSequence = null;
        
        //Act Statement(s)
        String result = target.replace(charSequence);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${replace5WhenSourceIsNotNull}, hash: 9CEA4CC5DFC54F9CC4A5C305DB096EAA
    @Test()
    void replace5WhenSourceIsNotNull() {
        /* Branches:
         * (source == null) : false
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        doReturn("return_of_replace1").when(target).replace("source1", 0, 7);
        
        //Act Statement(s)
        String result = target.replace((CharSequence) "source1");
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_replace1"));
            verify(target).replace("source1", 0, 7);
        });
    }

    //BaseRock generated method id: ${replace6WhenSourceIsNull}, hash: C655FBE7CDF498D26A8A8F62EC30791D
    @Test()
    void replace6WhenSourceIsNull() {
        /* Branches:
         * (source == null) : true
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        CharSequence charSequence = null;
        
        //Act Statement(s)
        String result = target.replace(charSequence, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${replace6WhenSourceIsNotNull}, hash: D646F494F481FB8C682ECCE523076467
    @Test()
    void replace6WhenSourceIsNotNull() {
        /* Branches:
         * (source == null) : false
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        doReturn(false).when(target).substitute((StrBuilder) any(), eq(0), eq(0));
        
        //Act Statement(s)
        String result = target.replace((CharSequence) "source1", 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(""));
            verify(target).substitute((StrBuilder) any(), eq(0), eq(0));
        });
    }

    //BaseRock generated method id: ${replace7WhenSourceIsNull}, hash: 581B58F8C92BAEF8A200F9CE27D0133A
    @Test()
    void replace7WhenSourceIsNull() {
        /* Branches:
         * (source == null) : true
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        Object object = null;
        
        //Act Statement(s)
        String result = target.replace(object);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${replace7WhenSourceIsNotNull}, hash: 9D394085BC8E3DE5B65271F22FFD7C54
    @Test()
    void replace7WhenSourceIsNotNull() {
        /* Branches:
         * (source == null) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        doReturn(false).when(target).substitute((StrBuilder) any(), eq(0), eq(0));
        Object object = new Object();
        
        //Act Statement(s)
        String result = target.replace(object);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("result1"));
            verify(target).substitute((StrBuilder) any(), eq(0), eq(0));
        });
    }

    //BaseRock generated method id: ${replace8WhenSourceIsNull}, hash: BA0B0B0DFE3858A9655B328F864A222E
    @Test()
    void replace8WhenSourceIsNull() {
        /* Branches:
         * (source == null) : true
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        StrBuilder strBuilder = null;
        
        //Act Statement(s)
        String result = target.replace(strBuilder);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${replace8WhenSourceIsNotNull}, hash: 097F650F667CCEA80EF840DC89154C95
    @Test()
    void replace8WhenSourceIsNotNull() {
        /* Branches:
         * (source == null) : false
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        doReturn(false).when(target).substitute((StrBuilder) any(), eq(0), eq(0));
        StrBuilder strBuilder = new StrBuilder();
        
        //Act Statement(s)
        String result = target.replace(strBuilder);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(""));
            verify(target).substitute((StrBuilder) any(), eq(0), eq(0));
        });
    }

    //BaseRock generated method id: ${replace9WhenSourceIsNull}, hash: 6CF8FF333123427A0CEE93207DFCD730
    @Test()
    void replace9WhenSourceIsNull() {
        /* Branches:
         * (source == null) : true
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        StrBuilder strBuilder = null;
        
        //Act Statement(s)
        String result = target.replace(strBuilder, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${replace9WhenSourceIsNotNull}, hash: 1AE35D085D1CFAA62CDBC15FE8B6FF67
    @Test()
    void replace9WhenSourceIsNotNull() {
        /* Branches:
         * (source == null) : false
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        doReturn(false).when(target).substitute((StrBuilder) any(), eq(0), eq(0));
        StrBuilder strBuilder = new StrBuilder();
        
        //Act Statement(s)
        String result = target.replace(strBuilder, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(""));
            verify(target).substitute((StrBuilder) any(), eq(0), eq(0));
        });
    }

    //BaseRock generated method id: ${replace10WhenSourceIsNull}, hash: ECEE06A3D7EA35A810C5722AA652C333
    @Test()
    void replace10WhenSourceIsNull() {
        /* Branches:
         * (source == null) : true
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        
        //Act Statement(s)
        String result = target.replace((String) null);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${replace10WhenSubstituteNotBuf0SourceLength}, hash: 8C73C820F9844070B8D21923DCE26920
    @Test()
    void replace10WhenSubstituteNotBuf0SourceLength() {
        /* Branches:
         * (source == null) : false
         * (!substitute(buf, 0, source.length())) : true
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        doReturn(false).when(target).substitute((StrBuilder) any(), eq(0), eq(7));
        
        //Act Statement(s)
        String result = target.replace("source1");
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("source1"));
            verify(target).substitute((StrBuilder) any(), eq(0), eq(7));
        });
    }

    //BaseRock generated method id: ${replace10WhenSubstituteBuf0SourceLength}, hash: E5925F21A883660C1A983A4DDF17BD08
    @Test()
    void replace10WhenSubstituteBuf0SourceLength() {
        /* Branches:
         * (source == null) : false
         * (!substitute(buf, 0, source.length())) : false
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        doReturn(true).when(target).substitute((StrBuilder) any(), eq(0), eq(7));
        
        //Act Statement(s)
        String result = target.replace("source1");
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("source1"));
            verify(target).substitute((StrBuilder) any(), eq(0), eq(7));
        });
    }

    //BaseRock generated method id: ${replace11WhenSourceIsNull}, hash: B665E67860937308E82066B5356D9D60
    @Test()
    void replace11WhenSourceIsNull() {
        /* Branches:
         * (source == null) : true
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        
        //Act Statement(s)
        String result = target.replace((String) null, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${replace11WhenSubstituteNotBuf0Length}, hash: FAB4ADCEC06859E4C41648AD6E060561
    @Test()
    void replace11WhenSubstituteNotBuf0Length() {
        /* Branches:
         * (source == null) : false
         * (!substitute(buf, 0, length)) : true
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        doReturn(false).when(target).substitute((StrBuilder) any(), eq(0), eq(0));
        
        //Act Statement(s)
        String result = target.replace("source1", 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(""));
            verify(target).substitute((StrBuilder) any(), eq(0), eq(0));
        });
    }

    //BaseRock generated method id: ${replace11WhenSubstituteBuf0Length}, hash: 12349495523327049D1F77D1A910FE93
    @Test()
    void replace11WhenSubstituteBuf0Length() {
        /* Branches:
         * (source == null) : false
         * (!substitute(buf, 0, length)) : false
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        doReturn(true).when(target).substitute((StrBuilder) any(), eq(0), eq(0));
        
        //Act Statement(s)
        String result = target.replace("source1", 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(""));
            verify(target).substitute((StrBuilder) any(), eq(0), eq(0));
        });
    }

    //BaseRock generated method id: ${replace12WhenSourceIsNull}, hash: 66E966A5375A6F41BB78845E0801CC30
    @Test()
    void replace12WhenSourceIsNull() {
        /* Branches:
         * (source == null) : true
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        StringBuffer stringBuffer = null;
        
        //Act Statement(s)
        String result = target.replace(stringBuffer);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${replace12WhenSourceIsNotNull}, hash: 45C66039F562B170E9A0E62E252C0F60
    @Test()
    void replace12WhenSourceIsNotNull() {
        /* Branches:
         * (source == null) : false
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        doReturn(false).when(target).substitute((StrBuilder) any(), eq(0), eq(0));
        StringBuffer stringBuffer = new StringBuffer();
        
        //Act Statement(s)
        String result = target.replace(stringBuffer);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(""));
            verify(target).substitute((StrBuilder) any(), eq(0), eq(0));
        });
    }

    //BaseRock generated method id: ${replace13WhenSourceIsNull}, hash: 9EC15AE08E7A230052B7333E52924E7B
    @Test()
    void replace13WhenSourceIsNull() {
        /* Branches:
         * (source == null) : true
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        StringBuffer stringBuffer = null;
        
        //Act Statement(s)
        String result = target.replace(stringBuffer, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${replace13WhenSourceIsNotNull}, hash: CDA645A7082E5C3D5481614DE73EE0E0
    @Test()
    void replace13WhenSourceIsNotNull() {
        /* Branches:
         * (source == null) : false
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        doReturn(false).when(target).substitute((StrBuilder) any(), eq(0), eq(0));
        StringBuffer stringBuffer = new StringBuffer();
        
        //Act Statement(s)
        String result = target.replace(stringBuffer, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(""));
            verify(target).substitute((StrBuilder) any(), eq(0), eq(0));
        });
    }

    //BaseRock generated method id: ${replaceInWhenSourceIsNull}, hash: 6E98F490C346818935125394E4BE0C35
    @Test()
    void replaceInWhenSourceIsNull() {
        /* Branches:
         * (source == null) : true
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        StrBuilder strBuilder = null;
        
        //Act Statement(s)
        boolean result = target.replaceIn(strBuilder);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${replaceInWhenSubstituteSource0SourceLength}, hash: D2BD803C569101AD2A7BF1B93996576E
    @Test()
    void replaceInWhenSubstituteSource0SourceLength() {
        /* Branches:
         * (source == null) : false
         * (substitute(source, 0, source.length())) : true
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(true).when(target).substitute(strBuilder, 0, 0);
        
        //Act Statement(s)
        boolean result = target.replaceIn(strBuilder);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.TRUE));
            verify(target).substitute(strBuilder, 0, 0);
        });
    }

    //BaseRock generated method id: ${replaceInWhenSubstituteNotSource0SourceLength}, hash: 7D96C091668D01BA6FAC3A4D9263B2BA
    @Test()
    void replaceInWhenSubstituteNotSource0SourceLength() {
        /* Branches:
         * (source == null) : false
         * (substitute(source, 0, source.length())) : false
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(false).when(target).substitute(strBuilder, 0, 0);
        
        //Act Statement(s)
        boolean result = target.replaceIn(strBuilder);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.FALSE));
            verify(target).substitute(strBuilder, 0, 0);
        });
    }

    //BaseRock generated method id: ${replaceIn1WhenSourceIsNull}, hash: 33DBEC922A14A27F8F247D118050B34C
    @Test()
    void replaceIn1WhenSourceIsNull() {
        /* Branches:
         * (source == null) : true
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        StrBuilder strBuilder = null;
        
        //Act Statement(s)
        boolean result = target.replaceIn(strBuilder, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${replaceIn1WhenSubstituteSourceOffsetLength}, hash: C5024DF444BAAC7FE6E3E5083BC4BBFA
    @Test()
    void replaceIn1WhenSubstituteSourceOffsetLength() {
        /* Branches:
         * (source == null) : false
         * (substitute(source, offset, length)) : true
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(true).when(target).substitute(strBuilder, 0, 0);
        
        //Act Statement(s)
        boolean result = target.replaceIn(strBuilder, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.TRUE));
            verify(target).substitute(strBuilder, 0, 0);
        });
    }

    //BaseRock generated method id: ${replaceIn1WhenSubstituteNotSourceOffsetLength}, hash: BD03EFC47864B37495A8E633372C3A46
    @Test()
    void replaceIn1WhenSubstituteNotSourceOffsetLength() {
        /* Branches:
         * (source == null) : false
         * (substitute(source, offset, length)) : false
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(false).when(target).substitute(strBuilder, 0, 0);
        
        //Act Statement(s)
        boolean result = target.replaceIn(strBuilder, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.FALSE));
            verify(target).substitute(strBuilder, 0, 0);
        });
    }

    //BaseRock generated method id: ${replaceIn2WhenSourceIsNull}, hash: 58AECABF2B6A369FEBCB4FD4FF3B9DAC
    @Test()
    void replaceIn2WhenSourceIsNull() {
        /* Branches:
         * (source == null) : true
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        StringBuffer stringBuffer = null;
        
        //Act Statement(s)
        boolean result = target.replaceIn(stringBuffer);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${replaceIn2WhenReplaceInSource0SourceLength}, hash: 3CCED636798A6A30555897C0FFC26856
    @Test()
    void replaceIn2WhenReplaceInSource0SourceLength() {
        /* Branches:
         * (source == null) : false
         * (replaceIn(source, 0, source.length())) : true
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        StringBuffer stringBuffer = new StringBuffer();
        doReturn(true).when(target).replaceIn(stringBuffer, 0, 0);
        
        //Act Statement(s)
        boolean result = target.replaceIn(stringBuffer);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.TRUE));
            verify(target).replaceIn(stringBuffer, 0, 0);
        });
    }

    //BaseRock generated method id: ${replaceIn2WhenReplaceInNotSource0SourceLength}, hash: 4A294621545832A8EBEABC2D01AF50CF
    @Test()
    void replaceIn2WhenReplaceInNotSource0SourceLength() {
        /* Branches:
         * (source == null) : false
         * (replaceIn(source, 0, source.length())) : false
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        StringBuffer stringBuffer = new StringBuffer();
        doReturn(false).when(target).replaceIn(stringBuffer, 0, 0);
        
        //Act Statement(s)
        boolean result = target.replaceIn(stringBuffer);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.FALSE));
            verify(target).replaceIn(stringBuffer, 0, 0);
        });
    }

    //BaseRock generated method id: ${replaceIn3WhenSourceIsNull}, hash: D69C36B36EAD8B0DDAD3DAC813A02C3B
    @Test()
    void replaceIn3WhenSourceIsNull() {
        /* Branches:
         * (source == null) : true
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        StringBuffer stringBuffer = null;
        
        //Act Statement(s)
        boolean result = target.replaceIn(stringBuffer, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${replaceIn3WhenSubstituteNotBuf0Length}, hash: 9A729C4E3D05AD33F457FC2D5FF4208D
    @Test()
    void replaceIn3WhenSubstituteNotBuf0Length() {
        /* Branches:
         * (source == null) : false
         * (!substitute(buf, 0, length)) : true
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        doReturn(false).when(target).substitute((StrBuilder) any(), eq(0), eq(0));
        StringBuffer stringBuffer = new StringBuffer();
        
        //Act Statement(s)
        boolean result = target.replaceIn(stringBuffer, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.FALSE));
            verify(target).substitute((StrBuilder) any(), eq(0), eq(0));
        });
    }

    //BaseRock generated method id: ${replaceIn3WhenSubstituteBuf0Length}, hash: 4CA57B3A042FBA01297B25575BD31DC0
    @Test()
    void replaceIn3WhenSubstituteBuf0Length() {
        /* Branches:
         * (source == null) : false
         * (!substitute(buf, 0, length)) : false
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        doReturn(true).when(target).substitute((StrBuilder) any(), eq(0), eq(0));
        StringBuffer stringBuffer = new StringBuffer();
        
        //Act Statement(s)
        boolean result = target.replaceIn(stringBuffer, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.TRUE));
            verify(target).substitute((StrBuilder) any(), eq(0), eq(0));
        });
    }

    //BaseRock generated method id: ${replaceIn4WhenSourceIsNull}, hash: 056CBB679F60A24E533BF42EF1FB09C5
    @Test()
    void replaceIn4WhenSourceIsNull() {
        /* Branches:
         * (source == null) : true
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        StringBuilder stringBuilder = null;
        
        //Act Statement(s)
        boolean result = target.replaceIn(stringBuilder);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${replaceIn4WhenReplaceInSource0SourceLength}, hash: B524B8423E4B7B1DE617A5D2DD27C2DD
    @Test()
    void replaceIn4WhenReplaceInSource0SourceLength() {
        /* Branches:
         * (source == null) : false
         * (replaceIn(source, 0, source.length())) : true
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        StringBuilder stringBuilder = new StringBuilder("str1");
        doReturn(true).when(target).replaceIn(stringBuilder, 0, 4);
        
        //Act Statement(s)
        boolean result = target.replaceIn(stringBuilder);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.TRUE));
            verify(target).replaceIn(stringBuilder, 0, 4);
        });
    }

    //BaseRock generated method id: ${replaceIn4WhenReplaceInNotSource0SourceLength}, hash: 59F85F1BB33FA7D332BCCDA505BBEC7F
    @Test()
    void replaceIn4WhenReplaceInNotSource0SourceLength() {
        /* Branches:
         * (source == null) : false
         * (replaceIn(source, 0, source.length())) : false
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        StringBuilder stringBuilder = new StringBuilder("str1");
        doReturn(false).when(target).replaceIn(stringBuilder, 0, 4);
        
        //Act Statement(s)
        boolean result = target.replaceIn(stringBuilder);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.FALSE));
            verify(target).replaceIn(stringBuilder, 0, 4);
        });
    }

    //BaseRock generated method id: ${replaceIn5WhenSourceIsNull}, hash: 4774621C502BB57B53A4396EDB445D78
    @Test()
    void replaceIn5WhenSourceIsNull() {
        /* Branches:
         * (source == null) : true
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        StringBuilder stringBuilder = null;
        
        //Act Statement(s)
        boolean result = target.replaceIn(stringBuilder, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${replaceIn5WhenSubstituteNotBuf0Length}, hash: EC37F06E432EF346A92C7D54D4BDE528
    @Test()
    void replaceIn5WhenSubstituteNotBuf0Length() {
        /* Branches:
         * (source == null) : false
         * (!substitute(buf, 0, length)) : true
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        doReturn(false).when(target).substitute((StrBuilder) any(), eq(0), eq(0));
        StringBuilder stringBuilder = new StringBuilder();
        
        //Act Statement(s)
        boolean result = target.replaceIn(stringBuilder, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.FALSE));
            verify(target).substitute((StrBuilder) any(), eq(0), eq(0));
        });
    }

    //BaseRock generated method id: ${replaceIn5WhenSubstituteBuf0Length}, hash: AEA716116B811763E3A1FF35342E59DC
    @Test()
    void replaceIn5WhenSubstituteBuf0Length() {
        /* Branches:
         * (source == null) : false
         * (!substitute(buf, 0, length)) : false
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        doReturn(true).when(target).substitute((StrBuilder) any(), eq(0), eq(0));
        StringBuilder stringBuilder = new StringBuilder();
        
        //Act Statement(s)
        boolean result = target.replaceIn(stringBuilder, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.TRUE));
            verify(target).substitute((StrBuilder) any(), eq(0), eq(0));
        });
    }

    //BaseRock generated method id: ${resolveVariableWhenResolverIsNull}, hash: ED9BE45B9D4F7D28E1365606699A8885
    @Test()
    void resolveVariableWhenResolverIsNull() {
        /* Branches:
         * (resolver == null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        StrBuilder strBuilder = new StrBuilder();
        
        //Act Statement(s)
        String result = target.resolveVariable("variableName1", strBuilder, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${resolveVariableWhenResolverIsNotNull}, hash: 0D93FDABF9C76FD605517524116F9A96
    @Test()
    void resolveVariableWhenResolverIsNotNull() {
        /* Branches:
         * (resolver == null) : false
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        StrBuilder strBuilder = new StrBuilder();
        
        //Act Statement(s)
        String result = target.resolveVariable("variableName1", strBuilder, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${setEnableSubstitutionInVariablesTest}, hash: 2A562C5CBB12E0839BCCCCF5555F2BF8
    @Test()
    void setEnableSubstitutionInVariablesTest() {
        //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        
        //Act Statement(s)
        target.setEnableSubstitutionInVariables(false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(target.isEnableSubstitutionInVariables(), equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${setEscapeCharTest}, hash: EBDA0EDE78399A978495C73B6B5169D2
    @Test()
    void setEscapeCharTest() {
        //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        
        //Act Statement(s)
        target.setEscapeChar('A');
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(target.getEscapeChar(), equalTo('A')));
    }

    //BaseRock generated method id: ${setPreserveEscapesTest}, hash: 5F34D0F78DE7574ED930B1536E66615D
    @Test()
    void setPreserveEscapesTest() {
        //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        
        //Act Statement(s)
        target.setPreserveEscapes(false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(target.isPreserveEscapes(), equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${setValueDelimiterTest}, hash: C7ECDFA50E04E04E105B50825A03B8F2
    @Test()
    void setValueDelimiterTest() {
        //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        
        //Act Statement(s)
        StrSubstitutor result = target.setValueDelimiter('A');
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${setValueDelimiter1WhenStringUtilsIsEmptyValueDelimiter}, hash: E47307DCC788601BD4A4556A0E55D6C0
    @Test()
    void setValueDelimiter1WhenStringUtilsIsEmptyValueDelimiter() {
        /* Branches:
         * (StringUtils.isEmpty(valueDelimiter)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        
        //Act Statement(s)
        StrSubstitutor result = target.setValueDelimiter("valueDelimiter1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${setValueDelimiter1WhenStringUtilsNotIsEmptyValueDelimiter}, hash: 6C15EA866D2F5E06B25C57E30534E868
    @Test()
    void setValueDelimiter1WhenStringUtilsNotIsEmptyValueDelimiter() {
        /* Branches:
         * (StringUtils.isEmpty(valueDelimiter)) : false
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        
        //Act Statement(s)
        StrSubstitutor result = target.setValueDelimiter("valueDelimiter1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${setValueDelimiterMatcherTest}, hash: D2965DBBAC87E102802899E5E1622AE1
    @Test()
    void setValueDelimiterMatcherTest() {
        //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        StrMatcher strMatcher = StrMatcher.stringMatcher("valueDelimiter1");
        
        //Act Statement(s)
        StrSubstitutor result = target.setValueDelimiterMatcher(strMatcher);
        
        //Assert statement(s)
        //TODO: Please implement equals method in StrMatcher for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            assertThat(target.getValueDelimiterMatcher(), is(notNullValue()));
        });
    }

    //BaseRock generated method id: ${setVariablePrefixTest}, hash: 8F56E82A41AAB44D605DD4299FDAE9FE
    @Test()
    void setVariablePrefixTest() {
        //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        
        //Act Statement(s)
        StrSubstitutor result = target.setVariablePrefix('A');
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${setVariablePrefix1Test}, hash: 4074FE343E9058413DD1AE49EBC7319E
    @Test()
    void setVariablePrefix1Test() {
        //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        
        //Act Statement(s)
        StrSubstitutor result = target.setVariablePrefix("prefix1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${setVariablePrefixMatcherTest}, hash: D30AB54D8B15439F4E4B2D6D81C11EE9
    @Test()
    void setVariablePrefixMatcherTest() {
        //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        StrMatcher strMatcher = StrMatcher.stringMatcher("prefix1");
        
        //Act Statement(s)
        StrSubstitutor result = target.setVariablePrefixMatcher(strMatcher);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${setVariableResolverTest}, hash: D9E3025F2E0644A0B17B61C400F53296
    @Test()
    void setVariableResolverTest() {
        //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        StrLookup<?> strLookup = StrLookup.mapLookup(stringObjectMap);
        
        //Act Statement(s)
        target.setVariableResolver(strLookup);
        
        //Assert statement(s)
        //TODO: Please implement equals method in StrLookup for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(target.getVariableResolver(), is(notNullValue())));
    }

    //BaseRock generated method id: ${setVariableSuffixTest}, hash: CC4B33D122ED089CB98B89EFA0452CC7
    @Test()
    void setVariableSuffixTest() {
        //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        
        //Act Statement(s)
        StrSubstitutor result = target.setVariableSuffix('A');
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${setVariableSuffix1Test}, hash: 5292BD60FDEA2BC99516A92C1135F02D
    @Test()
    void setVariableSuffix1Test() {
        //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        
        //Act Statement(s)
        StrSubstitutor result = target.setVariableSuffix("suffix1");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${setVariableSuffixMatcherTest}, hash: 33035D0708838A02FA669A995191BA42
    @Test()
    void setVariableSuffixMatcherTest() {
        //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        StrMatcher strMatcher = StrMatcher.stringMatcher("suffix1");
        
        //Act Statement(s)
        StrSubstitutor result = target.setVariableSuffixMatcher(strMatcher);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${substituteWhenNotAlteredAndSubstituteBufOffsetLengthNullNotGreaterThan0}, hash: 3C2146FE3C1DFAD66D7A25427CF3027A
    @Test()
    void substituteWhenNotAlteredAndSubstituteBufOffsetLengthNullNotGreaterThan0() {
        /* Branches:
         * (priorVariables == null) : true  #  inside substitute method
         * (pos < bufEnd) : true  #  inside substitute method
         * (startMatchLen == 0) : true  #  inside substitute method
         * (top) : true  #  inside substitute method
         * (altered) : false  #  inside substitute method
         * (substitute(buf, offset, length, null) > 0) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        StrBuilder strBuilder = new StrBuilder();
        
        //Act Statement(s)
        boolean result = target.substitute(strBuilder, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${substituteWhenPriorVariablesContainsVarNameThrowsIllegalStateException}, hash: 0EA28E61C3CECB29B150FB522981E185
    @Test()
    void substituteWhenPriorVariablesContainsVarNameThrowsIllegalStateException() {
        /* Branches:
         * (priorVariables == null) : true  #  inside substitute method
         * (pos < bufEnd) : true  #  inside substitute method
         * (startMatchLen == 0) : false  #  inside substitute method
         * (pos > offset) : false  #  inside substitute method
         * (pos < bufEnd) : true  #  inside substitute method
         * (substitutionInVariablesEnabled) : false  #  inside substitute method
         * (endMatchLen == 0) : true  #  inside substitute method
         * (nestedVarCount == 0) : true  #  inside substitute method
         * (substitutionInVariablesEnabled) : false  #  inside substitute method
         * (valueDelimMatcher != null) : false  #  inside substitute method
         * (priorVariables == null) : true  #  inside substitute method
         * (!priorVariables.contains(varName)) : false  #  inside checkCyclicSubstitution method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1");
        StrBuilder strBuilder = new StrBuilder();
        IllegalStateException illegalStateException = new IllegalStateException("s1");
        //Act Statement(s)
        final IllegalStateException result = assertThrows(IllegalStateException.class, () -> {
            target.substitute(strBuilder, 0, 0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalStateException.getMessage()));
        });
    }

    //BaseRock generated method id: ${substituteWhenVarValueIsNullAndTopAndNotAlteredAndSubstituteBufOffsetLengthNullNotGreaterThan0}, hash: B50F64C381F7804412A7D23BDDE7D428
    @Test()
    void substituteWhenVarValueIsNullAndTopAndNotAlteredAndSubstituteBufOffsetLengthNullNotGreaterThan0() {
        /* Branches:
         * (priorVariables == null) : true  #  inside substitute method
         * (pos < bufEnd) : true  #  inside substitute method
         * (startMatchLen == 0) : false  #  inside substitute method
         * (pos > offset) : false  #  inside substitute method
         * (pos < bufEnd) : true  #  inside substitute method
         * (substitutionInVariablesEnabled) : false  #  inside substitute method
         * (endMatchLen == 0) : true  #  inside substitute method
         * (nestedVarCount == 0) : true  #  inside substitute method
         * (substitutionInVariablesEnabled) : false  #  inside substitute method
         * (valueDelimMatcher != null) : false  #  inside substitute method
         * (priorVariables == null) : true  #  inside substitute method
         * (!priorVariables.contains(varName)) : true  #  inside checkCyclicSubstitution method
         * (varValue == null) : true  #  inside substitute method
         * (varValue != null) : false  #  inside substitute method
         * (top) : true  #  inside substitute method
         * (altered) : false  #  inside substitute method
         * (substitute(buf, offset, length, null) > 0) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        StrBuilder strBuilder = new StrBuilder();
        doReturn(null).when(target).resolveVariable("string3", strBuilder, 0, 0);
        
        //Act Statement(s)
        boolean result = target.substitute(strBuilder, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.FALSE));
            verify(target).resolveVariable("string3", strBuilder, 0, 0);
        });
    }

    //BaseRock generated method id: ${substituteWhenSubstituteBufOffsetLengthNullGreaterThan0}, hash: 56B6550DAC9663441F2AD3B8C20791C5
    @Test()
    void substituteWhenSubstituteBufOffsetLengthNullGreaterThan0() {
        /* Branches:
         * (priorVariables == null) : true  #  inside substitute method
         * (pos < bufEnd) : true  #  inside substitute method
         * (startMatchLen == 0) : false  #  inside substitute method
         * (pos > offset) : false  #  inside substitute method
         * (pos < bufEnd) : true  #  inside substitute method
         * (substitutionInVariablesEnabled) : false  #  inside substitute method
         * (endMatchLen == 0) : true  #  inside substitute method
         * (nestedVarCount == 0) : true  #  inside substitute method
         * (substitutionInVariablesEnabled) : false  #  inside substitute method
         * (valueDelimMatcher != null) : false  #  inside substitute method
         * (priorVariables == null) : true  #  inside substitute method
         * (!priorVariables.contains(varName)) : true  #  inside checkCyclicSubstitution method
         * (varValue == null) : false  #  inside substitute method
         * (varValue != null) : true  #  inside substitute method
         * (top) : true  #  inside substitute method
         * (altered) : true  #  inside substitute method
         * (substitute(buf, offset, length, null) > 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Map<String, Object> stringObjectMap = new HashMap<>();
        StrSubstitutor target = spy(new StrSubstitutor(stringObjectMap, "prefix1", "suffix1", 'A', "valueDelimiter1"));
        StrBuilder strBuilder = new StrBuilder();
        doReturn("return_of_resolveVariable1").when(target).resolveVariable("string3", strBuilder, 0, 0);
        
        //Act Statement(s)
        boolean result = target.substitute(strBuilder, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.TRUE));
            verify(target).resolveVariable("string3", strBuilder, 0, 0);
        });
    }
}

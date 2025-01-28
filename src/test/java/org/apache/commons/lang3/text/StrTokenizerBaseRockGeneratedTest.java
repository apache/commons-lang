package org.apache.commons.lang3.text;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.List;
import java.util.Arrays;
import java.util.NoSuchElementException;
import java.util.ArrayList;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInRelativeOrder;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.Mockito.doThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class StrTokenizerBaseRockGeneratedTest {

    //BaseRock generated method id: ${getCSVInstanceTest}, hash: 03465680A7AC2FC33A014A6D9780B288
    @Test()
    void getCSVInstanceTest() {
        
        //Act Statement(s)
        StrTokenizer result = StrTokenizer.getCSVInstance();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getCSVInstance1Test}, hash: 92D424A3ADE538A50A0EFD39E5D1CC3B
    @Test()
    void getCSVInstance1Test() {
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        
        //Act Statement(s)
        StrTokenizer result = StrTokenizer.getCSVInstance(charArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getCSVInstance2Test}, hash: 9D9E305C6BA892730E8BB0C3E2B4BA20
    @Test()
    void getCSVInstance2Test() {
        
        //Act Statement(s)
        StrTokenizer result = StrTokenizer.getCSVInstance("A");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getTSVInstanceTest}, hash: 0B4C4A5A753D7CD0629B90FFCA6BA28A
    @Test()
    void getTSVInstanceTest() {
        
        //Act Statement(s)
        StrTokenizer result = StrTokenizer.getTSVInstance();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getTSVInstance1Test}, hash: 625F9A40CA797FF4C53FA1A2A1931733
    @Test()
    void getTSVInstance1Test() {
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        
        //Act Statement(s)
        StrTokenizer result = StrTokenizer.getTSVInstance(charArray);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getTSVInstance2Test}, hash: 737A0506898F069C3C9039B6D92C60AF
    @Test()
    void getTSVInstance2Test() {
        
        //Act Statement(s)
        StrTokenizer result = StrTokenizer.getTSVInstance("A");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${addThrowsUnsupportedOperationException}, hash: 2364DC6A28892CE09D9AA3D1D59DAFC4
    @Test()
    void addThrowsUnsupportedOperationException() {
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = new StrTokenizer(charArray, 'A', 'A');
        UnsupportedOperationException unsupportedOperationException = new UnsupportedOperationException("add() is unsupported");
        //Act Statement(s)
        final UnsupportedOperationException result = assertThrows(UnsupportedOperationException.class, () -> {
            target.add("obj1");
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(unsupportedOperationException.getMessage()));
        });
    }

    //BaseRock generated method id: ${cloneTest}, hash: 62461885A168EB8291562E40F123A975
    @Test()
    void cloneTest() throws CloneNotSupportedException {
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        Object object = new Object();
        doReturn(object).when(target).cloneReset();
        
        //Act Statement(s)
        Object result = target.clone();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(object));
            verify(target).cloneReset();
        });
    }

    //BaseRock generated method id: ${cloneWhenCaughtCloneNotSupportedException}, hash: 84F7419EE05FCAC30753E2779FDC93B1
    @Test()
    void cloneWhenCaughtCloneNotSupportedException() throws CloneNotSupportedException {
        /* Branches:
         * (catch-exception (CloneNotSupportedException)) : true
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        CloneNotSupportedException cloneNotSupportedException = new CloneNotSupportedException();
        doThrow(cloneNotSupportedException).when(target).cloneReset();
        
        //Act Statement(s)
        Object result = target.clone();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(nullValue()));
            verify(target).cloneReset();
        });
    }

    //BaseRock generated method id: ${cloneResetWhenClonedCharsIsNull}, hash: A0B01031F11D83902983E699C11CE8D4
    @Test()
    void cloneResetWhenClonedCharsIsNull() throws CloneNotSupportedException {
        /* Branches:
         * (cloned.chars != null) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = new StrTokenizer(charArray, 'A', 'A');
        
        //Act Statement(s)
        Object result = target.cloneReset();
        
        //Assert statement(s)
        //TODO: Please implement equals method in StrTokenizer for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getContentWhenCharsIsNull}, hash: A1BD66530AD216156678B5C4979C3D48
    @Test()
    void getContentWhenCharsIsNull() {
        /* Branches:
         * (chars == null) : true
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = new StrTokenizer(charArray, 'A', 'A');
        
        //Act Statement(s)
        String result = target.getContent();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getContentWhenCharsIsNotNull}, hash: 029CF5A033A20E6E18582AAE7252A12D
    @Test()
    void getContentWhenCharsIsNotNull() {
        /* Branches:
         * (chars == null) : false
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = new StrTokenizer(charArray, 'A', 'A');
        
        //Act Statement(s)
        String result = target.getContent();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${getDelimiterMatcherTest}, hash: FF32BC4CEC7C7DDD8494C354C473FDCE
    @Test()
    void getDelimiterMatcherTest() {
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = new StrTokenizer(charArray, 'A', 'A');
        
        //Act Statement(s)
        StrMatcher result = target.getDelimiterMatcher();
        
        //Assert statement(s)
        //TODO: Please implement equals method in StrMatcher for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getIgnoredMatcherTest}, hash: 770B99731008E6B3AAFDD56A28EA00CC
    @Test()
    void getIgnoredMatcherTest() {
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = new StrTokenizer(charArray, 'A', 'A');
        
        //Act Statement(s)
        StrMatcher result = target.getIgnoredMatcher();
        
        //Assert statement(s)
        //TODO: Please implement equals method in StrMatcher for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getQuoteMatcherTest}, hash: 67DCCACF27B7FB7E69D5A5D36D05F765
    @Test()
    void getQuoteMatcherTest() {
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = new StrTokenizer(charArray, 'A', 'A');
        
        //Act Statement(s)
        StrMatcher result = target.getQuoteMatcher();
        
        //Assert statement(s)
        //TODO: Please implement equals method in StrMatcher for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${getTokenArrayWhenCharsIsNull}, hash: AA09DA5577E68E258E5EC5AE3C4EBEA5
    @Test()
    void getTokenArrayWhenCharsIsNull() {
        /* Branches:
         * (tokens == null) : true  #  inside checkTokenized method
         * (chars == null) : true  #  inside checkTokenized method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        List<String> stringList = new ArrayList<>();
        doReturn(stringList).when(target).tokenize((char[]) null, 0, 0);
        
        //Act Statement(s)
        String[] result = target.getTokenArray();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result.length, equalTo(0));
            verify(target).tokenize((char[]) null, 0, 0);
        });
    }

    //BaseRock generated method id: ${getTokenArrayWhenCharsIsNotNull}, hash: 4685F7FD6F3291EE0445C4F0C82591D3
    @Test()
    void getTokenArrayWhenCharsIsNotNull() {
        /* Branches:
         * (tokens == null) : true  #  inside checkTokenized method
         * (chars == null) : false  #  inside checkTokenized method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        List<String> stringList = new ArrayList<>();
        char[] charArray2 = new char[] {};
        doReturn(stringList).when(target).tokenize(charArray2, 0, 0);
        
        //Act Statement(s)
        String[] result = target.getTokenArray();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result.length, equalTo(0));
            verify(target).tokenize(charArray2, 0, 0);
        });
    }

    //BaseRock generated method id: ${getTokenListWhenCharsIsNull}, hash: 2E1BAD42FDEA513B2078572005837EAF
    @Test()
    void getTokenListWhenCharsIsNull() {
        /* Branches:
         * (tokens == null) : true  #  inside checkTokenized method
         * (chars == null) : true  #  inside checkTokenized method
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        List<String> stringList = new ArrayList<>();
        doReturn(stringList).when(target).tokenize((char[]) null, 0, 0);
        
        //Act Statement(s)
        List<String> result = target.getTokenList();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result.size(), equalTo(0));
            verify(target).tokenize((char[]) null, 0, 0);
        });
    }

    //BaseRock generated method id: ${getTokenListWhenCharsIsNotNull}, hash: 4F63FF862DC24FE9F74050F7153827AD
    @Test()
    void getTokenListWhenCharsIsNotNull() {
        /* Branches:
         * (tokens == null) : true  #  inside checkTokenized method
         * (chars == null) : false  #  inside checkTokenized method
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        List<String> stringList = new ArrayList<>();
        char[] charArray2 = new char[] {};
        doReturn(stringList).when(target).tokenize(charArray2, 0, 0);
        
        //Act Statement(s)
        List<String> result = target.getTokenList();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result.size(), equalTo(0));
            verify(target).tokenize(charArray2, 0, 0);
        });
    }

    //BaseRock generated method id: ${getTrimmerMatcherTest}, hash: 6F25BB6BF47AB8400EF9BDC7214279C5
    @Test()
    void getTrimmerMatcherTest() {
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = new StrTokenizer(charArray, 'A', 'A');
        
        //Act Statement(s)
        StrMatcher result = target.getTrimmerMatcher();
        
        //Assert statement(s)
        //TODO: Please implement equals method in StrMatcher for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${hasNextWhenTokenPosLessThanTokensLength}, hash: 8EBF976BD7EC5E6C4EC151A2625628DB
    @Test()
    void hasNextWhenTokenPosLessThanTokensLength() {
        /* Branches:
         * (tokens == null) : true  #  inside checkTokenized method
         * (chars == null) : true  #  inside checkTokenized method
         * (tokenPos < tokens.length) : true
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        List<String> stringList = new ArrayList<>();
        doReturn(stringList).when(target).tokenize((char[]) null, 0, 0);
        
        //Act Statement(s)
        boolean result = target.hasNext();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.TRUE));
            verify(target).tokenize((char[]) null, 0, 0);
        });
    }

    //BaseRock generated method id: ${hasNextWhenCharsIsNotNullAndTokenPosNotLessThanTokensLength}, hash: 4A2ECF8EC319EC36709B59C3471E6605
    @Test()
    void hasNextWhenCharsIsNotNullAndTokenPosNotLessThanTokensLength() {
        /* Branches:
         * (tokens == null) : true  #  inside checkTokenized method
         * (chars == null) : false  #  inside checkTokenized method
         * (tokenPos < tokens.length) : false
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        List<String> stringList = new ArrayList<>();
        char[] charArray2 = new char[] {};
        doReturn(stringList).when(target).tokenize(charArray2, 0, 0);
        
        //Act Statement(s)
        boolean result = target.hasNext();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.FALSE));
            verify(target).tokenize(charArray2, 0, 0);
        });
    }

    //BaseRock generated method id: ${hasPreviousWhenTokenPosNotGreaterThan0}, hash: 835F87443EEF85954BEC9AFD70F24780
    @Test()
    void hasPreviousWhenTokenPosNotGreaterThan0() {
        /* Branches:
         * (tokens == null) : true  #  inside checkTokenized method
         * (chars == null) : true  #  inside checkTokenized method
         * (tokenPos > 0) : false
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        List<String> stringList = new ArrayList<>();
        doReturn(stringList).when(target).tokenize((char[]) null, 0, 0);
        
        //Act Statement(s)
        boolean result = target.hasPrevious();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.FALSE));
            verify(target).tokenize((char[]) null, 0, 0);
        });
    }

    //BaseRock generated method id: ${hasPreviousWhenCharsIsNotNullAndTokenPosNotGreaterThan0}, hash: 8FCE8AA7D1FB534F0AEA6E8EBB39102A
    @Test()
    void hasPreviousWhenCharsIsNotNullAndTokenPosNotGreaterThan0() {
        /* Branches:
         * (tokens == null) : true  #  inside checkTokenized method
         * (chars == null) : false  #  inside checkTokenized method
         * (tokenPos > 0) : false
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        List<String> stringList = new ArrayList<>();
        char[] charArray2 = new char[] {};
        doReturn(stringList).when(target).tokenize(charArray2, 0, 0);
        
        //Act Statement(s)
        boolean result = target.hasPrevious();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.FALSE));
            verify(target).tokenize(charArray2, 0, 0);
        });
    }

    //BaseRock generated method id: ${isEmptyTokenAsNullTest}, hash: B61BC59C3DBF78F2A2B351E1A53A5A47
    @Test()
    void isEmptyTokenAsNullTest() {
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = new StrTokenizer(charArray, 'A', 'A');
        
        //Act Statement(s)
        boolean result = target.isEmptyTokenAsNull();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${isIgnoreEmptyTokensTest}, hash: 5FD6B994BC9466B7919D0F4D27F74A82
    @Test()
    void isIgnoreEmptyTokensTest() {
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = new StrTokenizer(charArray, 'A', 'A');
        
        //Act Statement(s)
        boolean result = target.isIgnoreEmptyTokens();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${nextWhenHasNext}, hash: B34EFF2DEDFCC1CBE602C132C3CEB35C
    @Test()
    void nextWhenHasNext() {
        /* Branches:
         * (hasNext()) : true
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        doReturn(true).when(target).hasNext();
        
        //Act Statement(s)
        String result = target.next();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("result1"));
            verify(target).hasNext();
        });
    }

    //BaseRock generated method id: ${nextWhenHasNextNotThrowsNoSuchElementException}, hash: 8B62F7788D1AC4BCEAB9A16B23950A89
    @Test()
    void nextWhenHasNextNotThrowsNoSuchElementException() {
        /* Branches:
         * (hasNext()) : false
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        doReturn(false).when(target).hasNext();
        //Act Statement(s)
        final NoSuchElementException result = assertThrows(NoSuchElementException.class, () -> {
            target.next();
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            verify(target).hasNext();
        });
    }

    //BaseRock generated method id: ${nextIndexTest}, hash: 91782323874120D4F68E2739ECC23EC4
    @Test()
    void nextIndexTest() {
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = new StrTokenizer(charArray, 'A', 'A');
        
        //Act Statement(s)
        int result = target.nextIndex();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${nextTokenWhenHasNext}, hash: 96687D1E0BFF41D616C4B9C0978232BB
    @Test()
    void nextTokenWhenHasNext() {
        /* Branches:
         * (hasNext()) : true
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        doReturn(true).when(target).hasNext();
        
        //Act Statement(s)
        String result = target.nextToken();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("result1"));
            verify(target).hasNext();
        });
    }

    //BaseRock generated method id: ${nextTokenWhenHasNextNot}, hash: A2F7471AEB99CCBA2208CD8DA3C4A97A
    @Test()
    void nextTokenWhenHasNextNot() {
        /* Branches:
         * (hasNext()) : false
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        doReturn(false).when(target).hasNext();
        
        //Act Statement(s)
        String result = target.nextToken();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(nullValue()));
            verify(target).hasNext();
        });
    }

    //BaseRock generated method id: ${previousWhenHasPrevious}, hash: F060FD9A6AA0B6C079F8B21EE77DE512
    @Test()
    void previousWhenHasPrevious() {
        /* Branches:
         * (hasPrevious()) : true
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        doReturn(true).when(target).hasPrevious();
        
        //Act Statement(s)
        String result = target.previous();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("result1"));
            verify(target).hasPrevious();
        });
    }

    //BaseRock generated method id: ${previousWhenHasPreviousNotThrowsNoSuchElementException}, hash: 8AA443A58BD30E21A84902A3F6484EBB
    @Test()
    void previousWhenHasPreviousNotThrowsNoSuchElementException() {
        /* Branches:
         * (hasPrevious()) : false
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        doReturn(false).when(target).hasPrevious();
        //Act Statement(s)
        final NoSuchElementException result = assertThrows(NoSuchElementException.class, () -> {
            target.previous();
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            verify(target).hasPrevious();
        });
    }

    //BaseRock generated method id: ${previousIndexTest}, hash: 773EB15F0A739CA4C023A8E8AA11E8A3
    @Test()
    void previousIndexTest() {
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = new StrTokenizer(charArray, 'A', 'A');
        
        //Act Statement(s)
        int result = target.previousIndex();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(-1)));
    }

    //BaseRock generated method id: ${previousTokenWhenHasPrevious}, hash: D28F44F2826E2B59793A673D559DAA8A
    @Test()
    void previousTokenWhenHasPrevious() {
        /* Branches:
         * (hasPrevious()) : true
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        doReturn(true).when(target).hasPrevious();
        
        //Act Statement(s)
        String result = target.previousToken();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("result1"));
            verify(target).hasPrevious();
        });
    }

    //BaseRock generated method id: ${previousTokenWhenHasPreviousNot}, hash: 3689FBA190F7E660EDFF661627EFE470
    @Test()
    void previousTokenWhenHasPreviousNot() {
        /* Branches:
         * (hasPrevious()) : false
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        doReturn(false).when(target).hasPrevious();
        
        //Act Statement(s)
        String result = target.previousToken();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(nullValue()));
            verify(target).hasPrevious();
        });
    }

    //BaseRock generated method id: ${removeThrowsUnsupportedOperationException}, hash: 9E50932247BBCD98DEAD6C56C2C6FB54
    @Test()
    void removeThrowsUnsupportedOperationException() {
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = new StrTokenizer(charArray, 'A', 'A');
        UnsupportedOperationException unsupportedOperationException = new UnsupportedOperationException("remove() is unsupported");
        //Act Statement(s)
        final UnsupportedOperationException result = assertThrows(UnsupportedOperationException.class, () -> {
            target.remove();
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(unsupportedOperationException.getMessage()));
        });
    }

    //BaseRock generated method id: ${resetTest}, hash: B055E3F61FB99EBBB11C1F4BFDFE8AA0
    @Test()
    void resetTest() {
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = new StrTokenizer(charArray, 'A', 'A');
        
        //Act Statement(s)
        StrTokenizer result = target.reset();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${reset1Test}, hash: 5497AEA268D10AFA8D505714D18429AF
    @Test()
    void reset1Test() {
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = new StrTokenizer(charArray, 'A', 'A');
        char[] charArray2 = new char[] {};
        
        //Act Statement(s)
        StrTokenizer result = target.reset(charArray2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${reset2WhenInputIsNotNull}, hash: 6E225B9322957B81694B56423F49FD8B
    @Test()
    void reset2WhenInputIsNotNull() {
        /* Branches:
         * (input != null) : true
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = new StrTokenizer(charArray, 'A', 'A');
        
        //Act Statement(s)
        StrTokenizer result = target.reset("");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${reset2WhenInputIsNull}, hash: 5D776710C2B2DFD096A0D16801278EC3
    @Test()
    void reset2WhenInputIsNull() {
        /* Branches:
         * (input != null) : false
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = new StrTokenizer(charArray, 'A', 'A');
        
        //Act Statement(s)
        StrTokenizer result = target.reset((String) null);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${setThrowsUnsupportedOperationException}, hash: 5885F0854067D8D6985BECAF3241C50D
    @Test()
    void setThrowsUnsupportedOperationException() {
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = new StrTokenizer(charArray, 'A', 'A');
        UnsupportedOperationException unsupportedOperationException = new UnsupportedOperationException("set() is unsupported");
        //Act Statement(s)
        final UnsupportedOperationException result = assertThrows(UnsupportedOperationException.class, () -> {
            target.set("obj1");
        });
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(unsupportedOperationException.getMessage()));
        });
    }

    //BaseRock generated method id: ${setDelimiterCharTest}, hash: CB41CEC7B3DD1C18A5B5C4D2682F6712
    @Test()
    void setDelimiterCharTest() {
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        StrTokenizer strTokenizer = new StrTokenizer();
        doReturn(strTokenizer).when(target).setDelimiterMatcher((StrMatcher) any());
        
        //Act Statement(s)
        StrTokenizer result = target.setDelimiterChar('A');
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strTokenizer));
            verify(target).setDelimiterMatcher((StrMatcher) any());
        });
    }

    //BaseRock generated method id: ${setDelimiterMatcherWhenDelimIsNull}, hash: C0BE923C980500F1A047E80D36E7EB2B
    @Test()
    void setDelimiterMatcherWhenDelimIsNull() {
        /* Branches:
         * (delim == null) : true
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = new StrTokenizer(charArray, 'A', 'A');
        StrMatcher strMatcher = null;
        
        //Act Statement(s)
        StrTokenizer result = target.setDelimiterMatcher(strMatcher);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${setDelimiterStringTest}, hash: 896BF2D9C9DBF0C633EF79C46746A44F
    @Test()
    void setDelimiterStringTest() {
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        StrTokenizer strTokenizer = new StrTokenizer();
        doReturn(strTokenizer).when(target).setDelimiterMatcher((StrMatcher) any());
        
        //Act Statement(s)
        StrTokenizer result = target.setDelimiterString("");
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strTokenizer));
            verify(target).setDelimiterMatcher((StrMatcher) any());
        });
    }

    //BaseRock generated method id: ${setEmptyTokenAsNullTest}, hash: CC1684B32E69C5AFF8B67425679E2D7E
    @Test()
    void setEmptyTokenAsNullTest() {
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = new StrTokenizer(charArray, 'A', 'A');
        
        //Act Statement(s)
        StrTokenizer result = target.setEmptyTokenAsNull(false);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(target)));
    }

    //BaseRock generated method id: ${setIgnoredCharTest}, hash: 20C97D888CD2DECBE4570678966723E3
    @Test()
    void setIgnoredCharTest() {
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        StrTokenizer strTokenizer = new StrTokenizer();
        doReturn(strTokenizer).when(target).setIgnoredMatcher((StrMatcher) any());
        
        //Act Statement(s)
        StrTokenizer result = target.setIgnoredChar('A');
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strTokenizer));
            verify(target).setIgnoredMatcher((StrMatcher) any());
        });
    }

    //BaseRock generated method id: ${setIgnoredMatcherWhenIgnoredIsNotNull}, hash: 8722318654AD848D2C98E3C2D17662B6
    @Test()
    void setIgnoredMatcherWhenIgnoredIsNotNull() {
        /* Branches:
         * (ignored != null) : true
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = new StrTokenizer(charArray, 'A', 'A');
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        
        //Act Statement(s)
        StrTokenizer result = target.setIgnoredMatcher(strMatcher);
        
        //Assert statement(s)
        //TODO: Please implement equals method in StrMatcher for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            assertThat(target.getIgnoredMatcher(), is(notNullValue()));
        });
    }

    //BaseRock generated method id: ${setIgnoreEmptyTokensTest}, hash: 43FE6049F5F54745BBFDCD9965BB3FEE
    @Test()
    void setIgnoreEmptyTokensTest() {
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = new StrTokenizer(charArray, 'A', 'A');
        
        //Act Statement(s)
        StrTokenizer result = target.setIgnoreEmptyTokens(false);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            assertThat(target.isIgnoreEmptyTokens(), equalTo(Boolean.FALSE));
        });
    }

    //BaseRock generated method id: ${setQuoteCharTest}, hash: E605F8056894872592168EEA6D47C0DC
    @Test()
    void setQuoteCharTest() {
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        StrTokenizer strTokenizer = new StrTokenizer();
        doReturn(strTokenizer).when(target).setQuoteMatcher((StrMatcher) any());
        
        //Act Statement(s)
        StrTokenizer result = target.setQuoteChar('A');
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(strTokenizer));
            verify(target).setQuoteMatcher((StrMatcher) any());
        });
    }

    //BaseRock generated method id: ${setQuoteMatcherWhenQuoteIsNotNull}, hash: B5B7C124A8A6CE28D288180580DB19AD
    @Test()
    void setQuoteMatcherWhenQuoteIsNotNull() {
        /* Branches:
         * (quote != null) : true
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = new StrTokenizer(charArray, 'A', 'A');
        StrMatcher strMatcher = StrMatcher.charMatcher('A');
        
        //Act Statement(s)
        StrTokenizer result = target.setQuoteMatcher(strMatcher);
        
        //Assert statement(s)
        //TODO: Please implement equals method in StrMatcher for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            assertThat(target.getQuoteMatcher(), is(notNullValue()));
        });
    }

    //BaseRock generated method id: ${setTrimmerMatcherWhenTrimmerIsNotNull}, hash: 9F1B0E9AF70F3036D05C142BD64101CE
    @Test()
    void setTrimmerMatcherWhenTrimmerIsNotNull() {
        /* Branches:
         * (trimmer != null) : true
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = new StrTokenizer(charArray, 'A', 'A');
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        
        //Act Statement(s)
        StrTokenizer result = target.setTrimmerMatcher(strMatcher);
        
        //Assert statement(s)
        //TODO: Please implement equals method in StrMatcher for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> {
            assertThat(result, equalTo(target));
            assertThat(target.getTrimmerMatcher(), is(notNullValue()));
        });
    }

    //BaseRock generated method id: ${sizeWhenCharsIsNull}, hash: A453910F5FD357FE59CAFD2EB4576D7F
    @Test()
    void sizeWhenCharsIsNull() {
        /* Branches:
         * (tokens == null) : true  #  inside checkTokenized method
         * (chars == null) : true  #  inside checkTokenized method
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        List<String> stringList = new ArrayList<>();
        doReturn(stringList).when(target).tokenize((char[]) null, 0, 0);
        
        //Act Statement(s)
        int result = target.size();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(0));
            verify(target).tokenize((char[]) null, 0, 0);
        });
    }

    //BaseRock generated method id: ${sizeWhenCharsIsNotNull}, hash: 36C8C5F04EB2A8FC7FEBB1E2DDB5AB15
    @Test()
    void sizeWhenCharsIsNotNull() {
        /* Branches:
         * (tokens == null) : true  #  inside checkTokenized method
         * (chars == null) : false  #  inside checkTokenized method
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        List<String> stringList = new ArrayList<>();
        char[] charArray2 = new char[] {};
        doReturn(stringList).when(target).tokenize(charArray2, 0, 0);
        
        //Act Statement(s)
        int result = target.size();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(0));
            verify(target).tokenize(charArray2, 0, 0);
        });
    }

    //BaseRock generated method id: ${tokenizeWhenArrayUtilsIsEmptySrcChars}, hash: 45BEF2CB6DC530BA545100EA2CAB7BB8
    @Test()
    void tokenizeWhenArrayUtilsIsEmptySrcChars() {
        /* Branches:
         * (ArrayUtils.isEmpty(srcChars)) : true
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = new StrTokenizer(charArray, 'A', 'A');
        char[] charArray2 = new char[] {};
        
        //Act Statement(s)
        List<String> result = target.tokenize(charArray2, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result.size(), equalTo(0)));
    }

    //BaseRock generated method id: ${tokenizeWhenIsIgnoreEmptyTokensAndPosGreaterThanOrEqualsToCount}, hash: 58E4889A6FD006FC2563048EE7F46324
    @Test()
    void tokenizeWhenIsIgnoreEmptyTokensAndPosGreaterThanOrEqualsToCount() {
        /* Branches:
         * (ArrayUtils.isEmpty(srcChars)) : false
         * (pos >= 0) : true
         * (pos < count) : true
         * (start < len) : true  #  inside readNextToken method
         * (removeLen == 0) : false  #  inside readNextToken method
         * (getDelimiterMatcher().isMatch(srcChars, start, start, len) > 0) : false  #  inside readNextToken method
         * (getQuoteMatcher().isMatch(srcChars, start, start, len) > 0) : true  #  inside readNextToken method
         * (start >= len) : false  #  inside readNextToken method
         * (delimLen > 0) : true  #  inside readNextToken method
         * (StringUtils.isEmpty(tok)) : true  #  inside addToken method
         * (isIgnoreEmptyTokens()) : true  #  inside addToken method
         * (pos >= count) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        doReturn(strMatcher).when(target).getIgnoredMatcher();
        StrMatcher strMatcher2 = StrMatcher.commaMatcher();
        doReturn(strMatcher2).when(target).getTrimmerMatcher();
        StrMatcher strMatcher3 = StrMatcher.commaMatcher();
        StrMatcher strMatcher4 = StrMatcher.commaMatcher();
        doReturn(strMatcher3, strMatcher4).when(target).getDelimiterMatcher();
        doReturn(false).when(target).isIgnoreEmptyTokens();
        char[] charArray2 = new char[] {};
        
        //Act Statement(s)
        List<String> result = target.tokenize(charArray2, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result.size(), equalTo(0));
            verify(target).getIgnoredMatcher();
            verify(target).getTrimmerMatcher();
            verify(target, times(2)).getDelimiterMatcher();
            verify(target, times(2)).isIgnoreEmptyTokens();
        });
    }

    //BaseRock generated method id: ${tokenizeWhenIsEmptyTokenAsNullNot}, hash: 8227F45E17BE4BAD5FDC0A903B619776
    @Test()
    void tokenizeWhenIsEmptyTokenAsNullNot() {
        /* Branches:
         * (ArrayUtils.isEmpty(srcChars)) : false
         * (pos >= 0) : true
         * (pos < count) : true
         * (start < len) : true  #  inside readNextToken method
         * (removeLen == 0) : false  #  inside readNextToken method
         * (getDelimiterMatcher().isMatch(srcChars, start, start, len) > 0) : false  #  inside readNextToken method
         * (getQuoteMatcher().isMatch(srcChars, start, start, len) > 0) : false  #  inside readNextToken method
         * (start >= len) : true  #  inside readNextToken method
         * (StringUtils.isEmpty(tok)) : true  #  inside addToken method
         * (isIgnoreEmptyTokens()) : true  #  inside addToken method
         * (pos >= count) : true
         * (isEmptyTokenAsNull()) : false  #  inside addToken method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        doReturn(strMatcher).when(target).getIgnoredMatcher();
        StrMatcher strMatcher2 = StrMatcher.commaMatcher();
        doReturn(strMatcher2).when(target).getTrimmerMatcher();
        StrMatcher strMatcher3 = StrMatcher.commaMatcher();
        doReturn(strMatcher3).when(target).getDelimiterMatcher();
        doReturn(false).when(target).isIgnoreEmptyTokens();
        doReturn(false).when(target).isEmptyTokenAsNull();
        char[] charArray2 = new char[] {};
        
        //Act Statement(s)
        List<String> result = target.tokenize(charArray2, 0, 0);
        List<String> stringResultList = new ArrayList<>(Arrays.asList(""));
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result.size(), equalTo(1));
            assertThat(result, containsInRelativeOrder(stringResultList.toArray()));
            verify(target).getIgnoredMatcher();
            verify(target).getTrimmerMatcher();
            verify(target).getDelimiterMatcher();
            verify(target, times(2)).isIgnoreEmptyTokens();
            verify(target).isEmptyTokenAsNull();
        });
    }

    //BaseRock generated method id: ${tokenizeWhenIsEmptyTokenAsNullAndPosGreaterThanOrEqualsToCount}, hash: 1CCA30ED5E7A4C90DD13981569FB5700
    @Test()
    void tokenizeWhenIsEmptyTokenAsNullAndPosGreaterThanOrEqualsToCount() {
        /* Branches:
         * (ArrayUtils.isEmpty(srcChars)) : false
         * (pos >= 0) : true
         * (pos < count) : true
         * (start < len) : true  #  inside readNextToken method
         * (removeLen == 0) : false  #  inside readNextToken method
         * (getDelimiterMatcher().isMatch(srcChars, start, start, len) > 0) : false  #  inside readNextToken method
         * (getQuoteMatcher().isMatch(srcChars, start, start, len) > 0) : false  #  inside readNextToken method
         * (start >= len) : true  #  inside readNextToken method
         * (StringUtils.isEmpty(tok)) : true  #  inside addToken method
         * (isIgnoreEmptyTokens()) : false  #  inside addToken method
         * (isEmptyTokenAsNull()) : true  #  inside addToken method
         * (pos >= count) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        doReturn(strMatcher).when(target).getIgnoredMatcher();
        StrMatcher strMatcher2 = StrMatcher.commaMatcher();
        doReturn(strMatcher2).when(target).getTrimmerMatcher();
        StrMatcher strMatcher3 = StrMatcher.commaMatcher();
        doReturn(strMatcher3).when(target).getDelimiterMatcher();
        doReturn(false).when(target).isIgnoreEmptyTokens();
        doReturn(false).when(target).isEmptyTokenAsNull();
        char[] charArray2 = new char[] {};
        
        //Act Statement(s)
        List<String> result = target.tokenize(charArray2, 0, 0);
        List<String> stringResultList = new ArrayList<>();
        stringResultList.add((String) null);
        stringResultList.add((String) null);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result.size(), equalTo(2));
            assertThat(result, containsInRelativeOrder(stringResultList.toArray()));
            verify(target).getIgnoredMatcher();
            verify(target).getTrimmerMatcher();
            verify(target).getDelimiterMatcher();
            verify(target, times(2)).isIgnoreEmptyTokens();
            verify(target, times(2)).isEmptyTokenAsNull();
        });
    }

    //BaseRock generated method id: ${tokenizeWhenIsEmptyTokenAsNull}, hash: 3A2A2826A49A1AAF6ED9EB2B3142CAFF
    @Test()
    void tokenizeWhenIsEmptyTokenAsNull() {
        /* Branches:
         * (ArrayUtils.isEmpty(srcChars)) : false
         * (pos >= 0) : true
         * (pos < count) : true
         * (start < len) : true  #  inside readNextToken method
         * (removeLen == 0) : false  #  inside readNextToken method
         * (getDelimiterMatcher().isMatch(srcChars, start, start, len) > 0) : false  #  inside readNextToken method
         * (getQuoteMatcher().isMatch(srcChars, start, start, len) > 0) : true  #  inside readNextToken method
         * (start >= len) : false  #  inside readNextToken method
         * (delimLen > 0) : true  #  inside readNextToken method
         * (StringUtils.isEmpty(tok)) : true  #  inside addToken method
         * (isIgnoreEmptyTokens()) : true  #  inside addToken method
         * (pos >= count) : true
         * (isEmptyTokenAsNull()) : true  #  inside addToken method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        doReturn(strMatcher).when(target).getIgnoredMatcher();
        StrMatcher strMatcher2 = StrMatcher.commaMatcher();
        doReturn(strMatcher2).when(target).getTrimmerMatcher();
        StrMatcher strMatcher3 = StrMatcher.commaMatcher();
        StrMatcher strMatcher4 = StrMatcher.commaMatcher();
        doReturn(strMatcher3, strMatcher4).when(target).getDelimiterMatcher();
        doReturn(false).when(target).isIgnoreEmptyTokens();
        doReturn(false).when(target).isEmptyTokenAsNull();
        char[] charArray2 = new char[] {};
        
        //Act Statement(s)
        List<String> result = target.tokenize(charArray2, 0, 0);
        List<String> stringResultList = new ArrayList<>();
        stringResultList.add((String) null);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result.size(), equalTo(1));
            assertThat(result, containsInRelativeOrder(stringResultList.toArray()));
            verify(target).getIgnoredMatcher();
            verify(target).getTrimmerMatcher();
            verify(target, times(2)).getDelimiterMatcher();
            verify(target, times(2)).isIgnoreEmptyTokens();
            verify(target).isEmptyTokenAsNull();
        });
    }

    //BaseRock generated method id: ${tokenizeWhenIsIgnoreEmptyTokensNotAndIsEmptyTokenAsNullAndPosGreaterThanOrEqualsToCount}, hash: F9E48DB96D659891965B06E29904DED8
    @Test()
    void tokenizeWhenIsIgnoreEmptyTokensNotAndIsEmptyTokenAsNullAndPosGreaterThanOrEqualsToCount() {
        /* Branches:
         * (ArrayUtils.isEmpty(srcChars)) : false
         * (pos >= 0) : true
         * (pos < count) : true
         * (start < len) : true  #  inside readNextToken method
         * (removeLen == 0) : false  #  inside readNextToken method
         * (getDelimiterMatcher().isMatch(srcChars, start, start, len) > 0) : false  #  inside readNextToken method
         * (getQuoteMatcher().isMatch(srcChars, start, start, len) > 0) : true  #  inside readNextToken method
         * (start >= len) : false  #  inside readNextToken method
         * (delimLen > 0) : true  #  inside readNextToken method
         * (StringUtils.isEmpty(tok)) : true  #  inside addToken method
         * (isIgnoreEmptyTokens()) : false  #  inside addToken method
         * (isEmptyTokenAsNull()) : true  #  inside addToken method
         * (pos >= count) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        doReturn(strMatcher).when(target).getIgnoredMatcher();
        StrMatcher strMatcher2 = StrMatcher.commaMatcher();
        doReturn(strMatcher2).when(target).getTrimmerMatcher();
        StrMatcher strMatcher3 = StrMatcher.commaMatcher();
        StrMatcher strMatcher4 = StrMatcher.commaMatcher();
        doReturn(strMatcher3, strMatcher4).when(target).getDelimiterMatcher();
        doReturn(false).when(target).isIgnoreEmptyTokens();
        doReturn(false).when(target).isEmptyTokenAsNull();
        char[] charArray2 = new char[] {};
        
        //Act Statement(s)
        List<String> result = target.tokenize(charArray2, 0, 0);
        List<String> stringResultList = new ArrayList<>();
        stringResultList.add((String) null);
        stringResultList.add((String) null);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result.size(), equalTo(2));
            assertThat(result, containsInRelativeOrder(stringResultList.toArray()));
            verify(target).getIgnoredMatcher();
            verify(target).getTrimmerMatcher();
            verify(target, times(2)).getDelimiterMatcher();
            verify(target, times(2)).isIgnoreEmptyTokens();
            verify(target, times(2)).isEmptyTokenAsNull();
        });
    }

    //BaseRock generated method id: ${tokenizeWhenIsIgnoreEmptyTokensAndPosGreaterThanOrEqualsToCountAndIsEmptyTokenAsNullNot}, hash: 2933F464E718F41636D8C5E686A81866
    @Test()
    void tokenizeWhenIsIgnoreEmptyTokensAndPosGreaterThanOrEqualsToCountAndIsEmptyTokenAsNullNot() {
        /* Branches:
         * (ArrayUtils.isEmpty(srcChars)) : false
         * (pos >= 0) : true
         * (pos < count) : true
         * (start < len) : true  #  inside readNextToken method
         * (removeLen == 0) : false  #  inside readNextToken method
         * (getDelimiterMatcher().isMatch(srcChars, start, start, len) > 0) : false  #  inside readNextToken method
         * (getQuoteMatcher().isMatch(srcChars, start, start, len) > 0) : true  #  inside readNextToken method
         * (start >= len) : false  #  inside readNextToken method
         * (delimLen > 0) : false  #  inside readNextToken method
         * (quoteLen > 0) : false  #  inside readNextToken method
         * (quoteLen > 0) : false  #  inside readWithQuotes method
         * (pos < len) : true  #  inside readWithQuotes method
         * (quoting) : false  #  inside readWithQuotes method
         * (delimLen > 0) : true  #  inside readWithQuotes method
         * (StringUtils.isEmpty(tok)) : true  #  inside addToken method
         * (isIgnoreEmptyTokens()) : true  #  inside addToken method
         * (pos >= count) : true
         * (isEmptyTokenAsNull()) : false  #  inside addToken method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        doReturn(strMatcher).when(target).getIgnoredMatcher();
        StrMatcher strMatcher2 = StrMatcher.commaMatcher();
        doReturn(strMatcher2).when(target).getTrimmerMatcher();
        StrMatcher strMatcher3 = StrMatcher.commaMatcher();
        StrMatcher strMatcher4 = StrMatcher.commaMatcher();
        StrMatcher strMatcher5 = StrMatcher.commaMatcher();
        doReturn(strMatcher3, strMatcher4, strMatcher5).when(target).getDelimiterMatcher();
        doReturn(false).when(target).isIgnoreEmptyTokens();
        doReturn(false).when(target).isEmptyTokenAsNull();
        char[] charArray2 = new char[] {};
        
        //Act Statement(s)
        List<String> result = target.tokenize(charArray2, 0, 0);
        List<String> stringResultList = new ArrayList<>(Arrays.asList(""));
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result.size(), equalTo(1));
            assertThat(result, containsInRelativeOrder(stringResultList.toArray()));
            verify(target).getIgnoredMatcher();
            verify(target).getTrimmerMatcher();
            verify(target, times(3)).getDelimiterMatcher();
            verify(target, times(2)).isIgnoreEmptyTokens();
            verify(target).isEmptyTokenAsNull();
        });
    }

    //BaseRock generated method id: ${tokenizeWhenStringUtilsIsEmptyTokAndIsIgnoreEmptyTokensNotAndIsEmptyTokenAsNullAndPosGreaterThanOrEqualsToCount}, hash: 6BADD273AC810F772870EFD36FE0B689
    @Test()
    void tokenizeWhenStringUtilsIsEmptyTokAndIsIgnoreEmptyTokensNotAndIsEmptyTokenAsNullAndPosGreaterThanOrEqualsToCount() {
        /* Branches:
         * (ArrayUtils.isEmpty(srcChars)) : false
         * (pos >= 0) : true
         * (pos < count) : true
         * (start < len) : true  #  inside readNextToken method
         * (removeLen == 0) : false  #  inside readNextToken method
         * (getDelimiterMatcher().isMatch(srcChars, start, start, len) > 0) : false  #  inside readNextToken method
         * (getQuoteMatcher().isMatch(srcChars, start, start, len) > 0) : true  #  inside readNextToken method
         * (start >= len) : false  #  inside readNextToken method
         * (delimLen > 0) : false  #  inside readNextToken method
         * (quoteLen > 0) : false  #  inside readNextToken method
         * (quoteLen > 0) : false  #  inside readWithQuotes method
         * (pos < len) : true  #  inside readWithQuotes method
         * (quoting) : false  #  inside readWithQuotes method
         * (delimLen > 0) : true  #  inside readWithQuotes method
         * (StringUtils.isEmpty(tok)) : true  #  inside addToken method
         * (isIgnoreEmptyTokens()) : false  #  inside addToken method
         * (isEmptyTokenAsNull()) : true  #  inside addToken method
         * (pos >= count) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        doReturn(strMatcher).when(target).getIgnoredMatcher();
        StrMatcher strMatcher2 = StrMatcher.commaMatcher();
        doReturn(strMatcher2).when(target).getTrimmerMatcher();
        StrMatcher strMatcher3 = StrMatcher.commaMatcher();
        StrMatcher strMatcher4 = StrMatcher.commaMatcher();
        StrMatcher strMatcher5 = StrMatcher.commaMatcher();
        doReturn(strMatcher3, strMatcher4, strMatcher5).when(target).getDelimiterMatcher();
        doReturn(false).when(target).isIgnoreEmptyTokens();
        doReturn(false).when(target).isEmptyTokenAsNull();
        char[] charArray2 = new char[] {};
        
        //Act Statement(s)
        List<String> result = target.tokenize(charArray2, 0, 0);
        List<String> stringResultList = new ArrayList<>();
        stringResultList.add((String) null);
        stringResultList.add((String) null);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result.size(), equalTo(2));
            assertThat(result, containsInRelativeOrder(stringResultList.toArray()));
            verify(target).getIgnoredMatcher();
            verify(target).getTrimmerMatcher();
            verify(target, times(3)).getDelimiterMatcher();
            verify(target, times(2)).isIgnoreEmptyTokens();
            verify(target, times(2)).isEmptyTokenAsNull();
        });
    }

    //BaseRock generated method id: ${tokenizeWhenStringUtilsIsEmptyTokAndIsIgnoreEmptyTokensAndPosGreaterThanOrEqualsToCountAndIsEmptyTokenAsNullNot}, hash: D67C3ADADEA67B6A2EAA6767A6606865
    @Test()
    void tokenizeWhenStringUtilsIsEmptyTokAndIsIgnoreEmptyTokensAndPosGreaterThanOrEqualsToCountAndIsEmptyTokenAsNullNot() {
        /* Branches:
         * (ArrayUtils.isEmpty(srcChars)) : false
         * (pos >= 0) : true
         * (pos < count) : true
         * (start < len) : true  #  inside readNextToken method
         * (removeLen == 0) : false  #  inside readNextToken method
         * (getDelimiterMatcher().isMatch(srcChars, start, start, len) > 0) : false  #  inside readNextToken method
         * (getQuoteMatcher().isMatch(srcChars, start, start, len) > 0) : true  #  inside readNextToken method
         * (start >= len) : false  #  inside readNextToken method
         * (delimLen > 0) : false  #  inside readNextToken method
         * (quoteLen > 0) : false  #  inside readNextToken method
         * (quoteLen > 0) : false  #  inside readWithQuotes method
         * (pos < len) : true  #  inside readWithQuotes method
         * (quoting) : false  #  inside readWithQuotes method
         * (delimLen > 0) : false  #  inside readWithQuotes method
         * (quoteLen > 0) : false  #  inside readWithQuotes method
         * (ignoredLen > 0) : true  #  inside readWithQuotes method
         * (StringUtils.isEmpty(tok)) : true  #  inside addToken method
         * (isIgnoreEmptyTokens()) : true  #  inside addToken method
         * (pos >= count) : true
         * (isEmptyTokenAsNull()) : false  #  inside addToken method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        StrMatcher strMatcher2 = StrMatcher.commaMatcher();
        doReturn(strMatcher2).when(target).getTrimmerMatcher();
        StrMatcher strMatcher3 = StrMatcher.commaMatcher();
        StrMatcher strMatcher4 = StrMatcher.commaMatcher();
        StrMatcher strMatcher5 = StrMatcher.commaMatcher();
        doReturn(strMatcher3, strMatcher4, strMatcher5).when(target).getDelimiterMatcher();
        StrMatcher strMatcher6 = StrMatcher.commaMatcher();
        doReturn(strMatcher, strMatcher6).when(target).getIgnoredMatcher();
        doReturn(false).when(target).isIgnoreEmptyTokens();
        doReturn(false).when(target).isEmptyTokenAsNull();
        char[] charArray2 = new char[] {};
        
        //Act Statement(s)
        List<String> result = target.tokenize(charArray2, 0, 0);
        List<String> stringResultList = new ArrayList<>(Arrays.asList(""));
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result.size(), equalTo(1));
            assertThat(result, containsInRelativeOrder(stringResultList.toArray()));
            verify(target, times(2)).getIgnoredMatcher();
            verify(target).getTrimmerMatcher();
            verify(target, times(3)).getDelimiterMatcher();
            verify(target, times(2)).isIgnoreEmptyTokens();
            verify(target).isEmptyTokenAsNull();
        });
    }

    //BaseRock generated method id: ${tokenizeWhenIsQuoteNotSrcCharsPosLenQuoteStartQuoteLenAndStringUtilsIsEmptyTokAndIsIgnoreEmptyTokensNotAndIsEmptyTokenA}, hash: 735F3A7FDF972450576EB3DF83FB0065
    @Test()
    void tokenizeWhenIsQuoteNotSrcCharsPosLenQuoteStartQuoteLenAndStringUtilsIsEmptyTokAndIsIgnoreEmptyTokensNotAndIsEmptyTokenA() {
        /* Branches:
         * (ArrayUtils.isEmpty(srcChars)) : false
         * (pos >= 0) : true
         * (pos < count) : true
         * (start < len) : true  #  inside readNextToken method
         * (removeLen == 0) : false  #  inside readNextToken method
         * (getDelimiterMatcher().isMatch(srcChars, start, start, len) > 0) : false  #  inside readNextToken method
         * (getQuoteMatcher().isMatch(srcChars, start, start, len) > 0) : true  #  inside readNextToken method
         * (start >= len) : false  #  inside readNextToken method
         * (delimLen > 0) : false  #  inside readNextToken method
         * (quoteLen > 0) : true  #  inside readNextToken method
         * (quoteLen > 0) : true  #  inside readWithQuotes method
         * (pos < len) : true  #  inside readWithQuotes method
         * (quoting) : true  #  inside readWithQuotes method
         * (i < quoteLen) : true  #  inside isQuote method
         * (pos + i >= len) : false  #  inside isQuote method
         * (srcChars[pos + i] != srcChars[quoteStart + i]) : true  #  inside isQuote method
         * (isQuote(srcChars, pos, len, quoteStart, quoteLen)) : false  #  inside readWithQuotes method
         * (StringUtils.isEmpty(tok)) : true  #  inside addToken method
         * (isIgnoreEmptyTokens()) : false  #  inside addToken method
         * (isEmptyTokenAsNull()) : true  #  inside addToken method
         * (pos >= count) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        doReturn(strMatcher).when(target).getIgnoredMatcher();
        StrMatcher strMatcher2 = StrMatcher.commaMatcher();
        doReturn(strMatcher2).when(target).getTrimmerMatcher();
        StrMatcher strMatcher3 = StrMatcher.commaMatcher();
        StrMatcher strMatcher4 = StrMatcher.commaMatcher();
        doReturn(strMatcher3, strMatcher4).when(target).getDelimiterMatcher();
        doReturn(false).when(target).isIgnoreEmptyTokens();
        doReturn(false).when(target).isEmptyTokenAsNull();
        char[] charArray2 = new char[] { 'A', 'A' };
        
        //Act Statement(s)
        List<String> result = target.tokenize(charArray2, 0, 0);
        List<String> stringResultList = new ArrayList<>();
        stringResultList.add((String) null);
        stringResultList.add((String) null);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result.size(), equalTo(2));
            assertThat(result, containsInRelativeOrder(stringResultList.toArray()));
            verify(target).getIgnoredMatcher();
            verify(target).getTrimmerMatcher();
            verify(target, times(2)).getDelimiterMatcher();
            verify(target, times(2)).isIgnoreEmptyTokens();
            verify(target, times(2)).isEmptyTokenAsNull();
        });
    }

    //BaseRock generated method id: ${tokenizeWhenTrimmedLenGreaterThan0AndStringUtilsIsEmptyTokAndIsIgnoreEmptyTokensNotAndIsEmptyTokenAsNullAndPosGreaterTh}, hash: F27B905C2F63123F24DEAA1C052D8F4C
    @Test()
    void tokenizeWhenTrimmedLenGreaterThan0AndStringUtilsIsEmptyTokAndIsIgnoreEmptyTokensNotAndIsEmptyTokenAsNullAndPosGreaterTh() {
        /* Branches:
         * (ArrayUtils.isEmpty(srcChars)) : false
         * (pos >= 0) : true
         * (pos < count) : true
         * (start < len) : true  #  inside readNextToken method
         * (removeLen == 0) : false  #  inside readNextToken method
         * (getDelimiterMatcher().isMatch(srcChars, start, start, len) > 0) : false  #  inside readNextToken method
         * (getQuoteMatcher().isMatch(srcChars, start, start, len) > 0) : true  #  inside readNextToken method
         * (start >= len) : false  #  inside readNextToken method
         * (delimLen > 0) : false  #  inside readNextToken method
         * (quoteLen > 0) : false  #  inside readNextToken method
         * (quoteLen > 0) : false  #  inside readWithQuotes method
         * (pos < len) : true  #  inside readWithQuotes method
         * (quoting) : false  #  inside readWithQuotes method
         * (delimLen > 0) : false  #  inside readWithQuotes method
         * (quoteLen > 0) : false  #  inside readWithQuotes method
         * (ignoredLen > 0) : false  #  inside readWithQuotes method
         * (trimmedLen > 0) : true  #  inside readWithQuotes method
         * (StringUtils.isEmpty(tok)) : true  #  inside addToken method
         * (isIgnoreEmptyTokens()) : false  #  inside addToken method
         * (isEmptyTokenAsNull()) : true  #  inside addToken method
         * (pos >= count) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        StrMatcher strMatcher2 = StrMatcher.commaMatcher();
        StrMatcher strMatcher3 = StrMatcher.commaMatcher();
        StrMatcher strMatcher4 = StrMatcher.commaMatcher();
        StrMatcher strMatcher5 = StrMatcher.commaMatcher();
        doReturn(strMatcher3, strMatcher4, strMatcher5).when(target).getDelimiterMatcher();
        StrMatcher strMatcher6 = StrMatcher.commaMatcher();
        doReturn(strMatcher, strMatcher6).when(target).getIgnoredMatcher();
        StrMatcher strMatcher7 = StrMatcher.commaMatcher();
        doReturn(strMatcher2, strMatcher7).when(target).getTrimmerMatcher();
        doReturn(false).when(target).isIgnoreEmptyTokens();
        doReturn(false).when(target).isEmptyTokenAsNull();
        char[] charArray2 = new char[] {};
        
        //Act Statement(s)
        List<String> result = target.tokenize(charArray2, 0, 0);
        List<String> stringResultList = new ArrayList<>();
        stringResultList.add((String) null);
        stringResultList.add((String) null);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result.size(), equalTo(2));
            assertThat(result, containsInRelativeOrder(stringResultList.toArray()));
            verify(target, times(2)).getIgnoredMatcher();
            verify(target, times(2)).getTrimmerMatcher();
            verify(target, times(3)).getDelimiterMatcher();
            verify(target, times(2)).isIgnoreEmptyTokens();
            verify(target, times(2)).isEmptyTokenAsNull();
        });
    }

    //BaseRock generated method id: ${tokenizeWhenStringUtilsNotIsEmptyTokAndPosGreaterThanOrEqualsToCountAndIsIgnoreEmptyTokensNotAndIsEmptyTokenAsNullNot}, hash: 933B72B8EA0933A76825439D3D79E55B
    @Test()
    void tokenizeWhenStringUtilsNotIsEmptyTokAndPosGreaterThanOrEqualsToCountAndIsIgnoreEmptyTokensNotAndIsEmptyTokenAsNullNot() {
        /* Branches:
         * (ArrayUtils.isEmpty(srcChars)) : false
         * (pos >= 0) : true
         * (pos < count) : true
         * (start < len) : true  #  inside readNextToken method
         * (removeLen == 0) : false  #  inside readNextToken method
         * (getDelimiterMatcher().isMatch(srcChars, start, start, len) > 0) : false  #  inside readNextToken method
         * (getQuoteMatcher().isMatch(srcChars, start, start, len) > 0) : true  #  inside readNextToken method
         * (start >= len) : false  #  inside readNextToken method
         * (delimLen > 0) : false  #  inside readNextToken method
         * (quoteLen > 0) : false  #  inside readNextToken method
         * (quoteLen > 0) : false  #  inside readWithQuotes method
         * (pos < len) : true  #  inside readWithQuotes method
         * (quoting) : false  #  inside readWithQuotes method
         * (delimLen > 0) : false  #  inside readWithQuotes method
         * (quoteLen > 0) : false  #  inside readWithQuotes method
         * (ignoredLen > 0) : false  #  inside readWithQuotes method
         * (trimmedLen > 0) : false  #  inside readWithQuotes method
         * (StringUtils.isEmpty(tok)) : false  #  inside addToken method
         * (pos >= count) : true
         * (isIgnoreEmptyTokens()) : false  #  inside addToken method
         * (isEmptyTokenAsNull()) : false  #  inside addToken method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        StrMatcher strMatcher2 = StrMatcher.commaMatcher();
        StrMatcher strMatcher3 = StrMatcher.commaMatcher();
        StrMatcher strMatcher4 = StrMatcher.commaMatcher();
        StrMatcher strMatcher5 = StrMatcher.commaMatcher();
        doReturn(strMatcher3, strMatcher4, strMatcher5).when(target).getDelimiterMatcher();
        StrMatcher strMatcher6 = StrMatcher.commaMatcher();
        doReturn(strMatcher, strMatcher6).when(target).getIgnoredMatcher();
        StrMatcher strMatcher7 = StrMatcher.commaMatcher();
        doReturn(strMatcher2, strMatcher7).when(target).getTrimmerMatcher();
        doReturn(false).when(target).isIgnoreEmptyTokens();
        doReturn(false).when(target).isEmptyTokenAsNull();
        char[] charArray2 = new char[] { 'A' };
        
        //Act Statement(s)
        List<String> result = target.tokenize(charArray2, 0, 1);
        List<String> stringResultList = new ArrayList<>(Arrays.asList("A", ""));
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result.size(), equalTo(2));
            assertThat(result, containsInRelativeOrder(stringResultList.toArray()));
            verify(target, times(2)).getIgnoredMatcher();
            verify(target, times(2)).getTrimmerMatcher();
            verify(target, times(3)).getDelimiterMatcher();
            verify(target).isIgnoreEmptyTokens();
            verify(target).isEmptyTokenAsNull();
        });
    }

    //BaseRock generated method id: ${tokenizeWhenIsQuoteSrcCharsPosPlusQuoteLenLenQuoteStartQuoteLenAndStringUtilsNotIsEmptyTokAndPosGreaterThanOrEqualsToCo}, hash: 73A904921F11FF134F3009DCC20A9ABD
    @Test()
    void tokenizeWhenIsQuoteSrcCharsPosPlusQuoteLenLenQuoteStartQuoteLenAndStringUtilsNotIsEmptyTokAndPosGreaterThanOrEqualsToCo() {
        /* Branches:
         * (ArrayUtils.isEmpty(srcChars)) : false
         * (pos >= 0) : true
         * (pos < count) : true
         * (start < len) : true  #  inside readNextToken method
         * (removeLen == 0) : false  #  inside readNextToken method
         * (getDelimiterMatcher().isMatch(srcChars, start, start, len) > 0) : false  #  inside readNextToken method
         * (getQuoteMatcher().isMatch(srcChars, start, start, len) > 0) : true  #  inside readNextToken method
         * (start >= len) : false  #  inside readNextToken method
         * (delimLen > 0) : false  #  inside readNextToken method
         * (quoteLen > 0) : true  #  inside readNextToken method
         * (quoteLen > 0) : true  #  inside readWithQuotes method
         * (pos < len) : true  #  inside readWithQuotes method
         * (quoting) : true  #  inside readWithQuotes method
         * (i < quoteLen) : true  #  inside isQuote method
         * (pos + i >= len) : false  #  inside isQuote method
         * (srcChars[pos + i] != srcChars[quoteStart + i]) : false  #  inside isQuote method
         * (isQuote(srcChars, pos, len, quoteStart, quoteLen)) : true  #  inside readWithQuotes method
         * (isQuote(srcChars, pos + quoteLen, len, quoteStart, quoteLen)) : true  #  inside readWithQuotes method
         * (StringUtils.isEmpty(tok)) : false  #  inside addToken method
         * (pos >= count) : true
         * (isIgnoreEmptyTokens()) : false  #  inside addToken method
         * (isEmptyTokenAsNull()) : false  #  inside addToken method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        doReturn(strMatcher).when(target).getIgnoredMatcher();
        StrMatcher strMatcher2 = StrMatcher.commaMatcher();
        doReturn(strMatcher2).when(target).getTrimmerMatcher();
        StrMatcher strMatcher3 = StrMatcher.commaMatcher();
        StrMatcher strMatcher4 = StrMatcher.commaMatcher();
        doReturn(strMatcher3, strMatcher4).when(target).getDelimiterMatcher();
        doReturn(false).when(target).isIgnoreEmptyTokens();
        doReturn(false).when(target).isEmptyTokenAsNull();
        char[] charArray2 = new char[] { 'A', 'A', 'A' };
        
        //Act Statement(s)
        List<String> result = target.tokenize(charArray2, 0, 0);
        List<String> stringResultList = new ArrayList<>(Arrays.asList("resultItem1", ""));
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result.size(), equalTo(2));
            assertThat(result, containsInRelativeOrder(stringResultList.toArray()));
            verify(target).getIgnoredMatcher();
            verify(target).getTrimmerMatcher();
            verify(target, times(2)).getDelimiterMatcher();
            verify(target).isIgnoreEmptyTokens();
            verify(target).isEmptyTokenAsNull();
        });
    }

    //BaseRock generated method id: ${tokenizeWhenIsQuoteNotSrcCharsPosPlusQuoteLenLenQuoteStartQuoteLenAndStringUtilsIsEmptyTokAndIsIgnoreEmptyTokensAndPosG}, hash: 5519855F4B782309F96852C1DEB3EF6F
    @Test()
    void tokenizeWhenIsQuoteNotSrcCharsPosPlusQuoteLenLenQuoteStartQuoteLenAndStringUtilsIsEmptyTokAndIsIgnoreEmptyTokensAndPosG() {
        /* Branches:
         * (ArrayUtils.isEmpty(srcChars)) : false
         * (pos >= 0) : true
         * (pos < count) : true
         * (start < len) : true  #  inside readNextToken method
         * (removeLen == 0) : false  #  inside readNextToken method
         * (getDelimiterMatcher().isMatch(srcChars, start, start, len) > 0) : false  #  inside readNextToken method
         * (getQuoteMatcher().isMatch(srcChars, start, start, len) > 0) : true  #  inside readNextToken method
         * (start >= len) : false  #  inside readNextToken method
         * (delimLen > 0) : false  #  inside readNextToken method
         * (quoteLen > 0) : true  #  inside readNextToken method
         * (quoteLen > 0) : true  #  inside readWithQuotes method
         * (pos < len) : true  #  inside readWithQuotes method
         * (quoting) : true  #  inside readWithQuotes method
         * (i < quoteLen) : true  #  inside isQuote method
         * (pos + i >= len) : false  #  inside isQuote method
         * (srcChars[pos + i] != srcChars[quoteStart + i]) : false  #  inside isQuote method
         * (isQuote(srcChars, pos, len, quoteStart, quoteLen)) : true  #  inside readWithQuotes method
         * (isQuote(srcChars, pos + quoteLen, len, quoteStart, quoteLen)) : false  #  inside readWithQuotes method
         * (StringUtils.isEmpty(tok)) : true  #  inside addToken method
         * (isIgnoreEmptyTokens()) : true  #  inside addToken method
         * (pos >= count) : true
         * (isEmptyTokenAsNull()) : false  #  inside addToken method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = spy(new StrTokenizer(charArray, 'A', 'A'));
        StrMatcher strMatcher = StrMatcher.commaMatcher();
        doReturn(strMatcher).when(target).getIgnoredMatcher();
        StrMatcher strMatcher2 = StrMatcher.commaMatcher();
        doReturn(strMatcher2).when(target).getTrimmerMatcher();
        StrMatcher strMatcher3 = StrMatcher.commaMatcher();
        StrMatcher strMatcher4 = StrMatcher.commaMatcher();
        doReturn(strMatcher3, strMatcher4).when(target).getDelimiterMatcher();
        doReturn(true, false).when(target).isIgnoreEmptyTokens();
        doReturn(false).when(target).isEmptyTokenAsNull();
        char[] charArray2 = new char[] { 'D', 'B', 'C' };
        
        //Act Statement(s)
        List<String> result = target.tokenize(charArray2, 0, 3);
        List<String> stringResultList = new ArrayList<>(Arrays.asList(""));
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result.size(), equalTo(1));
            assertThat(result, containsInRelativeOrder(stringResultList.toArray()));
            verify(target).getIgnoredMatcher();
            verify(target).getTrimmerMatcher();
            verify(target, times(2)).getDelimiterMatcher();
            verify(target, times(2)).isIgnoreEmptyTokens();
            verify(target).isEmptyTokenAsNull();
        });
    }

    //BaseRock generated method id: ${toStringWhenTokensIsNull}, hash: 52928F0C2FD1103DAA0EA26EDC76BBB2
    @Test()
    void toStringWhenTokensIsNull() {
        /* Branches:
         * (tokens == null) : true
         */
         //Arrange Statement(s)
        char[] charArray = new char[] {};
        StrTokenizer target = new StrTokenizer(charArray, 'A', 'A');
        
        //Act Statement(s)
        String result = target.toString();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("StrTokenizer[not tokenized yet]")));
    }
}

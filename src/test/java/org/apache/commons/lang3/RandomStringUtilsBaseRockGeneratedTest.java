package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import java.util.Random;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.verify;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.doReturn;
import static org.hamcrest.Matchers.is;
import org.junit.jupiter.api.Disabled;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class RandomStringUtilsBaseRockGeneratedTest {

    //BaseRock generated method id: ${insecureTest}, hash: 97B5A9C263BADD098BEC60E0A3020B0F
    @Test()
    void insecureTest() {
        //Act Statement(s)
        RandomStringUtils result = RandomStringUtils.insecure();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${randomTest}, hash: 5FF783FDF65B12FAD1A3585B072206DD
    @Disabled()
    @Test()
    void randomTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomStringUtils> randomStringUtils = mockStatic(RandomStringUtils.class, CALLS_REAL_METHODS)) {
            RandomStringUtils randomStringUtils2 = new RandomStringUtils();
            randomStringUtils.when(() -> RandomStringUtils.secure()).thenReturn(randomStringUtils2);
            //Act Statement(s)
            String result = RandomStringUtils.random(0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                randomStringUtils.verify(() -> RandomStringUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${random1Test}, hash: A0C43895F54E11536FFB5C4305E8BD56
    @Disabled()
    @Test()
    void random1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomStringUtils> randomStringUtils = mockStatic(RandomStringUtils.class, CALLS_REAL_METHODS)) {
            RandomStringUtils randomStringUtils2 = new RandomStringUtils();
            randomStringUtils.when(() -> RandomStringUtils.secure()).thenReturn(randomStringUtils2);
            //Act Statement(s)
            String result = RandomStringUtils.random(0, false, false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                randomStringUtils.verify(() -> RandomStringUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${random2Test}, hash: 2F7CCEA6DF6CDA462DCDF263D2B99A90
    @Disabled()
    @Test()
    void random2Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomStringUtils> randomStringUtils = mockStatic(RandomStringUtils.class, CALLS_REAL_METHODS)) {
            RandomStringUtils randomStringUtils2 = new RandomStringUtils();
            randomStringUtils.when(() -> RandomStringUtils.secure()).thenReturn(randomStringUtils2);
            char[] charArray = new char[] {};
            //Act Statement(s)
            String result = RandomStringUtils.random(0, charArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                randomStringUtils.verify(() -> RandomStringUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${random3Test}, hash: AB4B9443DD014DC31625116E5605FB43
    @Disabled()
    @Test()
    void random3Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomStringUtils> randomStringUtils = mockStatic(RandomStringUtils.class, CALLS_REAL_METHODS)) {
            RandomStringUtils randomStringUtils2 = new RandomStringUtils();
            randomStringUtils.when(() -> RandomStringUtils.secure()).thenReturn(randomStringUtils2);
            //Act Statement(s)
            String result = RandomStringUtils.random(0, 0, 0, false, false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                randomStringUtils.verify(() -> RandomStringUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${random4Test}, hash: B2B1D4CB2A1425B25BDBB8AC220230CD
    @Disabled()
    @Test()
    void random4Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomStringUtils> randomStringUtils = mockStatic(RandomStringUtils.class, CALLS_REAL_METHODS)) {
            RandomStringUtils randomStringUtils2 = new RandomStringUtils();
            randomStringUtils.when(() -> RandomStringUtils.secure()).thenReturn(randomStringUtils2);
            char[] charArray = new char[] {};
            //Act Statement(s)
            String result = RandomStringUtils.random(0, 0, 0, false, false, charArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                randomStringUtils.verify(() -> RandomStringUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${random5WhenCountEquals0}, hash: DDFE42EF3CEEE7DBE7F7C53F600A33D3
    @Test()
    void random5WhenCountEquals0() {
        /* Branches:
         * (count == 0) : true
         */
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        Random random = new Random();
        //Act Statement(s)
        String result = RandomStringUtils.random(0, 0, 0, false, false, charArray, random);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${random5WhenCountLessThan0ThrowsIllegalArgumentException}, hash: 435365E83A5B977898F4FE63B0EE46C3
    @Disabled()
    @Test()
    void random5WhenCountLessThan0ThrowsIllegalArgumentException() {
        /* Branches:
         * (count == 0) : false
         * (count < 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        Random random = new Random();
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("s1");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            RandomStringUtils.random(0, 0, 0, false, false, charArray, random);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${random5WhenCharsLengthEquals0ThrowsIllegalArgumentException}, hash: D8C23D86ACB153684A341D54C26E0BB9
    @Test()
    void random5WhenCharsLengthEquals0ThrowsIllegalArgumentException() {
        /* Branches:
         * (count == 0) : false
         * (count < 0) : false
         * (chars != null) : true
         * (chars.length == 0) : true
         */
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        Random random = new Random();
        IllegalArgumentException illegalArgumentException = new IllegalArgumentException("The chars array must not be empty");
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            RandomStringUtils.random(1, 0, 0, false, false, charArray, random);
        });
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, is(notNullValue()));
            assertThat(result.getMessage(), equalTo(illegalArgumentException.getMessage()));
        });
    }

    //BaseRock generated method id: ${random5WhenRandomValueGreaterThanOrEqualsToEnd}, hash: 9CA6699AF840187915DFBDB64B41E190
    @Test()
    void random5WhenRandomValueGreaterThanOrEqualsToEnd() {
        /* Branches:
         * (count == 0) : false
         * (count < 0) : false
         * (chars != null) : true
         * (chars.length == 0) : false
         * (start == 0) : true
         * (end == 0) : true
         * (chars != null) : true
         * (end > Character.MAX_CODE_POINT) : true
         * (chars == null) : false
         * (count-- != 0) : true
         * (randomValue >= end) : true
         *
         * TODO: Help needed! This method is not unit testable!
         *  No constructor found to create an object without any exception for class org.apache.commons.lang3.CachedRandomBits
         *  Suggestions:
         *  You can pass them as constructor arguments or create a setter for them (avoid new operator)
         *  or adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        char[] charArray = new char[] {};
        Random random = new Random();
        //Act Statement(s)
        String result = RandomStringUtils.random(0, 0, 0, false, false, charArray, random);
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${random6Test}, hash: D401F7FD0DEA23CD9647D6CF94E79FDF
    @Disabled()
    @Test()
    void random6Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomStringUtils> randomStringUtils = mockStatic(RandomStringUtils.class, CALLS_REAL_METHODS)) {
            RandomStringUtils randomStringUtils2 = new RandomStringUtils();
            randomStringUtils.when(() -> RandomStringUtils.secure()).thenReturn(randomStringUtils2);
            //Act Statement(s)
            String result = RandomStringUtils.random(0, "chars1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                randomStringUtils.verify(() -> RandomStringUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${randomAlphabeticTest}, hash: 25FCAAACB88EF80CF9ACE8817CC77698
    @Disabled()
    @Test()
    void randomAlphabeticTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomStringUtils> randomStringUtils = mockStatic(RandomStringUtils.class, CALLS_REAL_METHODS)) {
            RandomStringUtils randomStringUtils2 = new RandomStringUtils();
            randomStringUtils.when(() -> RandomStringUtils.secure()).thenReturn(randomStringUtils2);
            //Act Statement(s)
            String result = RandomStringUtils.randomAlphabetic(0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                randomStringUtils.verify(() -> RandomStringUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${randomAlphabetic1Test}, hash: 89AD780122BD3B6913EE80A0B7450690
    @Disabled()
    @Test()
    void randomAlphabetic1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomStringUtils> randomStringUtils = mockStatic(RandomStringUtils.class, CALLS_REAL_METHODS)) {
            RandomStringUtils randomStringUtils2 = new RandomStringUtils();
            randomStringUtils.when(() -> RandomStringUtils.secure()).thenReturn(randomStringUtils2);
            //Act Statement(s)
            String result = RandomStringUtils.randomAlphabetic(0, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                randomStringUtils.verify(() -> RandomStringUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${randomAlphanumericTest}, hash: 4315E17A85926977DE8977B686348B13
    @Disabled()
    @Test()
    void randomAlphanumericTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomStringUtils> randomStringUtils = mockStatic(RandomStringUtils.class, CALLS_REAL_METHODS)) {
            RandomStringUtils randomStringUtils2 = new RandomStringUtils();
            randomStringUtils.when(() -> RandomStringUtils.secure()).thenReturn(randomStringUtils2);
            //Act Statement(s)
            String result = RandomStringUtils.randomAlphanumeric(0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                randomStringUtils.verify(() -> RandomStringUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${randomAlphanumeric1Test}, hash: 59FF0360856A220A54071E7905AA3246
    @Disabled()
    @Test()
    void randomAlphanumeric1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomStringUtils> randomStringUtils = mockStatic(RandomStringUtils.class, CALLS_REAL_METHODS)) {
            RandomStringUtils randomStringUtils2 = new RandomStringUtils();
            randomStringUtils.when(() -> RandomStringUtils.secure()).thenReturn(randomStringUtils2);
            //Act Statement(s)
            String result = RandomStringUtils.randomAlphanumeric(0, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                randomStringUtils.verify(() -> RandomStringUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${randomAsciiTest}, hash: 020335F3CAD1A16B2B6BC039BC32DC4D
    @Disabled()
    @Test()
    void randomAsciiTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomStringUtils> randomStringUtils = mockStatic(RandomStringUtils.class, CALLS_REAL_METHODS)) {
            RandomStringUtils randomStringUtils2 = new RandomStringUtils();
            randomStringUtils.when(() -> RandomStringUtils.secure()).thenReturn(randomStringUtils2);
            //Act Statement(s)
            String result = RandomStringUtils.randomAscii(0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                randomStringUtils.verify(() -> RandomStringUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${randomAscii1Test}, hash: 5569DAF1E5E3AE72A5EC5B668D700221
    @Disabled()
    @Test()
    void randomAscii1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomStringUtils> randomStringUtils = mockStatic(RandomStringUtils.class, CALLS_REAL_METHODS)) {
            RandomStringUtils randomStringUtils2 = new RandomStringUtils();
            randomStringUtils.when(() -> RandomStringUtils.secure()).thenReturn(randomStringUtils2);
            //Act Statement(s)
            String result = RandomStringUtils.randomAscii(0, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                randomStringUtils.verify(() -> RandomStringUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${randomGraphTest}, hash: B5D8EFA621B1FA413505B557AE92A019
    @Disabled()
    @Test()
    void randomGraphTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomStringUtils> randomStringUtils = mockStatic(RandomStringUtils.class, CALLS_REAL_METHODS)) {
            RandomStringUtils randomStringUtils2 = new RandomStringUtils();
            randomStringUtils.when(() -> RandomStringUtils.secure()).thenReturn(randomStringUtils2);
            //Act Statement(s)
            String result = RandomStringUtils.randomGraph(0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                randomStringUtils.verify(() -> RandomStringUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${randomGraph1Test}, hash: C663D5219D9D9B0E2428E378F70202AA
    @Disabled()
    @Test()
    void randomGraph1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomStringUtils> randomStringUtils = mockStatic(RandomStringUtils.class, CALLS_REAL_METHODS)) {
            RandomStringUtils randomStringUtils2 = new RandomStringUtils();
            randomStringUtils.when(() -> RandomStringUtils.secure()).thenReturn(randomStringUtils2);
            //Act Statement(s)
            String result = RandomStringUtils.randomGraph(0, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                randomStringUtils.verify(() -> RandomStringUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${randomNumericTest}, hash: 78495B81214309678CAB669A3DBDAA51
    @Disabled()
    @Test()
    void randomNumericTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomStringUtils> randomStringUtils = mockStatic(RandomStringUtils.class, CALLS_REAL_METHODS)) {
            RandomStringUtils randomStringUtils2 = new RandomStringUtils();
            randomStringUtils.when(() -> RandomStringUtils.secure()).thenReturn(randomStringUtils2);
            //Act Statement(s)
            String result = RandomStringUtils.randomNumeric(0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                randomStringUtils.verify(() -> RandomStringUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${randomNumeric1Test}, hash: 034277CBAA0AB1B6ABFC2DB540672E99
    @Disabled()
    @Test()
    void randomNumeric1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomStringUtils> randomStringUtils = mockStatic(RandomStringUtils.class, CALLS_REAL_METHODS)) {
            RandomStringUtils randomStringUtils2 = new RandomStringUtils();
            randomStringUtils.when(() -> RandomStringUtils.secure()).thenReturn(randomStringUtils2);
            //Act Statement(s)
            String result = RandomStringUtils.randomNumeric(0, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                randomStringUtils.verify(() -> RandomStringUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${randomPrintTest}, hash: 91506D620B8EE5B25B3FDF0852DEF7BD
    @Disabled()
    @Test()
    void randomPrintTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomStringUtils> randomStringUtils = mockStatic(RandomStringUtils.class, CALLS_REAL_METHODS)) {
            RandomStringUtils randomStringUtils2 = new RandomStringUtils();
            randomStringUtils.when(() -> RandomStringUtils.secure()).thenReturn(randomStringUtils2);
            //Act Statement(s)
            String result = RandomStringUtils.randomPrint(0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                randomStringUtils.verify(() -> RandomStringUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${randomPrint1Test}, hash: C98AF5FBD06DE2DD0E3D5858747B30D8
    @Disabled()
    @Test()
    void randomPrint1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomStringUtils> randomStringUtils = mockStatic(RandomStringUtils.class, CALLS_REAL_METHODS)) {
            RandomStringUtils randomStringUtils2 = new RandomStringUtils();
            randomStringUtils.when(() -> RandomStringUtils.secure()).thenReturn(randomStringUtils2);
            //Act Statement(s)
            String result = RandomStringUtils.randomPrint(0, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("result1"));
                randomStringUtils.verify(() -> RandomStringUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${secureTest}, hash: 83851A5F7339A43A0AB0BCBBA800BC0C
    @Test()
    void secureTest() {
        //Act Statement(s)
        RandomStringUtils result = RandomStringUtils.secure();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${secureStrongTest}, hash: CA1441DC08592A7733496208C784A5CF
    @Test()
    void secureStrongTest() {
        //Act Statement(s)
        RandomStringUtils result = RandomStringUtils.secureStrong();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${nextTest}, hash: 465F8456619904584518414DA86306C0
    @Test()
    void nextTest() {
        //Arrange Statement(s)
        RandomStringUtils target = spy(new RandomStringUtils());
        doReturn("return_of_next1").when(target).next(0, false, false);
        //Act Statement(s)
        String result = target.next(0);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_next1"));
            verify(target).next(0, false, false);
        });
    }

    //BaseRock generated method id: ${next1Test}, hash: 52F507496BFD2D80CB9BFB614CFCE365
    @Test()
    void next1Test() {
        //Arrange Statement(s)
        RandomStringUtils target = spy(new RandomStringUtils());
        doReturn("return_of_next1").when(target).next(0, 0, 0, false, false);
        //Act Statement(s)
        String result = target.next(0, false, false);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_next1"));
            verify(target).next(0, 0, 0, false, false);
        });
    }

    //BaseRock generated method id: ${next2WhenCharsIsNull}, hash: 6DFDAB4C60A105D926215F0D907C338A
    @Disabled()
    @Test()
    void next2WhenCharsIsNull() {
        /* Branches:
         * (chars == null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomStringUtils> randomStringUtils = mockStatic(RandomStringUtils.class)) {
            Random random = new Random();
            randomStringUtils.when(() -> RandomStringUtils.random(0, 0, 0, false, false, (char[]) null, random)).thenReturn("return_of_random1");
            RandomStringUtils target = new RandomStringUtils();
            char[] _char = null;
            //Act Statement(s)
            String result = target.next(0, _char);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_random1"));
                randomStringUtils.verify(() -> RandomStringUtils.random(0, 0, 0, false, false, (char[]) null, random), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${next2WhenCharsIsNotNull}, hash: 56F6748FA7B65405F041B02B78EEDC04
    @Disabled()
    @Test()
    void next2WhenCharsIsNotNull() {
        /* Branches:
         * (chars == null) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomStringUtils> randomStringUtils = mockStatic(RandomStringUtils.class)) {
            char[] charArray = new char[] {};
            Random random = new Random();
            randomStringUtils.when(() -> RandomStringUtils.random(0, 0, 0, false, false, charArray, random)).thenReturn("return_of_random1");
            RandomStringUtils target = new RandomStringUtils();
            //Act Statement(s)
            String result = target.next(0, charArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_random1"));
                randomStringUtils.verify(() -> RandomStringUtils.random(0, 0, 0, false, false, charArray, random), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${next3Test}, hash: 7933A94466FCCEF00CCE96DDC7762AF2
    @Disabled()
    @Test()
    void next3Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomStringUtils> randomStringUtils = mockStatic(RandomStringUtils.class)) {
            Random random = new Random();
            randomStringUtils.when(() -> RandomStringUtils.random(0, 0, 0, false, false, (char[]) null, random)).thenReturn("return_of_random1");
            RandomStringUtils target = new RandomStringUtils();
            //Act Statement(s)
            String result = target.next(0, 0, 0, false, false);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_random1"));
                randomStringUtils.verify(() -> RandomStringUtils.random(0, 0, 0, false, false, (char[]) null, random), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${next4Test}, hash: 8EDD39FB69D45845AFC353214889A631
    @Disabled()
    @Test()
    void next4Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomStringUtils> randomStringUtils = mockStatic(RandomStringUtils.class)) {
            char[] charArray = new char[] {};
            Random random = new Random();
            randomStringUtils.when(() -> RandomStringUtils.random(0, 0, 0, false, false, charArray, random)).thenReturn("return_of_random1");
            RandomStringUtils target = new RandomStringUtils();
            //Act Statement(s)
            String result = target.next(0, 0, 0, false, false, charArray);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_random1"));
                randomStringUtils.verify(() -> RandomStringUtils.random(0, 0, 0, false, false, charArray, random), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${next5WhenCharsIsNull}, hash: 2B4635EABDF532D4BA6F0BFDCA86E395
    @Disabled()
    @Test()
    void next5WhenCharsIsNull() {
        /* Branches:
         * (chars == null) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomStringUtils> randomStringUtils = mockStatic(RandomStringUtils.class)) {
            Random random = new Random();
            randomStringUtils.when(() -> RandomStringUtils.random(0, 0, 0, false, false, (char[]) null, random)).thenReturn("return_of_random1");
            RandomStringUtils target = new RandomStringUtils();
            //Act Statement(s)
            String result = target.next(0, (String) null);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo("return_of_random1"));
                randomStringUtils.verify(() -> RandomStringUtils.random(0, 0, 0, false, false, (char[]) null, random), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${next5WhenCharsIsNotNull}, hash: 51538B4FCA01B697D5BECAFF1D9E346A
    @Test()
    void next5WhenCharsIsNotNull() {
        /* Branches:
         * (chars == null) : false
         */
        //Arrange Statement(s)
        RandomStringUtils target = spy(new RandomStringUtils());
        char[] charArray = new char[] { 'A' };
        doReturn("return_of_next1").when(target).next(0, charArray);
        //Act Statement(s)
        String result = target.next(0, "A");
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_next1"));
            verify(target).next(0, charArray);
        });
    }

    //BaseRock generated method id: ${nextAlphabeticTest}, hash: CD506963986CB992F7999522CF787621
    @Test()
    void nextAlphabeticTest() {
        //Arrange Statement(s)
        RandomStringUtils target = spy(new RandomStringUtils());
        doReturn("return_of_next1").when(target).next(0, true, false);
        //Act Statement(s)
        String result = target.nextAlphabetic(0);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_next1"));
            verify(target).next(0, true, false);
        });
    }

    //BaseRock generated method id: ${nextAlphabetic1Test}, hash: B33DA8833E667ED9EF048486CE79CC77
    @Test()
    void nextAlphabetic1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        RandomStringUtils target = spy(new RandomStringUtils());
        doReturn("return_of_nextAlphabetic1").when(target).nextAlphabetic(0);
        //Act Statement(s)
        String result = target.nextAlphabetic(0, 0);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_nextAlphabetic1"));
            verify(target).nextAlphabetic(0);
        });
    }

    //BaseRock generated method id: ${nextAlphanumericTest}, hash: 720F79F967ACBEED1E572CDA3EFD9DA6
    @Test()
    void nextAlphanumericTest() {
        //Arrange Statement(s)
        RandomStringUtils target = spy(new RandomStringUtils());
        doReturn("return_of_next1").when(target).next(0, true, true);
        //Act Statement(s)
        String result = target.nextAlphanumeric(0);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_next1"));
            verify(target).next(0, true, true);
        });
    }

    //BaseRock generated method id: ${nextAlphanumeric1Test}, hash: 7BF3DC17453C27456BCD3057B5E7AB84
    @Test()
    void nextAlphanumeric1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        RandomStringUtils target = spy(new RandomStringUtils());
        doReturn("return_of_nextAlphanumeric1").when(target).nextAlphanumeric(0);
        //Act Statement(s)
        String result = target.nextAlphanumeric(0, 0);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_nextAlphanumeric1"));
            verify(target).nextAlphanumeric(0);
        });
    }

    //BaseRock generated method id: ${nextAsciiTest}, hash: 7779C3740B471B13615B683A2D7C996A
    @Test()
    void nextAsciiTest() {
        //Arrange Statement(s)
        RandomStringUtils target = spy(new RandomStringUtils());
        doReturn("return_of_next1").when(target).next(0, 32, 127, false, false);
        //Act Statement(s)
        String result = target.nextAscii(0);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_next1"));
            verify(target).next(0, 32, 127, false, false);
        });
    }

    //BaseRock generated method id: ${nextAscii1Test}, hash: A4769CA0072EE3E0277D3283F1B8E186
    @Test()
    void nextAscii1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        RandomStringUtils target = spy(new RandomStringUtils());
        doReturn("return_of_nextAscii1").when(target).nextAscii(0);
        //Act Statement(s)
        String result = target.nextAscii(0, 0);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_nextAscii1"));
            verify(target).nextAscii(0);
        });
    }

    //BaseRock generated method id: ${nextGraphTest}, hash: 5F6F8AE82A3CEDAC8FCF557A1819F1BB
    @Test()
    void nextGraphTest() {
        //Arrange Statement(s)
        RandomStringUtils target = spy(new RandomStringUtils());
        doReturn("return_of_next1").when(target).next(0, 33, 126, false, false);
        //Act Statement(s)
        String result = target.nextGraph(0);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_next1"));
            verify(target).next(0, 33, 126, false, false);
        });
    }

    //BaseRock generated method id: ${nextGraph1Test}, hash: 56D3AF4F8EC6EFC2DBCD687591DCA32B
    @Test()
    void nextGraph1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        RandomStringUtils target = spy(new RandomStringUtils());
        doReturn("return_of_nextGraph1").when(target).nextGraph(0);
        //Act Statement(s)
        String result = target.nextGraph(0, 0);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_nextGraph1"));
            verify(target).nextGraph(0);
        });
    }

    //BaseRock generated method id: ${nextNumericTest}, hash: F450EF3174D161D1F349B09D9FDD6A8C
    @Test()
    void nextNumericTest() {
        //Arrange Statement(s)
        RandomStringUtils target = spy(new RandomStringUtils());
        doReturn("return_of_next1").when(target).next(0, false, true);
        //Act Statement(s)
        String result = target.nextNumeric(0);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_next1"));
            verify(target).next(0, false, true);
        });
    }

    //BaseRock generated method id: ${nextNumeric1Test}, hash: 7F94A0E6188582C34489AFA9D0227DA4
    @Test()
    void nextNumeric1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        RandomStringUtils target = spy(new RandomStringUtils());
        doReturn("return_of_nextNumeric1").when(target).nextNumeric(0);
        //Act Statement(s)
        String result = target.nextNumeric(0, 0);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_nextNumeric1"));
            verify(target).nextNumeric(0);
        });
    }

    //BaseRock generated method id: ${nextPrintTest}, hash: DA08C80A8A1A8E7CA3B1DB21D84558D6
    @Test()
    void nextPrintTest() {
        //Arrange Statement(s)
        RandomStringUtils target = spy(new RandomStringUtils());
        doReturn("return_of_next1").when(target).next(0, 32, 126, false, false);
        //Act Statement(s)
        String result = target.nextPrint(0);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_next1"));
            verify(target).next(0, 32, 126, false, false);
        });
    }

    //BaseRock generated method id: ${nextPrint1Test}, hash: AFE67081177AB318DD5AC05412FDD506
    @Test()
    void nextPrint1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        RandomStringUtils target = spy(new RandomStringUtils());
        doReturn("return_of_nextPrint1").when(target).nextPrint(0);
        //Act Statement(s)
        String result = target.nextPrint(0, 0);
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_nextPrint1"));
            verify(target).nextPrint(0);
        });
    }

    //BaseRock generated method id: ${toStringTest}, hash: D6CC37D9B9571726B591F66A2A954F51
    @Disabled()
    @Test()
    void toStringTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        RandomStringUtils target = new RandomStringUtils();
        //Act Statement(s)
        String result = target.toString();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("RandomStringUtils [random=random]")));
    }
}

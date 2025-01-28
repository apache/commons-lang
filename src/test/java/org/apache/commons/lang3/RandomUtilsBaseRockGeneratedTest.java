package org.apache.commons.lang3;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.function.Supplier;
import java.security.SecureRandom;
import org.mockito.MockedStatic;
import java.util.Random;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.doReturn;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mockStatic;
import static org.hamcrest.Matchers.closeTo;
import org.junit.jupiter.api.Disabled;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class RandomUtilsBaseRockGeneratedTest {

    private final Supplier<Random> randomMock = mock(Supplier.class, "random");

    private final Random randomMock2 = mock(Random.class);

    //BaseRock generated method id: ${insecureTest}, hash: 674927C3DE574647412409673BF2D7B7
    @Test()
    void insecureTest() {
        //Act Statement(s)
        RandomUtils result = RandomUtils.insecure();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${nextBooleanWhenSecureRandomBoolean}, hash: C18F2D57C3AEF4CD1ED4BD69CA7B042A
    @Disabled()
    @Test()
    void nextBooleanWhenSecureRandomBoolean() {
        /* Branches:
         * (secure().randomBoolean()) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomUtils> randomUtils = mockStatic(RandomUtils.class, CALLS_REAL_METHODS)) {
            RandomUtils randomUtils2 = new RandomUtils();
            randomUtils.when(() -> RandomUtils.secure()).thenReturn(randomUtils2);
            //Act Statement(s)
            boolean result = RandomUtils.nextBoolean();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.TRUE));
                randomUtils.verify(() -> RandomUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nextBooleanWhenSecureNotRandomBoolean}, hash: F8500B945A0A44E6D44F0EBB7D7865EF
    @Test()
    void nextBooleanWhenSecureNotRandomBoolean() {
        /* Branches:
         * (secure().randomBoolean()) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomUtils> randomUtils = mockStatic(RandomUtils.class, CALLS_REAL_METHODS)) {
            RandomUtils randomUtils2 = new RandomUtils();
            randomUtils.when(() -> RandomUtils.secure()).thenReturn(randomUtils2);
            //Act Statement(s)
            boolean result = RandomUtils.nextBoolean();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Boolean.FALSE));
                randomUtils.verify(() -> RandomUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nextBytesTest}, hash: 8DA983E9F0E821294BF0B32C273727E0
    @Test()
    void nextBytesTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomUtils> randomUtils = mockStatic(RandomUtils.class, CALLS_REAL_METHODS)) {
            RandomUtils randomUtils2 = new RandomUtils();
            randomUtils.when(() -> RandomUtils.secure()).thenReturn(randomUtils2);
            //Act Statement(s)
            byte[] result = RandomUtils.nextBytes(0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result.length, equalTo(0));
                randomUtils.verify(() -> RandomUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nextDoubleTest}, hash: 754BC5E89643E9A654626E223AEE111F
    @Disabled()
    @Test()
    void nextDoubleTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomUtils> randomUtils = mockStatic(RandomUtils.class, CALLS_REAL_METHODS)) {
            RandomUtils randomUtils2 = new RandomUtils();
            randomUtils.when(() -> RandomUtils.secure()).thenReturn(randomUtils2);
            //Act Statement(s)
            double result = RandomUtils.nextDouble();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001));
                randomUtils.verify(() -> RandomUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nextDouble1Test}, hash: 2150A734933EDF2B120E3E7CA7B64BEE
    @Test()
    void nextDouble1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomUtils> randomUtils = mockStatic(RandomUtils.class, CALLS_REAL_METHODS)) {
            RandomUtils randomUtils2 = new RandomUtils();
            randomUtils.when(() -> RandomUtils.secure()).thenReturn(randomUtils2);
            //Act Statement(s)
            double result = RandomUtils.nextDouble(Double.parseDouble("0.0"), Double.parseDouble("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001));
                randomUtils.verify(() -> RandomUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nextFloatTest}, hash: 04E0AD6031A82A69590A434ED74F5F73
    @Disabled()
    @Test()
    void nextFloatTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomUtils> randomUtils = mockStatic(RandomUtils.class, CALLS_REAL_METHODS)) {
            RandomUtils randomUtils2 = new RandomUtils();
            randomUtils.when(() -> RandomUtils.secure()).thenReturn(randomUtils2);
            //Act Statement(s)
            float result = RandomUtils.nextFloat();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Float.parseFloat("0.0")));
                randomUtils.verify(() -> RandomUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nextFloat1Test}, hash: E8CEE65A85B942FB731022A5754B797B
    @Test()
    void nextFloat1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomUtils> randomUtils = mockStatic(RandomUtils.class, CALLS_REAL_METHODS)) {
            RandomUtils randomUtils2 = new RandomUtils();
            randomUtils.when(() -> RandomUtils.secure()).thenReturn(randomUtils2);
            //Act Statement(s)
            float result = RandomUtils.nextFloat(Float.parseFloat("0.0"), Float.parseFloat("0.0"));
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(Float.parseFloat("0.0")));
                randomUtils.verify(() -> RandomUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nextIntTest}, hash: 91AC8FD48AA294B8D1F9CC9F981E4EFD
    @Disabled()
    @Test()
    void nextIntTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomUtils> randomUtils = mockStatic(RandomUtils.class, CALLS_REAL_METHODS)) {
            RandomUtils randomUtils2 = new RandomUtils();
            randomUtils.when(() -> RandomUtils.secure()).thenReturn(randomUtils2);
            //Act Statement(s)
            int result = RandomUtils.nextInt();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                randomUtils.verify(() -> RandomUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nextInt1Test}, hash: 9AECCB374532F084CB5DD715E17ACC27
    @Test()
    void nextInt1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomUtils> randomUtils = mockStatic(RandomUtils.class, CALLS_REAL_METHODS)) {
            RandomUtils randomUtils2 = new RandomUtils();
            randomUtils.when(() -> RandomUtils.secure()).thenReturn(randomUtils2);
            //Act Statement(s)
            int result = RandomUtils.nextInt(0, 0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0));
                randomUtils.verify(() -> RandomUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nextLongTest}, hash: CA0757F1160F4F3D161748BF6AAD362A
    @Disabled()
    @Test()
    void nextLongTest() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomUtils> randomUtils = mockStatic(RandomUtils.class, CALLS_REAL_METHODS)) {
            RandomUtils randomUtils2 = new RandomUtils();
            randomUtils.when(() -> RandomUtils.secure()).thenReturn(randomUtils2);
            //Act Statement(s)
            long result = RandomUtils.nextLong();
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0L));
                randomUtils.verify(() -> RandomUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${nextLong1Test}, hash: 05A3C7C5C62A737C2C8CE5B67FE383D8
    @Test()
    void nextLong1Test() {
        /*
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        try (MockedStatic<RandomUtils> randomUtils = mockStatic(RandomUtils.class, CALLS_REAL_METHODS)) {
            RandomUtils randomUtils2 = new RandomUtils();
            randomUtils.when(() -> RandomUtils.secure()).thenReturn(randomUtils2);
            //Act Statement(s)
            long result = RandomUtils.nextLong(0L, 0L);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(0L));
                randomUtils.verify(() -> RandomUtils.secure(), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${secureTest}, hash: 9FFDD1A2F9EA316CBA7C5D299E399236
    @Test()
    void secureTest() {
        //Act Statement(s)
        RandomUtils result = RandomUtils.secure();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${secureRandomTest}, hash: 1803C2735E3687ACE75CBB9241B9F471
    @Test()
    void secureRandomTest() {
        //Act Statement(s)
        SecureRandom result = RandomUtils.secureRandom();
        //Assert statement(s)
        //TODO: Please implement equals method in SecureRandom for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${secureStrongTest}, hash: 71ECB3A73728CECBBD789107B0C03749
    @Test()
    void secureStrongTest() {
        //Act Statement(s)
        RandomUtils result = RandomUtils.secureStrong();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${randomTest}, hash: B39355440F24D8C082119856F1ECD7C1
    @Disabled()
    @Test()
    void randomTest() {
        //Arrange Statement(s)
        Random random = new Random();
        doReturn(random).when(randomMock).get();
        RandomUtils target = new RandomUtils();
        //Act Statement(s)
        Random result = target.random();
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(random));
            verify(randomMock).get();
        });
    }

    //BaseRock generated method id: ${randomBooleanWhenRandomNextBoolean}, hash: CACDB02C240A15E523182B69BCF5B443
    @Disabled()
    @Test()
    void randomBooleanWhenRandomNextBoolean() {
        /* Branches:
         * (random().nextBoolean()) : true
         */
        //Arrange Statement(s)
        doReturn(randomMock2).when(randomMock).get();
        doReturn(true).when(randomMock2).nextBoolean();
        RandomUtils target = new RandomUtils();
        //Act Statement(s)
        boolean result = target.randomBoolean();
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.TRUE));
            verify(randomMock).get();
            verify(randomMock2).nextBoolean();
        });
    }

    //BaseRock generated method id: ${randomBooleanWhenRandomNotNextBoolean}, hash: A554317C8401E9A42F899F1AE2CB8FD7
    @Disabled()
    @Test()
    void randomBooleanWhenRandomNotNextBoolean() {
        /* Branches:
         * (random().nextBoolean()) : false
         */
        //Arrange Statement(s)
        doReturn(randomMock2).when(randomMock).get();
        doReturn(false).when(randomMock2).nextBoolean();
        RandomUtils target = new RandomUtils();
        //Act Statement(s)
        boolean result = target.randomBoolean();
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Boolean.FALSE));
            verify(randomMock).get();
            verify(randomMock2).nextBoolean();
        });
    }

    //BaseRock generated method id: ${randomBytesWhenCountGreaterThanOrEqualsTo0ThrowsIllegalArgumentException}, hash: 0ED2C47DF82C42285F99BEACD109E152
    @Disabled()
    @Test()
    void randomBytesWhenCountGreaterThanOrEqualsTo0ThrowsIllegalArgumentException() {
        /* Branches:
         * (count >= 0) : true
         */
        //Arrange Statement(s)
        RandomUtils target = new RandomUtils();
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            target.randomBytes(1);
        });
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${randomBytesWhenCountLessThan0ThrowsIllegalArgumentException}, hash: 31F117C7F163F3AE023B895B3C36EBF6
    @Test()
    void randomBytesWhenCountLessThan0ThrowsIllegalArgumentException() {
        /* Branches:
         * (count >= 0) : false
         */
        //Arrange Statement(s)
        RandomUtils target = new RandomUtils();
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            target.randomBytes(-1);
        });
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${randomDoubleTest}, hash: BBE6BC45CA285C352F66E5C2123B15E7
    @Test()
    void randomDoubleTest() {
        //Arrange Statement(s)
        RandomUtils target = spy(new RandomUtils());
        doReturn(Double.parseDouble("0.0")).when(target).randomDouble(Double.parseDouble("0.0"), Double.parseDouble("1.7976931348623157E308"));
        //Act Statement(s)
        double result = target.randomDouble();
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, closeTo(Double.parseDouble("0.0"), 0.00001));
            verify(target).randomDouble(Double.parseDouble("0.0"), Double.parseDouble("1.7976931348623157E308"));
        });
    }

    //BaseRock generated method id: ${randomDouble1WhenEndExclusiveGreaterThanOrEqualsToStartInclusiveThrowsIllegalArgumentException}, hash: 6F50396F63E17F925CF5BE300055DE33
    @Disabled()
    @Test()
    void randomDouble1WhenEndExclusiveGreaterThanOrEqualsToStartInclusiveThrowsIllegalArgumentException() {
        /* Branches:
         * (endExclusive >= startInclusive) : true
         */
        //Arrange Statement(s)
        RandomUtils target = new RandomUtils();
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            target.randomDouble(Double.parseDouble("0.5"), Double.parseDouble("0.5"));
        });
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${randomDouble1WhenEndExclusiveLessThanStartInclusiveThrowsIllegalArgumentException}, hash: 99B4955776DDCD09698EF60F6A857FDF
    @Test()
    void randomDouble1WhenEndExclusiveLessThanStartInclusiveThrowsIllegalArgumentException() {
        /* Branches:
         * (endExclusive >= startInclusive) : false
         */
        //Arrange Statement(s)
        RandomUtils target = new RandomUtils();
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            target.randomDouble(Double.parseDouble("0.5"), Double.parseDouble("0.0"));
        });
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${randomFloatTest}, hash: FB9C0AF3A886720430D612E6FCC341A0
    @Test()
    void randomFloatTest() {
        //Arrange Statement(s)
        RandomUtils target = spy(new RandomUtils());
        doReturn(Float.parseFloat("0.0")).when(target).randomFloat(Float.parseFloat("0.0"), Float.parseFloat("3.4028235E38"));
        //Act Statement(s)
        float result = target.randomFloat();
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(Float.parseFloat("0.0")));
            verify(target).randomFloat(Float.parseFloat("0.0"), Float.parseFloat("3.4028235E38"));
        });
    }

    //BaseRock generated method id: ${randomFloat1WhenEndExclusiveGreaterThanOrEqualsToStartInclusiveThrowsIllegalArgumentException}, hash: 24AAC140F5306B1EF1A991C764577997
    @Disabled()
    @Test()
    void randomFloat1WhenEndExclusiveGreaterThanOrEqualsToStartInclusiveThrowsIllegalArgumentException() {
        /* Branches:
         * (endExclusive >= startInclusive) : true
         */
        //Arrange Statement(s)
        RandomUtils target = new RandomUtils();
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            target.randomFloat(Float.parseFloat("0.5"), Float.parseFloat("0.5"));
        });
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${randomFloat1WhenEndExclusiveLessThanStartInclusiveThrowsIllegalArgumentException}, hash: 929CCE53DDBD25DE2D936998E4177628
    @Test()
    void randomFloat1WhenEndExclusiveLessThanStartInclusiveThrowsIllegalArgumentException() {
        /* Branches:
         * (endExclusive >= startInclusive) : false
         */
        //Arrange Statement(s)
        RandomUtils target = new RandomUtils();
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            target.randomFloat(Float.parseFloat("0.5"), Float.parseFloat("0.0"));
        });
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${randomIntTest}, hash: 1B02B2B4F9E60FC2A821DB9886E893D3
    @Test()
    void randomIntTest() {
        //Arrange Statement(s)
        RandomUtils target = spy(new RandomUtils());
        doReturn(0).when(target).randomInt(0, 2147483647);
        //Act Statement(s)
        int result = target.randomInt();
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(0));
            verify(target).randomInt(0, 2147483647);
        });
    }

    //BaseRock generated method id: ${randomInt1WhenEndExclusiveGreaterThanOrEqualsToStartInclusiveThrowsIllegalArgumentException}, hash: 6874346FC93AFD113BAE0E2A3B0D4370
    @Disabled()
    @Test()
    void randomInt1WhenEndExclusiveGreaterThanOrEqualsToStartInclusiveThrowsIllegalArgumentException() {
        /* Branches:
         * (endExclusive >= startInclusive) : true
         */
        //Arrange Statement(s)
        RandomUtils target = new RandomUtils();
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            target.randomInt(1, 1);
        });
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${randomInt1WhenEndExclusiveLessThanStartInclusiveThrowsIllegalArgumentException}, hash: 2D119064FE6962E9AEDAE50C61FB100B
    @Test()
    void randomInt1WhenEndExclusiveLessThanStartInclusiveThrowsIllegalArgumentException() {
        /* Branches:
         * (endExclusive >= startInclusive) : false
         */
        //Arrange Statement(s)
        RandomUtils target = new RandomUtils();
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            target.randomInt(2, 1);
        });
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${randomLongWhenBitsMinusValPlusNMinus1NotLessThan0}, hash: F59BBBAD8066F05B6D2F2BC3E352C636
    @Disabled()
    @Test()
    void randomLongWhenBitsMinusValPlusNMinus1NotLessThan0() {
        /* Branches:
         * (bits - val + n - 1 < 0) : false  #  inside randomLong method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
        //Arrange Statement(s)
        RandomUtils target = new RandomUtils();
        //Act Statement(s)
        long result = target.randomLong();
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0L)));
    }

    //BaseRock generated method id: ${randomLong2WhenEndExclusiveGreaterThanOrEqualsToStartInclusiveThrowsIllegalArgumentException}, hash: CC2E7101B54F20A78FB956F26258EAAE
    @Disabled()
    @Test()
    void randomLong2WhenEndExclusiveGreaterThanOrEqualsToStartInclusiveThrowsIllegalArgumentException() {
        /* Branches:
         * (endExclusive >= startInclusive) : true
         */
        //Arrange Statement(s)
        RandomUtils target = new RandomUtils();
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            target.randomLong(1L, 2L);
        });
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${randomLong2WhenEndExclusiveLessThanStartInclusiveThrowsIllegalArgumentException}, hash: 6629176A7F3402097AF1BEC48C5FF971
    @Test()
    void randomLong2WhenEndExclusiveLessThanStartInclusiveThrowsIllegalArgumentException() {
        /* Branches:
         * (endExclusive >= startInclusive) : false
         */
        //Arrange Statement(s)
        RandomUtils target = new RandomUtils();
        //Act Statement(s)
        final IllegalArgumentException result = assertThrows(IllegalArgumentException.class, () -> {
            target.randomLong(2L, 1L);
        });
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${toStringTest}, hash: A0ACC93644E9AB84151CA93F85EC7102
    @Disabled()
    @Test()
    void toStringTest() {
        //Arrange Statement(s)
        Random randomMock2 = mock(Random.class, "object");
        doReturn(randomMock2).when(randomMock).get();
        RandomUtils target = new RandomUtils();
        //Act Statement(s)
        String result = target.toString();
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("RandomUtils [random=object]"));
            verify(randomMock).get();
        });
    }
}

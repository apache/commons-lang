package org.apache.commons.lang3.text.translate;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mockStatic;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class JavaUnicodeEscaperBaseRockGeneratedTest {

    //BaseRock generated method id: ${above1Test}, hash: 82EE621131A54836EE38EB0B446E0C05
    @Test()
    void above1Test() {
        //Arrange Statement(s)
        try (MockedStatic<JavaUnicodeEscaper> javaUnicodeEscaper = mockStatic(JavaUnicodeEscaper.class, CALLS_REAL_METHODS)) {
            JavaUnicodeEscaper javaUnicodeEscaper2 = new JavaUnicodeEscaper(0, 0, false);
            javaUnicodeEscaper.when(() -> JavaUnicodeEscaper.outsideOf(0, 0)).thenReturn(javaUnicodeEscaper2);
            //Act Statement(s)
            JavaUnicodeEscaper result = JavaUnicodeEscaper.above(0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(javaUnicodeEscaper2));
                javaUnicodeEscaper.verify(() -> JavaUnicodeEscaper.outsideOf(0, 0), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${below1Test}, hash: 83E72683627876BC91B2A32EF4DFB73E
    @Test()
    void below1Test() {
        //Arrange Statement(s)
        try (MockedStatic<JavaUnicodeEscaper> javaUnicodeEscaper = mockStatic(JavaUnicodeEscaper.class, CALLS_REAL_METHODS)) {
            JavaUnicodeEscaper javaUnicodeEscaper2 = new JavaUnicodeEscaper(0, 0, false);
            javaUnicodeEscaper.when(() -> JavaUnicodeEscaper.outsideOf(0, 2147483647)).thenReturn(javaUnicodeEscaper2);
            //Act Statement(s)
            JavaUnicodeEscaper result = JavaUnicodeEscaper.below(0);
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(javaUnicodeEscaper2));
                javaUnicodeEscaper.verify(() -> JavaUnicodeEscaper.outsideOf(0, 2147483647), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${between1Test}, hash: F54D4368CCB5D1EF9ECBE38D6B3E218C
    @Test()
    void between1Test() {
        
        //Act Statement(s)
        JavaUnicodeEscaper result = JavaUnicodeEscaper.between(0, 0);
        
        //Assert statement(s)
        //TODO: Please implement equals method in JavaUnicodeEscaper for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${outsideOf1Test}, hash: 4AC5DF412FE7B7525EF90A1DBBB18CD0
    @Test()
    void outsideOf1Test() {
        
        //Act Statement(s)
        JavaUnicodeEscaper result = JavaUnicodeEscaper.outsideOf(0, 0);
        
        //Assert statement(s)
        //TODO: Please implement equals method in JavaUnicodeEscaper for verification of the entire object or you need to adjust respective assertion statements
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${toUtf16EscapeTest}, hash: B318250B040BCA7D7ABDF3B2BAB622E2
    @Test()
    void toUtf16EscapeTest() {
        //Arrange Statement(s)
        JavaUnicodeEscaper target = new JavaUnicodeEscaper(0, 0, false);
        //Act Statement(s)
        final ArrayIndexOutOfBoundsException result = assertThrows(ArrayIndexOutOfBoundsException.class, () -> {
            target.toUtf16Escape(0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }
}

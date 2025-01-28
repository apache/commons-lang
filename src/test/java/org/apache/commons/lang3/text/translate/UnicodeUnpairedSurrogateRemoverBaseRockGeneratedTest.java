package org.apache.commons.lang3.text.translate;

import org.apache.commons.lang3.text.translate.UnicodeUnpairedSurrogateRemover;

import org.junit.jupiter.api.Timeout;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.hamcrest.MatcherAssert.assertThat;
import org.junit.jupiter.api.Test;
import static org.hamcrest.Matchers.equalTo;
import java.io.StringWriter;
import java.io.IOException;
import java.io.Writer;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class UnicodeUnpairedSurrogateRemoverBaseRockGeneratedTest {

    //BaseRock generated method id: ${translate3WhenCodePointLessThanOrEqualsToCharacterMAX_SURROGATE}, hash: 8AD06BE34D31B4F394641E7A3F49616C
    @Test
    void translate3WhenCodePointLessThanOrEqualsToCharacterMAX_SURROGATE() throws IOException {
        // Arrange
        UnicodeUnpairedSurrogateRemover target = new UnicodeUnpairedSurrogateRemover();
        Writer writer = new StringWriter();
        // Act
        boolean result = target.translate(55296, writer);
        // Assert
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${translate3WhenCodePointGreaterThanCharacterMAX_SURROGATE}, hash: ADB74C377A052387DE263FEFF5693329
    @Test
    void translate3WhenCodePointGreaterThanCharacterMAX_SURROGATE() throws IOException {
        // Arrange
        UnicodeUnpairedSurrogateRemover target = new UnicodeUnpairedSurrogateRemover();
        Writer writer = new StringWriter();
        // Act
        boolean result = target.translate(57344, writer);
        // Assert
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }
}

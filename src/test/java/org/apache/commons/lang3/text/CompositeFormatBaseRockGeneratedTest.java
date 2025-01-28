package org.apache.commons.lang3.text;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.text.ParseException;
import java.text.Format;
import java.text.ParsePosition;
import java.text.FieldPosition;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doReturn;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class CompositeFormatBaseRockGeneratedTest {

    private final Format formatterMock = mock(Format.class, "formatter");

    private final Format parserMock = mock(Format.class, "parser");

    //BaseRock generated method id: ${formatTest}, hash: B44A198376A5B7DDC7001ED4C453EBA2
    @Test()
    void formatTest() {
        //Arrange Statement(s)
        StringBuffer stringBuffer = new StringBuffer();
        Object object = new Object();
        StringBuffer stringBuffer2 = new StringBuffer();
        FieldPosition fieldPosition = new FieldPosition(0);
        doReturn(stringBuffer).when(formatterMock).format(object, stringBuffer2, fieldPosition);
        CompositeFormat target = new CompositeFormat(parserMock, formatterMock);
        
        //Act Statement(s)
        StringBuffer result = target.format(object, stringBuffer2, fieldPosition);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(stringBuffer));
            verify(formatterMock).format(object, stringBuffer2, fieldPosition);
        });
    }

    //BaseRock generated method id: ${getFormatterTest}, hash: 513178255C862AA7A3D24FDC21D801B5
    @Test()
    void getFormatterTest() {
        //Arrange Statement(s)
        CompositeFormat target = new CompositeFormat(parserMock, formatterMock);
        
        //Act Statement(s)
        Format result = target.getFormatter();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(formatterMock)));
    }

    //BaseRock generated method id: ${getParserTest}, hash: D7795D479EBD0F5A8E2D204C3E0536E7
    @Test()
    void getParserTest() {
        //Arrange Statement(s)
        CompositeFormat target = new CompositeFormat(parserMock, formatterMock);
        
        //Act Statement(s)
        Format result = target.getParser();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(parserMock)));
    }

    //BaseRock generated method id: ${parseObjectTest}, hash: FD5A16B5DFA8F698DF66C7AB030BC96F
    @Test()
    void parseObjectTest() {
        //Arrange Statement(s)
        Object object = new Object();
        ParsePosition parsePosition = new ParsePosition(0);
        doReturn(object).when(parserMock).parseObject("source1", parsePosition);
        CompositeFormat target = new CompositeFormat(parserMock, formatterMock);
        
        //Act Statement(s)
        Object result = target.parseObject("source1", parsePosition);
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo(object));
            verify(parserMock).parseObject("source1", parsePosition);
        });
    }

    //BaseRock generated method id: ${reformatTest}, hash: 17ADCC52D5C6D99FFCEF6B2F14779EA2
    @Test()
    void reformatTest() throws ParseException {
        //Arrange Statement(s)
        CompositeFormat target = spy(new CompositeFormat(parserMock, formatterMock));
        Object object = new Object();
        doReturn(object).when(target).parseObject("input1");
        doReturn("return_of_format1").when(target).format(object);
        
        //Act Statement(s)
        String result = target.reformat("input1");
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_format1"));
            verify(target).parseObject("input1");
            verify(target).format(object);
        });
    }
}

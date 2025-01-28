package org.apache.commons.lang3.builder;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.Mockito.verify;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.mockito.Mockito.doReturn;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class DiffResultBaseRockGeneratedTest {

    private final Diff<?> diffMock = mock(Diff.class);

    //BaseRock generated method id: ${getDiffsTest}, hash: D98026615F868522DDB58756BB0F8C7F
    @Test()
    void getDiffsTest() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        List<Diff<?>> anyList = new ArrayList<>();
        anyList.add(diffMock);
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffResult<Object> target = new DiffResult(object, object2, anyList, standardToStringStyle, "toStringFormat1");
        
        //Act Statement(s)
        List<Diff<?>> result = target.getDiffs();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result.size(), equalTo(1));
            assertThat(result.get(0), is(instanceOf(Diff.class)));
        });
    }

    //BaseRock generated method id: ${getLeftTest}, hash: E7D633D8FEA018D5D4694F721344029E
    @Test()
    void getLeftTest() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        List<Diff<?>> anyList = new ArrayList<>();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffResult<Object> target = new DiffResult(object, object2, anyList, standardToStringStyle, "toStringFormat1");
        
        //Act Statement(s)
        Object result = target.getLeft();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(object)));
    }

    //BaseRock generated method id: ${getNumberOfDiffsTest}, hash: AA28C850CCEB2D891A0081C37834EC78
    @Test()
    void getNumberOfDiffsTest() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        List<Diff<?>> anyList = new ArrayList<>();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffResult<Object> target = new DiffResult(object, object2, anyList, standardToStringStyle, "toStringFormat1");
        
        //Act Statement(s)
        int result = target.getNumberOfDiffs();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(0)));
    }

    //BaseRock generated method id: ${getRightTest}, hash: 53096E5218E86805809E3D16293E5155
    @Test()
    void getRightTest() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        List<Diff<?>> anyList = new ArrayList<>();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffResult<Object> target = new DiffResult(object, object2, anyList, standardToStringStyle, "toStringFormat1");
        
        //Act Statement(s)
        Object result = target.getRight();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(object2)));
    }

    //BaseRock generated method id: ${getToStringStyleTest}, hash: CA21FE1A22CC19316331BD0D0EF5BAD6
    @Test()
    void getToStringStyleTest() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        List<Diff<?>> anyList = new ArrayList<>();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffResult<Object> target = new DiffResult(object, object2, anyList, standardToStringStyle, "toStringFormat1");
        
        //Act Statement(s)
        ToStringStyle result = target.getToStringStyle();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(standardToStringStyle)));
    }

    //BaseRock generated method id: ${iteratorTest}, hash: ADC00CE7CC09E815C3F98EF2E4957680
    @Test()
    void iteratorTest() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        List<Diff<?>> anyList = new ArrayList<>();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffResult<Object> target = new DiffResult(object, object2, anyList, standardToStringStyle, "toStringFormat1");
        
        //Act Statement(s)
        Iterator<Diff<?>> result = target.iterator();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${toStringTest}, hash: 37589ABA16CBC981A0032BBEFF3E37CF
    @Test()
    void toStringTest() {
        //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        List<Diff<?>> anyList = new ArrayList<>();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffResult<Object> target = spy(new DiffResult(object, object2, anyList, standardToStringStyle, "toStringFormat1"));
        doReturn("return_of_toString1").when(target).toString(standardToStringStyle);
        
        //Act Statement(s)
        String result = target.toString();
        
        //Assert statement(s)
        assertAll("result", () -> {
            assertThat(result, equalTo("return_of_toString1"));
            verify(target).toString(standardToStringStyle);
        });
    }

    //BaseRock generated method id: ${toString1WhenDiffListIsEmpty}, hash: 5B5E9B29F1E6BC5E20DE36347258BAA0
    @Test()
    void toString1WhenDiffListIsEmpty() {
        /* Branches:
         * (diffList.isEmpty()) : true
         */
         //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        List<Diff<?>> anyList = new ArrayList<>();
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffResult<Object> target = new DiffResult(object, object2, anyList, standardToStringStyle, "toStringFormat1");
        StandardToStringStyle standardToStringStyle2 = new StandardToStringStyle();
        
        //Act Statement(s)
        String result = target.toString(standardToStringStyle2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("")));
    }

    //BaseRock generated method id: ${toString1WhenDiffListNotIsEmpty}, hash: B047C4D1BBB30652592267FFB68D5FCB
    @Test()
    void toString1WhenDiffListNotIsEmpty() {
        /* Branches:
         * (diffList.isEmpty()) : false
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        Object object = new Object();
        Object object2 = new Object();
        List<Diff<?>> anyList = new ArrayList<>();
        anyList.add(diffMock);
        StandardToStringStyle standardToStringStyle = new StandardToStringStyle();
        DiffResult<Object> target = new DiffResult(object, object2, anyList, standardToStringStyle, "A");
        StandardToStringStyle standardToStringStyle2 = new StandardToStringStyle();
        
        //Act Statement(s)
        String result = target.toString(standardToStringStyle2);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("A")));
    }
}

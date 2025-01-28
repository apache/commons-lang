package org.apache.commons.lang3.time;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import java.util.Date;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.mock;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class GmtTimeZoneBaseRockGeneratedTest {

    private final GmtTimeZone gmtTimeZoneMock = mock(GmtTimeZone.class);

    //BaseRock generated method id: ${equalsWhenThisEqualsObj}, hash: 7A5667B8B8123DC0DC9FA100FCCBD056
    @Test()
    void equalsWhenThisEqualsObj() {
        /* Branches:
         * (this == obj) : true
         */
         //Arrange Statement(s)
        GmtTimeZone target = new GmtTimeZone(true, -371, -160);
        
        //Act Statement(s)
        boolean result = target.equals(target);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsWhenObjNotInstanceOfGmtTimeZone}, hash: 8F6FA30F95783916C54B856FC21AB483
    @Test()
    void equalsWhenObjNotInstanceOfGmtTimeZone() {
        /* Branches:
         * (this == obj) : false
         * (!(obj instanceof GmtTimeZone)) : true
         */
         //Arrange Statement(s)
        GmtTimeZone target = new GmtTimeZone(true, -478, -151);
        Object object = new Object();
        
        //Act Statement(s)
        boolean result = target.equals(object);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${equalsWhenObjectsEqualsZoneIdOtherZoneId}, hash: 6ECF4EDD18100425E820BBAA3C4678E0
    @Test()
    void equalsWhenObjectsEqualsZoneIdOtherZoneId() {
        /* Branches:
         * (this == obj) : false
         * (!(obj instanceof GmtTimeZone)) : false
         * (offset == other.offset) : true
         * (Objects.equals(zoneId, other.zoneId)) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         //Arrange Statement(s)
        GmtTimeZone target = new GmtTimeZone(false, 0, 0);
        
        //Act Statement(s)
        boolean result = target.equals(gmtTimeZoneMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.TRUE)));
    }

    //BaseRock generated method id: ${equalsWhenObjectsNotEqualsZoneIdOtherZoneId}, hash: 915EB43812A8136F899238ABB4CAD54A
    @Test()
    void equalsWhenObjectsNotEqualsZoneIdOtherZoneId() {
        /* Branches:
         * (this == obj) : false
         * (!(obj instanceof GmtTimeZone)) : false
         * (offset == other.offset) : true
         * (Objects.equals(zoneId, other.zoneId)) : false
         */
         //Arrange Statement(s)
        GmtTimeZone target = new GmtTimeZone(true, 8, -480);
        
        //Act Statement(s)
        boolean result = target.equals(gmtTimeZoneMock);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${getIDTest}, hash: 145D5662135AA301FD8219E84AE19827
    @Test()
    void getIDTest() {
        //Arrange Statement(s)
        GmtTimeZone target = new GmtTimeZone(true, -371, -160);
        
        //Act Statement(s)
        String result = target.getID();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("GMT-\n9: 0")));
    }

    //BaseRock generated method id: ${getOffsetTest}, hash: D645DAA6D58EF1E29D8958CB101EFAD8
    @Test()
    void getOffsetTest() {
        //Arrange Statement(s)
        GmtTimeZone target = new GmtTimeZone(true, -371, -160);
        
        //Act Statement(s)
        int result = target.getOffset(0, 0, 0, 0, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1345200000)));
    }

    //BaseRock generated method id: ${getRawOffsetTest}, hash: B24A17B23618E3304245496A5A2027FD
    @Test()
    void getRawOffsetTest() {
        //Arrange Statement(s)
        GmtTimeZone target = new GmtTimeZone(true, -371, -160);
        
        //Act Statement(s)
        int result = target.getRawOffset();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(1345200000)));
    }

    //BaseRock generated method id: ${inDaylightTimeTest}, hash: ACA2F1D825A1143113483C97EACD69AD
    @Test()
    void inDaylightTimeTest() {
        //Arrange Statement(s)
        GmtTimeZone target = new GmtTimeZone(true, -371, -160);
        Date date = new Date();
        
        //Act Statement(s)
        boolean result = target.inDaylightTime(date);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }

    //BaseRock generated method id: ${setRawOffsetThrowsUnsupportedOperationException}, hash: 9446387DBFAB2DD45972DE8CEB208C1D
    @Test()
    void setRawOffsetThrowsUnsupportedOperationException() {
        //Arrange Statement(s)
        GmtTimeZone target = new GmtTimeZone(true, -371, -160);
        //Act Statement(s)
        final UnsupportedOperationException result = assertThrows(UnsupportedOperationException.class, () -> {
            target.setRawOffset(0);
        });
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(notNullValue())));
    }

    //BaseRock generated method id: ${toStringTest}, hash: D275549B410297E4B755DA257E4473BE
    @Test()
    void toStringTest() {
        //Arrange Statement(s)
        GmtTimeZone target = new GmtTimeZone(true, 8, -480);
        
        //Act Statement(s)
        String result = target.toString();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo("[GmtTimeZone id=\"GMT-08:\u00000\",offset=0]")));
    }

    //BaseRock generated method id: ${useDaylightTimeTest}, hash: 7713F1055325F104BF1E18C080691613
    @Test()
    void useDaylightTimeTest() {
        //Arrange Statement(s)
        GmtTimeZone target = new GmtTimeZone(true, -371, -160);
        
        //Act Statement(s)
        boolean result = target.useDaylightTime();
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(Boolean.FALSE)));
    }
}

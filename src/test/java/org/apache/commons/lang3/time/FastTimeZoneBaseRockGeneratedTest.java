package org.apache.commons.lang3.time;

import org.junit.jupiter.api.Timeout;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import java.util.TimeZone;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.mockStatic;
import static org.hamcrest.Matchers.is;

@Timeout(value = 5, threadMode = Timeout.ThreadMode.SEPARATE_THREAD)
class FastTimeZoneBaseRockGeneratedTest {

    //BaseRock generated method id: ${getGmtTimeZoneTest}, hash: A79E8093553E990121320CD34CB5EED7
    @Test()
    void getGmtTimeZoneTest() {
        
        //Act Statement(s)
        TimeZone result = FastTimeZone.getGmtTimeZone();
        GmtTimeZone gmtTimeZone = new GmtTimeZone(false, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(gmtTimeZone)));
    }

    //BaseRock generated method id: ${getGmtTimeZone1WhenUTCEqualsPattern}, hash: 76DF506BC6608C9464237108E88264C7
    @Test()
    void getGmtTimeZone1WhenUTCEqualsPattern() {
        /* Branches:
         * ("Z".equals(pattern)) : false
         * ("UTC".equals(pattern)) : true
         */
         
        //Act Statement(s)
        TimeZone result = FastTimeZone.getGmtTimeZone("UTC");
        GmtTimeZone gmtTimeZone = new GmtTimeZone(false, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(gmtTimeZone)));
    }

    //BaseRock generated method id: ${getGmtTimeZone1WhenMNotMatches}, hash: 33E8603C82D4089D6D391998AE2EA983
    @Test()
    void getGmtTimeZone1WhenMNotMatches() {
        /* Branches:
         * ("Z".equals(pattern)) : false
         * ("UTC".equals(pattern)) : false
         * (m.matches()) : false
         */
         
        //Act Statement(s)
        TimeZone result = FastTimeZone.getGmtTimeZone("A");
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, is(nullValue())));
    }

    //BaseRock generated method id: ${getGmtTimeZone1WhenHoursEquals0AndMinutesEquals0}, hash: 2997007D95A6ECF7B2826AB4BEE5EB40
    @Test()
    void getGmtTimeZone1WhenHoursEquals0AndMinutesEquals0() {
        /* Branches:
         * ("Z".equals(pattern)) : false
         * ("UTC".equals(pattern)) : false
         * (m.matches()) : true
         * (group != null) : false  #  inside parseInt method
         * (hours == 0) : true
         * (minutes == 0) : true
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        TimeZone result = FastTimeZone.getGmtTimeZone("pattern1");
        GmtTimeZone gmtTimeZone = new GmtTimeZone(false, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(gmtTimeZone)));
    }

    //BaseRock generated method id: ${getGmtTimeZone1WhenGroupCharAt0Equals___}, hash: C09E5F83257BD14B0A9D83918DA53BD3
    @Test()
    void getGmtTimeZone1WhenGroupCharAt0Equals___() {
        /* Branches:
         * ("Z".equals(pattern)) : false
         * ("UTC".equals(pattern)) : false
         * (m.matches()) : true
         * (group != null) : true  #  inside parseInt method
         * (hours == 0) : true
         * (minutes == 0) : false
         * (group != null) : true  #  inside parseSign method
         * (group.charAt(0) == '-') : true  #  inside parseSign method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        TimeZone result = FastTimeZone.getGmtTimeZone("pattern1");
        GmtTimeZone gmtTimeZone = new GmtTimeZone(true, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(gmtTimeZone)));
    }

    //BaseRock generated method id: ${getGmtTimeZone1WhenGroupCharAt0NotEquals___}, hash: B3782CC4264FF9D185AC05BA8FE611F0
    @Test()
    void getGmtTimeZone1WhenGroupCharAt0NotEquals___() {
        /* Branches:
         * ("Z".equals(pattern)) : false
         * ("UTC".equals(pattern)) : false
         * (m.matches()) : true
         * (group != null) : true  #  inside parseInt method
         * (hours == 0) : true
         * (minutes == 0) : false
         * (group != null) : true  #  inside parseSign method
         * (group.charAt(0) == '-') : false  #  inside parseSign method
         *
         * TODO: Help needed! Please adjust the input/test parameter values manually to satisfy the requirements of the given test scenario.
         *  The test code, including the assertion statements, has been successfully generated.
         */
         
        //Act Statement(s)
        TimeZone result = FastTimeZone.getGmtTimeZone("pattern1");
        GmtTimeZone gmtTimeZone = new GmtTimeZone(false, 0, 0);
        
        //Assert statement(s)
        assertAll("result", () -> assertThat(result, equalTo(gmtTimeZone)));
    }

    //BaseRock generated method id: ${getTimeZoneWhenTzIsNotNull}, hash: BAE531AC7FC91E870CD0DE1B0DE2381B
    @Test()
    void getTimeZoneWhenTzIsNotNull() {
        /* Branches:
         * (tz != null) : true
         */
         //Arrange Statement(s)
        try (MockedStatic<FastTimeZone> fastTimeZone = mockStatic(FastTimeZone.class, CALLS_REAL_METHODS)) {
            TimeZone timeZone = TimeZone.getDefault();
            fastTimeZone.when(() -> FastTimeZone.getGmtTimeZone("id1")).thenReturn(timeZone);
            //Act Statement(s)
            TimeZone result = FastTimeZone.getTimeZone("id1");
            //Assert statement(s)
            assertAll("result", () -> {
                assertThat(result, equalTo(timeZone));
                fastTimeZone.verify(() -> FastTimeZone.getGmtTimeZone("id1"), atLeast(1));
            });
        }
    }

    //BaseRock generated method id: ${getTimeZoneWhenTzIsNull}, hash: 34F38DC432EECAEB928DB56F00373960
    @Test()
    void getTimeZoneWhenTzIsNull() {
        /* Branches:
         * (tz != null) : false
         */
         //Arrange Statement(s)
        try (MockedStatic<FastTimeZone> fastTimeZone = mockStatic(FastTimeZone.class, CALLS_REAL_METHODS)) {
            fastTimeZone.when(() -> FastTimeZone.getGmtTimeZone("A")).thenReturn(null);
            //Act Statement(s)
            TimeZone result = FastTimeZone.getTimeZone("A");
            //Assert statement(s)
            //TODO: Please implement equals method in TimeZone for verification of the entire object or you need to adjust respective assertion statements
            assertAll("result", () -> {
                assertThat(result, is(notNullValue()));
                fastTimeZone.verify(() -> FastTimeZone.getGmtTimeZone("A"), atLeast(1));
            });
        }
    }
}

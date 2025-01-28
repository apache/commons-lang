package org.apache.commons.lang3.time;

import org.apache.commons.lang3.time.TimeZones;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.provider.CsvSource;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.params.ParameterizedTest;
import java.util.TimeZone;
import org.apache.commons.lang3.ObjectUtils;
import static org.mockito.Mockito.*;
import static org.hamcrest.Matchers.startsWith;
import static org.hamcrest.Matchers.endsWith;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

class TimeZonesBaseRockGeneratedTest {

    //BaseRock generated method id: ${testGMT_ID}, hash: DBD8BBB563D6EFEF2DC8763C5848B64E
    @Test
    void testGMT_ID() {
        assertEquals("GMT", TimeZones.GMT_ID);
    }

    //BaseRock generated method id: ${testGMT}, hash: ADD708B7E472DCF9A1B8965F70DA42E5
    @Test
    void testGMT() {
        assertEquals(TimeZone.getTimeZone("GMT"), TimeZones.GMT);
    }

    //BaseRock generated method id: ${testToTimeZone}, hash: 2A503F16ACD3F6CBA71858C67EE6A255
    @ParameterizedTest
    @CsvSource({ "America/New_York, America/New_York", "Europe/London, Europe/London", ", " })
    void testToTimeZone(String input, String expected) {
        TimeZone inputTimeZone = input != null ? TimeZone.getTimeZone(input) : null;
        TimeZone expectedTimeZone = expected != null ? TimeZone.getTimeZone(expected) : TimeZone.getDefault();
        assertEquals(expectedTimeZone, TimeZones.toTimeZone(inputTimeZone));
    }

    //BaseRock generated method id: ${testToTimeZoneWithMock}, hash: B4F173C132B1B4EFBE4CD3767FBDCEBA
    @Test
    void testToTimeZoneWithMock() {
        TimeZone mockTimeZone = mock(TimeZone.class);
        assertEquals(mockTimeZone, TimeZones.toTimeZone(mockTimeZone));
    }

    //BaseRock generated method id: ${testToTimeZoneWithNull}, hash: 3DB71D8E76F01541D6B6A3A1160DAFB8
    @Test
    void testToTimeZoneWithNull() {
        TimeZone defaultTimeZone = TimeZone.getDefault();
        assertEquals(defaultTimeZone, TimeZones.toTimeZone(null));
    }

    //BaseRock generated method id: ${testPrivateConstructor}, hash: 191D6AE6DF543333E5DCE12908F6EC11
    @Test
    void testPrivateConstructor() throws Exception {
        java.lang.reflect.Constructor<TimeZones> constructor = TimeZones.class.getDeclaredConstructor();
        constructor.setAccessible(true);
        constructor.newInstance();
    }
}

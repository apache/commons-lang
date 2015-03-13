package org.apache.commons.lang3.time;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.List;
import java.util.TimeZone;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(Parameterized.class)
public class FastDatePrinterTimeZonesTest {

    private static final String PATTERN = "h:mma z";

    @Parameterized.Parameters
    public static Collection<TimeZone> data() {
        final String[] zoneIds = TimeZone.getAvailableIDs();
        List<TimeZone> timeZones = new ArrayList<TimeZone>();
        for (String zoneId : zoneIds) {
            timeZones.add(TimeZone.getTimeZone(zoneId));
        }
        return timeZones;
    }

    private TimeZone timeZone;

    public FastDatePrinterTimeZonesTest(TimeZone timeZone) {
        this.timeZone = timeZone;
    }

    @Test
    public void testCalendarTimezoneRespected() {
        final Calendar cal = Calendar.getInstance(timeZone);

        final SimpleDateFormat sdf = new SimpleDateFormat(PATTERN);
        sdf.setTimeZone(timeZone);
        final String expectedValue = sdf.format(cal.getTime());
        final String actualValue = FastDateFormat.getInstance(PATTERN).format(cal);
        assertEquals(expectedValue, actualValue);
    }

}

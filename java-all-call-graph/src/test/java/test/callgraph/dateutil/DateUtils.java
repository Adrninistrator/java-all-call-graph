package test.callgraph.dateutil;

import java.util.Calendar;
import java.util.Date;

/**
 * @author adrninistrator
 * @date 2025/2/7
 * @description:
 */
public class DateUtils extends org.apache.commons.lang3.time.DateUtils {
    public static Date getEndOfDate(Date date) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(date);
        calendar.set(calendar.get(Calendar.YEAR), calendar.get(Calendar.MONTH), calendar.get(Calendar.DAY_OF_MONTH),
                23, 59, 59);
        calendar.set(Calendar.MILLISECOND, 0);
        return calendar.getTime();
    }

    public static void useGetEndOfDate() {
        getEndOfDate(null);
    }
}

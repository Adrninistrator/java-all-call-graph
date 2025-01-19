package test.callgraph.array;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * @author adrninistrator
 * @date 2025/1/13
 * @description:
 */
public class TestArray2 {
    private static Method j = null;
    private static Method m = null;

    private static void a(InvocationTargetException paramInvocationTargetException) {
        Throwable throwable = paramInvocationTargetException.getTargetException();
        String str1 = throwable.getClass().getName();
        String str2 = "Exception: " + str1 + ", message: " + throwable.getMessage();


        if (str1.compareTo("InvalidRequestException") == 0 || str1.compareTo("RolledBackException") == 0) {


            Integer integer = null;


            try {
                integer = (Integer) j.invoke(throwable, null);
            } catch (IllegalAccessException illegalAccessException) {
            } catch (IllegalArgumentException illegalArgumentException) {
            } catch (InvocationTargetException invocationTargetException) {
            }


            if (integer != null) {
                str2 = str2 + ", RESP2: " + integer;
            }

            if (str1.compareTo("InvalidRequestException") == 0) {


                byte[] arrayOfByte = null;


                try {
                    arrayOfByte = (byte[]) m.invoke(throwable, null);
                } catch (IllegalAccessException illegalAccessException) {
                } catch (IllegalArgumentException illegalArgumentException) {
                } catch (InvocationTargetException invocationTargetException) {
                }


                if (arrayOfByte != null) {

                    str2 = str2 + ", Response Code: ";
                    for (byte b1 = 0; b1 < arrayOfByte.length; b1++) {

                        str2 = str2 + arrayOfByte[b1];
                        if (b1 != arrayOfByte.length - 1) {
                            str2 = str2 + " ";
                        }
                    }
                }
            }
        }

        Object[] arrayOfObject = new Object[1];
        arrayOfObject[0] = str2;
    }
}

package test.callgraph.branch;

import java.util.Arrays;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/4/9
 * @description:
 */
public class TestLoop {

    public void test1() {
        while (true) {
            System.out.println("a");
        }
    }

    public void test2() {
        int times = 0;
        while (true) {
            System.out.println(times);
            times++;
            if (times > 10) {
                break;
            }
        }
    }

    public void test3() {
        int times = 0;
        while (times < 10) {
            System.out.println(times);
            times++;
        }
    }

    public void test4() {
        List<String> stringList = Arrays.asList("a", "b", "c");
        for (String string : stringList) {
            if ("a".equals(string)) {
                System.out.println(string);
            }
            if ("b".equals(string)) {
                System.getProperty(string);
            }
            if ("c".equals(string)) {
                System.setProperty(string, string);
            }
        }
    }
}

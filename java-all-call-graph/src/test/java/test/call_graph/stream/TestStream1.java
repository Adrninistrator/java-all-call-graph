package test.call_graph.stream;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author adrninistrator
 * @date 2021/8/17
 * @description:
 */
public class TestStream1 {

    public static void main(String[] args) {
        TestStream1 testStream1 = new TestStream1();
        testStream1.test1();
    }

    public void test1() {
        String[] strArray = new String[]{"aaa1", "d", "aaa3", "ss"};
        List<String> list = Arrays.asList(strArray);

        List<String> list2 = list.stream().map(this::map1).filter(this::filter1).collect(Collectors.toList());
        System.out.println(list2);
    }

    public boolean filter1(String str) {
        return str.startsWith("aaa");
    }

    public String map1(String str) {
        return str + "-";
    }
}

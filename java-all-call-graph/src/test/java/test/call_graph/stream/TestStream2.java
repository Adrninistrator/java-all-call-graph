package test.call_graph.stream;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author adrninistrator
 * @date 2021/8/21
 * @description:
 */
public class TestStream2 {
    public void test2() {
        List<TestDto1> list = new ArrayList<>(0);

        List<TestDto1> list2 = list.stream().map(this::map2).filter(this::filter2).collect(Collectors.toList());
        System.out.println(list2);

        List<String> list3 = list.stream().map(TestDto1::getStr).collect(Collectors.toList());
        System.out.println(list3);
    }

    public boolean filter2(TestDto1 testDto1) {
        return true;
    }

    public TestDto1 map2(TestDto1 testDto1) {
        return testDto1;
    }
}

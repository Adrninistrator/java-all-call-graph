package test.call_graph.lambda;

import java.util.HashMap;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/4/7
 * @description:
 */
public class TestLambda {
    public void test() {
        Map<String, String> map = new HashMap<>();
        map.computeIfAbsent("1", k -> System.getProperty("1"));
    }
}

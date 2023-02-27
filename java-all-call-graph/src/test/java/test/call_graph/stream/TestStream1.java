package test.call_graph.stream;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author adrninistrator
 * @date 2021/8/17
 * @description:
 */
public class TestStream1 {

    public static void main(String[] args) {
        TestStream1 testStream1 = new TestStream1();
        System.out.println("\ntest1");
        testStream1.test1();
        System.out.println("\ntest2");
        testStream1.test2();
        System.out.println("\ntest3");
        testStream1.test3();
        System.out.println("\ntest4");
        testStream1.test4();
    }

    public void test1() {
        String[] strArray = new String[]{"aaa1", "d", "aaa3", "ss"};
        List<String> list = Arrays.asList(strArray);
        List<String> list2 = list.stream().map(this::map1).filter(this::filter1).collect(Collectors.toList());
        System.out.println(list2);
    }

    public boolean filter1(String str) {
        System.out.println("filter1 " + str);
        return str.startsWith("aaa");
    }

    public String map1(String str) {
        System.out.println("map1 " + str);
        return str + "-";
    }

    public void test2() {
        String[] strArray = new String[]{"aaa1", "d", "aaa3", "ss"};
        List<String> list = Arrays.asList(strArray);
        list.forEach(e -> {
            System.out.println(e);
            System.out.println(e.toLowerCase(Locale.ROOT));
        });
    }

    public void test3() {
        Map<String, Double> map = new HashMap<>();
        map.put("a", 1.0D);
        map.put("b", 2.0D);
        map.put("c", 3.0D);

        boolean matches1 = map.values().stream().filter(Objects::nonNull).mapToDouble(value -> {
            System.out.println("mapToDouble1 " + value);
            return value;
        }).anyMatch(d -> {
            System.out.println("anyMatch1 " + d);
            return d > 4.0D;
        });
        System.out.println("matches1 " + matches1);

        boolean matches2 = map.values().stream().filter(Objects::nonNull).mapToDouble(value -> {
            System.out.println("mapToDouble2 " + value);
            return value;
        }).anyMatch(d -> {
            System.out.println("anyMatch2 " + d);
            return d < 4.0D;
        });
        System.out.println("matches2 " + matches2);
    }

    public void test4() {
        List<Map<String, String>> list = new ArrayList<>();
        Map<String, String> map1 = new HashMap<>();
        list.add(map1);
        map1.put("k1", "v1");
        map1.put("k2", "v2");
        Map<String, String> map2 = new HashMap<>();
        list.add(map2);
        map1.put("k1", "v1");
        map1.put("k3", "v3");

        Map<String, String> resultMap = list.stream().map(Map::entrySet).flatMap(Set::stream).collect(
                HashMap::new,
                (map, entry) -> map.put(entry.getKey(), entry.getValue()),
                HashMap::putAll);

        for (Map.Entry<String, String> entry : resultMap.entrySet()) {
            System.out.println("key: " + entry.getKey() + " value: " + entry.getValue());
        }
    }
}

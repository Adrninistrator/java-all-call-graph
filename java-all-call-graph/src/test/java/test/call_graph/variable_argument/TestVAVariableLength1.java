package test.call_graph.variable_argument;

/**
 * @author adrninistrator
 * @date 2022/5/8
 * @description:
 */
public class TestVAVariableLength1 {
    private void test() {
        String[] sArray1 = new String[]{"a", "b", "c", "d", "e", "f", "g"};
        log("s1a", 1);
        log("s1b", 10, "1", "2");
        int i = (int) System.currentTimeMillis() % 10;
        String s1 = (i == 7 ? "ifa" : "ifb");
        log("s1c", 100, new String[]{"11", "22", s1});
        log("s1d", 1000, sArray1);
        log("s1c", 10000, (i == 5 ? new String[]{"aa1", "bb1"} : new String[]{"aa2", "bb2"}));
    }

    private void log(String s1, int i1, String... data) {
        if (data == null) {
            return;
        }

        for (String s : data) {
            System.out.println(s);
        }
    }
}

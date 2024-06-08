package test.callgraph.argument;

/**
 * @author adrninistrator
 * @date 2023/4/7
 * @description:
 */
public class TestArgument3 {
    public void test1(Object obj, String str, int i) {
    }

    public void test2(Object obj, String str, int i) {
    }

    private String chooseOne(String str1, String str2, String str3) {
        int i = (int) System.currentTimeMillis() % 7;
        switch (i) {
            case 0:
                return str1;
            case 1:
                return str2;
            case 2:
                return str3;
            default:
                return System.getProperty("");
        }
    }
}

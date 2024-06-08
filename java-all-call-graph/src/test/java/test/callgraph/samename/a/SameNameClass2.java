package test.callgraph.samename.a;

/**
 * @author adrninistrator
 * @date 2022/4/28
 * @description:
 */
public class SameNameClass2 {

    public static String testNotSame1(String aaa1) {
        String bbb1 = "aaa";
        return aaa1 + bbb1;
    }
}

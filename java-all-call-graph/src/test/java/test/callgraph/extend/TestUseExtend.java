package test.callgraph.extend;

/**
 * @author adrninistrator
 * @date 2025/6/2
 * @description:
 */
public class TestUseExtend {

    private I3_1_1_2 i3112;
    private I2_1_2 i212;

    public void test1() {
        A1_1 a11;
        boolean flag = System.currentTimeMillis() % 7 == 1;
        a11 = flag ? new I3_1_1_1() : new I3_2_1();
        System.out.println(a11);
    }

    public void test2() {
        A1_1 a11;
        boolean flag = System.currentTimeMillis() % 7 == 1;
        a11 = flag ? i3112 : i212;
        System.out.println(a11);
    }
}

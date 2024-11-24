package test.callgraph.wcm.wc20240924;

/**
 * @author adrninistrator
 * @date 2024/9/25
 * @description:
 */
public class TestUse {
    public static void main(String[] args) {
        Parent parent = new Child1();
        parent.f1();
    }
}

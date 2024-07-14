package test.callgraph.interfaces.classes;

/**
 * @author adrninistrator
 * @date 2024/7/11
 * @description:
 */
public class ImplClass3A extends AbstractImplClass3A {
    @Override
    public void customer1() {
        System.setErr(null);
    }

    public void test1() {
        abstractCommon();
    }
}

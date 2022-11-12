package test.call_graph.spring.bean.use;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description:
 */
public class TestSpringBeanB1 extends AbstractTestSpringBeanB {
    public void test() {
        springInterfaceB1.test1();
        springInterfaceB1.test2();

        springInterfaceB2.test1();

        springServiceC2.test1();
    }
}

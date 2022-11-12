package test.call_graph.spring.bean.use;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description:
 */
public class TestSpringBeanB2A extends AbstractTestSpringBeanB2 {
    public void test() {
        springInterfaceB2.test1();
    }
}

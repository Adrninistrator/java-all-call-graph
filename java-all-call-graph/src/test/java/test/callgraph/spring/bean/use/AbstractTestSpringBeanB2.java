package test.callgraph.spring.bean.use;

/**
 * @author adrninistrator
 * @date 2022/9/20
 * @description:
 */
public abstract class AbstractTestSpringBeanB2 extends AbstractTestSpringBeanB {
    public void test() {
        springInterfaceB1.test1();
    }
}

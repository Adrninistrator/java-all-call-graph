package test.callgraph.wcm.wc20240924;

/**
 * @author adrninistrator
 * @date 2024/9/25
 * @description:
 */
public class Child2 extends Parent {
    public void child1() {
        super.f1();
    }

    @Override
    public void f2() {
        System.out.println("child2 f2");
    }
}
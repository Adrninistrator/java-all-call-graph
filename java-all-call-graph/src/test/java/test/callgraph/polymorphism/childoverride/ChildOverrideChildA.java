package test.callgraph.polymorphism.childoverride;

/**
 * @author adrninistrator
 * @date 2023/6/1
 * @description:
 */
public class ChildOverrideChildA extends ChildOverrideSuper {

    @Override
    protected void custom() {
        printA();
    }

    private void printA() {
        System.out.println("a");
    }
}

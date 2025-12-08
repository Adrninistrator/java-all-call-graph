package test.callgraph.polymorphism.childoverride;

/**
 * @author adrninistrator
 * @date 2023/6/1
 * @description:
 */
public class ChildOverrideChildAA extends ChildOverrideChildA {

    public String exec() {
        System.out.println("do something");
        return super.exec();
    }

    public void exec2() {
        System.out.println("do something2");
        super.exec();
    }

    @Override
    protected void custom() {
        printAA();
    }

    private void printAA() {
        System.out.println("aa");
    }
}

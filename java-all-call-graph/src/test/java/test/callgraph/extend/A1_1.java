package test.callgraph.extend;

/**
 * @author adrninistrator
 * @date 2021/6/27
 * @description:
 */

public abstract class A1_1 {

    public A1_1() {
        System.out.println("");
    }

    public abstract void f1();

    public abstract void f2();

    protected void fa1_1() {
        String a = "123";
        System.out.println(a);
    }
}

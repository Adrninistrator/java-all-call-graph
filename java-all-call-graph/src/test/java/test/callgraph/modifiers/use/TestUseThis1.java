package test.callgraph.modifiers.use;

/**
 * @author adrninistrator
 * @date 2025/9/15
 * @description:
 */
public class TestUseThis1 {

    private String thisFlag;

    private void thisMethod() {
    }

    private TestUseThis1 thisMethod2() {
        return this;
    }

    private void compareUseType() {
        System.out.println(thisFlag);
        System.out.println(this.thisFlag);

        thisMethod();
        this.thisMethod();

        thisMethod2().thisMethod2().thisMethod2();

        TestUseThis1 instance = new TestUseThis1();
        System.out.println(instance.thisFlag);
        instance.thisMethod();
    }
}

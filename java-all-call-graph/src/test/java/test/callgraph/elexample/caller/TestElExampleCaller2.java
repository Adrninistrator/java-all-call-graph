package test.callgraph.elexample.caller;

import test.callgraph.elexample.annotation.TestExampleAnnotation1;
import test.callgraph.elexample.annotation.TestExampleAnnotation2;
import test.callgraph.elexample.dto.TestElExampleDto1;

/**
 * @author adrninistrator
 * @date 2025/9/26
 * @description:
 */
public class TestElExampleCaller2 {

    private final TestElExampleDto1 testElExampleDto1 = new TestElExampleDto1();

    @TestExampleAnnotation1
    public void test1() {
        test2();
    }

    public void test2() {
        testElExampleDto1.getInt1();
        test3();
        testElExampleDto1.setString3(null);
    }

    @TestExampleAnnotation2
    public void test3() {
    }
}

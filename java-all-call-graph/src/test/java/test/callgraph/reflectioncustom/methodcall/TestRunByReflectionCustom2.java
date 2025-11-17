package test.callgraph.reflectioncustom.methodcall;

import org.junit.Test;
import test.callgraph.fieldrelationships.frc.FRCClass1;
import test.callgraph.fieldrelationships.frc.FRCDtoA;
import test.callgraph.fieldrelationships.frc.FRCDtoB;
import test.callgraph.fieldrelationships.frd.FRDClass1;
import test.callgraph.methodcall.TestMCCallee;
import test.callgraph.reflectioncustom.util.TestReflectionCustomUtil2;
import test.callgraph.spring.bean.define.SpringInterfaceC;
import test.junit.base.TestSpringBase;

import javax.annotation.Resource;

/**
 * @author adrninistrator
 * @date 2025/2/15
 * @description:
 */
public class TestRunByReflectionCustom2 extends TestSpringBase {

    private final TestMCCallee testMCCallee = new TestMCCallee();

    @Resource(name = "ThisIsSpringServiceImplC2")
    private SpringInterfaceC springServiceC2C;

    @Test
    public void test() {
        test1AField();
        test1BSpringField();
        test2AMethodCallReturn();
        test2BMethodCallReturn();
        test2CMethodCallReturn();
        test2DMethodCallReturn();
        test2EMethodCallReturnInSupper();
        FRCDtoA frcDtoA = new FRCDtoA();
        test3Arg(frcDtoA);
    }

    public String test1AField() {
        TestReflectionCustomUtil2.runByReflection("test_thread_name", testMCCallee, "test2", "test1AField", "");
        return Thread.currentThread().getName();
    }

    public String test1BSpringField() {
        TestReflectionCustomUtil2.runByReflection("test_thread_name", springServiceC2C, "test3", "test1BSpringField");
        return Thread.currentThread().getName();
    }

    public String test2AMethodCallReturn() {
        TestReflectionCustomUtil2.runByReflection("test_thread_name", FRDClass1.test6b(null), "testString", "test2AMethodCallReturn");
        return Thread.currentThread().getName();
    }

    public String test2BMethodCallReturn() {
        FRCDtoA frcDtoA = FRCClass1.genFRCDtoA();
        TestReflectionCustomUtil2.runByReflection("test_thread_name", frcDtoA, "testStrFRCDtoA", "test2BMethodCallReturn");
        return Thread.currentThread().getName();
    }

    public String test2CMethodCallReturn() {
        FRCDtoA frcDtoA = FRCClass1.genFRCDtoA();
        TestReflectionCustomUtil2.runByReflection("test_thread_name", (FRCDtoB) frcDtoA, "testStrFRCDtoB", "test2CMethodCallReturn");
        return Thread.currentThread().getName();
    }

    public String test2DMethodCallReturn() {
        TestReflectionCustomUtil2.runByReflection("test_thread_name", new FRCDtoA(), "testStrFRCDtoA", "test2DMethodCallReturn");
        return Thread.currentThread().getName();
    }

    public String test2EMethodCallReturnInSupper() {
        FRCDtoA frcDtoA = new FRCDtoB();
        TestReflectionCustomUtil2.runByReflection("test_thread_name", (FRCDtoB) frcDtoA, "testStrFRCDtoB", "test2EMethodCallReturnInSupper");
        return Thread.currentThread().getName();
    }

    public String test3Arg(FRCDtoA frcDtoA) {
        TestReflectionCustomUtil2.runByReflection("test_thread_name", frcDtoA, "testStrFRCDtoA", "test3Arg");
        return Thread.currentThread().getName();
    }
}

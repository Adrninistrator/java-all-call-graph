package test.callgraph.reflection1.methodcall;

import test.callgraph.fieldrelationships.frc.FRCClass1;
import test.callgraph.fieldrelationships.frc.FRCDtoA;
import test.callgraph.fieldrelationships.frc.FRCDtoB;
import test.callgraph.fieldrelationships.frd.FRDClass1;
import test.callgraph.methodcall.TestMCCallee;
import test.callgraph.reflection1.util.TestReflectionUtil1;
import test.callgraph.spring.bean.define.SpringInterfaceC;

import javax.annotation.Resource;

/**
 * @author adrninistrator
 * @date 2025/2/15
 * @description:
 */
public class TestRunByReflection2 {

    private TestMCCallee testMCCallee;

    @Resource(name = "ThisIsSpringServiceImplC2")
    private SpringInterfaceC springServiceC2C;

    public String test1AField() {
        TestReflectionUtil1.runByReflection("test_thread_name", testMCCallee, "test2", "test1AField", "");
        return Thread.currentThread().getName();
    }

    public String test1BSpringField() {
        TestReflectionUtil1.runByReflection("test_thread_name", springServiceC2C, "test3", "test1BSpringField");
        return Thread.currentThread().getName();
    }

    public String test2AMethodCallReturn() {
        TestReflectionUtil1.runByReflection("test_thread_name", FRDClass1.test6b(null), "testString", "test2AMethodCallReturn");
        return Thread.currentThread().getName();
    }

    public String test2BMethodCallReturn() {
        FRCDtoA frcDtoA = FRCClass1.genFRCDtoA();
        TestReflectionUtil1.runByReflection("test_thread_name", frcDtoA, "testStrFRCDtoA", "test2BMethodCallReturn");
        return Thread.currentThread().getName();
    }

    public String test2CMethodCallReturn() {
        FRCDtoA frcDtoA = FRCClass1.genFRCDtoA();
        TestReflectionUtil1.runByReflection("test_thread_name", (FRCDtoB) frcDtoA, "testStrFRCDtoB", "test2CMethodCallReturn");
        return Thread.currentThread().getName();
    }

    public String test2DMethodCallReturn() {
        TestReflectionUtil1.runByReflection("test_thread_name", new FRCDtoA(), "testStrFRCDtoA", "test2DMethodCallReturn");
        return Thread.currentThread().getName();
    }

    public String test2EMethodCallReturnInSupper() {
        FRCDtoA frcDtoA = new FRCDtoA();
        TestReflectionUtil1.runByReflection("test_thread_name", (FRCDtoB) frcDtoA, "testStrFRCDtoA", "test2EMethodCallReturnInSupper");
        return Thread.currentThread().getName();
    }

    public String test3Arg(FRCDtoA frcDtoA) {
        TestReflectionUtil1.runByReflection("test_thread_name", frcDtoA, "testStrFRCDtoA", "test3Arg");
        return Thread.currentThread().getName();
    }
}

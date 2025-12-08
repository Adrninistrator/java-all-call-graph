package test.callgraph.polymorphism.calleeargtype;

import org.springframework.stereotype.Service;
import test.callgraph.polymorphism.calleeargtype.superchild.CalleeArgTypePolymorphismChild1A;
import test.callgraph.polymorphism.calleeargtype.superchild.CalleeArgTypePolymorphismChild1B;
import test.callgraph.polymorphism.calleeargtype.superchild.CalleeArgTypePolymorphismChild2;
import test.callgraph.polymorphism.calleeargtype.superchild.CalleeArgTypePolymorphismChild3;
import test.callgraph.polymorphism.calleeargtype.superchild.CalleeArgTypePolymorphismInterface1;

import javax.annotation.Resource;

/**
 * @author adrninistrator
 * @date 2025/9/18
 * @description:
 */
@Service
public class CalleeArgTypePolymorphismService1 {

    @Resource(name = CalleeArgTypePolymorphismChild1A.NAME)
    private CalleeArgTypePolymorphismInterface1 calleeArgTypePolymorphismInterface1;

    public void testRun0Use1A() {
        CalleeArgTypePolymorphismTool1 calleeArgTypePolymorphismTool1 = new CalleeArgTypePolymorphismTool1();
        calleeArgTypePolymorphismTool1.run0("1", calleeArgTypePolymorphismInterface1, "2");
    }

    public void testRun0Use1B() {
        CalleeArgTypePolymorphismTool1 calleeArgTypePolymorphismTool1 = new CalleeArgTypePolymorphismTool1();
        CalleeArgTypePolymorphismInterface1 calleeArgTypePolymorphismChild1B = new CalleeArgTypePolymorphismChild1B();
        calleeArgTypePolymorphismTool1.run0("a", calleeArgTypePolymorphismChild1B, "b");
    }

    public void testRun1Use1A() {
        CalleeArgTypePolymorphismTool1 calleeArgTypePolymorphismTool1 = new CalleeArgTypePolymorphismTool1();
        calleeArgTypePolymorphismTool1.run1("1", calleeArgTypePolymorphismInterface1, "2");
    }

    public void testRun1Use1B() {
        CalleeArgTypePolymorphismTool1 calleeArgTypePolymorphismTool1 = new CalleeArgTypePolymorphismTool1();
        CalleeArgTypePolymorphismInterface1 calleeArgTypePolymorphismChild1B = new CalleeArgTypePolymorphismChild1B();
        calleeArgTypePolymorphismTool1.run1("a", calleeArgTypePolymorphismChild1B, "b");
    }

    public void testRun2Use1B23() {
        CalleeArgTypePolymorphismTool1 calleeArgTypePolymorphismTool1 = new CalleeArgTypePolymorphismTool1();
        CalleeArgTypePolymorphismInterface1 calleeArgTypePolymorphismChild1B = new CalleeArgTypePolymorphismChild1B();
        CalleeArgTypePolymorphismChild2 calleeArgTypePolymorphismChild2 = new CalleeArgTypePolymorphismChild2();
        CalleeArgTypePolymorphismChild3 calleeArgTypePolymorphismChild3 = new CalleeArgTypePolymorphismChild3();
        calleeArgTypePolymorphismTool1.run2(calleeArgTypePolymorphismChild1B, calleeArgTypePolymorphismChild2, calleeArgTypePolymorphismChild3);
    }

    public void testNotSupport() {
        CalleeArgTypePolymorphismTool1 calleeArgTypePolymorphismTool1 = new CalleeArgTypePolymorphismTool1();
        boolean use1A = System.currentTimeMillis() % 7 == 1;
        CalleeArgTypePolymorphismInterface1 calleeArgTypePolymorphismChild1B = new CalleeArgTypePolymorphismChild1B();
        calleeArgTypePolymorphismTool1.run1("1", use1A ? calleeArgTypePolymorphismInterface1 : calleeArgTypePolymorphismChild1B, "2");
    }
}

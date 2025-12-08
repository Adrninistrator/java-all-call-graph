package test.callgraph.polymorphism.calleeargtype;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.callgraph.polymorphism.calleeargtype.superchild.CalleeArgTypePolymorphismInterface1;
import test.callgraph.polymorphism.calleeargtype.superchild.CalleeArgTypePolymorphismInterface2;
import test.callgraph.polymorphism.calleeargtype.superchild.CalleeArgTypePolymorphismInterface3;

/**
 * @author adrninistrator
 * @date 2025/9/18
 * @description:
 */
public class CalleeArgTypePolymorphismTool1 {

    private static final Logger logger = LoggerFactory.getLogger(CalleeArgTypePolymorphismTool1.class);

    public void run0(String str1, CalleeArgTypePolymorphismInterface1 arg1, String str2) {
        arg1.cmd1();
        arg1.cmd2();
    }

    public void run1(String str1, CalleeArgTypePolymorphismInterface1 arg1, String str2) {
        System.out.println(str1);

        arg1.cmd1();

        System.getProperty(str2);

        arg1.cmd2();

        System.currentTimeMillis();
    }

    public void run2(CalleeArgTypePolymorphismInterface1 arg1, CalleeArgTypePolymorphismInterface2 arg2, CalleeArgTypePolymorphismInterface3 arg3) {
        arg1.cmd1();
        arg1.cmd2();
        arg2.cmd1();

        logger.info("{}", arg3);
        logger.error("{}", arg3.hashCode());
    }
}

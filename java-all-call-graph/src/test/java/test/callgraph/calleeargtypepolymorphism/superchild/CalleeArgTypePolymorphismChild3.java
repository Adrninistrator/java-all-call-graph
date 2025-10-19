package test.callgraph.calleeargtypepolymorphism.superchild;

import java.util.HashMap;

/**
 * @author adrninistrator
 * @date 2025/10/16
 * @description:
 */
public class CalleeArgTypePolymorphismChild3 implements CalleeArgTypePolymorphismInterface3 {
    @Override
    public void cmd1() {
        new HashMap<>();
    }
}

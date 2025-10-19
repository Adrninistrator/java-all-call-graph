package test.callgraph.calleeargtypepolymorphism.superchild;

import java.util.ArrayList;

/**
 * @author adrninistrator
 * @date 2025/10/16
 * @description:
 */
public class CalleeArgTypePolymorphismChild2 implements CalleeArgTypePolymorphismInterface2 {
    @Override
    public void cmd1() {
        new ArrayList<>();
    }
}

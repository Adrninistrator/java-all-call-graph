package test.callgraph.calleeargtypepolymorphism.superchild;

import org.springframework.stereotype.Service;

/**
 * @author adrninistrator
 * @date 2025/9/18
 * @description:
 */
@Service(value = CalleeArgTypePolymorphismChild1A.NAME)
public class CalleeArgTypePolymorphismChild1A extends CalleeArgTypePolymorphismAbstract1 {

    public static final String NAME = "calleeArgTypePolymorphismChild1A";

    @Override
    public void cmd1() {
        System.setProperty("", "");
    }

    @Override
    public void cmd2() {
        System.setIn(null);
    }
}

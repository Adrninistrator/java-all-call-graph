package test.callgraph.calleeargtypepolymorphism.superchild;

/**
 * @author adrninistrator
 * @date 2025/10/12
 * @description:
 */
public class CalleeArgTypePolymorphismChild1B implements CalleeArgTypePolymorphismInterface1 {

    @Override
    public void cmd1() {
        System.getProperty("");
    }

    @Override
    public void cmd2() {
        System.setErr(null);
    }
}

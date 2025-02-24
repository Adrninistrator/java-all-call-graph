package test.callgraph.chain.define;

import org.apache.commons.chain.Command;
import org.apache.commons.chain.Context;

/**
 * @author adrninistrator
 * @date 2025/2/22
 * @description:
 */
public class TestChainCommand1 implements Command {
    @Override
    public boolean execute(Context context) throws Exception {
        System.out.println(System.currentTimeMillis() + " " + this.getClass().getName());
        return false;
    }
}

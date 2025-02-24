package test.callgraph.chain.define;

import org.apache.commons.chain.Command;
import org.apache.commons.chain.Context;
import org.springframework.stereotype.Service;

/**
 * @author adrninistrator
 * @date 2025/2/22
 * @description:
 */
@Service(TestChainCommandService1.SERVICE_NAME)
public class TestChainCommandService1 implements Command {

    public static final String SERVICE_NAME = "testChainCommandService1";

    @Override
    public boolean execute(Context context) throws Exception {
        System.out.println(System.currentTimeMillis() + " " +this.getClass().getName());
        return false;
    }
}

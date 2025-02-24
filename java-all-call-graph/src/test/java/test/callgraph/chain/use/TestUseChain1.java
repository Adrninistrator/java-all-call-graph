package test.callgraph.chain.use;

import org.apache.commons.chain.Command;
import org.apache.commons.chain.Context;
import org.apache.commons.chain.impl.ChainBase;
import org.apache.commons.chain.impl.ContextBase;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import test.callgraph.chain.define.TestChainCommand1;
import test.callgraph.chain.define.TestChainCommandService1;
import test.callgraph.chain.define.TestChainCommandService2;
import test.junit.base.TestSpringBase;

import javax.annotation.Resource;

/**
 * @author adrninistrator
 * @date 2025/2/22
 * @description:
 */
public class TestUseChain1 extends TestSpringBase {

    @Resource(name = TestChainCommandService1.SERVICE_NAME)
    private Command commandService1;

    @Autowired
    @Qualifier(TestChainCommandService2.SERVICE_NAME)
    private Command commandService2;

    @Test
    public void run() throws Exception {
        ChainBase chain = new ChainBase();
        chain.addCommand(new TestChainCommand1());
        chain.addCommand(commandService1);
        chain.addCommand(commandService2);
        Context context = new ContextBase();
        chain.execute(context);
    }
}

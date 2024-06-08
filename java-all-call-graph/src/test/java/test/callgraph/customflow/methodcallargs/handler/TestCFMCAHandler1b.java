package test.callgraph.customflow.methodcallargs.handler;

import org.springframework.stereotype.Service;
import test.callgraph.customflow.methodcallargs.dto.TestCFMCARequestDto1;
import test.callgraph.customflow.methodcallargs.handler.base.TestBaseCFMCAHandler;

/**
 * @author adrninistrator
 * @date 2023/3/13
 * @description:
 */
@Service("test.callgraph.custom_flow.method_call_args.handler.TestCFMCAHandler1b")
public class TestCFMCAHandler1b extends TestBaseCFMCAHandler<TestCFMCARequestDto1> {

    @Override
    protected void doHandle(TestCFMCARequestDto1 dto) {
        System.currentTimeMillis();
    }
}

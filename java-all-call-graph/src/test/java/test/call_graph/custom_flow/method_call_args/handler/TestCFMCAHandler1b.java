package test.call_graph.custom_flow.method_call_args.handler;

import org.springframework.stereotype.Service;
import test.call_graph.custom_flow.method_call_args.dto.TestCFMCARequestDto1;
import test.call_graph.custom_flow.method_call_args.handler.base.TestBaseCFMCAHandler;

/**
 * @author adrninistrator
 * @date 2023/3/13
 * @description:
 */
@Service("test.call_graph.custom_flow.method_call_args.handler.TestCFMCAHandler1b")
public class TestCFMCAHandler1b extends TestBaseCFMCAHandler<TestCFMCARequestDto1> {

    @Override
    protected void doHandle(TestCFMCARequestDto1 dto) {
        System.currentTimeMillis();
    }
}

package test.call_graph.custom_flow.method_call_args.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import test.call_graph.custom_flow.method_call_args.dto.TestCFMCARequestDto1;
import test.call_graph.custom_flow.method_call_args.flow.TestCFMCAFlow;
import test.call_graph.custom_flow.method_call_args.handler.TestCFMCAHandler1a;
import test.call_graph.custom_flow.method_call_args.handler.base.TestBaseCFMCAHandler;

import javax.annotation.Resource;

/**
 * @author adrninistrator
 * @date 2023/3/13
 * @description:
 */
@Service
public class TestCFMCAService1 {

    @Autowired
    private TestCFMCAHandler1a handler1a;

    @Resource(name = "test.call_graph.custom_flow.method_call_args.handler.TestCFMCAHandler1b")
    private TestBaseCFMCAHandler<?> handler1b;

    public void start() {
        TestCFMCARequestDto1 dto = new TestCFMCARequestDto1();
        dto.setData(this.getClass().getSimpleName());
        TestCFMCAFlow.build(dto)
                .handle(handler1a, handler1b);
    }
}

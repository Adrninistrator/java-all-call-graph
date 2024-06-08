package test.callgraph.customflow.methodcallargs.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import test.callgraph.customflow.methodcallargs.dto.TestCFMCARequestDto1;
import test.callgraph.customflow.methodcallargs.flow.TestCFMCAFlow;
import test.callgraph.customflow.methodcallargs.handler.TestCFMCAHandler1a;
import test.callgraph.customflow.methodcallargs.handler.base.TestBaseCFMCAHandler;

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

    @Resource(name = "test.callgraph.custom_flow.method_call_args.handler.TestCFMCAHandler1b")
    private TestBaseCFMCAHandler<?> handler1b;

    public void start() {
        TestCFMCARequestDto1 dto = new TestCFMCARequestDto1();
        dto.setData(this.getClass().getSimpleName());
        TestCFMCAFlow.build(dto)
                .handle(handler1a, handler1b);
    }
}

package test.call_graph.custom_flow.method_call_args.flow;

import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import org.apache.commons.lang3.ArrayUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.call_graph.custom_flow.method_call_args.dto.base.TestBaseCFMCARequestDto;
import test.call_graph.custom_flow.method_call_args.handler.base.TestBaseCFMCAHandler;

/**
 * @author adrninistrator
 * @date 2023/3/13
 * @description:
 */
public class TestCFMCAFlow {
    private static final Logger logger = LoggerFactory.getLogger(TestCFMCAFlow.class);

    private final TestBaseCFMCARequestDto dto;

    private TestCFMCAFlow(TestBaseCFMCARequestDto dto) {
        this.dto = dto;
    }

    public static TestCFMCAFlow build(TestBaseCFMCARequestDto dto) {
        logger.info("创建新对象 {}", dto.getData());
        return new TestCFMCAFlow(dto);
    }

    @SuppressWarnings("unchecked")
    public void handle(TestBaseCFMCAHandler<?>... handlers) {
        if (ArrayUtils.isEmpty(handlers)) {
            throw new JavaCGRuntimeException("不允许为空");
        }

        for (TestBaseCFMCAHandler handler : handlers) {
            handler.handle(dto);
        }
    }
}

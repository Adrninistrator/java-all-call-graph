package test.callgraph.customflow.methodcallargs.flow;

import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import org.apache.commons.lang3.ArrayUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.callgraph.customflow.methodcallargs.dto.base.TestBaseCFMCARequestDto;
import test.callgraph.customflow.methodcallargs.handler.base.TestBaseCFMCAHandler;

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
            throw new JavaCG2RuntimeException("不允许为空");
        }

        for (TestBaseCFMCAHandler handler : handlers) {
            handler.handle(dto);
        }
    }
}

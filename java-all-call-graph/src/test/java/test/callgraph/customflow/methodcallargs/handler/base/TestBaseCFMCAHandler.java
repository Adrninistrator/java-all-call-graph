package test.callgraph.customflow.methodcallargs.handler.base;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.callgraph.customflow.methodcallargs.dto.base.TestBaseCFMCARequestDto;

/**
 * @author adrninistrator
 * @date 2023/3/13
 * @description:
 */
public abstract class TestBaseCFMCAHandler<T extends TestBaseCFMCARequestDto> {
    private static final Logger logger = LoggerFactory.getLogger(TestBaseCFMCAHandler.class);

    protected final String simpleClassName = this.getClass().getSimpleName();

    // 自定义处理
    protected abstract void doHandle(T dto);

    public void handle(T dto) {
        logger.info("开始处理 [{}] data: {}", simpleClassName, dto.getData());
        doHandle(dto);
        logger.info("处理完毕 [{}]", simpleClassName);
    }
}

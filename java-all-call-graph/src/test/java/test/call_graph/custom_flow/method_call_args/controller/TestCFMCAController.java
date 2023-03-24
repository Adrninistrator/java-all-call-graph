package test.call_graph.custom_flow.method_call_args.controller;

import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import org.apache.commons.lang3.ArrayUtils;
import test.call_graph.custom_flow.method_call_args.dto.base.TestBaseCFMCARequestDto;
import test.call_graph.custom_flow.method_call_args.handler.base.TestBaseCFMCAHandler;

/**
 * @author adrninistrator
 * @date 2023/3/13
 * @description:
 */
public class TestCFMCAController {
    private TestBaseCFMCARequestDto dto;

    private TestCFMCAController(TestBaseCFMCARequestDto dto) {
        this.dto = dto;
    }

    public static TestCFMCAController build(TestBaseCFMCARequestDto dto) {
        return new TestCFMCAController(dto);
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

package test.run_by_code.handler.method;

import com.adrninistrator.jacg.handler.method.FindMethodByDtoHandler;
import org.apache.commons.lang3.StringUtils;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.call_graph.argument.TestArgument1;
import test.run_by_code.base.TestRunByCodeBase;

import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/4/13
 * @description:
 */
public class TestFindMethodByDtoHandler extends TestRunByCodeBase {
    private static final Logger logger = LoggerFactory.getLogger(TestFindMethodByDtoHandler.class);

    @Test
    public void test() {
        try (FindMethodByDtoHandler findMethodByDtoHandler = new FindMethodByDtoHandler(configureWrapper)) {
            doTest(findMethodByDtoHandler, String.class.getName());
            doTest(findMethodByDtoHandler, "int");
            doTest(findMethodByDtoHandler, TestArgument1.class.getName());
        }
    }

    protected void doTest(FindMethodByDtoHandler findMethodByDtoHandler, String className) {
        Set<String> set = findMethodByDtoHandler.findMethodByAllType(className);
        logger.info("查询dto相关的方法 [{}]\n{}", className, StringUtils.join(set, "\n"));
    }
}

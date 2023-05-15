package test.run_by_code.handler.method;

import com.adrninistrator.jacg.handler.method.FindMethodByDtoHandler;
import org.junit.Test;
import test.call_graph.argument.TestArgument1;
import test.run_by_code.base.TestRunByCodeBase;

import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/4/13
 * @description:
 */
public class TestFindMethodByDtoHandler extends TestRunByCodeBase {

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
        printSetContent(set, className);
    }
}

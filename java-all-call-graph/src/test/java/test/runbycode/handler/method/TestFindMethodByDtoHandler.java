package test.runbycode.handler.method;

import com.adrninistrator.jacg.handler.method.MethodArgReturnHandler;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.argument.TestArgument1;
import test.runbycode.base.TestRunByCodeBase;

import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/4/13
 * @description:
 */
public class TestFindMethodByDtoHandler extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDb();
    }

    @Test
    public void test() {
        try (MethodArgReturnHandler methodArgReturnHandler = new MethodArgReturnHandler(configureWrapper)) {
            doTest(methodArgReturnHandler, String.class.getName());
            doTest(methodArgReturnHandler, "int");
            doTest(methodArgReturnHandler, TestArgument1.class.getName());
        }
    }

    protected void doTest(MethodArgReturnHandler findMethodByDtoHandler, String className) {
        Set<String> set = findMethodByDtoHandler.findMethodByAllType(className);
        Assert.assertFalse(JavaCGUtil.isCollectionEmpty(set));
        printSetContent(set, className);
    }
}

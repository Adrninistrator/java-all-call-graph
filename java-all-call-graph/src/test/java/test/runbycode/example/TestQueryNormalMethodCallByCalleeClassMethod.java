package test.runbycode.example;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.handler.methodcall.MethodCallHandler;
import org.junit.Test;
import test.annotation.JACGExample;
import test.callgraph.methodcall.TestMCCallee;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/8/24
 * @description:
 */
@JACGExample(title = "查询方法被直接调用的情况",
        desc = {"对于指定的类名及方法名，查询这些方法在其他方法中被直接调用的情况（字节码中存在对应的方法调用指令）",
                "查询结果包含相关的调用方法与被调用方法的详情信息"}
)
public class TestQueryNormalMethodCallByCalleeClassMethod extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDbForce();
    }

    @Test
    public void test() {
        try (MethodCallHandler methodCallHandler = new MethodCallHandler(configureWrapper)) {
            List<WriteDbData4MethodCall> list = methodCallHandler.queryNormalMethodCallByCalleeClassMethod(TestMCCallee.class.getName(), "test1", true);
            printListContent(list);
        }
    }
}

package test.runbycode.handler.methodcall;

import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.dto.methodcall.MethodCallCountInfo;
import com.adrninistrator.jacg.handler.methodcall.MethodCallHandler;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import test.annotation.JACGExample;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycode.config.TestConfigGenerator;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2026/7/4
 * @description: 测试 MethodCallHandler.queryTopMethods（热点方法统计）
 */
@JACGExample(title = "查询被调用最多或调用最多的方法",
        desc = {"byCallee=true: 被调用最多；byCallee=false: 调用最多",
                "按调用次数降序返回，含外部库方法"})
public class TestQueryTopMethods extends TestRunByCodeBase {

    // 强制使用H2数据库，避免依赖外部数据库
    @Before
    public void forceUseH2Db() {
        TestConfigGenerator.useH2Db(configureWrapper);
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_FLAG, currentClassName + "_" + currentMethodName);
    }

    @Test
    public void $test0WriteDb() {
        commonWriteDbForce();
    }

    // 场景1：byCallee=true，降序、字段非空
    @Test
    public void test1Callee() {
        try (MethodCallHandler methodCallHandler = new MethodCallHandler(configureWrapper)) {
            List<MethodCallCountInfo> top = methodCallHandler.queryTopMethods(true, 10);
            Assert.assertFalse("热点方法列表应非空", top.isEmpty());
            Assert.assertTrue("返回数量应 <= 10", top.size() <= 10);
            for (int i = 1; i < top.size(); i++) {
                Assert.assertTrue("callCount 应降序: " + top.get(i - 1).getCallCount() + " >= " + top.get(i).getCallCount(),
                        top.get(i - 1).getCallCount() >= top.get(i).getCallCount());
            }
            MethodCallCountInfo first = top.get(0);
            Assert.assertNotNull(first.getFullMethod());
            Assert.assertNotNull(first.getMethodHash());
            Assert.assertTrue("callCount 应 > 0", first.getCallCount() > 0);
            printListContent(top, "callee 热点方法");
        }
    }

    // 场景2：byCallee=false
    @Test
    public void test2Caller() {
        try (MethodCallHandler methodCallHandler = new MethodCallHandler(configureWrapper)) {
            List<MethodCallCountInfo> top = methodCallHandler.queryTopMethods(false, 10);
            Assert.assertFalse("热点方法列表应非空", top.isEmpty());
            printListContent(top, "caller 热点方法");
        }
    }

    // 场景3：应包含外部库方法（非 test. 包）
    @Test
    public void test3ContainsExternal() {
        try (MethodCallHandler methodCallHandler = new MethodCallHandler(configureWrapper)) {
            List<MethodCallCountInfo> top = methodCallHandler.queryTopMethods(true, 20);
            boolean hasExternal = false;
            for (MethodCallCountInfo m : top) {
                if (!m.getFullMethod().startsWith("test.")) {
                    hasExternal = true;
                    break;
                }
            }
            Assert.assertTrue("应包含外部库方法（非 test. 包）", hasExternal);
        }
    }

    // 场景4：limit 非法时仍返回结果（不抛异常）
    @Test
    public void test4InvalidLimit() {
        try (MethodCallHandler methodCallHandler = new MethodCallHandler(configureWrapper)) {
            List<MethodCallCountInfo> top = methodCallHandler.queryTopMethods(true, 0);
            // limit 0 时数据库返回空或 0 行，不应抛异常
            Assert.assertNotNull(top);
        }
    }
}

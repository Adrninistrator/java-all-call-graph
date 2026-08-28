package test.runbycode.handler.methodcall;

import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.dto.methodcall.MethodCallCountInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
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
 * @description: 测试 MethodCallHandler.queryMethodCallByCalleeHash（按 callee methodHash 精确查）
 */
@JACGExample(title = "按被调用方方法HASH查询方法调用",
        desc = {"使用独立缓存 MC_QUERY_MC_BY_CALLEE_HASH（与 queryMethodCallByCalleeMethodWithReturn 的 MC_QUERY_METHOD_CALL_BY_CALLEE_HASH 区分）",
                "适用于已有 methodHash（如来自 queryTopMethods）直接查询的场景"})
public class TestQueryMethodCallByCalleeHash extends TestRunByCodeBase {

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

    // 场景1：用 queryTopMethods 取一个 hash，queryMethodCallByCalleeHash 应返回非空
    @Test
    public void test1ByHashFromTopMethods() {
        try (MethodCallHandler methodCallHandler = new MethodCallHandler(configureWrapper)) {
            List<MethodCallCountInfo> top = methodCallHandler.queryTopMethods(true, 5);
            Assert.assertFalse("热点方法列表应非空", top.isEmpty());
            String hash = top.get(0).getMethodHash();
            Assert.assertNotNull(hash);
            List<WriteDbData4MethodCall> rows = methodCallHandler.queryMethodCallByCalleeHash(hash);
            Assert.assertFalse("按 callee hash 查询应非空", rows.isEmpty());
            // 返回的 calleeMethodHash 应与传入一致
            Assert.assertEquals(hash, rows.get(0).getCalleeMethodHash());
            printListContent(rows, "按 callee hash 查询 " + hash);
        }
    }

    // 场景2：不存在的 hash → 返回空
    @Test
    public void test2NonExistHash() {
        try (MethodCallHandler methodCallHandler = new MethodCallHandler(configureWrapper)) {
            List<WriteDbData4MethodCall> rows = methodCallHandler.queryMethodCallByCalleeHash("NonExistHash#000");
            Assert.assertTrue("不存在的 hash 应返回空", rows.isEmpty());
        }
    }
}

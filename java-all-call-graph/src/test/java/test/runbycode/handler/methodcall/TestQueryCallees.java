package test.runbycode.handler.methodcall;

import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.handler.methodcall.MethodCallHandler;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import test.annotation.JACGExample;
import test.callgraph.methodcall.TestMCCaller;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycode.config.TestConfigGenerator;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2026/7/4
 * @description: 测试 MethodCallHandler.queryCallees（向下调用链，按类名+方法名查 method_call）
 */
@JACGExample(title = "查询指定方法调用了哪些方法",
        desc = {"按类名+方法名直接查 method_call，返回被调用方列表",
                "支持简单类名/完整类名；同名类须用完整类名"})
public class TestQueryCallees extends TestRunByCodeBase {

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

    // 场景1：被解析 jar 内方法，简单类名输入
    @Test
    public void test1InJarSimpleClassName() {
        try (MethodCallHandler methodCallHandler = new MethodCallHandler(configureWrapper)) {
            List<WriteDbData4MethodCall> callees = methodCallHandler.queryCallees(TestMCCaller.class.getSimpleName(), "test1a");
            Assert.assertFalse("TestMCCaller.test1a 的被调用方应非空", callees.isEmpty());
            boolean found = false;
            for (WriteDbData4MethodCall c : callees) {
                Assert.assertNotNull(c.getCalleeFullMethod());
                Assert.assertNotNull(c.getCallType());
                if (c.getCalleeFullMethod().startsWith("test.callgraph.methodcall.TestMCCallee:test1")) {
                    found = true;
                }
            }
            Assert.assertTrue("应包含 TestMCCallee.test1", found);
            printListContent(callees, "TestMCCaller.test1a 被调用方");
        }
    }

    // 场景2：完整类名输入，结果与简单类名一致
    @Test
    public void test2InJarFullClassName() {
        try (MethodCallHandler methodCallHandler = new MethodCallHandler(configureWrapper)) {
            List<WriteDbData4MethodCall> callees1 = methodCallHandler.queryCallees(TestMCCaller.class.getSimpleName(), "test1a");
            List<WriteDbData4MethodCall> callees2 = methodCallHandler.queryCallees(TestMCCaller.class.getName(), "test1a");
            Assert.assertEquals("简单类名与完整类名输入结果行数应一致", callees1.size(), callees2.size());
        }
    }

    // 场景3：同名类简单类名输入 → 应抛出异常
    @Test
    public void test3DuplicateSimpleClassName() {
        try (MethodCallHandler methodCallHandler = new MethodCallHandler(configureWrapper)) {
            boolean thrown = false;
            try {
                methodCallHandler.queryCallees("SameNameClass1", "test1");
            } catch (JavaCG2RuntimeException e) {
                thrown = true;
            }
            Assert.assertTrue("同名类简单类名输入应抛出 JavaCG2RuntimeException", thrown);
        }
    }

    // 场景4：不存在的类 → 返回空
    @Test
    public void test4NonExistClass() {
        try (MethodCallHandler methodCallHandler = new MethodCallHandler(configureWrapper)) {
            List<WriteDbData4MethodCall> callees = methodCallHandler.queryCallees("NonExistClass", "nonExistMethod");
            Assert.assertTrue("不存在的类应返回空", callees.isEmpty());
        }
    }

    // 场景5：countCallees 总数 == queryCallees 全量行数；不存在返回 0；同名类抛异常
    @Test
    public void test5Count() {
        try (MethodCallHandler methodCallHandler = new MethodCallHandler(configureWrapper)) {
            // in-jar 方法：count == 全量查询行数
            List<WriteDbData4MethodCall> all = methodCallHandler.queryCallees(TestMCCaller.class.getSimpleName(), "test1a", 0, 10000);
            long count = methodCallHandler.countCallees(TestMCCaller.class.getSimpleName(), "test1a");
            Assert.assertEquals("countCallees 应等于全量 queryCallees 行数", all.size(), count);

            // 完整类名一致
            long countFull = methodCallHandler.countCallees(TestMCCaller.class.getName(), "test1a");
            Assert.assertEquals("完整类名 count 应与简单类名一致", count, countFull);

            // 不存在的类 → 0
            long countNonExist = methodCallHandler.countCallees("NonExistClass", "nonExistMethod");
            Assert.assertEquals("不存在的类 count 应为 0", 0L, countNonExist);

            // 同名类简单输入 → 抛异常（与 queryCallees 同步）
            boolean thrown = false;
            try {
                methodCallHandler.countCallees("SameNameClass1", "test1");
            } catch (JavaCG2RuntimeException e) {
                thrown = true;
            }
            Assert.assertTrue("同名类简单类名输入应抛出 JavaCG2RuntimeException", thrown);
        }
    }
}

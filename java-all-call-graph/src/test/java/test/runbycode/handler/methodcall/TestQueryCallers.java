package test.runbycode.handler.methodcall;

import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.dto.methodcall.MethodCallCountInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;
import com.adrninistrator.jacg.handler.methodcall.MethodCallHandler;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import test.annotation.JACGExample;
import test.callgraph.methodcall.TestMCCallee;
import test.callgraph.methodcall.TestMCCaller;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycode.config.TestConfigGenerator;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2026/7/4
 * @description: 测试 MethodCallHandler.queryCallers（向上调用链，按类名+方法名查 method_call）
 */
@JACGExample(title = "查询谁调用了指定方法",
        desc = {"按类名+方法名直接查 method_call，返回调用方列表，外部库方法也能查到（F1 修复）",
                "支持简单类名/完整类名；同名类须用完整类名，简单类名输入会抛出异常"})
public class TestQueryCallers extends TestRunByCodeBase {

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
            List<WriteDbData4MethodCall> callers = methodCallHandler.queryCallers(TestMCCallee.class.getSimpleName(), "test1");
            Assert.assertFalse("TestMCCallee.test1 的调用方应非空", callers.isEmpty());
            boolean found = false;
            for (WriteDbData4MethodCall c : callers) {
                Assert.assertNotNull(c.getCallerFullMethod());
                Assert.assertNotNull(c.getCallType());
                if (c.getCallerFullMethod().startsWith(TestMCCaller.class.getName() + ":")) {
                    found = true;
                }
            }
            Assert.assertTrue("应包含 TestMCCaller 的调用方", found);
            printListContent(callers, "TestMCCallee.test1 调用方");
        }
    }

    // 场景2：完整类名输入，结果与简单类名一致
    @Test
    public void test2InJarFullClassName() {
        try (MethodCallHandler methodCallHandler = new MethodCallHandler(configureWrapper)) {
            List<WriteDbData4MethodCall> callers1 = methodCallHandler.queryCallers(TestMCCallee.class.getSimpleName(), "test1");
            List<WriteDbData4MethodCall> callers2 = methodCallHandler.queryCallers(TestMCCallee.class.getName(), "test1");
            Assert.assertEquals("简单类名与完整类名输入结果行数应一致", callers1.size(), callers2.size());
        }
    }

    // 场景3：外部库方法（不在被解析 jar 中）→ F1 修复验证，应非空
    @Test
    public void test3ExternalMethod() {
        try (MethodCallHandler methodCallHandler = new MethodCallHandler(configureWrapper)) {
            // 用 queryTopMethods 找一个外部库方法（非 test. 包）
            List<MethodCallCountInfo> top = methodCallHandler.queryTopMethods(true, 20);
            Assert.assertFalse("热点方法列表应非空", top.isEmpty());
            MethodCallCountInfo external = null;
            for (MethodCallCountInfo m : top) {
                if (!m.getFullMethod().startsWith("test.")) {
                    external = m;
                    break;
                }
            }
            Assert.assertNotNull("应存在外部库方法的热点", external);
            String fullMethod = external.getFullMethod();
            int colon = fullMethod.indexOf(':');
            int paren = fullMethod.indexOf('(');
            String className = fullMethod.substring(0, colon);
            String methodName = fullMethod.substring(colon + 1, paren);
            List<WriteDbData4MethodCall> callers = methodCallHandler.queryCallers(className, methodName);
            Assert.assertFalse("外部库方法 queryCallers 应非空（F1 修复）: " + fullMethod, callers.isEmpty());
            printListContent(callers, "外部库方法调用方 " + fullMethod);
        }
    }

    // 场景4：同名类简单类名输入 → 应抛出异常
    @Test
    public void test4DuplicateSimpleClassName() {
        try (MethodCallHandler methodCallHandler = new MethodCallHandler(configureWrapper)) {
            boolean thrown = false;
            try {
                // SameNameClass1 在 test.callgraph.samename.a 与 .b 两个包中存在，属于同名类
                methodCallHandler.queryCallers("SameNameClass1", "test1");
            } catch (JavaCG2RuntimeException e) {
                thrown = true;
            }
            Assert.assertTrue("同名类简单类名输入应抛出 JavaCG2RuntimeException", thrown);
        }
    }

    // 场景5：同名类完整类名输入 → 不抛异常
    @Test
    public void test5DuplicateFullClassName() {
        try (MethodCallHandler methodCallHandler = new MethodCallHandler(configureWrapper)) {
            List<WriteDbData4MethodCall> callers = methodCallHandler.queryCallers("test.callgraph.samename.a.SameNameClass1", "test1");
            Assert.assertNotNull("完整类名输入不应抛异常", callers);
        }
    }

    // 场景6：不存在的类 → 返回空
    @Test
    public void test6NonExistClass() {
        try (MethodCallHandler methodCallHandler = new MethodCallHandler(configureWrapper)) {
            List<WriteDbData4MethodCall> callers = methodCallHandler.queryCallers("NonExistClass", "nonExistMethod");
            Assert.assertTrue("不存在的类应返回空", callers.isEmpty());
        }
    }

    // 场景7：分页，两页无重叠
    @Test
    public void test7Paging() {
        try (MethodCallHandler methodCallHandler = new MethodCallHandler(configureWrapper)) {
            List<WriteDbData4MethodCall> page1 = methodCallHandler.queryCallers(TestMCCallee.class.getSimpleName(), "test1", 0, 2);
            List<WriteDbData4MethodCall> page2 = methodCallHandler.queryCallers(TestMCCallee.class.getSimpleName(), "test1", 2, 2);
            Assert.assertTrue("page1 行数应 <= 2", page1.size() <= 2);
            Assert.assertTrue("page2 行数应 <= 2", page2.size() <= 2);
            Set<Integer> ids1 = new HashSet<>();
            Set<Integer> ids2 = new HashSet<>();
            for (WriteDbData4MethodCall c : page1) {
                ids1.add(c.getCallId());
            }
            for (WriteDbData4MethodCall c : page2) {
                ids2.add(c.getCallId());
            }
            ids1.retainAll(ids2);
            Assert.assertEquals("两页不应有重叠的 call_id", 0, ids1.size());
        }
    }

    // 场景8：countCallers 总数 == queryCallers 全量行数；完整类名一致；不存在返回 0；同名类抛异常
    @Test
    public void test8Count() {
        try (MethodCallHandler methodCallHandler = new MethodCallHandler(configureWrapper)) {
            // in-jar 方法：count == 全量查询行数
            List<WriteDbData4MethodCall> all = methodCallHandler.queryCallers(TestMCCallee.class.getSimpleName(), "test1", 0, 10000);
            long count = methodCallHandler.countCallers(TestMCCallee.class.getSimpleName(), "test1");
            Assert.assertEquals("countCallers 应等于全量 queryCallers 行数", all.size(), count);

            // 完整类名一致
            long countFull = methodCallHandler.countCallers(TestMCCallee.class.getName(), "test1");
            Assert.assertEquals("完整类名 count 应与简单类名一致", count, countFull);

            // 不存在的类 → 0
            long countNonExist = methodCallHandler.countCallers("NonExistClass", "nonExistMethod");
            Assert.assertEquals("不存在的类 count 应为 0", 0L, countNonExist);

            // 同名类简单输入 → 抛异常（与 queryCallers 同步）
            boolean thrown = false;
            try {
                methodCallHandler.countCallers("SameNameClass1", "test1");
            } catch (JavaCG2RuntimeException e) {
                thrown = true;
            }
            Assert.assertTrue("同名类简单类名输入应抛出 JavaCG2RuntimeException", thrown);
        }
    }
}

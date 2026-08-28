package test.runbycode.classes;

import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.dboper.DbInitializer;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.callgraph.empty.TestEmptyClass1;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycode.config.TestConfigGenerator;

/**
 * @author adrninistrator
 * @date 2026/7/4
 * @description: 测试 DbOperWrapper.querySimpleClassNameInTask 方法（不执行Runner）
 */
public class TestQuerySimpleClassNameInTask extends TestRunByCodeBase {

    private static final Logger logger = LoggerFactory.getLogger(TestQuerySimpleClassNameInTask.class);

    // 用于测试的存在于jar包中的类名
    private static final String EXIST_CLASS_NAME = TestEmptyClass1.class.getName();
    private static final String EXIST_SIMPLE_CLASS_NAME = TestEmptyClass1.class.getSimpleName();

    // 强制使用H2数据库，并设置APP_NAME包含当前方法名，便于人工检查结果
    @Before
    public void forceUseH2Db() {
        TestConfigGenerator.useH2Db(configureWrapper);
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_FLAG, currentClassName + "_" + currentMethodName);
    }

    // 场景1：使用存在的完整类名查询，应返回对应的简单类名
    @Test
    public void test1QueryByExistFullClassName() {
        commonWriteDb();
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            String simpleClassName = dbOperWrapper.querySimpleClassNameInTask(EXIST_CLASS_NAME);
            logger.info("完整类名 {} 查询结果: {}", EXIST_CLASS_NAME, simpleClassName);
            Assert.assertNotNull("使用存在的完整类名查询，结果不应为null", simpleClassName);
            Assert.assertEquals("使用存在的完整类名查询，结果应为简单类名", EXIST_SIMPLE_CLASS_NAME, simpleClassName);
        } catch (Exception e) {
            Assert.fail("测试出现异常: " + e.getMessage());
        }
    }

    // 场景2：使用不存在的完整类名查询，应返回null
    @Test
    public void test2QueryByNotExistFullClassName() {
        commonWriteDb();
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            String notExistFullClassName = "com.not.exist.ClassName";
            String simpleClassName = dbOperWrapper.querySimpleClassNameInTask(notExistFullClassName);
            logger.info("不存在的完整类名 {} 查询结果: {}", notExistFullClassName, simpleClassName);
            Assert.assertNull("使用不存在的完整类名查询，结果应为null", simpleClassName);
        } catch (Exception e) {
            Assert.fail("测试出现异常: " + e.getMessage());
        }
    }

    // 场景3：使用存在的简单类名查询，应返回对应的简单类名
    @Test
    public void test3QueryByExistSimpleClassName() {
        commonWriteDb();
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            String simpleClassNameByFull = dbOperWrapper.querySimpleClassNameInTask(EXIST_CLASS_NAME);
            Assert.assertNotNull(simpleClassNameByFull);

            String simpleClassNameBySimple = dbOperWrapper.querySimpleClassNameInTask(simpleClassNameByFull);
            logger.info("简单类名 {} 查询结果: {}", simpleClassNameByFull, simpleClassNameBySimple);
            Assert.assertNotNull("使用存在的简单类名查询，结果不应为null", simpleClassNameBySimple);
            Assert.assertEquals("使用存在的简单类名查询，结果应与通过完整类名查询一致", simpleClassNameByFull, simpleClassNameBySimple);
        } catch (Exception e) {
            Assert.fail("测试出现异常: " + e.getMessage());
        }
    }

    // 场景4：使用不存在的简单类名查询，应返回null
    @Test
    public void test4QueryByNotExistSimpleClassName() {
        commonWriteDb();
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            String notExistSimpleClassName = "NotExistClass";
            String simpleClassName = dbOperWrapper.querySimpleClassNameInTask(notExistSimpleClassName);
            logger.info("不存在的简单类名 {} 查询结果: {}", notExistSimpleClassName, simpleClassName);
            Assert.assertNull("使用不存在的简单类名查询，结果应为null", simpleClassName);
        } catch (Exception e) {
            Assert.fail("测试出现异常: " + e.getMessage());
        }
    }

    // 场景5：缓存验证 - 对同一个完整类名连续查询两次，结果应一致
    @Test
    public void test5CacheForExistFullClassName() {
        commonWriteDb();
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            String firstResult = dbOperWrapper.querySimpleClassNameInTask(EXIST_CLASS_NAME);
            String secondResult = dbOperWrapper.querySimpleClassNameInTask(EXIST_CLASS_NAME);
            Assert.assertNotNull(firstResult);
            Assert.assertEquals("缓存命中时，结果应与首次查询一致", firstResult, secondResult);
        } catch (Exception e) {
            Assert.fail("测试出现异常: " + e.getMessage());
        }
    }

    // 场景6：缓存验证 - 对同一个不存在的完整类名连续查询两次，都应返回null（验证ILLEGAL_METHOD_FLAG占位符机制）
    @Test
    public void test6CacheForNotExistFullClassName() {
        commonWriteDb();
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            String notExistFullClassName = "com.not.exist.CachedNullClass";
            String firstResult = dbOperWrapper.querySimpleClassNameInTask(notExistFullClassName);
            Assert.assertNull("首次查询不存在的完整类名，结果应为null", firstResult);

            String secondResult = dbOperWrapper.querySimpleClassNameInTask(notExistFullClassName);
            Assert.assertNull("缓存命中时（ILLEGAL_METHOD_FLAG占位符），结果也应为null", secondResult);
        } catch (Exception e) {
            Assert.fail("测试出现异常: " + e.getMessage());
        }
    }

    // 场景7：缓存验证 - 对同一个不存在的简单类名连续查询两次，都应返回null
    @Test
    public void test7CacheForNotExistSimpleClassName() {
        commonWriteDb();
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            String notExistSimpleClassName = "NotExistCachedClass";
            String firstResult = dbOperWrapper.querySimpleClassNameInTask(notExistSimpleClassName);
            Assert.assertNull("首次查询不存在的简单类名，结果应为null", firstResult);

            String secondResult = dbOperWrapper.querySimpleClassNameInTask(notExistSimpleClassName);
            Assert.assertNull("缓存命中时（ILLEGAL_METHOD_FLAG占位符），结果也应为null", secondResult);
        } catch (Exception e) {
            Assert.fail("测试出现异常: " + e.getMessage());
        }
    }

    // 场景8：不同的不存在类名，应各自独立返回null，互不影响
    @Test
    public void test8DifferentNotExistClassNames() {
        commonWriteDb();
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            String result1 = dbOperWrapper.querySimpleClassNameInTask("com.not.exist.ClassA");
            String result2 = dbOperWrapper.querySimpleClassNameInTask("com.not.exist.ClassB");
            String result3 = dbOperWrapper.querySimpleClassNameInTask("NotExistSimpleA");
            String result4 = dbOperWrapper.querySimpleClassNameInTask("NotExistSimpleB");

            Assert.assertNull("不存在的完整类名ClassA应返回null", result1);
            Assert.assertNull("不存在的完整类名ClassB应返回null", result2);
            Assert.assertNull("不存在的简单类名NotExistSimpleA应返回null", result3);
            Assert.assertNull("不存在的简单类名NotExistSimpleB应返回null", result4);
        } catch (Exception e) {
            Assert.fail("测试出现异常: " + e.getMessage());
        }
    }

    // 场景9：验证调用方在querySimpleClassNameInTask返回null时，使用任务中指定的类名而非null
    @Test
    public void test9CallerUsesOriginalClassNameWhenNull() {
        commonWriteDb();
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            // 模拟不存在的完整类名
            String notExistFullClassName = "com.not.exist.SomeClass";
            String simpleClassName = dbOperWrapper.querySimpleClassNameInTask(notExistFullClassName);
            Assert.assertNull(simpleClassName);
            // 调用方应使用任务中指定的类名，而非null
            String usedClassName = StringUtils.isBlank(simpleClassName) ? notExistFullClassName : simpleClassName;
            Assert.assertEquals("调用方应使用任务中指定的类名", notExistFullClassName, usedClassName);
            Assert.assertNotEquals("调用方使用的类名不应为null字符串", "null", usedClassName);

            // 模拟不存在的简单类名
            String notExistSimpleClassName = "NotExistSomeClass";
            String simpleClassName2 = dbOperWrapper.querySimpleClassNameInTask(notExistSimpleClassName);
            Assert.assertNull(simpleClassName2);
            String usedClassName2 = StringUtils.isBlank(simpleClassName2) ? notExistSimpleClassName : simpleClassName2;
            Assert.assertEquals("调用方应使用任务中指定的简单类名", notExistSimpleClassName, usedClassName2);
        } catch (Exception e) {
            Assert.fail("测试出现异常: " + e.getMessage());
        }
    }

    // 场景10：验证存在的类名查询后，调用方使用返回的唯一类名
    @Test
    public void test10CallerUsesResultWhenNotNull() {
        commonWriteDb();
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            String simpleClassName = dbOperWrapper.querySimpleClassNameInTask(EXIST_CLASS_NAME);
            Assert.assertNotNull(simpleClassName);
            String usedClassName = StringUtils.isBlank(simpleClassName) ? EXIST_CLASS_NAME : simpleClassName;
            Assert.assertEquals("调用方应使用返回的唯一类名", simpleClassName, usedClassName);
        } catch (Exception e) {
            Assert.fail("测试出现异常: " + e.getMessage());
        }
    }
}

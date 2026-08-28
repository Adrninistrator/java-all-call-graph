package test.runbycode.classes;

import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.dboper.DbInitializer;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.callgraph.methodcall.TestMCCallee;
import test.callgraph.samename.a.SameNameClass1;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycode.config.TestConfigGenerator;

/**
 * @author adrninistrator
 * @date 2026/7/4
 * @description: 测试 DbOperWrapper.isDuplicateSimpleClassName（识别同名类简单类名输入）
 */
public class TestIsDuplicateSimpleClassName extends TestRunByCodeBase {
    private static final Logger logger = LoggerFactory.getLogger(TestIsDuplicateSimpleClassName.class);

    // 强制使用H2数据库，并设置APP_NAME包含当前方法名，便于人工检查结果
    @Before
    public void forceUseH2Db() {
        TestConfigGenerator.useH2Db(configureWrapper);
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_FLAG, currentClassName + "_" + currentMethodName);
    }

    @Test
    public void $test0WriteDb() {
        commonWriteDbForce();
    }

    // 场景1：同名类简单名 → true（SameNameClass1 在 test.callgraph.samename.a 与 .b 两个包中存在）
    @Test
    public void test1DuplicateSimpleClassName() {
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        boolean result = dbOperWrapper.isDuplicateSimpleClassName(SameNameClass1.class.getSimpleName());
        Assert.assertTrue("SameNameClass1 应为同名类", result);
        logger.info("SameNameClass1 isDuplicate: {}", result);
    }

    // 场景2：非同名类简单名 → false
    @Test
    public void test2NonDuplicateSimpleClassName() {
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        boolean result = dbOperWrapper.isDuplicateSimpleClassName(TestMCCallee.class.getSimpleName());
        Assert.assertFalse("TestMCCallee 应非同名类", result);
        logger.info("TestMCCallee isDuplicate: {}", result);
    }

    // 场景3：不存在的类 → false
    @Test
    public void test3NonExistClassName() {
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        boolean result = dbOperWrapper.isDuplicateSimpleClassName("NonExistClassName");
        Assert.assertFalse("不存在的类应非同名类", result);
    }

    // 场景4：blank 输入 → false
    @Test
    public void test4BlankInput() {
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        Assert.assertFalse(dbOperWrapper.isDuplicateSimpleClassName(null));
        Assert.assertFalse(dbOperWrapper.isDuplicateSimpleClassName(""));
        Assert.assertFalse(dbOperWrapper.isDuplicateSimpleClassName("   "));
    }
}

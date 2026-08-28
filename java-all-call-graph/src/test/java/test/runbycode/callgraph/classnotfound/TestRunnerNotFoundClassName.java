package test.runbycode.callgraph.classnotfound;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.runbycode.base.TestRunByCodeBase;
import test.runbycode.config.TestConfigGenerator;

import java.io.File;

/**
 * @author adrninistrator
 * @date 2026/7/4
 * @description: 测试查询目标类不存在时，执行Runner生成的!not_found文件名包含任务指定的类名而非null
 */
public class TestRunnerNotFoundClassName extends TestRunByCodeBase {

    private static final Logger logger = LoggerFactory.getLogger(TestRunnerNotFoundClassName.class);

    // 强制使用H2数据库，并设置APP_NAME包含当前方法名，便于人工检查结果
    @Before
    public void forceUseH2Db() {
        TestConfigGenerator.useH2Db(configureWrapper);
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_FLAG, "_" + currentClassName + "_" + currentMethodName);
    }

    // 场景1：Callee端，不存在的简单类名，验证生成的!not_found文件名包含任务指定的类名而非null
    @Test
    public void test1CalleeNotFoundSimpleClassName() {
        commonWriteDb();
        String notExistSimpleClassName = "NotExistCalleeClass1";
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, Boolean.FALSE.toString());
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE, notExistSimpleClassName);
        RunnerGenAllGraph4Callee runner = new RunnerGenAllGraph4Callee(configureWrapper);
        Assert.assertTrue(runner.run());

        String outputDirPath = runner.getCurrentOutputDirPath();
        Assert.assertNotNull(outputDirPath);
        logger.info("test1 输出目录: {}", outputDirPath);
        checkNotFoundFileNotContainsNull(outputDirPath, notExistSimpleClassName);
    }

    // 场景2：Callee端，不存在的完整类名，验证生成的!not_found文件名包含任务指定的类名而非null
    @Test
    public void test2CalleeNotFoundFullClassName() {
        commonWriteDb();
        String notExistFullClassName = "com.not.exist.CalleeClass2";
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, Boolean.FALSE.toString());
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE, notExistFullClassName);
        RunnerGenAllGraph4Callee runner = new RunnerGenAllGraph4Callee(configureWrapper);
        Assert.assertTrue(runner.run());

        String outputDirPath = runner.getCurrentOutputDirPath();
        Assert.assertNotNull(outputDirPath);
        logger.info("test2 输出目录: {}", outputDirPath);
        checkNotFoundFileNotContainsNull(outputDirPath, notExistFullClassName);
    }

    // 场景3：Caller端，不存在的简单类名，验证生成的!not_found文件名包含任务指定的类名而非null
    @Test
    public void test3CallerNotFoundSimpleClassName() {
        commonWriteDb();
        String notExistSimpleClassName = "NotExistCallerClass1";
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, Boolean.FALSE.toString());
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER, notExistSimpleClassName + ":test1()");
        RunnerGenAllGraph4Caller runner = new RunnerGenAllGraph4Caller(configureWrapper);
        Assert.assertTrue(runner.run());

        String outputDirPath = runner.getCurrentOutputDirPath();
        Assert.assertNotNull(outputDirPath);
        logger.info("test3 输出目录: {}", outputDirPath);
        checkNotFoundFileNotContainsNull(outputDirPath, notExistSimpleClassName);
    }

    // 场景4：Caller端，不存在的完整类名，验证生成的!not_found文件名包含任务指定的类名而非null
    @Test
    public void test4CallerNotFoundFullClassName() {
        commonWriteDb();
        String notExistFullClassName = "com.not.exist.CallerClass2";
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, Boolean.FALSE.toString());
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER, notExistFullClassName + ":test1()");
        RunnerGenAllGraph4Caller runner = new RunnerGenAllGraph4Caller(configureWrapper);
        Assert.assertTrue(runner.run());

        String outputDirPath = runner.getCurrentOutputDirPath();
        Assert.assertNotNull(outputDirPath);
        logger.info("test4 输出目录: {}", outputDirPath);
        checkNotFoundFileNotContainsNull(outputDirPath, notExistFullClassName);
    }

    // 场景5：Callee端，不存在的简单类名+方法名，验证生成的!not_found文件名包含任务指定的类名而非null
    @Test
    public void test5CalleeNotFoundSimpleClassNameWithMethod() {
        commonWriteDb();
        String notExistSimpleClassName = "NotExistCalleeClassM1";
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, Boolean.FALSE.toString());
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE, notExistSimpleClassName + ":test1()");
        RunnerGenAllGraph4Callee runner = new RunnerGenAllGraph4Callee(configureWrapper);
        Assert.assertTrue(runner.run());

        String outputDirPath = runner.getCurrentOutputDirPath();
        Assert.assertNotNull(outputDirPath);
        logger.info("test5 输出目录: {}", outputDirPath);
        checkNotFoundFileNotContainsNull(outputDirPath, notExistSimpleClassName);
    }

    // 场景6：Callee端，不存在的完整类名+方法名，验证生成的!not_found文件名包含任务指定的类名而非null
    @Test
    public void test6CalleeNotFoundFullClassNameWithMethod() {
        commonWriteDb();
        String notExistFullClassName = "com.not.exist.CalleeClassM2";
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, Boolean.FALSE.toString());
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE, notExistFullClassName + ":test1()");
        RunnerGenAllGraph4Callee runner = new RunnerGenAllGraph4Callee(configureWrapper);
        Assert.assertTrue(runner.run());

        String outputDirPath = runner.getCurrentOutputDirPath();
        Assert.assertNotNull(outputDirPath);
        logger.info("test6 输出目录: {}", outputDirPath);
        checkNotFoundFileNotContainsNull(outputDirPath, notExistFullClassName);
    }

    // 场景7：Callee端，多个不存在的类名，验证每个类各自生成独立的!not_found文件，不会合并
    @Test
    public void test7CalleeMultipleNotFoundClasses() {
        commonWriteDb();
        String notExistClass1 = "NotExistCalleeMulti1";
        String notExistClass2 = "NotExistCalleeMulti2";
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, Boolean.FALSE.toString());
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE, notExistClass1, notExistClass2);
        RunnerGenAllGraph4Callee runner = new RunnerGenAllGraph4Callee(configureWrapper);
        Assert.assertTrue(runner.run());

        String outputDirPath = runner.getCurrentOutputDirPath();
        Assert.assertNotNull(outputDirPath);
        logger.info("test7 输出目录: {}", outputDirPath);
        checkNotFoundFilesForMultipleClasses(outputDirPath, notExistClass1, notExistClass2);
    }

    // 场景8：Callee端，不存在的简单类名+代码行号，验证生成的!not_found文件名包含任务指定的类名而非null
    @Test
    public void test8CalleeNotFoundSimpleClassNameWithLineNumber() {
        commonWriteDb();
        String notExistSimpleClassName = "NotExistCalleeClassL1";
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, Boolean.FALSE.toString());
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE, notExistSimpleClassName + ":123");
        RunnerGenAllGraph4Callee runner = new RunnerGenAllGraph4Callee(configureWrapper);
        Assert.assertTrue(runner.run());

        String outputDirPath = runner.getCurrentOutputDirPath();
        Assert.assertNotNull(outputDirPath);
        logger.info("test12 输出目录: {}", outputDirPath);
        checkNotFoundFileNotContainsNull(outputDirPath, notExistSimpleClassName);
    }

    // 场景9：Callee端，不存在的完整类名+代码行号，验证生成的!not_found文件名包含任务指定的类名而非null
    @Test
    public void test9CalleeNotFoundFullClassNameWithLineNumber() {
        commonWriteDb();
        String notExistFullClassName = "com.not.exist.CalleeClassL2";
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, Boolean.FALSE.toString());
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE, notExistFullClassName + ":456");
        RunnerGenAllGraph4Callee runner = new RunnerGenAllGraph4Callee(configureWrapper);
        Assert.assertTrue(runner.run());

        String outputDirPath = runner.getCurrentOutputDirPath();
        Assert.assertNotNull(outputDirPath);
        logger.info("test13 输出目录: {}", outputDirPath);
        checkNotFoundFileNotContainsNull(outputDirPath, notExistFullClassName);
    }

    // 场景10：Caller端，不存在的简单类名+代码行号，验证生成的!not_found文件名包含任务指定的类名而非null
    @Test
    public void test10CallerNotFoundSimpleClassNameWithLineNumber() {
        commonWriteDb();
        String notExistSimpleClassName = "NotExistCallerClassL1";
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, Boolean.FALSE.toString());
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER, notExistSimpleClassName + ":123");
        RunnerGenAllGraph4Caller runner = new RunnerGenAllGraph4Caller(configureWrapper);
        Assert.assertTrue(runner.run());

        String outputDirPath = runner.getCurrentOutputDirPath();
        Assert.assertNotNull(outputDirPath);
        logger.info("test10 输出目录: {}", outputDirPath);
        checkNotFoundFileNotContainsNull(outputDirPath, notExistSimpleClassName);
    }

    // 场景11：Caller端，不存在的完整类名+代码行号，验证生成的!not_found文件名包含任务指定的类名而非null
    @Test
    public void test11CallerNotFoundFullClassNameWithLineNumber() {
        commonWriteDb();
        String notExistFullClassName = "com.not.exist.CallerClassL2";
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, Boolean.FALSE.toString());
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER, notExistFullClassName + ":456");
        RunnerGenAllGraph4Caller runner = new RunnerGenAllGraph4Caller(configureWrapper);
        Assert.assertTrue(runner.run());

        String outputDirPath = runner.getCurrentOutputDirPath();
        Assert.assertNotNull(outputDirPath);
        logger.info("test11 输出目录: {}", outputDirPath);
        checkNotFoundFileNotContainsNull(outputDirPath, notExistFullClassName);
    }

    // 场景12：Caller端，不存在的简单类名（仅类名，不指定方法），验证生成的!not_found文件名包含任务指定的类名而非null
    @Test
    public void test12CallerNotFoundSimpleClassNameOnly() {
        commonWriteDb();
        String notExistSimpleClassName = "NotExistCallerClassOnly1";
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, Boolean.FALSE.toString());
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER, notExistSimpleClassName);
        RunnerGenAllGraph4Caller runner = new RunnerGenAllGraph4Caller(configureWrapper);
        Assert.assertTrue(runner.run());

        String outputDirPath = runner.getCurrentOutputDirPath();
        Assert.assertNotNull(outputDirPath);
        logger.info("test12 输出目录: {}", outputDirPath);
        checkNotFoundFileNotContainsNull(outputDirPath, notExistSimpleClassName);
    }

    // 场景13：Caller端，不存在的完整类名（仅类名，不指定方法），验证生成的!not_found文件名包含任务指定的类名而非null
    @Test
    public void test13CallerNotFoundFullClassNameOnly() {
        commonWriteDb();
        String notExistFullClassName = "com.not.exist.CallerClassOnly2";
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE, Boolean.TRUE.toString());
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY, Boolean.FALSE.toString());
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER, notExistFullClassName);
        RunnerGenAllGraph4Caller runner = new RunnerGenAllGraph4Caller(configureWrapper);
        Assert.assertTrue(runner.run());

        String outputDirPath = runner.getCurrentOutputDirPath();
        Assert.assertNotNull(outputDirPath);
        logger.info("test13 输出目录: {}", outputDirPath);
        checkNotFoundFileNotContainsNull(outputDirPath, notExistFullClassName);
    }

    /**
     * 检查输出目录中的!not_found文件，验证文件名包含任务指定的类名且不包含"null"
     *
     * @param outputDirPath 输出目录路径
     * @param className     任务中指定的类名
     */
    private void checkNotFoundFileNotContainsNull(String outputDirPath, String className) {
        File outputDir = new File(outputDirPath);
        Assert.assertTrue("输出目录应存在", outputDir.exists());

        File[] notFoundFiles = outputDir.listFiles((dir, name) ->
                name.contains(JACGConstants.FLAG_NOT_FOUND) && name.endsWith(JACGConstants.NOT_FOUND_TXT));

        Assert.assertNotNull("应生成!not_found文件", notFoundFiles);
        Assert.assertTrue("应至少生成一个!not_found文件，类名: " + className, notFoundFiles.length > 0);
        logger.info("类名 {} 对应的!not_found文件数量: {}", className, notFoundFiles.length);

        for (File notFoundFile : notFoundFiles) {
            String fileName = notFoundFile.getName();
            logger.info("检查!not_found文件: {}", fileName);
            // 文件名不应包含"null"
            Assert.assertFalse("!not_found文件名不应包含\"null\"，实际文件名: " + fileName, fileName.contains("null"));
            // 文件名应包含任务指定的类名
            Assert.assertTrue("!not_found文件名应包含任务指定的类名 " + className + "，实际文件名: " + fileName,
                    fileName.contains(className));
        }
    }

    /**
     * 检查输出目录中的!not_found文件，验证多个不存在的类各自生成独立文件，且文件名不包含"null"
     *
     * @param outputDirPath 输出目录路径
     * @param classNames    任务中指定的多个类名
     */
    private void checkNotFoundFilesForMultipleClasses(String outputDirPath, String... classNames) {
        File outputDir = new File(outputDirPath);
        Assert.assertTrue("输出目录应存在", outputDir.exists());

        File[] notFoundFiles = outputDir.listFiles((dir, name) ->
                name.contains(JACGConstants.FLAG_NOT_FOUND) && name.endsWith(JACGConstants.NOT_FOUND_TXT));

        Assert.assertNotNull("应生成!not_found文件", notFoundFiles);
        Assert.assertTrue("应至少生成" + classNames.length + "个!not_found文件", notFoundFiles.length >= classNames.length);
        logger.info("多个类名对应的!not_found文件数量: {} 期望类名数量: {}", notFoundFiles.length, classNames.length);

        for (File notFoundFile : notFoundFiles) {
            logger.info("检查!not_found文件: {}", notFoundFile.getName());
        }

        // 验证每个类名都有对应的!not_found文件
        for (String className : classNames) {
            boolean found = false;
            for (File notFoundFile : notFoundFiles) {
                String fileName = notFoundFile.getName();
                // 文件名不应包含"null"
                Assert.assertFalse("!not_found文件名不应包含\"null\"，实际文件名: " + fileName, fileName.contains("null"));
                if (fileName.contains(className)) {
                    found = true;
                }
            }
            Assert.assertTrue("应为类 " + className + " 生成包含该类名的!not_found文件", found);
            logger.info("类名 {} 对应的!not_found文件检查通过", className);
        }
    }
}

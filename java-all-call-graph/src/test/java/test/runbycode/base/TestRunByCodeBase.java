package test.runbycode.base;

import com.adrninistrator.jacg.common.DC;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbInitializer;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.dto.callstack.CallStackFileResult;
import com.adrninistrator.jacg.findstack.FindCallStackTrace;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.jacg.util.JACGJsonUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Rule;
import org.junit.rules.TestName;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.runbycode.config.TestConfigGenerator;
import test.runbycode.junit.JUnit4ClassRunnerSortMethod;
import test.runbycode.util.JACGTestUtil;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
/*
    @FixMethodOrder注解指定在父类无效
    使用JUnit4ClassRunnerSortMethod之后，不需要再通过@FixMethodOrder注解指定方法执行顺序
 */
@RunWith(JUnit4ClassRunnerSortMethod.class)
public abstract class TestRunByCodeBase {
    private static final Logger logger = LoggerFactory.getLogger(TestRunByCodeBase.class);

    public static final String TEST_FLAG_RECORD_LOG_TO_FILE = "test.flag.record.log.to.file";

    // 当前执行的测试方法及执行次数
    private final Map<String, Integer> currentMethodRunTimesMap = new HashMap<>();

    protected final String currentClassName = this.getClass().getName();

    // 当前执行的测试方法名
    protected String currentMethodName;

    /*
        是否将输出记录到文件，当以下JVM参数非空时将日志中的输出记录到文件
        使用Gradle执行单元测试时，假如需要将日志中的输出记录到文件，使用unittest.gradle中的命令
     */
    private final boolean recordLogToFile = System.getProperty(TEST_FLAG_RECORD_LOG_TO_FILE) != null;

    // 当前输出到文件的Writer
    private Writer currentOutputWriter;

    protected ConfigureWrapper configureWrapper;

    @Rule
    public TestName name = new TestName();

    @Before
    public void initTestRunByCodeBase() {
        currentMethodName = name.getMethodName();
        logger.info("当前执行的测试方法 {} {}", currentClassName, currentMethodName);

        // 生成通用的参数配置
        configureWrapper = TestConfigGenerator.genConfigureWrapper();

        if (recordLogToFile && !JavaCGFileUtil.isDirectoryExists(JACGConstants.DIR_OUTPUT_UNITTEST)) {
            throw new JavaCGRuntimeException("创建目录失败");
        }

        // 使用本地的配置参数
        JACGTestUtil.useLocalConfig(configureWrapper);
    }

    @After
    public void afterMethod() {
        if (currentOutputWriter != null) {
            // 需要将输出记录到文件时，关闭上次的Writer
            IOUtils.closeQuietly(currentOutputWriter);
        }
    }

    // 将jar包解析结果写入数据库，若指定的jar包未发生变化不则不执行
    protected void commonWriteDb() {
        commonWriteDb(configureWrapper, false);
    }

    // 将jar包解析结果写入数据库
    protected void commonWriteDbForce() {
        commonWriteDb(configureWrapper, true);
    }

    // 将jar包解析结果写入数据库
    protected void commonWriteDb(ConfigureWrapper usedConfigureWrapper, boolean force) {
        RunnerWriteDb runnerWriteDb = new RunnerWriteDb(usedConfigureWrapper);
        if (!force) {
            // jar包及允许处理的类名或包名前缀没有变化时跳过写数据库操作
            runnerWriteDb.setSkipWhenNotModified(true);
        }
        boolean result = runnerWriteDb.run();
        Assert.assertTrue(result);
    }

    protected void writeDbSupportField(ConfigureWrapper usedConfigureWrapper) {
        usedConfigureWrapper.setMainConfig(ConfigKeyEnum.CKE_HANDLE_GET_SET_FIELD_RELATIONSHIP, Boolean.TRUE.toString());
        DbOperWrapper dbOperWrapper = DbInitializer.genDbOperWrapper(usedConfigureWrapper, this);
        try (DbOperator dbOperator = dbOperWrapper.getDbOperator()) {
            if (dbOperator.checkTableExists(DbTableInfoEnum.DTIE_GET_METHOD)) {
                String sql = "select " + DC.GSM_RECORD_ID +
                        " from " + DbTableInfoEnum.DTIE_GET_METHOD.getTableName() +
                        " limit 1";
                sql = dbOperWrapper.formatSql(sql);
                String id = dbOperator.queryObjectOneColumn(sql, String.class);
                if (id != null) {
                    commonWriteDb(usedConfigureWrapper, true);
                    return;
                }
            }
            Assert.assertTrue(new RunnerWriteDb(usedConfigureWrapper).run());
        } catch (Exception e) {
            logger.error("error ", e);
            Assert.fail();
        }
    }

    private void printInfo(String info) {
        printInfo(info, false);
    }

    private void printInfo(String info, boolean printFlag) {
        if (printFlag) {
            logger.info("[flag]: [{}]", info);
        } else {
            logger.info("[info]: [{}]", info);
        }

        if (!recordLogToFile || currentOutputWriter == null) {
            return;
        }
        // 将输出记录到文件
        try {
            currentOutputWriter.write(info + JavaCGConstants.NEW_LINE);
        } catch (IOException e) {
            throw new JavaCGRuntimeException(e);
        }
    }

    private void commonPrintHandle(String... flags) {
        if (recordLogToFile) {
            // 需要将输出记录到文件
            // 获取当前执行的测试方法执行次数
            Integer currentMethodRunTimes = currentMethodRunTimesMap.computeIfAbsent(currentMethodName, k -> 0);
            currentMethodRunTimesMap.put(currentMethodName, ++currentMethodRunTimes);

            if (currentOutputWriter != null) {
                // 关闭上次的Writer
                IOUtils.closeQuietly(currentOutputWriter);
            }

            // 生成当前输出的文件
            String currentOutputFilePath = JACGConstants.DIR_OUTPUT_UNITTEST + File.separator + currentClassName + JACGConstants.FLAG_AT + currentMethodName +
                    JACGConstants.FLAG_AT + currentMethodRunTimes + JACGConstants.EXT_MD;
            try {
                currentOutputWriter = JavaCGFileUtil.genBufferedWriter(currentOutputFilePath);
            } catch (FileNotFoundException e) {
                throw new JavaCGRuntimeException(e);
            }
        }
        if (!ArrayUtils.isEmpty(flags)) {
            for (String flag : flags) {
                printInfo(flag, true);
            }
        }
    }

    protected <T> void printListContent(List<T> list, String... flags) {
        printListContent(list, false, flags);
    }

    protected <T> void printListContent(List<T> list, boolean printToString, String... flags) {
        commonPrintHandle(flags);

        if (JavaCGUtil.isCollectionEmpty(list)) {
            printInfo("list为空");
            return;
        }
        for (T object : list) {
            if (printToString) {
                printInfo(object.toString());
            } else {
                printInfo(JACGJsonUtil.getJsonStrPretty(object));
            }
        }
    }

    protected void printSetContent(Set<String> set, String... flags) {
        commonPrintHandle(flags);

        if (JavaCGUtil.isCollectionEmpty(set)) {
            printInfo("set为空");
            return;
        }
        List<String> list = new ArrayList<>(set);
        Collections.sort(list);
        for (String str : list) {
            printInfo(str);
        }
    }

    protected <T> void printMapContent(Map<String, T> map, String... flags) {
        commonPrintHandle(flags);

        if (JavaCGUtil.isMapEmpty(map)) {
            printInfo("map为空");
            return;
        }
        List<String> keyList = new ArrayList<>(map.keySet());
        Collections.sort(keyList);
        for (String key : keyList) {
            T value = map.get(key);
            printInfo("key: " + key);
            printInfo("value: " + JACGJsonUtil.getJsonStrPretty(value));
        }
    }

    protected <T> void printObjectContent(T value, String... flags) {
        commonPrintHandle(flags);
        if (value == null) {
            printInfo("value为空");
            return;
        }
        printInfo("value: " + JACGJsonUtil.getJsonStrPretty(value));
    }

    protected void runFindCallStackTraceAndCheck(FindCallStackTrace findCallStackTrace) {
        CallStackFileResult callStackFileResult = findCallStackTrace.find();
        Assert.assertTrue(callStackFileResult.isSuccess());
        Assert.assertNotNull(callStackFileResult);
        Assert.assertFalse(JavaCGUtil.isCollectionEmpty(callStackFileResult.getStackFilePathList()));

        ConfigureWrapper usedConfigureWrapper = findCallStackTrace.getConfigureWrapper();
        if (OutputDetailEnum.ODE_1.getDetail().equals(usedConfigureWrapper.getMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, false))) {
            Assert.assertFalse(JavaCGUtil.isCollectionEmpty(callStackFileResult.getSeparateStackDirPathList()));
            return;
        }
    }
}

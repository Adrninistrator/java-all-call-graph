package test.run_by_code.base;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGJsonUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.rules.TestName;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.run_by_code.config.TestConfigGenerator;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.Writer;
import java.lang.reflect.Method;
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

        if (recordLogToFile && !JACGFileUtil.isDirectoryExists(JACGConstants.DIR_OUTPUT_UNITTEST)) {
            throw new JavaCGRuntimeException("创建目录失败");
        }

        // 以下为本地调试时使用
        try {
            Class<?> testRunLocalConfigClass = Class.forName("test.run_local.TestRunLocalConfig");
            Method testRunLocalConfigSetMethod = testRunLocalConfigClass.getMethod("setConfig", ConfigureWrapper.class);
            testRunLocalConfigSetMethod.invoke(testRunLocalConfigClass, configureWrapper);
            logger.warn("!!!调用本地的修改配置参数方法!!!");
        } catch (ClassNotFoundException e) {
            logger.info("未找到指定的类，不执行调用本地的修改配置参数方法 {}", e.getMessage());
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }

    @After
    public void afterMethod() {
        if (currentOutputWriter != null) {
            // 需要将输出记录到文件时，关闭上次的Writer
            IOUtils.closeQuietly(currentOutputWriter);
        }
    }

    // 将jar包解析结果写入数据库
    protected boolean commonInsertDb() {
        return new RunnerWriteDb().run(configureWrapper);
    }

    private void printInfo(String info) {
        logger.info("{}", info);

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
        if (ArrayUtils.isEmpty(flags)) {
            return;
        }
        for (String flag : flags) {
            printInfo("[" + flag + "]");
        }
    }

    protected <T> void printListContent(List<T> list, String... flags) {
        commonPrintHandle(flags);

        if (JavaCGUtil.isCollectionEmpty(list)) {
            printInfo("list为空");
            return;
        }
        for (T object : list) {
            printInfo(JACGJsonUtil.getJsonStrPretty(object));
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

        if (JACGUtil.isMapEmpty(map)) {
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
}

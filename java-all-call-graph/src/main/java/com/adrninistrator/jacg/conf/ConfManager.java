package com.adrninistrator.jacg.conf;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.InputDirEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.common.enums.interfaces.BaseConfigInterface;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.util.Properties;
import java.util.regex.Pattern;

/**
 * @author adrninistrator
 * @date 2021/6/17
 * @description: 配置管理类
 */

public class ConfManager {
    public static final Logger logger = LoggerFactory.getLogger(ConfManager.class);

    private static final Pattern APP_NAME_PATTERN = Pattern.compile("[A-Za-z0-9_]*");

    /**
     * 获取使用的配置信息，每次返回独立的对象，可重复执行
     *
     * @param configureWrapper
     * @param printLog
     * @return
     */
    public static ConfInfo getConfInfo(ConfigureWrapper configureWrapper, boolean printLog) {
        ConfInfo confInfo = new ConfInfo();
        String configFilePath = getInputRootPath() + InputDirEnum.IDE_CONFIG.getDirName() + "/" + JACGConstants.FILE_CONFIG;
        String configDbFilePath = getInputRootPath() + InputDirEnum.IDE_CONFIG.getDirName() + "/" + JACGConstants.FILE_CONFIG_DB;
        try (BufferedReader br4Config = JavaCGFileUtil.genBufferedReader(JACGFileUtil.getFileInputStream(configFilePath));
             BufferedReader br4ConfigDb = JavaCGFileUtil.genBufferedReader(JACGFileUtil.getFileInputStream(configDbFilePath))) {
            Properties properties4Config = new Properties();
            properties4Config.load(br4Config);
            Properties properties4ConfigDb = new Properties();
            properties4ConfigDb.load(br4ConfigDb);

            // 当前应用的调用关系写入数据库里的表名后缀
            String appName = configureWrapper.getConfig(properties4Config, ConfigKeyEnum.CKE_APP_NAME, printLog);
            if (checkBlank(appName, ConfigKeyEnum.CKE_APP_NAME, configFilePath) || !checkAppName(appName)) {
                return null;
            }

            // 并发处理线程数量/数据源连接池数量
            String strThreadNum = configureWrapper.getConfig(properties4Config, ConfigKeyEnum.CKE_THREAD_NUM, printLog);
            if (checkBlank(strThreadNum, ConfigKeyEnum.CKE_THREAD_NUM, configFilePath)) {
                return null;
            }
            int threadNum = handleThreadNum(strThreadNum);
            if (threadNum == 0) {
                return null;
            }

            // 生成调用链时的详细程度
            String callGraphOutputDetail = configureWrapper.getConfig(properties4Config, ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, printLog);
            if (checkBlank(callGraphOutputDetail, ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, configFilePath)) {
                return null;
            }
            OutputDetailEnum outputDetailEnum = OutputDetailEnum.getFromDetail(callGraphOutputDetail);
            if (OutputDetailEnum.ODE_ILLEGAL == outputDetailEnum) {
                logger.error("{}\n{} 参数配置非法，可选值如下\n{}", configFilePath, ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL.getKey(), OutputDetailEnum.getValidValues());
                return null;
            }

            // 在一个调用方法中出现多次的被调用方法（包含方法调用业务功能数据），是否需要忽略
            String ignoreDupCalleeInOneCaller = configureWrapper.getConfig(properties4Config, ConfigKeyEnum.CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER, printLog);
            if (StringUtils.isBlank(ignoreDupCalleeInOneCaller)) {
                // 允许对应配置为空
                ignoreDupCalleeInOneCaller = Boolean.FALSE.toString();
            }

            // 生成文件的根目录
            String outputRootPath = configureWrapper.getConfig(properties4Config, ConfigKeyEnum.CKE_OUTPUT_ROOT_PATH, printLog);

            // 批量写入数据库时每次插入的数量
            String dbInsertBatchSizeStr = configureWrapper.getConfig(properties4Config, ConfigKeyEnum.CKE_DB_INSERT_BATCH_SIZE, printLog);
            if (checkBlank(dbInsertBatchSizeStr, ConfigKeyEnum.CKE_DB_INSERT_BATCH_SIZE, configFilePath)) {
                return null;
            }
            int dbInsertBatchSize = Integer.parseInt(dbInsertBatchSizeStr);
            if (dbInsertBatchSize <= 0 || dbInsertBatchSize > JACGConstants.MAX_DB_INSERT_BATCH_SIZE) {
                logger.error("{}\n{} 参数配置非法，应在以下范围: (0,{}]", configFilePath, ConfigKeyEnum.CKE_DB_INSERT_BATCH_SIZE.getKey(), JACGConstants.MAX_DB_INSERT_BATCH_SIZE);
                return null;
            }

            // 检查jar包文件是否有更新
            String checkJarFileUpdated = configureWrapper.getConfig(properties4Config, ConfigKeyEnum.CKE_CHECK_JAR_FILE_UPDATED, printLog);

            // 数据库相关配置
            String strDbUseH2 = configureWrapper.getConfig(properties4ConfigDb, ConfigDbKeyEnum.CDKE_DB_USE_H2, printLog);
            if (checkBlank(strDbUseH2, ConfigDbKeyEnum.CDKE_DB_USE_H2, configDbFilePath)) {
                return null;
            }

            confInfo.setDbUseH2(Boolean.parseBoolean(strDbUseH2));
            if (confInfo.isDbUseH2()) {
                if (!handleH2Db(configureWrapper, confInfo, properties4ConfigDb, configDbFilePath, printLog)) {
                    return null;
                }
            } else {
                if (!handleNonH2Db(configureWrapper, confInfo, properties4ConfigDb, configDbFilePath, printLog)) {
                    return null;
                }
            }

            confInfo.setAppName(appName);
            confInfo.setCallGraphOutputDetail(callGraphOutputDetail);
            confInfo.setThreadNum(threadNum);
            confInfo.setOriginalThreadNum(threadNum);
            confInfo.setIgnoreDupCalleeInOneCaller(Boolean.parseBoolean(ignoreDupCalleeInOneCaller));
            confInfo.setOutputRootPath(outputRootPath);
            confInfo.setDbInsertBatchSize(dbInsertBatchSize);
            confInfo.setCheckJarFileUpdated(Boolean.parseBoolean(checkJarFileUpdated));

            return confInfo;
        } catch (Exception e) {
            logger.error("error {} {} ", configFilePath, configDbFilePath, e);
            return null;
        }
    }

    private static boolean checkAppName(String appName) {
        if (!APP_NAME_PATTERN.matcher(appName).matches()) {
            logger.error("{} 属性只支持字母、数字及下划线 {}", ConfigKeyEnum.CKE_APP_NAME.getKey(), appName);
            return false;
        }
        return true;
    }

    // 处理线程数，返回0代表失败
    private static int handleThreadNum(String strThreadNum) {
        int threadNum;
        try {
            threadNum = Integer.parseInt(strThreadNum);
        } catch (NumberFormatException e) {
            logger.error("非法线程数 {} {}", ConfigKeyEnum.CKE_THREAD_NUM.getKey(), strThreadNum);
            return 0;
        }

        if (threadNum <= 0) {
            logger.error("线程数过小 {} {}", ConfigKeyEnum.CKE_THREAD_NUM.getKey(), strThreadNum);
            return 0;
        }
        if (threadNum > JACGConstants.MAX_THREAD_NUM) {
            logger.error("线程数过大 {} {}", ConfigKeyEnum.CKE_THREAD_NUM.getKey(), strThreadNum);
            return 0;
        }

        return threadNum;
    }

    private static boolean checkBlank(String value, BaseConfigInterface baseConfig, String configFilePath) {
        String key = baseConfig.getKey();
        if (StringUtils.isBlank(value)) {
            logger.error("配置文件中未指定参数 {} {}", configFilePath, key);
            return true;
        }

        return false;
    }

    private static boolean handleH2Db(ConfigureWrapper configureWrapper, ConfInfo confInfo, Properties properties4ConfigDb, String configFilePath, boolean printLog) {
        logger.info("使用H2数据库");
        String dbH2FilePath = configureWrapper.getConfig(properties4ConfigDb, ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, printLog);
        if (checkBlank(dbH2FilePath, ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, configFilePath)) {
            return false;
        }

        if (StringUtils.endsWithIgnoreCase(dbH2FilePath, JACGConstants.H2_FILE_EXT)) {
            logger.error("{} 属性不需要指定H2数据库的后缀 {} {}", ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH, JACGConstants.H2_FILE_EXT, dbH2FilePath);
            return false;
        }

        confInfo.setDbH2FilePath(dbH2FilePath);

        return true;
    }

    private static boolean handleNonH2Db(ConfigureWrapper configureWrapper, ConfInfo confInfo, Properties properties4ConfigDb, String configFilePath, boolean printLog) {
        logger.info("使用非H2数据库");
        String dbDriverName = configureWrapper.getConfig(properties4ConfigDb, ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME, printLog);
        if (checkBlank(dbDriverName, ConfigDbKeyEnum.CDKE_DB_DRIVER_NAME, configFilePath)) {
            return false;
        }

        String dbUrl = configureWrapper.getConfig(properties4ConfigDb, ConfigDbKeyEnum.CDKE_DB_URL, printLog);
        if (checkBlank(dbUrl, ConfigDbKeyEnum.CDKE_DB_URL, configFilePath)) {
            return false;
        }

        String dbUsername = configureWrapper.getConfig(properties4ConfigDb, ConfigDbKeyEnum.CDKE_DB_USERNAME, printLog);
        if (checkBlank(dbUsername, ConfigDbKeyEnum.CDKE_DB_USERNAME, configFilePath)) {
            return false;
        }

        String dbPassword = configureWrapper.getConfig(properties4ConfigDb, ConfigDbKeyEnum.CDKE_DB_PASSWORD, printLog);
        if (checkBlank(dbPassword, ConfigDbKeyEnum.CDKE_DB_PASSWORD, configFilePath)) {
            return false;
        }

        confInfo.setDbDriverName(dbDriverName);
        confInfo.setDbUrl(dbUrl);
        confInfo.setDbUsername(dbUsername);
        confInfo.setDbPassword(dbPassword);

        return true;
    }

    /**
     * 获取配置文件根目录
     *
     * @return
     */
    public static String getInputRootPath() {
        return JavaCGUtil.getDirPathInJvmOptions(JavaCGConstants.PROPERTY_INPUT_ROOT_PATH);
    }

    private ConfManager() {
        throw new IllegalStateException("illegal");
    }
}

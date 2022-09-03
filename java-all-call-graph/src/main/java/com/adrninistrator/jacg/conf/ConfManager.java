package com.adrninistrator.jacg.conf;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.InputDirEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.util.Properties;
import java.util.regex.Pattern;

/**
 * @author adrninistrator
 * @date 2021/6/17
 * @description:
 */

public class ConfManager {

    public static final Logger logger = LoggerFactory.getLogger(ConfManager.class);

    private static ConfInfo CONF_INFO = new ConfInfo();

    private static final Pattern APP_NAME_PATTERN = Pattern.compile("[A-Za-z0-9_]*");

    private static boolean inited = false;

    public static boolean isInited() {
        return inited;
    }

    public static ConfInfo getConfInfo() {
        if (inited) {
            return CONF_INFO;
        }

        inited = true;

        if (JACGConstants.DB_INSERT_BATCH_SIZE <= 0) {
            logger.error("通过-D{}=参数指定的批量写入数据库时每次插入的数量参数非法 {}", JACGConstants.PROPERTY_DB_INSERT_BATCH_SIZE, JACGConstants.DB_INSERT_BATCH_SIZE);
            return null;
        }

        String configFilePath = getInputRootPath() + InputDirEnum.IDE_CONFIG.getDirName() + "/" + JACGConstants.FILE_CONFIG;
        try (Reader reader = new InputStreamReader(JACGFileUtil.getFileInputStream(configFilePath), StandardCharsets.UTF_8)) {
            Properties properties = new Properties();
            properties.load(reader);

            String appName = ConfigureWrapper.getConfig(properties, ConfigKeyEnum.CKE_APPNAME);
            if (checkBlank(appName, ConfigKeyEnum.CKE_APPNAME, configFilePath) || !checkAppName(appName)) {
                return null;
            }

            String callGraphJarList = ConfigureWrapper.getConfig(properties, ConfigKeyEnum.CKE_CALL_GRAPH_JAR_LIST);
            if (checkBlank(callGraphJarList, ConfigKeyEnum.CKE_CALL_GRAPH_JAR_LIST, configFilePath)) {
                return null;
            }

            String inputIgnoreOtherPackage = ConfigureWrapper.getConfig(properties, ConfigKeyEnum.CKE_INPUT_IGNORE_OTHER_PACKAGE);
            if (checkBlank(inputIgnoreOtherPackage, ConfigKeyEnum.CKE_INPUT_IGNORE_OTHER_PACKAGE, configFilePath)) {
                return null;
            }

            String genCombinedOutput = ConfigureWrapper.getConfig(properties, ConfigKeyEnum.CKE_GEN_COMBINED_OUTPUT);
            if (checkBlank(genCombinedOutput, ConfigKeyEnum.CKE_GEN_COMBINED_OUTPUT, configFilePath)) {
                return null;
            }

            // 是否需要显示调用者源代码行号
            String showCallerLineNum = ConfigureWrapper.getConfig(properties, ConfigKeyEnum.CKE_SHOW_CALLER_LINE_NUM);
            if (checkBlank(showCallerLineNum, ConfigKeyEnum.CKE_SHOW_CALLER_LINE_NUM, configFilePath)) {
                return null;
            }

            // 在一个调用方法中出现多次的被调用方法（包含自定义数据），是否需要忽略
            String ignoreDupCalleeInOneCaller = ConfigureWrapper.getConfig(properties, ConfigKeyEnum.CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER);
            if (StringUtils.isBlank(ignoreDupCalleeInOneCaller)) {
                // 允许对应配置为空
                ignoreDupCalleeInOneCaller = String.valueOf(false);
            }

            // 生成向下的调用链时，若接口或父类存在多个实现类或子类，接口或父类方法调用多个实现类或子类方法的调用关系是否需要在当前文件中继续生成，否则会在单独的目录中生成
            String multiImplGenInCurrentFile = ConfigureWrapper.getConfig(properties, ConfigKeyEnum.CKE_MULTI_IMPL_GEN_IN_CURRENT_FILE);
            if (StringUtils.isBlank(multiImplGenInCurrentFile)) {
                // 允许对应配置为空
                multiImplGenInCurrentFile = String.valueOf(true);
            }

            // 生成调用链时的详细程度
            String callGraphOutputDetail = ConfigureWrapper.getConfig(properties, ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL);
            if (checkBlank(callGraphOutputDetail, ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, configFilePath)) {
                return null;
            }

            OutputDetailEnum outputDetailEnum = OutputDetailEnum.getFromDetail(callGraphOutputDetail);
            if (OutputDetailEnum.ODE_ILLEGAL == outputDetailEnum) {
                logger.error("参数配置非法，可选值如下 {}", ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL);
                for (OutputDetailEnum outputDetailEnum1 : OutputDetailEnum.values()) {
                    logger.info("{}", outputDetailEnum1.getDetail());
                }
                return null;
            }

            String strThreadNum = ConfigureWrapper.getConfig(properties, ConfigKeyEnum.CKE_THREAD_NUM);
            if (checkBlank(strThreadNum, ConfigKeyEnum.CKE_THREAD_NUM, configFilePath)) {
                return null;
            }
            int threadNum = handleThreadNum(strThreadNum);
            if (threadNum == 0) {
                return null;
            }

            String showMethodAnnotation = ConfigureWrapper.getConfig(properties, ConfigKeyEnum.CKE_SHOW_METHOD_ANNOTATION);
            if (checkBlank(showMethodAnnotation, ConfigKeyEnum.CKE_SHOW_METHOD_ANNOTATION, configFilePath)) {
                return null;
            }

            String strDbUseH2 = ConfigureWrapper.getConfig(properties, ConfigKeyEnum.CKE_DB_USE_H2);
            if (checkBlank(strDbUseH2, ConfigKeyEnum.CKE_DB_USE_H2, configFilePath)) {
                return null;
            }

            CONF_INFO.setDbUseH2(Boolean.parseBoolean(strDbUseH2));
            if (CONF_INFO.isDbUseH2()) {
                logger.info("使用H2数据库");
                if (!handleH2Db(properties, configFilePath)) {
                    return null;
                }
            } else {
                logger.info("使用非H2数据库");
                if (!handleNonH2Db(properties, configFilePath)) {
                    return null;
                }
            }

            CONF_INFO.setAppName(appName);
            CONF_INFO.setCallGraphJarList(callGraphJarList);
            CONF_INFO.setInputIgnoreOtherPackage(Boolean.parseBoolean(inputIgnoreOtherPackage));
            CONF_INFO.setCallGraphOutputDetail(callGraphOutputDetail);
            CONF_INFO.setThreadNum(threadNum);
            CONF_INFO.setOriginalThreadNum(threadNum);
            CONF_INFO.setShowMethodAnnotation(Boolean.parseBoolean(showMethodAnnotation));
            CONF_INFO.setGenCombinedOutput(Boolean.parseBoolean(genCombinedOutput));
            CONF_INFO.setShowCallerLineNum(Boolean.parseBoolean(showCallerLineNum));
            CONF_INFO.setIgnoreDupCalleeInOneCaller(Boolean.parseBoolean(ignoreDupCalleeInOneCaller));
            CONF_INFO.setMultiImplGenInCurrentFile(Boolean.parseBoolean(multiImplGenInCurrentFile));
            if (System.getProperty(JACGConstants.PROPERTY_WRITE_CONFIG_IN_RESULT) != null) {
                CONF_INFO.setWriteConf(true);
            }

            return CONF_INFO;
        } catch (Exception e) {
            logger.error("error {} ", configFilePath, e);
            return null;
        }
    }

    private static boolean checkAppName(String appName) {
        if (!APP_NAME_PATTERN.matcher(appName).matches()) {
            logger.error("{} 属性只支持字母、数字及下划线 {}", ConfigKeyEnum.CKE_APPNAME, appName);
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
            logger.error("非法线程数 {} {}", ConfigKeyEnum.CKE_THREAD_NUM, strThreadNum);
            return 0;
        }

        if (threadNum <= 0) {
            logger.error("线程数过小 {} {}", ConfigKeyEnum.CKE_THREAD_NUM, strThreadNum);
            return 0;
        }
        if (threadNum > JACGConstants.MAX_THREAD_NUM) {
            logger.error("线程数过大 {} {}", ConfigKeyEnum.CKE_THREAD_NUM, strThreadNum);
            return 0;
        }

        return threadNum;
    }

    private static boolean checkBlank(String value, ConfigKeyEnum configKeyEnum, String configFilePath) {
        String key = configKeyEnum.getKey();
        if (StringUtils.isBlank(value)) {
            logger.error("配置文件中未指定参数 {} {}", configFilePath, key);
            return true;
        }

        logger.info("读取到配置信息 [{}] [{}]", key, value);

        return false;
    }

    private static boolean handleH2Db(Properties properties, String configFilePath) {
        String dbH2FilePath = ConfigureWrapper.getConfig(properties, ConfigKeyEnum.CKE_DB_H2_FILE_PATH);
        if (checkBlank(dbH2FilePath, ConfigKeyEnum.CKE_DB_H2_FILE_PATH, configFilePath)) {
            return false;
        }

        if (StringUtils.endsWithIgnoreCase(dbH2FilePath, JACGConstants.H2_FILE_EXT)) {
            logger.error("{} 属性不需要指定H2数据库的后缀 {} {}", ConfigKeyEnum.CKE_DB_H2_FILE_PATH, JACGConstants.H2_FILE_EXT, dbH2FilePath);
            return false;
        }

        CONF_INFO.setDbH2FilePath(dbH2FilePath);

        return true;
    }

    private static boolean handleNonH2Db(Properties properties, String configFilePath) {
        String dbDriverName = ConfigureWrapper.getConfig(properties, ConfigKeyEnum.CKE_DB_DRIVER_NAME);
        if (checkBlank(dbDriverName, ConfigKeyEnum.CKE_DB_DRIVER_NAME, configFilePath)) {
            return false;
        }

        String dbUrl = ConfigureWrapper.getConfig(properties, ConfigKeyEnum.CKE_DB_URL);
        if (checkBlank(dbUrl, ConfigKeyEnum.CKE_DB_URL, configFilePath)) {
            return false;
        }

        String dbUsername = ConfigureWrapper.getConfig(properties, ConfigKeyEnum.CKE_DB_USERNAME);
        if (checkBlank(dbUsername, ConfigKeyEnum.CKE_DB_USERNAME, configFilePath)) {
            return false;
        }

        String dbPassword = ConfigureWrapper.getConfig(properties, ConfigKeyEnum.CKE_DB_PASSWORD);
        if (checkBlank(dbPassword, ConfigKeyEnum.CKE_DB_PASSWORD, configFilePath)) {
            return false;
        }

        CONF_INFO.setDbDriverName(dbDriverName);
        CONF_INFO.setDbUrl(dbUrl);
        CONF_INFO.setDbUsername(dbUsername);
        CONF_INFO.setDbPassword(dbPassword);

        return true;
    }

    /**
     * 获取配置文件根目录
     *
     * @return
     */
    public static String getInputRootPath() {
        return JACGUtil.getDirPathInJvmOptions(JACGConstants.PROPERTY_INPUT_ROOT_PATH);
    }

    private ConfManager() {
        throw new IllegalStateException("illegal");
    }
}

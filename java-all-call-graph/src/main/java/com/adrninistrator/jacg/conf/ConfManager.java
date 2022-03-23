package com.adrninistrator.jacg.conf;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.util.FileUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileInputStream;
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

        String configFilePath = JACGConstants.DIR_CONFIG + File.separator + JACGConstants.FILE_CONFIG;

        try (Reader reader = new InputStreamReader(new FileInputStream(FileUtil.findFile(configFilePath)), StandardCharsets.UTF_8)) {
            Properties properties = new Properties();
            properties.load(reader);

            String appName = properties.getProperty(JACGConstants.KEY_APPNAME);
            if (checkBlank(appName, JACGConstants.KEY_APPNAME, configFilePath) || !checkAppName(appName)) {
                return null;
            }

            String callGraphJarList = properties.getProperty(JACGConstants.KEY_CALL_GRAPH_JAR_LIST);
            if (checkBlank(callGraphJarList, JACGConstants.KEY_CALL_GRAPH_JAR_LIST, configFilePath)) {
                return null;
            }

            String inputIgnoreOtherPackage = properties.getProperty(JACGConstants.KEY_INPUT_IGNORE_OTHER_PACKAGE);
            if (checkBlank(inputIgnoreOtherPackage, JACGConstants.KEY_INPUT_IGNORE_OTHER_PACKAGE, configFilePath)) {
                return null;
            }

            String genCombinedOutput = properties.getProperty(JACGConstants.KEY_GEN_COMBINED_OUTPUT);
            if (checkBlank(genCombinedOutput, JACGConstants.KEY_GEN_COMBINED_OUTPUT, configFilePath)) {
                return null;
            }

            // 是否需要显示调用者源代码行号，首先从JVM参数获取，为空时再从配置文件获取
            String showCallerLineNum = getPropertiesOrder(properties, JACGConstants.KEY_SHOW_CALLER_LINE_NUM);
            if (checkBlank(showCallerLineNum, JACGConstants.KEY_SHOW_CALLER_LINE_NUM, configFilePath)) {
                return null;
            }

            String genUpwardsMethodsFile = properties.getProperty(JACGConstants.KEY_GEN_UPWARDS_METHODS_FILE);
            if (checkBlank(genUpwardsMethodsFile, JACGConstants.KEY_GEN_UPWARDS_METHODS_FILE, configFilePath)) {
                return null;
            }

            // 在一个调用方法中出现多次的被调用方法（包含自定义数据），是否需要忽略，首先从JVM参数获取，为空时再从配置文件获取
            String ignoreDupCalleeInOneCaller = getPropertiesOrder(properties, JACGConstants.KEY_IGNORE_DUP_CALLEE_IN_ONE_CALLER);
            if (StringUtils.isBlank(ignoreDupCalleeInOneCaller)) {
                // 允许对应配置为空
                ignoreDupCalleeInOneCaller = String.valueOf(false);
            }

            // 生成调用链时的详细程度，首先从JVM参数获取，为空时再从配置文件获取
            String callGraphOutputDetail = getPropertiesOrder(properties, JACGConstants.KEY_CALL_GRAPH_OUTPUT_DETAIL);
            if (checkBlank(callGraphOutputDetail, JACGConstants.KEY_CALL_GRAPH_OUTPUT_DETAIL, configFilePath)) {
                return null;
            }

            if (!StringUtils.equalsAny(callGraphOutputDetail, JACGConstants.CONFIG_OUTPUT_DETAIL_1, JACGConstants.CONFIG_OUTPUT_DETAIL_2,
                    JACGConstants.CONFIG_OUTPUT_DETAIL_3)) {
                logger.error("参数配置非法，可选值为 {} {} {} {}", JACGConstants.KEY_CALL_GRAPH_OUTPUT_DETAIL, JACGConstants.CONFIG_OUTPUT_DETAIL_1,
                        JACGConstants.CONFIG_OUTPUT_DETAIL_2, JACGConstants.CONFIG_OUTPUT_DETAIL_3);
                return null;
            }

            String strThreadNum = properties.getProperty(JACGConstants.KEY_THREAD_NUM);
            if (checkBlank(strThreadNum, JACGConstants.KEY_THREAD_NUM, configFilePath)) {
                return null;
            }
            int threadNum = handleThreadNum(strThreadNum);
            if (threadNum == 0) {
                return null;
            }

            String showMethodAnnotation = properties.getProperty(JACGConstants.KEY_SHOW_METHOD_ANNOTATION);
            if (checkBlank(showMethodAnnotation, JACGConstants.KEY_SHOW_METHOD_ANNOTATION, configFilePath)) {
                return null;
            }

            String strDbUseH2 = properties.getProperty(JACGConstants.KEY_DB_USE_H2);
            if (checkBlank(strDbUseH2, JACGConstants.KEY_DB_USE_H2, configFilePath)) {
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
            CONF_INFO.setGenUpwardsMethodsFile(Boolean.parseBoolean(genUpwardsMethodsFile));
            CONF_INFO.setIgnoreDupCalleeInOneCaller(Boolean.parseBoolean(ignoreDupCalleeInOneCaller));
            if (System.getProperty(JACGConstants.PROPERTY_WRITE_CONFIG_IN_RESULT) != null) {
                CONF_INFO.setWriteConf(true);
            }

            return CONF_INFO;
        } catch (Exception e) {
            logger.error("error: ", e);
            return null;
        }
    }

    private static boolean checkAppName(String appName) {
        if (!APP_NAME_PATTERN.matcher(appName).matches()) {
            logger.error("{} 属性只支持字母、数字及下划线 {}", JACGConstants.KEY_APPNAME, appName);
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
            logger.error("非法线程数 {} {}", JACGConstants.KEY_THREAD_NUM, strThreadNum);
            return 0;
        }

        if (threadNum <= 0) {
            logger.error("线程数过小 {} {}", JACGConstants.KEY_THREAD_NUM, strThreadNum);
            return 0;
        }
        if (threadNum > JACGConstants.MAX_THREAD_NUM) {
            logger.error("线程数过大 {} {}", JACGConstants.KEY_THREAD_NUM, strThreadNum);
            return 0;
        }

        return threadNum;
    }

    /**
     * 获取属性，优先通过JVM系统参数获取，再从配置文件获取
     *
     * @param properties
     * @param propKey
     * @return
     */
    private static String getPropertiesOrder(Properties properties, String propKey) {
        String propertiesInJVMArgs = System.getProperty(propKey);
        if (propertiesInJVMArgs != null) {
            return propertiesInJVMArgs;
        }
        return properties.getProperty(propKey);
    }

    private static boolean checkBlank(String value, String key, String configFilePath) {
        if (StringUtils.isBlank(value)) {
            logger.error("配置文件中未指定参数 {} {}", configFilePath, key);
            return true;
        }

        logger.info("读取到配置信息 [{}] [{}]", key, value);

        return false;
    }

    private static boolean handleH2Db(Properties properties, String configFilePath) {
        String dbH2FilePath = properties.getProperty(JACGConstants.KEY_DB_H2_FILE_PATH);
        if (checkBlank(dbH2FilePath, JACGConstants.KEY_DB_H2_FILE_PATH, configFilePath)) {
            return false;
        }

        if (StringUtils.endsWithIgnoreCase(dbH2FilePath, JACGConstants.H2_FILE_EXT)) {
            logger.error("{} 属性不需要指定H2数据库的后缀 {} {}", JACGConstants.KEY_DB_H2_FILE_PATH, JACGConstants.H2_FILE_EXT, dbH2FilePath);
            return false;
        }

        CONF_INFO.setDbH2FilePath(dbH2FilePath);

        return true;
    }

    private static boolean handleNonH2Db(Properties properties, String configFilePath) {
        String dbDriverName = properties.getProperty(JACGConstants.KEY_DB_DRIVER_NAME);
        if (checkBlank(dbDriverName, JACGConstants.KEY_DB_DRIVER_NAME, configFilePath)) {
            return false;
        }

        String dbUrl = properties.getProperty(JACGConstants.KEY_DB_URL);
        if (checkBlank(dbUrl, JACGConstants.KEY_DB_URL, configFilePath)) {
            return false;
        }

        String dbUsername = properties.getProperty(JACGConstants.KEY_DB_USERNAME);
        if (checkBlank(dbUsername, JACGConstants.KEY_DB_USERNAME, configFilePath)) {
            return false;
        }

        String dbPassword = properties.getProperty(JACGConstants.KEY_DB_PASSWORD);
        if (checkBlank(dbPassword, JACGConstants.KEY_DB_PASSWORD, configFilePath)) {
            return false;
        }

        CONF_INFO.setDbDriverName(dbDriverName);
        CONF_INFO.setDbUrl(dbUrl);
        CONF_INFO.setDbUsername(dbUsername);
        CONF_INFO.setDbPassword(dbPassword);

        return true;
    }

    private ConfManager() {
        throw new IllegalStateException("illegal");
    }
}

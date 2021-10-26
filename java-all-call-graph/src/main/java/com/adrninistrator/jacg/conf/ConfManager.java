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

/**
 * @author adrninistrator
 * @date 2021/6/17
 * @description:
 */

public class ConfManager {

    public static final Logger logger = LoggerFactory.getLogger(ConfManager.class);

    private static ConfInfo confInfo = new ConfInfo();

    private static boolean inited = false;

    public static boolean isInited() {
        return inited;
    }

    public static ConfInfo getConfInfo() {
        if (inited) {
            return confInfo;
        }

        inited = true;

        String configFilePath = JACGConstants.DIR_CONFIG + File.separator + JACGConstants.FILE_CONFIG;

        try (Reader reader = new InputStreamReader(new FileInputStream(FileUtil.findFile(configFilePath)), StandardCharsets.UTF_8)) {
            Properties properties = new Properties();
            properties.load(reader);

            String appName = properties.getProperty(JACGConstants.KEY_APPNAME);
            if (checkBlank(appName, JACGConstants.KEY_APPNAME, configFilePath)) {
                return null;
            }

            String callGraphJarList = properties.getProperty(JACGConstants.KEY_CALL_GRAPH_JAR_LIST);
            if (checkBlank(callGraphJarList, JACGConstants.KEY_CALL_GRAPH_JAR_LIST, configFilePath)) {
                return null;
            }

            // 生成的Java方法调用关系文件路径，使用指定的第1个jar包的路径加上“.txt”
            String callGraphInputFile = callGraphJarList.split(JACGConstants.FLAG_SPACE)[0] + JACGConstants.EXT_TXT;

            String inputIgnoreOtherPackage = properties.getProperty(JACGConstants.KEY_INPUT_IGNORE_OTHER_PACKAGE);
            if (checkBlank(inputIgnoreOtherPackage, JACGConstants.KEY_INPUT_IGNORE_OTHER_PACKAGE, configFilePath)) {
                return null;
            }

            String genCombinedOutput = properties.getProperty(JACGConstants.KEY_GEN_COMBINED_OUTPUT);
            if (checkBlank(genCombinedOutput, JACGConstants.KEY_GEN_COMBINED_OUTPUT, configFilePath)) {
                return null;
            }

            String showCallerLineNum = properties.getProperty(JACGConstants.KEY_SHOW_CALLER_LINE_NUM);
            if (checkBlank(showCallerLineNum, JACGConstants.KEY_SHOW_CALLER_LINE_NUM, configFilePath)) {
                return null;
            }

            String genUpwardsMethodsFile = properties.getProperty(JACGConstants.KEY_GEN_UPWARDS_METHODS_FILE);
            if (checkBlank(genUpwardsMethodsFile, JACGConstants.KEY_GEN_UPWARDS_METHODS_FILE, configFilePath)) {
                return null;
            }

            // 生成调用链时的详细程度，首先从JVM参数获取，为空时再从配置文件获取
            String callGraphOutputDetail = System.getProperty(JACGConstants.KEY_CALL_GRAPH_OUTPUT_DETAIL);
            if (callGraphOutputDetail == null) {
                callGraphOutputDetail = properties.getProperty(JACGConstants.KEY_CALL_GRAPH_OUTPUT_DETAIL);
            }
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

            String showMethodAnnotation = properties.getProperty(JACGConstants.KEY_SHOW_METHOD_ANNOTATION);
            if (checkBlank(showMethodAnnotation, JACGConstants.KEY_SHOW_METHOD_ANNOTATION, configFilePath)) {
                return null;
            }

            int threadNum;
            try {
                threadNum = Integer.parseInt(strThreadNum);
            } catch (NumberFormatException e) {
                logger.error("非法线程数 {} {}", JACGConstants.KEY_THREAD_NUM, strThreadNum);
                return null;
            }

            if (threadNum <= 0) {
                logger.error("线程数过小 {} {}", JACGConstants.KEY_THREAD_NUM, strThreadNum);
                return null;
            }
            if (threadNum > JACGConstants.MAX_THREAD_NUM) {
                logger.error("线程数过大 {} {}", JACGConstants.KEY_THREAD_NUM, strThreadNum);
                return null;
            }

            String dbDriverName = properties.getProperty(JACGConstants.KEY_DB_DRIVER_NAME);
            if (checkBlank(dbDriverName, JACGConstants.KEY_DB_DRIVER_NAME, configFilePath)) {
                return null;
            }

            String dbUrl = properties.getProperty(JACGConstants.KEY_DB_URL);
            if (checkBlank(dbUrl, JACGConstants.KEY_DB_URL, configFilePath)) {
                return null;
            }

            String dbUsername = properties.getProperty(JACGConstants.KEY_DB_USERNAME);
            if (checkBlank(dbUsername, JACGConstants.KEY_DB_USERNAME, configFilePath)) {
                return null;
            }

            String dbPassword = properties.getProperty(JACGConstants.KEY_DB_PASSWORD);
            if (checkBlank(dbPassword, JACGConstants.KEY_DB_PASSWORD, configFilePath)) {
                return null;
            }

            confInfo.setAppName(appName);
            confInfo.setCallGraphJarList(callGraphJarList);
            confInfo.setCallGraphInputFile(callGraphInputFile);
            confInfo.setInputIgnoreOtherPackage(Boolean.parseBoolean(inputIgnoreOtherPackage));
            confInfo.setCallGraphOutputDetail(callGraphOutputDetail);
            confInfo.setThreadNum(threadNum);
            confInfo.setShowMethodAnnotation(Boolean.parseBoolean(showMethodAnnotation));
            confInfo.setGenCombinedOutput(Boolean.parseBoolean(genCombinedOutput));
            confInfo.setShowCallerLineNum(Boolean.parseBoolean(showCallerLineNum));
            confInfo.setGenUpwardsMethodsFile(Boolean.parseBoolean(genUpwardsMethodsFile));
            confInfo.setDbDriverName(dbDriverName);
            confInfo.setDbUrl(dbUrl);
            confInfo.setDbUsername(dbUsername);
            confInfo.setDbPassword(dbPassword);
            if (System.getProperty(JACGConstants.PROPERTY_WRITE_CONFIG_IN_RESULT) != null) {
                confInfo.setWriteConf(true);
            }

            return confInfo;
        } catch (Exception e) {
            logger.error("error: ", e);
            return null;
        }
    }

    private static boolean checkBlank(String value, String key, String configFilePath) {
        if (StringUtils.isBlank(value)) {
            logger.error("配置文件中未指定参数 {} {}", configFilePath, key);
            return true;
        }

        logger.info("读取到配置信息 [{}] [{}]", key, value);

        return false;
    }

    private ConfManager() {
        throw new IllegalStateException("illegal");
    }
}

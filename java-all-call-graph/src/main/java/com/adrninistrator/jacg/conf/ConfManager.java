package com.adrninistrator.jacg.conf;

import com.adrninistrator.jacg.common.Constants;
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

    public static ConfInfo getConfInfo() {
        String configFilePath = Constants.DIR_CONFIG + File.separator + Constants.FILE_CONFIG;

        try (Reader reader = new InputStreamReader(new FileInputStream(FileUtil.findFile(configFilePath)), StandardCharsets.UTF_8)) {
            Properties properties = new Properties();
            properties.load(reader);

            String appName = properties.getProperty(Constants.KEY_APPNAME);
            if (checkBlank(appName, Constants.KEY_APPNAME, configFilePath)) {
                return null;
            }

            String callGraphJarList = properties.getProperty(Constants.KEY_CALL_GRAPH_JAR_LIST);
            if (checkBlank(callGraphJarList, Constants.KEY_CALL_GRAPH_JAR_LIST, configFilePath)) {
                return null;
            }

            // 生成的Java方法调用关系文件路径，使用指定的第1个jar包的路径加上“.txt”
            String callGraphInputFile = callGraphJarList.split(Constants.FLAG_SPACE)[0] + Constants.EXT_TXT;

            String inputIgnoreOtherPackage = properties.getProperty(Constants.KEY_INPUT_IGNORE_OTHER_PACKAGE);
            if (checkBlank(inputIgnoreOtherPackage, Constants.KEY_INPUT_IGNORE_OTHER_PACKAGE, configFilePath)) {
                return null;
            }

            String genCombinedOutput = properties.getProperty(Constants.KEY_GEN_COMBINED_OUTPUT);
            if (checkBlank(genCombinedOutput, Constants.KEY_GEN_COMBINED_OUTPUT, configFilePath)) {
                return null;
            }

            String callGraphOutputDetail = properties.getProperty(Constants.KEY_CALL_GRAPH_OUTPUT_DETAIL);
            if (checkBlank(callGraphOutputDetail, Constants.KEY_CALL_GRAPH_OUTPUT_DETAIL, configFilePath)) {
                return null;
            }

            if (!StringUtils.equalsAny(callGraphOutputDetail, Constants.CONFIG_OUTPUT_DETAIL_1, Constants.CONFIG_OUTPUT_DETAIL_2,
                    Constants.CONFIG_OUTPUT_DETAIL_3)) {
                logger.error("参数配置非法，可选值为 {} {} {} {}", Constants.KEY_CALL_GRAPH_OUTPUT_DETAIL, Constants.CONFIG_OUTPUT_DETAIL_1,
                        Constants.CONFIG_OUTPUT_DETAIL_2, Constants.CONFIG_OUTPUT_DETAIL_3);
                return null;
            }

            String strThreadNum = properties.getProperty(Constants.KEY_THREAD_NUM);
            if (checkBlank(strThreadNum, Constants.KEY_THREAD_NUM, configFilePath)) {
                return null;
            }

            String showMethodAnnotation = properties.getProperty(Constants.KEY_SHOW_METHOD_ANNOTATION);
            if (checkBlank(showMethodAnnotation, Constants.KEY_SHOW_METHOD_ANNOTATION, configFilePath)) {
                return null;
            }

            int threadNum;
            try {
                threadNum = Integer.parseInt(strThreadNum);
            } catch (NumberFormatException e) {
                logger.error("非法线程数 {} {}", Constants.KEY_THREAD_NUM, strThreadNum);
                return null;
            }

            if (threadNum <= 0) {
                logger.error("线程数过小 {} {}", Constants.KEY_THREAD_NUM, strThreadNum);
                return null;
            }
            if (threadNum > Constants.MAX_THREAD_NUM) {
                logger.error("线程数过大 {} {}", Constants.KEY_THREAD_NUM, strThreadNum);
                return null;
            }

            String dbDriverName = properties.getProperty(Constants.KEY_DB_DRIVER_NAME);
            if (checkBlank(dbDriverName, Constants.KEY_DB_DRIVER_NAME, configFilePath)) {
                return null;
            }

            String dbUrl = properties.getProperty(Constants.KEY_DB_URL);
            if (checkBlank(dbUrl, Constants.KEY_DB_URL, configFilePath)) {
                return null;
            }

            String dbUsername = properties.getProperty(Constants.KEY_DB_USERNAME);
            if (checkBlank(dbUsername, Constants.KEY_DB_USERNAME, configFilePath)) {
                return null;
            }

            String dbPassword = properties.getProperty(Constants.KEY_DB_PASSWORD);
            if (checkBlank(dbPassword, Constants.KEY_DB_PASSWORD, configFilePath)) {
                return null;
            }

            ConfInfo confInfo = new ConfInfo();
            confInfo.setAppName(appName);
            confInfo.setCallGraphJarList(callGraphJarList);
            confInfo.setCallGraphInputFile(callGraphInputFile);
            confInfo.setInputIgnoreOtherPackage(Boolean.parseBoolean(inputIgnoreOtherPackage));
            confInfo.setCallGraphOutputDetail(callGraphOutputDetail);
            confInfo.setThreadNum(threadNum);
            confInfo.setShowMethodAnnotation(Boolean.parseBoolean(showMethodAnnotation));
            confInfo.setGenCombinedOutput(Boolean.parseBoolean(genCombinedOutput));
            confInfo.setDbDriverName(dbDriverName);
            confInfo.setDbUrl(dbUrl);
            confInfo.setDbUsername(dbUsername);
            confInfo.setDbPassword(dbPassword);

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

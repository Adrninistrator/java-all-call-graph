package com.adrninistrator.jacg.conf;

import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.util.FileUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public class ConfigureWrapper {
    private static final Logger logger = LoggerFactory.getLogger(ConfigureWrapper.class);

    /*
        config.properties配置文件中的参数
        key 参数名
        value 参数值
     */
    private static Map<String, String> CONFIG_MAP = new HashMap<>();

    /*
        其他配置文件中的参数
        key 配置文件名称
        value 配置文件对应的参数Set
     */
    private static Map<String, Set<String>> OTHER_CONFIG_SET_MAP = new HashMap<>();

    /*
        其他配置文件中的参数
        key 配置文件名称
        value 配置文件对应的参数Set
     */
    private static Map<String, List<String>> OTHER_CONFIG_LIST_MAP = new HashMap<>();

    /**
     * 添加config.properties配置文件中的参数
     *
     * @param configKeyEnum
     * @param value
     */
    public static void addConfig(ConfigKeyEnum configKeyEnum, String value) {
        if (value == null) {
            return;
        }

        if (ConfigKeyEnum.CKE_APPNAME == configKeyEnum) {
            // 将app.name参数中的-替换为_
            CONFIG_MAP.put(configKeyEnum.getKey(), value.replace("-", "_"));
            return;
        }

        CONFIG_MAP.put(configKeyEnum.getKey(), value);
    }

    /**
     * 添加其他配置文件中的参数
     *
     * @param otherConfigFileUseSetEnum
     * @param configSet
     */
    public static void addOtherConfigSet(OtherConfigFileUseSetEnum otherConfigFileUseSetEnum, Set<String> configSet) {
        if (configSet == null) {
            return;
        }
        OTHER_CONFIG_SET_MAP.put(otherConfigFileUseSetEnum.getFileName(), configSet);
    }

    /**
     * 添加其他配置文件中的参数
     *
     * @param otherConfigFileUseListEnum
     * @param configList
     */
    public static void addOtherConfigList(OtherConfigFileUseListEnum otherConfigFileUseListEnum, List<String> configList) {
        if (configList == null) {
            return;
        }
        OTHER_CONFIG_LIST_MAP.put(otherConfigFileUseListEnum.getFileName(), configList);
    }

    /**
     * 获取config.properties配置文件中的参数，或通过代码添加的参数
     *
     * @param properties
     * @param configKeyEnum
     * @return
     */
    public static String getConfig(Properties properties, ConfigKeyEnum configKeyEnum) {
        String key = configKeyEnum.getKey();
        // 优先获取通过代码添加的参数
        String value = CONFIG_MAP.get(key);
        if (value != null) {
            logger.info("使用通过代码添加的参数 {}", key);
            return value;
        }

        if (properties == null) {
            return null;
        }

        // 获取config.properties配置文件中的参数
        return properties.getProperty(key);
    }

    /**
     * 获取其他配置文件中的参数，或通过代码添加的参数
     *
     * @param otherConfigFileUseSetEnum
     * @return
     */
    public static Set<String> getOtherConfigSet(OtherConfigFileUseSetEnum otherConfigFileUseSetEnum) {
        String configFileName = otherConfigFileUseSetEnum.getFileName();
        // 优先获取通过代码添加的参数
        Set<String> configSet = OTHER_CONFIG_SET_MAP.get(configFileName);
        if (configSet != null) {
            logger.info("使用通过代码添加的参数 {}", configFileName);
            return configSet;
        }

        // 获取其他配置文件中的参数
        return FileUtil.readFile2Set(ConfManager.getInputRootPath() + configFileName);
    }

    /**
     * 获取其他配置文件中的参数，或通过代码添加的参数
     *
     * @param otherConfigFileUseListEnum
     * @return
     */
    public static List<String> getOtherConfigList(OtherConfigFileUseListEnum otherConfigFileUseListEnum) {
        String configFileName = otherConfigFileUseListEnum.getFileName();
        // 优先获取通过代码添加的参数
        List<String> configList = OTHER_CONFIG_LIST_MAP.get(configFileName);
        if (configList != null) {
            logger.info("使用通过代码添加的参数 {}", configFileName);
            return configList;
        }

        // 获取其他配置文件中的参数
        return FileUtil.readFile2List(ConfManager.getInputRootPath() + configFileName);
    }

    private ConfigureWrapper() {
        throw new IllegalStateException("illegal");
    }
}

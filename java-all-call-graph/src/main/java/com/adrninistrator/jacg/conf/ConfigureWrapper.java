package com.adrninistrator.jacg.conf;

import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.util.JACGFileUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description:
 */
public class ConfigureWrapper {
    private static final Logger logger = LoggerFactory.getLogger(ConfigureWrapper.class);

    /*
        主要配置文件中的参数
        key 参数名
        value 参数值
     */
    private Map<String, String> configMap = new HashMap<>();

    /*
        其他配置文件中的参数
        key 配置文件名称
        value 配置文件对应的参数Set
     */
    private Map<String, Set<String>> otherConfigSetMap = new HashMap<>();

    /*
        其他配置文件中的参数
        key 配置文件名称
        value 配置文件对应的参数List
     */
    private Map<String, List<String>> otherConfigListMap = new HashMap<>();

    /**
     * 添加配置文件中的参数
     *
     * @param configKeyEnum
     * @param value
     */
    public void addConfig(ConfigKeyEnum configKeyEnum, String value) {
        if (value == null) {
            return;
        }

        if (ConfigKeyEnum.CKE_APP_NAME == configKeyEnum) {
            // 将app.name参数中的-替换为_
            configMap.put(configKeyEnum.getKey(), value.replace("-", "_"));
            return;
        }

        configMap.put(configKeyEnum.getKey(), value);
    }

    /**
     * 添加其他配置文件中的参数
     *
     * @param otherConfigFileUseSetEnum
     * @param configSet
     */
    public void addOtherConfigSet(OtherConfigFileUseSetEnum otherConfigFileUseSetEnum, Set<String> configSet) {
        if (configSet == null) {
            return;
        }
        otherConfigSetMap.put(otherConfigFileUseSetEnum.getFileName(), configSet);
    }

    /**
     * 添加其他配置文件中的参数
     *
     * @param otherConfigFileUseListEnum
     * @param configList
     */
    public void addOtherConfigList(OtherConfigFileUseListEnum otherConfigFileUseListEnum, List<String> configList) {
        if (configList == null) {
            return;
        }
        otherConfigListMap.put(otherConfigFileUseListEnum.getFileName(), configList);
    }

    /**
     * 获取配置文件中的参数，或通过代码添加的参数
     *
     * @param properties
     * @param configKeyEnum
     * @return
     */
    public String getConfig(Properties properties, ConfigKeyEnum configKeyEnum) {
        String key = configKeyEnum.getKey();
        // 优先获取通过代码添加的参数
        String value = configMap.get(key);
        if (value != null) {
            logger.info("使用通过代码添加的参数 {} {}", key, value);
            return value;
        }

        if (properties == null) {
            return null;
        }

        // 获取配置文件中的参数
        return properties.getProperty(key);
    }

    /**
     * 获取其他配置文件中的参数，或通过代码添加的参数
     *
     * @param otherConfigFileUseSetEnum
     * @return
     */
    public Set<String> getOtherConfigSet(OtherConfigFileUseSetEnum otherConfigFileUseSetEnum) {
        String configFileName = otherConfigFileUseSetEnum.getFileName();
        // 优先获取通过代码添加的参数
        Set<String> configSet = otherConfigSetMap.get(configFileName);
        if (configSet != null) {
            logger.info("使用通过代码添加的参数 {}", configFileName);
            return configSet;
        }

        // 获取其他配置文件中的参数
        return JACGFileUtil.readFile2Set(ConfManager.getInputRootPath() + configFileName);
    }

    /**
     * 获取其他配置文件中的参数，或通过代码添加的参数
     *
     * @param otherConfigFileUseListEnum
     * @return
     */
    public List<String> getOtherConfigList(OtherConfigFileUseListEnum otherConfigFileUseListEnum) {
        String configFileName = otherConfigFileUseListEnum.getFileName();
        // 优先获取通过代码添加的参数
        List<String> configList = otherConfigListMap.get(configFileName);
        if (configList != null) {
            logger.info("使用通过代码添加的参数 {}", configFileName);
            return configList;
        }

        // 获取其他配置文件中的参数
        return JACGFileUtil.readFile2List(ConfManager.getInputRootPath() + configFileName);
    }

    /**
     * 拷贝数据
     *
     * @return
     */
    public ConfigureWrapper copy() {
        ConfigureWrapper clone = new ConfigureWrapper();
        clone.configMap = new HashMap<>(this.configMap);

        clone.otherConfigSetMap = new HashMap<>();
        for (Map.Entry<String, Set<String>> entry : this.otherConfigSetMap.entrySet()) {
            clone.otherConfigSetMap.put(entry.getKey(), new HashSet<>(entry.getValue()));
        }

        clone.otherConfigListMap = new HashMap<>();
        for (Map.Entry<String, List<String>> entry : this.otherConfigListMap.entrySet()) {
            clone.otherConfigListMap.put(entry.getKey(), new ArrayList<>(entry.getValue()));
        }

        return clone;
    }
}

package com.adrninistrator.jacg.conf;

import com.adrninistrator.jacg.annotation.formatter.DefaultAnnotationFormatter;
import com.adrninistrator.jacg.annotation.formatter.SpringMvcRequestMappingFormatter;
import com.adrninistrator.jacg.annotation.formatter.SpringTransactionalFormatter;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.common.enums.interfaces.BaseConfigInterface;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description: 配置包装类
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
     * 设置配置文件中指定key的参数，清空指定key已有的参数
     *
     * @param baseConfig
     * @param value
     */
    public void setConfig(BaseConfigInterface baseConfig, String value) {
        if (value == null) {
            return;
        }

        if (ConfigKeyEnum.CKE_APP_NAME == baseConfig) {
            // 将app.name参数中的-替换为_
            configMap.put(baseConfig.getKey(), value.replace("-", "_"));
            return;
        }

        configMap.put(baseConfig.getKey(), value);
    }

    /**
     * 设置其他配置文件中指定key的参数，Set格式，清空指定key已有的参数
     *
     * @param otherConfigFileUseSetEnum
     * @param data
     */
    public void setOtherConfigSet(OtherConfigFileUseSetEnum otherConfigFileUseSetEnum, String... data) {
        setOtherConfigSet(otherConfigFileUseSetEnum, JACGUtil.genSetFromArray(data));
    }

    /**
     * 设置其他配置文件中指定key的参数，Set格式，清空指定key已有的参数
     *
     * @param otherConfigFileUseSetEnum
     * @param configSet
     */
    public void setOtherConfigSet(OtherConfigFileUseSetEnum otherConfigFileUseSetEnum, Set<String> configSet) {
        if (configSet == null) {
            throw new JavaCGRuntimeException("不允许传入null，只能传入内容为空的Set");
        }
        otherConfigSetMap.put(otherConfigFileUseSetEnum.getKey(), configSet);
    }

    /**
     * 添加其他配置文件中指定key的参数，Set格式，保留指定key已有的参数
     *
     * @param otherConfigFileUseSetEnum
     * @param data
     */
    public void addOtherConfigSet(OtherConfigFileUseSetEnum otherConfigFileUseSetEnum, String... data) {
        addOtherConfigSet(otherConfigFileUseSetEnum, JACGUtil.genSetFromArray(data));
    }

    /**
     * 添加其他配置文件中指定key的参数，Set格式，保留指定key已有的参数
     *
     * @param otherConfigFileUseSetEnum
     * @param configSet
     */
    public void addOtherConfigSet(OtherConfigFileUseSetEnum otherConfigFileUseSetEnum, Set<String> configSet) {
        if (configSet == null) {
            throw new JavaCGRuntimeException("不允许传入null，只能传入内容为空的Set");
        }
        // 原始的Set可能不允许修改，因此新创建一个
        Set<String> existedSet = otherConfigSetMap.get(otherConfigFileUseSetEnum.getKey());
        Set<String> newSet = new HashSet<>();
        if (existedSet != null) {
            newSet.addAll(existedSet);
        }
        newSet.addAll(configSet);
        otherConfigSetMap.put(otherConfigFileUseSetEnum.getKey(), newSet);
    }

    /**
     * 清空其他配置文件中指定key的参数，Set格式，避免jar包或项目中的配置文件有值时对生成结果产生干扰
     *
     * @param otherConfigFileUseSetEnums
     */
    public void clearOtherConfigSet(OtherConfigFileUseSetEnum... otherConfigFileUseSetEnums) {
        if (otherConfigFileUseSetEnums == null) {
            throw new JavaCGRuntimeException("传入的参数为空");
        }
        for (OtherConfigFileUseSetEnum otherConfigFileUseSetEnum : otherConfigFileUseSetEnums) {
            otherConfigSetMap.put(otherConfigFileUseSetEnum.getKey(), Collections.emptySet());
        }
    }

    /**
     * 设置其他配置文件中指定key的参数，List格式，清空指定key已有的参数
     *
     * @param otherConfigFileUseListEnum
     * @param data
     */
    public void setOtherConfigList(OtherConfigFileUseListEnum otherConfigFileUseListEnum, String... data) {
        setOtherConfigList(otherConfigFileUseListEnum, JACGUtil.genListFromArray(data));
    }

    /**
     * 设置其他配置文件中指定key的参数，List格式，清空指定key已有的参数
     *
     * @param otherConfigFileUseListEnum
     * @param configList
     */
    public void setOtherConfigList(OtherConfigFileUseListEnum otherConfigFileUseListEnum, List<String> configList) {
        if (configList == null) {
            throw new JavaCGRuntimeException("不允许传入null，只能传入内容为空的List");
        }
        otherConfigListMap.put(otherConfigFileUseListEnum.getKey(), configList);
    }

    /**
     * 添加其他配置文件中指定key的参数，List格式，保留指定key已有的参数
     *
     * @param otherConfigFileUseListEnum
     * @param data
     */
    public void addOtherConfigList(OtherConfigFileUseListEnum otherConfigFileUseListEnum, String... data) {
        addOtherConfigList(otherConfigFileUseListEnum, JACGUtil.genListFromArray(data));
    }

    /**
     * 添加其他配置文件中指定key的参数，List格式，保留指定key已有的参数
     *
     * @param otherConfigFileUseListEnum
     * @param configList
     */
    public void addOtherConfigList(OtherConfigFileUseListEnum otherConfigFileUseListEnum, List<String> configList) {
        if (configList == null) {
            throw new JavaCGRuntimeException("不允许传入null，只能传入内容为空的List");
        }
        // 原始的List可能不允许修改，因此新创建一个
        List<String> existedList = otherConfigListMap.get(otherConfigFileUseListEnum.getKey());
        List<String> newList = new ArrayList<>();
        if (existedList != null) {
            JACGUtil.addList2List(existedList, newList);
        }
        JACGUtil.addList2List(configList, newList);
        otherConfigListMap.put(otherConfigFileUseListEnum.getKey(), newList);
    }

    /**
     * 清空其他配置文件中指定key的参数，List格式，避免jar包或项目中的配置文件有值时对生成结果产生干扰
     *
     * @param otherConfigFileUseListEnums
     */
    public void clearOtherConfigList(OtherConfigFileUseListEnum... otherConfigFileUseListEnums) {
        if (otherConfigFileUseListEnums == null) {
            throw new JavaCGRuntimeException("传入的参数为空");
        }
        for (OtherConfigFileUseListEnum otherConfigFileUseListEnum : otherConfigFileUseListEnums) {
            otherConfigListMap.put(otherConfigFileUseListEnum.getKey(), Collections.emptyList());
        }
    }

    /**
     * 获取配置文件中的参数，或通过代码添加的参数
     *
     * @param properties
     * @param baseConfig
     * @param printLog
     * @return
     */
    public String getConfig(Properties properties, BaseConfigInterface baseConfig, boolean printLog) {
        String key = baseConfig.getKey();
        // 优先获取通过代码添加的参数
        String value = configMap.get(key);
        if (value != null) {
            if (printLog) {
                logger.info("使用通过代码指定的参数 [{}] [{}]", key, value);
            }
            return value;
        }

        if (properties == null) {
            return null;
        }

        // 获取配置文件中的参数
        value = properties.getProperty(key);
        if (printLog) {
            logger.info("使用配置文件中的参数 {} {}", key, value);
        }
        // 将配置文件中的参数记录到内存中，用于后续显示
        configMap.put(key, value);
        return value;
    }

    /**
     * 获取其他配置文件中的参数，或通过代码添加的参数
     *
     * @param otherConfigFileUseSetEnum
     * @param printLog
     * @return
     */
    public Set<String> getOtherConfigSet(OtherConfigFileUseSetEnum otherConfigFileUseSetEnum, boolean printLog) {
        String configFileName = otherConfigFileUseSetEnum.getKey();
        // 优先获取通过代码添加的参数
        Set<String> configSet = otherConfigSetMap.get(configFileName);
        if (configSet != null) {
            if (printLog) {
                logger.info("使用通过代码指定的参数 {}\n{}", configFileName, StringUtils.join(new ArrayList<>(configSet), " "));
            }
            return configSet;
        }

        // 获取其他配置文件中的参数
        configSet = JACGFileUtil.readFile2Set(ConfManager.getInputRootPath() + configFileName);
        if (printLog) {
            logger.info("使用配置文件中的参数 {}\n{}", configFileName, StringUtils.join(new ArrayList<>(configSet), " "));
        }
        // 将配置文件中的参数记录到内存中，用于后续显示
        otherConfigSetMap.put(configFileName, configSet);
        return configSet;
    }

    /**
     * 获取其他配置文件中的参数，或通过代码添加的参数
     *
     * @param otherConfigFileUseListEnum 参数key枚举
     * @param printLog                   是否打印日志
     * @return
     */
    public List<String> getOtherConfigList(OtherConfigFileUseListEnum otherConfigFileUseListEnum, boolean printLog) {
        String configFileName = otherConfigFileUseListEnum.getKey();
        // 优先获取通过代码添加的参数
        List<String> configList = otherConfigListMap.get(configFileName);
        if (configList != null) {
            if (printLog) {
                logger.info("使用通过代码指定的参数 {}\n{}", configFileName, StringUtils.join(configList, " "));
            }
            return configList;
        }

        // 获取其他配置文件中的参数
        configList = JACGFileUtil.readFile2List(ConfManager.getInputRootPath() + configFileName);
        if (printLog) {
            logger.info("使用配置文件中的参数 {}\n{}", configFileName, StringUtils.join(configList, " "));
        }
        // 将配置文件中的参数记录到内存中，用于后续显示
        otherConfigListMap.put(configFileName, configList);
        return configList;
    }

    /**
     * 清空全部参数，在设置参数之前执行，避免jar包或项目中的配置文件有值时对生成结果产生干扰
     */
    public void clearAllConfig() {
        for (ConfigKeyEnum configKeyEnum : ConfigKeyEnum.values()) {
            configMap.put(configKeyEnum.getKey(), "");
        }
        for (OtherConfigFileUseSetEnum otherConfigFileUseSetEnum : OtherConfigFileUseSetEnum.values()) {
            clearOtherConfigSet(otherConfigFileUseSetEnum);
        }
        for (OtherConfigFileUseListEnum otherConfigFileUseListEnum : OtherConfigFileUseListEnum.values()) {
            clearOtherConfigList(otherConfigFileUseListEnum);
        }
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

    /**
     * 添加所有预置的扩展类
     */
    public void addAllPreBuildExtensions() {
        addOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_METHOD_ANNOTATION_FORMATTER,
                SpringMvcRequestMappingFormatter.class.getName(),
                SpringTransactionalFormatter.class.getName(),
                DefaultAnnotationFormatter.class.getName()
        );
    }

    /**
     * 添加需要处理的类名前缀
     *
     * @param allowedClassNamePrefixes
     */
    public void addAllowedClassNamePrefixes(String... allowedClassNamePrefixes) {
        Set<String> allowedClassSet = getOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_ALLOWED_CLASS_PREFIX, false);
        if (allowedClassSet.isEmpty()) {
            // 未指定需要处理的类名前缀，不需要添加
            return;
        }

        // 有指定需要处理的类名前缀，检查是否有包含指定的类
        for (String allowedClassNamePrefix : allowedClassNamePrefixes) {
            boolean findSpTransactionTemplate = false;
            for (String allowedClass : allowedClassSet) {
                if (StringUtils.startsWith(allowedClassNamePrefix, allowedClass)) {
                    findSpTransactionTemplate = true;
                    break;
                }
            }

            if (!findSpTransactionTemplate) {
                // 在需要处理的类名前缀中增加Spring事务模板类
                logger.info("在需要处理的类名前缀中增加 {}", allowedClassNamePrefix);
                allowedClassSet.add(allowedClassNamePrefix);
            }
        }
    }

    /**
     * 设置允许处理所有的类
     */
    public void setAllowAllClasses() {
        setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_ALLOWED_CLASS_PREFIX, new HashSet<>());
    }
}

package com.adrninistrator.jacg.conf;

import com.adrninistrator.jacg.annotation.formatter.DefaultAnnotationFormatter;
import com.adrninistrator.jacg.annotation.formatter.SpringMvcRequestMappingFormatter;
import com.adrninistrator.jacg.annotation.formatter.SpringTransactionalFormatter;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.common.enums.interfaces.ConfigInterface;
import com.adrninistrator.jacg.common.enums.interfaces.MainConfigInterface;
import com.adrninistrator.jacg.markdown.writer.MarkdownWriter;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.enums.JavaCGConfigKeyEnum;
import com.adrninistrator.javacg.common.enums.JavaCGOtherConfigFileUseListEnum;
import com.adrninistrator.javacg.common.enums.JavaCGOtherConfigFileUseSetEnum;
import com.adrninistrator.javacg.conf.JavaCGConfigureWrapper;
import com.adrninistrator.javacg.exceptions.JavaCGError;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description: 配置包装类
 */
public class ConfigureWrapper {
    private static final Logger logger = LoggerFactory.getLogger(ConfigureWrapper.class);

    private static final Pattern APP_NAME_PATTERN = Pattern.compile("[A-Za-z0-9_]*");

    /*
        从配置文件中读取的内容
        key     文件名
        value   properties对象
     */
    private Map<String, Properties> propertiesMap = new HashMap<>();

    /*
        记录有被使用的主要配置
        key     文件名
        value   配置参数key
     */
    private Map<String, Set<String>> usedMainConfigMap = new HashMap<>();

    /*
        记录有被使用的其他List格式配置
        元素  配置文件名（对应OtherConfigFileUseListEnum.getKey()）
     */
    private Set<String> usedOtherListConfigSet = new HashSet<>();

    /*
        记录有被使用的其他List格式配置
        元素  配置文件名（对应OtherConfigFileUseSetEnum.getKey()）
     */
    private Set<String> usedOtherSetConfigSet = new HashSet<>();

    /*
        主要配置文件中的参数
        key 参数名
        value 参数值
     */
    private Map<String, Object> mainConfigMap = new HashMap<>();

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

    // 记录入口简单类名列表
    private List<String> entrySimpleClassNameList = new ArrayList<>();

    /**
     * 默认构造函数，忽略配置文件中的参数
     */
    public ConfigureWrapper() {
        this(true);
    }

    /**
     * @param useDefaultEmptyConfigFlag true: 使用默认的空参数（忽略配置文件中的参数） false: 使用配置文件中的参数
     */
    public ConfigureWrapper(boolean useDefaultEmptyConfigFlag) {
        if (useDefaultEmptyConfigFlag) {
            // 使用默认的空参数（忽略配置文件中的参数）
            useDefaultEmptyConfig();
        }
    }

    // 检查是否在工作线程中执行
    private void checkInWorkThread() {
        if (Thread.currentThread().getName().startsWith(JACGConstants.THREAD_NAME_PREFIX_WORKER)) {
            logger.error("获取配置的操作不应该在工作线程中执行");
            throw new JavaCGError("获取配置的操作不应该在工作线程中执行");
        }
    }

    // 记录有被使用的主要配置
    private void recordUsedMainConfig(MainConfigInterface mainConfig) {
        // 检查是否在工作线程中执行
        checkInWorkThread();
        Set<String> usedMainConfigSet = usedMainConfigMap.computeIfAbsent(mainConfig.getFileName(), k -> new HashSet<>());
        usedMainConfigSet.add(mainConfig.getKey());
    }

    // 记录有被使用的其他List格式配置
    private void recordUsedOtherListConfig(ConfigInterface config) {
        // 检查是否在工作线程中执行
        checkInWorkThread();
        usedOtherListConfigSet.add(config.getKey());
    }

    // 记录有被使用的其他Set格式配置
    private void recordUsedOtherSetConfig(ConfigInterface config) {
        // 检查是否在工作线程中执行
        checkInWorkThread();
        usedOtherSetConfigSet.add(config.getKey());
    }

    /**
     * 设置主要配置文件中指定key的参数，清空指定key已有的参数
     *
     * @param mainConfig
     * @param strValue
     */
    public Object setMainConfig(MainConfigInterface mainConfig, String strValue) {
        if (strValue == null) {
            throw new JavaCGError("配置参数不允许为null");
        }
        // 生成并检查主要配置参数值
        Object value = genMainConfigValue(mainConfig, strValue);
        if (value == null) {
            throw new JavaCGError("配置参数非法");
        }

        if (!mainConfig.getType().isAssignableFrom(value.getClass())) {
            logger.error("生成的参数值类型与预期的不一致\n{} {}\n{} {}", mainConfig.getFileName(), mainConfig.getKey(), value.getClass().getName(), mainConfig.getType().getName());
            return null;
        }

        mainConfigMap.put(mainConfig.getKey(), value);
        return value;
    }

    /**
     * 设置其他配置文件中指定key的参数，Set格式，清空指定key已有的参数
     *
     * @param otherConfigFileUseSetEnum
     * @param data                      若未指定则清空参数
     */
    public void setOtherConfigSet(OtherConfigFileUseSetEnum otherConfigFileUseSetEnum, String... data) {
        setOtherConfigSet(otherConfigFileUseSetEnum, JavaCGUtil.genSetFromArray(data));
    }

    /**
     * 设置其他配置文件中指定key的参数，Set格式，清空指定key已有的参数
     *
     * @param otherConfigFileUseSetEnum
     * @param configSet
     */
    public void setOtherConfigSet(OtherConfigFileUseSetEnum otherConfigFileUseSetEnum, Set<String> configSet) {
        if (configSet == null) {
            throw new JavaCGError("不允许传入null，只能传入内容为空的Set");
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
        addOtherConfigSet(otherConfigFileUseSetEnum, JavaCGUtil.genSetFromArray(data));
    }

    /**
     * 添加其他配置文件中指定key的参数，Set格式，保留指定key已有的参数
     *
     * @param otherConfigFileUseSetEnum
     * @param configSet
     */
    public void addOtherConfigSet(OtherConfigFileUseSetEnum otherConfigFileUseSetEnum, Set<String> configSet) {
        if (configSet == null) {
            throw new JavaCGError("不允许传入null，只能传入内容为空的Set");
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
     * 设置其他配置文件中指定key的参数，List格式，清空指定key已有的参数
     *
     * @param otherConfigFileUseListEnum
     * @param data                       若未指定则清空参数
     */
    public void setOtherConfigList(OtherConfigFileUseListEnum otherConfigFileUseListEnum, String... data) {
        setOtherConfigList(otherConfigFileUseListEnum, JavaCGUtil.genListFromArray(data));
    }

    /**
     * 设置其他配置文件中指定key的参数，List格式，清空指定key已有的参数
     *
     * @param otherConfigFileUseListEnum
     * @param configList
     */
    public void setOtherConfigList(OtherConfigFileUseListEnum otherConfigFileUseListEnum, List<String> configList) {
        if (configList == null) {
            throw new JavaCGError("不允许传入null，只能传入内容为空的List");
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
        addOtherConfigList(otherConfigFileUseListEnum, JavaCGUtil.genListFromArray(data));
    }

    /**
     * 添加其他配置文件中指定key的参数，List格式，保留指定key已有的参数
     *
     * @param otherConfigFileUseListEnum
     * @param configList
     */
    public void addOtherConfigList(OtherConfigFileUseListEnum otherConfigFileUseListEnum, List<String> configList) {
        if (configList == null) {
            throw new JavaCGError("不允许传入null，只能传入内容为空的List");
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
     * 获取主要配置文件中的参数，或通过代码添加的参数，实际使用配置参数
     *
     * @param mainConfig
     * @return
     */
    @SuppressWarnings("unchecked")
    public <T> T getMainConfig(MainConfigInterface mainConfig) {
        return getMainConfig(mainConfig, true);
    }

    /**
     * 获取主要配置文件中的参数，或通过代码添加的参数
     *
     * @param mainConfig
     * @param useConfig  true: 实际使用配置参数 false: 打印配置参数
     * @return
     */
    @SuppressWarnings("unchecked")
    public <T> T getMainConfig(MainConfigInterface mainConfig, boolean useConfig) {
        String key = mainConfig.getKey();
        // 优先获取通过代码添加的参数
        Object value = mainConfigMap.get(key);
        if (value != null) {
            if (useConfig) {
                // 记录有被使用的主要配置
                recordUsedMainConfig(mainConfig);
            }
            return (T) value;
        }

        // 读取配置文件
        String configFileName = mainConfig.getFileName();
        Properties properties = propertiesMap.get(configFileName);
        if (properties == null) {
            try (BufferedReader reader = JavaCGFileUtil.genBufferedReader(JACGFileUtil.getFileInputStream(JACGUtil.getInputRootPath() + configFileName))) {
                properties = new Properties();
                properties.load(reader);
                propertiesMap.put(configFileName, properties);
            } catch (Exception e) {
                logger.error("error ", e);
                throw new JavaCGError("读取配置文件出错: " + configFileName);
            }
        }

        // 获取配置文件中的参数
        String strValue = properties.getProperty(key);
        if (strValue == null) {
            if (useConfig) {
                logger.error("需要使用的配置参数未指定 {} {}", mainConfig.getFileName(), mainConfig.getKey());
                throw new JavaCGError("需要使用的配置参数未指定: " + mainConfig.getFileName() + " " + mainConfig.getKey());
            }
            return null;
        }
        if (useConfig) {
            // 记录有被使用的主要配置
            recordUsedMainConfig(mainConfig);
        }
        // 设置参数值
        return (T) setMainConfig(mainConfig, strValue);
    }

    /**
     * 获取其他配置文件中的参数，或通过代码添加的参数
     *
     * @param otherConfigFileUseSetEnum
     * @param useConfig                 true: 实际使用配置参数 false: 打印配置参数
     * @return
     */
    public Set<String> getOtherConfigSet(OtherConfigFileUseSetEnum otherConfigFileUseSetEnum, boolean useConfig) {
        String configFileName = otherConfigFileUseSetEnum.getKey();
        // 优先获取通过代码添加的参数
        Set<String> configSet = otherConfigSetMap.get(configFileName);
        if (configSet != null) {
            if (useConfig) {
                // 记录有被使用的其他配置
                recordUsedOtherSetConfig(otherConfigFileUseSetEnum);
            }
            return configSet;
        }

        // 获取其他配置文件中的参数
        configSet = JACGFileUtil.readFile2Set(JACGUtil.getInputRootPath() + configFileName);
        // 将配置文件中的参数记录到内存中，用于后续使用
        otherConfigSetMap.put(configFileName, configSet);
        if (useConfig) {
            // 记录有被使用的其他配置
            recordUsedOtherSetConfig(otherConfigFileUseSetEnum);
        }
        return configSet;
    }

    /**
     * 获取其他配置文件中的参数，或通过代码添加的参数
     *
     * @param otherConfigFileUseListEnum 参数key枚举
     * @param useConfig                  true: 实际使用配置参数 false: 打印配置参数
     * @return
     */
    public List<String> getOtherConfigList(OtherConfigFileUseListEnum otherConfigFileUseListEnum, boolean useConfig) {
        String configFileName = otherConfigFileUseListEnum.getKey();
        // 优先获取通过代码添加的参数
        List<String> configList = otherConfigListMap.get(configFileName);
        if (configList != null) {
            if (useConfig) {
                // 记录有被使用的其他配置
                recordUsedOtherListConfig(otherConfigFileUseListEnum);
            }
            return configList;
        }

        // 获取其他配置文件中的参数
        configList = JACGFileUtil.readFile2List(JACGUtil.getInputRootPath() + configFileName);
        // 将配置文件中的参数记录到内存中，用于后续使用
        otherConfigListMap.put(configFileName, configList);
        if (useConfig) {
            // 记录有被使用的其他配置
            recordUsedOtherListConfig(otherConfigFileUseListEnum);
        }
        return configList;
    }

    // 生成并检查主要配置参数值
    private Object genMainConfigValue(MainConfigInterface mainConfig, String strValue) {
        if (ConfigKeyEnum.CKE_APP_NAME == mainConfig) {
            return handleAppName(strValue);
        }

        if (ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL == mainConfig) {
            return handleOutputDetail(strValue);
        }

        if (ConfigKeyEnum.CKE_OUTPUT_ROOT_PATH == mainConfig && StringUtils.isBlank(strValue)) {
            // 当前参数允许为空，默认为""
            return "";
        }

        if (ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH == mainConfig) {
            return handleDbH2FilePath(strValue);
        }

        if (ConfigKeyEnum.CKE_IGNORE_DUP_CALLEE_IN_ONE_CALLER == mainConfig && StringUtils.isBlank(strValue)) {
            // 当前参数允许为空，默认为false
            return Boolean.FALSE;
        }

        if (ConfigKeyEnum.CKE_CHECK_JAR_FILE_UPDATED == mainConfig && StringUtils.isBlank(strValue)) {
            // 当前参数允许为空，默认为false
            return Boolean.FALSE;
        }

        if (ConfigKeyEnum.CKE_THREAD_NUM == mainConfig) {
            // 处理线程数
            return handleThreadNum(strValue);
        }

        if (ConfigKeyEnum.CKE_DB_INSERT_BATCH_SIZE == mainConfig) {
            // 处理批量写入数据库时每次插入的数量
            return handleBatchInsertSize(strValue);
        }

        if (String.class == mainConfig.getType()) {
            return strValue;
        }
        if (Boolean.class == mainConfig.getType()) {
            return Boolean.valueOf(strValue);
        }
        if (Integer.class == mainConfig.getType()) {
            return Integer.valueOf(strValue);
        }

        // 不可能执行到这里
        return null;
    }

    // 处理数据库里的表名后缀
    private String handleAppName(String appName) {
        if (!APP_NAME_PATTERN.matcher(appName).matches()) {
            logger.error("属性只支持字母、数字及下划线\n{} {} {}", ConfigKeyEnum.CKE_APP_NAME.getFileName(), ConfigKeyEnum.CKE_APP_NAME.getKey(), appName);
            return null;
        }
        // 将app.name参数中的-替换为_
        return appName.replace("-", "_");
    }

    // 处理线程数
    private Integer handleThreadNum(String strThreadNum) {
        int threadNum;
        try {
            threadNum = Integer.parseInt(strThreadNum);
        } catch (NumberFormatException e) {
            logger.error("非法线程数 {} {} {}", ConfigKeyEnum.CKE_THREAD_NUM.getFileName(), ConfigKeyEnum.CKE_THREAD_NUM.getKey(), strThreadNum);
            return null;
        }
        if (threadNum <= 0 || threadNum > JACGConstants.MAX_THREAD_NUM) {
            logger.error("参数配置非法\n{} {}\n应在以下范围: (0,{}]", ConfigKeyEnum.CKE_THREAD_NUM.getFileName(), ConfigKeyEnum.CKE_THREAD_NUM.getKey(), JACGConstants.MAX_THREAD_NUM);
            return null;
        }
        return threadNum;
    }

    // 处理批量写入数据库时每次插入的数量
    private Integer handleBatchInsertSize(String strDbBatchInsertSize) {
        int dbInsertBatchSize;
        try {
            dbInsertBatchSize = Integer.parseInt(strDbBatchInsertSize);
        } catch (NumberFormatException e) {
            logger.error("批量写入数据库时每次插入的数量非法 {} {} {}", ConfigKeyEnum.CKE_DB_INSERT_BATCH_SIZE.getFileName(), ConfigKeyEnum.CKE_DB_INSERT_BATCH_SIZE.getKey(), strDbBatchInsertSize);
            return null;
        }

        if (dbInsertBatchSize <= 0 || dbInsertBatchSize > JACGConstants.MAX_DB_INSERT_BATCH_SIZE) {
            logger.error("参数配置非法\n{} {}\n应在以下范围: (0,{}]", ConfigKeyEnum.CKE_DB_INSERT_BATCH_SIZE.getFileName(), ConfigKeyEnum.CKE_DB_INSERT_BATCH_SIZE.getKey(),
                    JACGConstants.MAX_DB_INSERT_BATCH_SIZE);
            return null;
        }
        return dbInsertBatchSize;
    }

    // 处理生成调用链时的详细程度
    private String handleOutputDetail(String outputDetail) {
        if (OutputDetailEnum.ODE_ILLEGAL == OutputDetailEnum.getFromDetail(outputDetail)) {
            logger.error("参数配置非法\n{} {} {}\n可选值如下: {}", ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL.getFileName(), ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL.getKey(), outputDetail,
                    OutputDetailEnum.getValidValues());
            return null;
        }
        return outputDetail;
    }

    // 处理H2数据库文件路径
    private String handleDbH2FilePath(String dbH2FilePath) {
        if (StringUtils.endsWithIgnoreCase(dbH2FilePath, JACGConstants.H2_FILE_EXT)) {
            logger.error("不需要指定H2数据库的后缀{}\n{} {}\n{}", JACGConstants.H2_FILE_EXT, ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH.getFileName(),
                    ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH.getKey(), dbH2FilePath);
            return null;
        }
        return dbH2FilePath;
    }

    /**
     * 使用默认的空参数（忽略配置文件中的参数）
     * 在设置参数之前执行，避免jar包或项目中的配置文件有值时对生成结果产生干扰
     */
    public void useDefaultEmptyConfig() {
        for (ConfigKeyEnum configKeyEnum : ConfigKeyEnum.values()) {
            clearMainConfig(configKeyEnum);
        }
        for (ConfigDbKeyEnum configDbKeyEnum : ConfigDbKeyEnum.values()) {
            clearMainConfig(configDbKeyEnum);
        }
        for (OtherConfigFileUseSetEnum otherConfigFileUseSetEnum : OtherConfigFileUseSetEnum.values()) {
            otherConfigSetMap.put(otherConfigFileUseSetEnum.getKey(), Collections.emptySet());
        }
        for (OtherConfigFileUseListEnum otherConfigFileUseListEnum : OtherConfigFileUseListEnum.values()) {
            otherConfigListMap.put(otherConfigFileUseListEnum.getKey(), Collections.emptyList());
        }

        // 添加所有预置的扩展类
        addAllPreBuildExtensions();
    }

    private void clearMainConfig(MainConfigInterface mainConfig) {
        if (String.class == mainConfig.getType()) {
            mainConfigMap.put(mainConfig.getKey(), "");
        } else if (Boolean.class == mainConfig.getType()) {
            mainConfigMap.put(mainConfig.getKey(), Boolean.FALSE);
        } else if (Integer.class == mainConfig.getType()) {
            mainConfigMap.put(mainConfig.getKey(), 0);
        }
    }

    /**
     * 拷贝数据
     *
     * @return
     */
    public ConfigureWrapper copy() {
        ConfigureWrapper clone = new ConfigureWrapper(false);
        clone.propertiesMap = new HashMap<>(this.propertiesMap);
        clone.usedMainConfigMap = new HashMap<>(this.usedMainConfigMap);
        clone.usedOtherListConfigSet = new HashSet<>(this.usedOtherListConfigSet);
        clone.usedOtherSetConfigSet = new HashSet<>(this.usedOtherSetConfigSet);
        clone.mainConfigMap = new HashMap<>(this.mainConfigMap);
        clone.otherConfigSetMap = new HashMap<>(this.otherConfigSetMap);
        clone.otherConfigListMap = new HashMap<>(this.otherConfigListMap);
        clone.entrySimpleClassNameList = new ArrayList<>(this.entrySimpleClassNameList);
        return clone;
    }

    /**
     * 添加所有预置的扩展类
     */
    public void addAllPreBuildExtensions() {
        logger.info("添加所有预置的扩展类");
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
        Set<String> allowedClassSet = getOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_ALLOWED_CLASS_PREFIX, true);
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

    /**
     * 记录入口简单类名
     *
     * @param simpleClassName
     */
    public void addEntryClass(String simpleClassName) {
        if (!entrySimpleClassNameList.contains(simpleClassName)) {
            entrySimpleClassNameList.add(simpleClassName);
        }
    }

    /**
     * 执行完毕时尝试打印当前使用的配置信息
     *
     * @param simpleClassName
     */
    public void tryPrintUsedConfigInfo(String simpleClassName, String outputDirPath) {
        if (!StringUtils.equals(simpleClassName, entrySimpleClassNameList.get(0))) {
            logger.info("不是第1个入口类尝试打印当前使用的配置信息，忽略 {}", simpleClassName);
            return;
        }

        String configMdFilePath = JavaCGUtil.addSeparator4FilePath(outputDirPath) + JACGConstants.FILE_JACG_USED_CONFIG_MD;
        logger.info("{} 使用的配置参数信息保存到以下文件\n{}", simpleClassName, configMdFilePath);
        // 打印使用的配置参数信息
        printConfigInfo(simpleClassName, configMdFilePath, false);
        // 清空入口简单类名，使一个ConfigureWrapper在多次使用时能够继续打印后续使用的配置参数（先写数据库，再执行其他类）
        entrySimpleClassNameList.clear();
    }

    /**
     * 打印配置参数信息
     *
     * @param simpleClassName
     * @param configMdFilePath
     * @param printAllConfigInfo true: 打印所有的配置参数信息 false: 打印使用的配置参数信息
     */
    public void printConfigInfo(String simpleClassName, String configMdFilePath, boolean printAllConfigInfo) {
        try (MarkdownWriter markdownWriter = new MarkdownWriter(configMdFilePath, true)) {
            if (!printAllConfigInfo) {
                // 打印使用的配置参数信息，先打印当前有使用的配置参数
                printUsedConfigKey(markdownWriter, simpleClassName);
            }

            // 打印主要的配置信息
            printMainConfigInfo(markdownWriter, ConfigKeyEnum.values(), printAllConfigInfo);
            printMainConfigInfo(markdownWriter, ConfigDbKeyEnum.values(), printAllConfigInfo);

            // 打印Set格式的其他配置信息
            printOtherSetConfigInfo(markdownWriter, OtherConfigFileUseSetEnum.values(), printAllConfigInfo);

            // 打印List格式的其他配置信息
            printOtherListConfigInfo(markdownWriter, OtherConfigFileUseListEnum.values(), printAllConfigInfo);
        } catch (Exception e) {
            logger.error("{} error ", simpleClassName, e);
        }
    }

    // 打印有使用的配置参数key及描述
    private void printUsedConfigKey(MarkdownWriter markdownWriter, String simpleClassName) throws IOException {
        markdownWriter.addTitle(1, "当前执行的类名");
        markdownWriter.addLineWithNewLine(simpleClassName);

        if (!usedMainConfigMap.isEmpty()) {
            // 当主要配置参数有使用时，打印有使用的主要的配置key及描述
            markdownWriter.addTitle(1, "当前有使用的主要配置参数");
            markdownWriter.addTableHead(JACGConstants.USED_CONFIG_FLAG_FILE_KEY, JACGConstants.USED_CONFIG_FLAG_CONF_KEY, JACGConstants.USED_CONFIG_FLAG_CONF_DESC);
            List<String> usedMainConfigFileList = new ArrayList<>(usedMainConfigMap.keySet());
            Collections.sort(usedMainConfigFileList);
            for (String usedMainConfigFile : usedMainConfigFileList) {
                List<String> usedMainConfigKeyList = new ArrayList<>(usedMainConfigMap.get(usedMainConfigFile));
                Collections.sort(usedMainConfigKeyList);
                for (String usedMainConfigKey : usedMainConfigKeyList) {
                    markdownWriter.addTableBody(usedMainConfigFile, usedMainConfigKey, getMainConfigDesc(usedMainConfigFile, usedMainConfigKey));
                }
            }
            markdownWriter.addEmptyLine();
        }

        Map<String, String> otherConfigMap = new HashMap<>();
        // 记录List格式的其他配置信息
        for (String usedOtherListConfig : usedOtherListConfigSet) {
            otherConfigMap.put(usedOtherListConfig, OtherConfigFileUseListEnum.getDescFromKey(usedOtherListConfig));
        }

        // 记录Set格式的其他配置信息
        for (String usedOtherSetConfig : usedOtherSetConfigSet) {
            otherConfigMap.put(usedOtherSetConfig, OtherConfigFileUseSetEnum.getDescFromKey(usedOtherSetConfig));
        }

        // 打印其他配置信息
        markdownWriter.addTitle(1, "当前有使用的其他配置参数");
        markdownWriter.addTableHead(JACGConstants.USED_CONFIG_FLAG_FILE_KEY, JACGConstants.USED_CONFIG_FLAG_FILE_DESC);
        List<String> usedOtherConfigFileList = new ArrayList<>(otherConfigMap.keySet());
        Collections.sort(usedOtherConfigFileList);
        for (String usedOtherConfigFile : usedOtherConfigFileList) {
            markdownWriter.addTableBody(usedOtherConfigFile, otherConfigMap.get(usedOtherConfigFile));
        }
        markdownWriter.addEmptyLine();
    }

    // 获取主要配置的描述
    private String getMainConfigDesc(String fileName, String configKey) {
        if (ConfigKeyEnum.CKE_APP_NAME.getFileName().equals(fileName)) {
            return ConfigKeyEnum.getDescFromKey(configKey);
        } else if (ConfigDbKeyEnum.CDKE_DB_USE_H2.getFileName().equals(fileName)) {
            return ConfigDbKeyEnum.getDescFromKey(configKey);
        }
        // 不会执行到这里
        return "";
    }

    // 打印主要的配置信息
    private void printMainConfigInfo(MarkdownWriter markdownWriter, MainConfigInterface[] configs, boolean printAllConfigInfo) throws IOException {
        if (!printAllConfigInfo && usedMainConfigMap.isEmpty()) {
            // 打印使用的配置参数信息，且主要的配置参数未使用，不打印
            return;
        }
        boolean headWritten = false;
        for (int i = 0; i < configs.length; i++) {
            MainConfigInterface mainConfig = configs[i];
            if (!printAllConfigInfo) {
                // 打印使用的配置参数信息
                Set<String> usedMainConfigSet = usedMainConfigMap.get(mainConfig.getFileName());
                if (usedMainConfigSet == null || !usedMainConfigSet.contains(mainConfig.getKey())) {
                    // 当前配置参数未使用，不打印
                    continue;
                }
            }
            if (!headWritten) {
                // 写入配置文件名
                markdownWriter.addTitle(1, mainConfig.getFileName());
                markdownWriter.addTableHead(JACGConstants.USED_CONFIG_FLAG_CONF_KEY, JACGConstants.USED_CONFIG_FLAG_CONF_DESC, JACGConstants.USED_CONFIG_FLAG_CONF_VALUE);
                headWritten = true;
            }

            // 执行打印主要的配置信息
            doPrintMainConfigInfo(markdownWriter, mainConfig.getKey(), mainConfig.getDesc(), getMainConfig(mainConfig, false));
        }
        // 最后写入空行
        markdownWriter.addEmptyLine();
    }

    // 打印List格式的其他配置信息
    private void printOtherListConfigInfo(MarkdownWriter markdownWriter, OtherConfigFileUseListEnum[] configs, boolean printAllConfigInfo) throws IOException {
        for (int i = 0; i < configs.length; i++) {
            OtherConfigFileUseListEnum currentConfig = configs[i];
            if (!printAllConfigInfo && !usedOtherListConfigSet.contains(currentConfig.getKey())) {
                // 打印使用的配置参数信息时，当前配置参数未使用，不打印
                continue;
            }
            // 执行打印List格式的配置信息
            doPrintListConfigInfo(markdownWriter, i, currentConfig.getKey(), currentConfig.getDesc(), getOtherConfigList(currentConfig, false));
        }
    }

    // 打印Set格式的其他配置信息
    private void printOtherSetConfigInfo(MarkdownWriter markdownWriter, OtherConfigFileUseSetEnum[] configs, boolean printAllConfigInfo) throws IOException {
        for (int i = 0; i < configs.length; i++) {
            OtherConfigFileUseSetEnum currentConfig = configs[i];
            if (!printAllConfigInfo && !usedOtherSetConfigSet.contains(currentConfig.getKey())) {
                // 打印使用的配置参数信息时，当前配置参数未使用，不打印
                continue;
            }
            // 执行打印Set格式的配置信息
            doPrintSetConfigInfo(markdownWriter, i, currentConfig.getKey(), currentConfig.getDesc(), getOtherConfigSet(currentConfig, false));
        }
    }

    // 执行打印主要配置信息
    public void doPrintMainConfigInfo(MarkdownWriter markdownWriter, String key, String desc, Object value) throws IOException {
        // 写入配置信息
        String strValue;
        if (value == null) {
            strValue = "";
        } else if (value instanceof String) {
            strValue = (String) value;
        } else {
            strValue = value.toString();
        }
        markdownWriter.addTableBody(key, desc, (value == null ? "" : strValue));
    }

    // 执行打印List格式的配置信息
    public void doPrintListConfigInfo(MarkdownWriter markdownWriter, int index, String key, String desc, List<String> configList) throws IOException {
        if (index == 0) {
            // 写入配置文件名
            markdownWriter.addTitle(1, JACGConstants.USED_CONFIG_FLAG_CONF_LIST);
        }
        // 写入配置文件名
        markdownWriter.addTitle(2, key);
        markdownWriter.addListWithNewLine(JACGConstants.USED_CONFIG_FLAG_CONF_DESC);
        markdownWriter.addLineWithNewLine(desc);
        markdownWriter.addListWithNewLine(JACGConstants.USED_CONFIG_FLAG_CONF_VALUE);
        markdownWriter.addCodeBlock();
        for (String configValue : configList) {
            markdownWriter.addLine(configValue);
        }
        markdownWriter.addCodeBlock();
    }

    // 执行打印Set格式的配置信息
    public void doPrintSetConfigInfo(MarkdownWriter markdownWriter, int index, String key, String desc, Set<String> configSet) throws IOException {
        if (index == 0) {
            // 写入配置文件名
            markdownWriter.addTitle(1, JACGConstants.USED_CONFIG_FLAG_CONF_SET);
        }
        markdownWriter.addTitle(2, key);
        markdownWriter.addListWithNewLine(JACGConstants.USED_CONFIG_FLAG_CONF_DESC);
        markdownWriter.addLineWithNewLine(desc);
        markdownWriter.addListWithNewLine(JACGConstants.USED_CONFIG_FLAG_CONF_VALUE);
        markdownWriter.addCodeBlock();
        List<String> configValueList = new ArrayList<>(configSet);
        // 排序后打印
        Collections.sort(configValueList);
        for (String configValue : configValueList) {
            markdownWriter.addLine(configValue);
        }
        markdownWriter.addCodeBlock();
    }

    /**
     * 生成java-callgraph2的配置
     *
     * @return
     */
    public JavaCGConfigureWrapper genJavaCGConfigureWrapper() {
        JavaCGConfigureWrapper javaCGConfigureWrapper = new JavaCGConfigureWrapper();
        javaCGConfigureWrapper.setConfig(JavaCGConfigKeyEnum.CKE_PARSE_METHOD_CALL_TYPE_VALUE, Boolean.TRUE.toString());
        javaCGConfigureWrapper.setConfig(JavaCGConfigKeyEnum.CKE_FIRST_PARSE_INIT_METHOD_TYPE, Boolean.TRUE.toString());
        javaCGConfigureWrapper.setConfig(JavaCGConfigKeyEnum.CKE_CONTINUE_WHEN_ERROR, Boolean.FALSE.toString());
        javaCGConfigureWrapper.setConfig(JavaCGConfigKeyEnum.CKE_DEBUG_PRINT, Boolean.FALSE.toString());
        javaCGConfigureWrapper.setConfig(JavaCGConfigKeyEnum.CKE_OUTPUT_ROOT_PATH, getMainConfig(ConfigKeyEnum.CKE_OUTPUT_ROOT_PATH));
        javaCGConfigureWrapper.setConfig(JavaCGConfigKeyEnum.CKE_OUTPUT_FILE_EXT, JACGConstants.EXT_MD);

        // 指定需要处理的jar包与目录
        javaCGConfigureWrapper.setOtherConfigList(JavaCGOtherConfigFileUseListEnum.OCFULE_JAR_DIR, getOtherConfigList(OtherConfigFileUseListEnum.OCFULE_JAR_DIR, true));
        // 指定需要处理的包名
        javaCGConfigureWrapper.setOtherConfigSet(JavaCGOtherConfigFileUseSetEnum.OCFUSE_PACKAGES, getOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_ALLOWED_CLASS_PREFIX, true));
        return javaCGConfigureWrapper;
    }
}

package com.adrninistrator.jacg.conf;

import com.adrninistrator.jacg.annotation.formatter.DefaultAnnotationFormatter;
import com.adrninistrator.jacg.annotation.formatter.SpringMvcRequestMappingFormatter;
import com.adrninistrator.jacg.annotation.formatter.SpringTransactionalFormatter;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.conf.enums.ConfigDbKeyEnum;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import com.adrninistrator.javacg2.conf.BaseConfigureWrapper;
import com.adrninistrator.javacg2.conf.enums.interfaces.MainConfigInterface;
import com.adrninistrator.javacg2.conf.enums.interfaces.OtherConfigInterface;
import com.adrninistrator.javacg2.el.enums.interfaces.ElConfigInterface;
import com.adrninistrator.javacg2.exceptions.JavaCG2Error;
import com.adrninistrator.javacg2.markdown.writer.MarkdownWriter;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.regex.Pattern;

/**
 * @author adrninistrator
 * @date 2022/4/20
 * @description: 配置包装类
 */
public class ConfigureWrapper extends BaseConfigureWrapper {
    private static final Logger logger = LoggerFactory.getLogger(ConfigureWrapper.class);

    private static final Pattern APP_NAME_PATTERN = Pattern.compile("[A-Za-z0-9_]*");

    // 数据库操作包装对象
    private DbOperWrapper dbOperWrapper;

    /**
     * 默认构造函数，仅使用代码中指定的参数，忽略配置文件中的参数
     */
    public ConfigureWrapper() {
        super(true);
    }

    /**
     * 构造函数，指定使用代码中指定的参数，还是使用配置文件中的参数
     *
     * @param onlyUseConfigInJavaCode true: 仅使用代码中指定的参数，忽略配置文件中的参数 false: 使用配置文件中的参数
     */
    public ConfigureWrapper(boolean onlyUseConfigInJavaCode) {
        super(onlyUseConfigInJavaCode);
    }

    @Override
    protected void customCheckWhenUseConfig() {
        checkInWorkThread();
    }

    @Override
    protected Object customGenMainConfigValue(MainConfigInterface mainConfig, String strValue) {
        if (ConfigKeyEnum.CKE_APP_NAME == mainConfig) {
            return handleAppName(strValue);
        }

        if (ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL == mainConfig) {
            return handleOutputDetail(strValue);
        }

        if (ConfigKeyEnum.CKE_THREAD_NUM == mainConfig) {
            // 处理线程数
            return handleThreadNum(strValue);
        }

        if (ConfigKeyEnum.CKE_DB_INSERT_BATCH_SIZE == mainConfig) {
            // 处理批量写入数据库时每次插入的数量
            return handleBatchInsertSize(strValue);
        }

        if (ConfigKeyEnum.CKE_OUTPUT_DIR_NAME == mainConfig) {
            // 处理生成调用链文件的目录名
            return handleOutputDirName(strValue);
        }

        if (ConfigKeyEnum.CKE_OUTPUT_DIR_FLAG == mainConfig) {
            // 处理生成调用链文件的目录名
            return handleOutputDirFlag(strValue);
        }

        if (ConfigDbKeyEnum.CDKE_DB_H2_FILE_PATH == mainConfig) {
            return handleDbH2FilePath(strValue);
        }
        return null;
    }

    // 检查是否在工作线程中执行
    private void checkInWorkThread() {
        if (Thread.currentThread().getName().startsWith(JACGConstants.THREAD_NAME_PREFIX_WORKER)) {
            logger.error("获取配置的操作不应该在工作线程中执行");
            throw new JavaCG2Error("获取配置的操作不应该在工作线程中执行");
        }
    }

    // 处理数据库里的表名后缀
    private String handleAppName(String appName) {
        if (!APP_NAME_PATTERN.matcher(appName).matches()) {
            logger.error("属性只支持字母、数字及下划线 {} {}", appName, ConfigKeyEnum.CKE_APP_NAME.genConfigUsage());
            return null;
        }
        // 将app.name参数中的-替换为_
        return appName.replace("-", "_");
    }

    // 处理线程数
    private Integer handleThreadNum(String strThreadNum) {
        int threadNum = Integer.parseInt(strThreadNum);
        if (threadNum <= 0 || threadNum > JACGConstants.MAX_THREAD_NUM) {
            logger.error("参数配置非法1 {} 应在以下范围: (0,{}] {}", strThreadNum, JACGConstants.MAX_THREAD_NUM, ConfigKeyEnum.CKE_THREAD_NUM.genConfigUsage());
            return null;
        }
        return threadNum;
    }

    // 处理生成调用链文件的目录名
    private String handleOutputDirName(String outputDirName) {
        if (StringUtils.isBlank(outputDirName)) {
            return "";
        }
        // 使用指定的名称作为子目录名
        if (JavaCG2FileUtil.checkFilePathContainsSeparator(outputDirName)) {
            logger.error("指定的目录名中不允许包含目录分隔符 {} {}", outputDirName, ConfigKeyEnum.CKE_OUTPUT_DIR_NAME.genConfigUsage());
            return null;
        }
        return outputDirName;
    }

    // 处理生成调用链文件的目录名
    private String handleOutputDirFlag(String outputDirFlag) {
        if (StringUtils.isBlank(outputDirFlag)) {
            return "";
        }
        // 使用指定的名称作为子目录名
        if (JavaCG2FileUtil.checkFilePathContainsSeparator(outputDirFlag)) {
            logger.error("指定的目录标志中不允许包含目录分隔符 {} {}", outputDirFlag, ConfigKeyEnum.CKE_OUTPUT_DIR_FLAG.genConfigUsage());
            return null;
        }
        return outputDirFlag;
    }

    // 处理批量写入数据库时每次插入的数量
    private Integer handleBatchInsertSize(String strDbBatchInsertSize) {
        int dbInsertBatchSize = Integer.parseInt(strDbBatchInsertSize);
        if (dbInsertBatchSize <= 0 || dbInsertBatchSize > JACGConstants.MAX_DB_INSERT_BATCH_SIZE) {
            logger.error("参数配置非法2 {} 应在以下范围: (0,{}] {}", strDbBatchInsertSize, JACGConstants.MAX_DB_INSERT_BATCH_SIZE, ConfigKeyEnum.CKE_DB_INSERT_BATCH_SIZE.genConfigUsage());
            return null;
        }
        return dbInsertBatchSize;
    }

    // 处理生成调用链时的详细程度
    private String handleOutputDetail(String outputDetail) {
        if (OutputDetailEnum.ODE_ILLEGAL == OutputDetailEnum.getFromDetail(outputDetail)) {
            logger.error("参数配置非法 {} 可选值如下 {} {}", outputDetail, OutputDetailEnum.getValidValuesAndDesc(true), ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL.genConfigUsage());
            return null;
        }
        return outputDetail;
    }

    // 处理H2数据库文件路径
    private String handleDbH2FilePath(String dbH2FilePath) {
        if (StringUtils.endsWithIgnoreCase(dbH2FilePath, JACGConstants.H2_FILE_EXT)) {
            String newDbH2FilePath = StringUtils.substringBeforeLast(dbH2FilePath, JACGConstants.H2_FILE_EXT);
            logger.info("原有的H2数据库文件路径 {} 修改为 {}", dbH2FilePath, newDbH2FilePath);
            return newDbH2FilePath;
        }
        return dbH2FilePath;
    }

    /**
     * 使用默认的空参数（忽略配置文件中的参数）
     * 在设置参数之前执行，避免jar包或项目中的配置文件有值时对生成结果产生干扰
     */
    public void useDefaultEmptyConfig() {
        clearMainConfigs(ConfigKeyEnum.values());
        clearMainConfigs(ConfigDbKeyEnum.values());
        clearOtherConfigUseList(OtherConfigFileUseListEnum.values());
        clearOtherConfigUseSet(OtherConfigFileUseSetEnum.values());
        // 添加所有预置的扩展类
        addAllPreBuildExtensions();
    }

    @Override
    protected Object customGetDefaultConfig(MainConfigInterface mainConfig) {
        return null;
    }

    @Override
    protected void customPrintConfigInfo(MarkdownWriter markdownWriter, boolean printAllConfigInfo) throws IOException {
        printMainConfigInfo(markdownWriter, ConfigKeyEnum.values(), printAllConfigInfo);
        printMainConfigInfo(markdownWriter, ConfigDbKeyEnum.values(), printAllConfigInfo);

        // 打印Set格式的其他配置参数
        printOtherSetConfigInfo(markdownWriter, OtherConfigFileUseSetEnum.values(), printAllConfigInfo);

        // 打印List格式的其他配置参数
        printOtherListConfigInfo(markdownWriter, OtherConfigFileUseListEnum.values(), printAllConfigInfo);

        // 打印表达式配置参数
        printElConfigInfo(markdownWriter, ElConfigEnum.values(), printAllConfigInfo);
    }

    // 获取主要配置的简单类名
    @Override
    protected String getMainConfigSCNFromFile(String mainConfigFile) {
        if (ConfigKeyEnum.values()[0].getFileName().equals(mainConfigFile)) {
            return ConfigKeyEnum.class.getSimpleName();
        }
        return ConfigDbKeyEnum.class.getSimpleName();
    }

    @Override
    public OtherConfigInterface[] chooseOtherConfigFileUseSetEnums() {
        return OtherConfigFileUseSetEnum.values();
    }

    @Override
    public OtherConfigInterface[] chooseOtherConfigFileUseListEnums() {
        return OtherConfigFileUseListEnum.values();
    }

    @Override
    public ElConfigInterface[] chooseElConfigEnums() {
        return ElConfigEnum.values();
    }

    @Override
    protected String[] chooseAllowedConfigClassNames() {
        return new String[]{
                ConfigKeyEnum.class.getName(),
                ConfigDbKeyEnum.class.getName(),
                OtherConfigFileUseListEnum.class.getName(),
                OtherConfigFileUseSetEnum.class.getName(),
                ElConfigEnum.class.getName()
        };
    }

    /**
     * 拷贝数据
     *
     * @return
     */
    public ConfigureWrapper copy() {
        return (ConfigureWrapper) baseCopy();
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

    // 使用固定app.name的H2数据库文件
    public void useFixedAppNameH2Db() {
        logger.info("使用H2数据库时，使用固定的参数 {} {}", ConfigKeyEnum.CKE_APP_NAME.genConfigUsage(), JACGConstants.FIXED_APP_NAME);
        setMainConfig(ConfigKeyEnum.CKE_APP_NAME, JACGConstants.FIXED_APP_NAME);
        setMainConfig(ConfigDbKeyEnum.CDKE_DB_USE_H2, Boolean.TRUE.toString());
    }

    // 记录数据库操作对象
    public void setDbOperWrapper(DbOperWrapper dbOperWrapper) {
        this.dbOperWrapper = dbOperWrapper;
    }

    public DbOperWrapper getDbOperWrapper() {
        return dbOperWrapper;
    }
}

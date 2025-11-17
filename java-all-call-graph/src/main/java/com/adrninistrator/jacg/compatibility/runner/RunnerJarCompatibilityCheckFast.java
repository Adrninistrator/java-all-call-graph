package com.adrninistrator.jacg.compatibility.runner;

import com.adrninistrator.jacg.compatibility.handler.ClassReferenceCheckHandler;
import com.adrninistrator.jacg.compatibility.handler.DupClassReferenceCheckHandler;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.el.enums.ElConfigEnum;
import com.adrninistrator.jacg.el.manager.ElManager;
import com.adrninistrator.jacg.runner.RunnerWriteDbOnlyClassMode;
import com.adrninistrator.jacg.writer.WriterSupportHeader;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2025/8/9
 * @description: Jar兼容性检查入口类，快速模式，仅检查被使用的类是否存在
 */
public class RunnerJarCompatibilityCheckFast extends RunnerJarCompatibilityCheck {
    private static final Logger logger = LoggerFactory.getLogger(RunnerJarCompatibilityCheckFast.class);

    public static final String FILE_NAME = "1.被引用的不存在的类信息" + JavaCG2Constants.EXT_MD;
    public static final String[] FILE_HEADER_ARRAY = new String[]{
            "引用类名",
            "引用类（及对应外部类）上的@ConditionalOnClass注解属性",
            "引用类在jar文件中的路径",
            "引用类所在jar文件路径",
            "引用类所在jar文件内部路径",
            "被引用的类名",
    };
    public static final String FILE_HEADER = StringUtils.join(FILE_HEADER_ARRAY, JavaCG2Constants.FLAG_TAB);

    private final JavaCG2ConfigureWrapper javaCG2ConfigureWrapper;

    private boolean skipWriteDb;

    public RunnerJarCompatibilityCheckFast(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper, ConfigureWrapper configureWrapper) {
        super(configureWrapper);
        this.javaCG2ConfigureWrapper = javaCG2ConfigureWrapper;
    }

    @Override
    protected void handle() {
        if (skipWriteDb) {
            logger.info("跳过写入数据库步骤");
        } else {
            // 仅解析类相关信息并写入数据库
            RunnerWriteDbOnlyClassMode runnerWriteDbOnlyClassMode = new RunnerWriteDbOnlyClassMode(javaCG2ConfigureWrapper, configureWrapper);
            if (!runnerWriteDbOnlyClassMode.run()) {
                recordTaskFail();
                return;
            }
        }

        // 查询解析的jar文件信息
        queryJarInfoMap();

        /*
            保存类名是否存在
            key 类名
            value   是否存在
         */
        Map<String, Boolean> classNameExistsMap = new HashMap<>();
        String outputFilePath = currentOutputDirPath + File.separator + FILE_NAME;
        try (ElManager elManager = new ElManager(configureWrapper, ElConfigEnum.values(), currentOutputDirPath);
             WriterSupportHeader writer = new WriterSupportHeader(outputFilePath, FILE_HEADER);
             ClassReferenceCheckHandler classReferenceCheckHandler = new ClassReferenceCheckHandler(configureWrapper);
             DupClassReferenceCheckHandler dupClassReferenceCheckHandler = new DupClassReferenceCheckHandler(configureWrapper)) {
            classReferenceCheckHandler.setOtherDbOperWrapperList(otherDbOperWrapperList);
            classReferenceCheckHandler.setClassNameExistsMap(classNameExistsMap);
            classReferenceCheckHandler.setCurrentOutputDirPath(currentOutputDirPath);
            classReferenceCheckHandler.setJarInfoMap(jarInfoMap);
            classReferenceCheckHandler.setWriter(writer);
            classReferenceCheckHandler.setElManager(elManager);
            classReferenceCheckHandler.init();
            if (!classReferenceCheckHandler.check()) {
                recordTaskFail();
                return;
            }

            dupClassReferenceCheckHandler.setOtherDbOperWrapperList(otherDbOperWrapperList);
            dupClassReferenceCheckHandler.setClassNameExistsMap(classNameExistsMap);
            dupClassReferenceCheckHandler.setCurrentOutputDirPath(currentOutputDirPath);
            dupClassReferenceCheckHandler.setJarInfoMap(jarInfoMap);
            dupClassReferenceCheckHandler.setWriter(writer);
            dupClassReferenceCheckHandler.init();
            if (!dupClassReferenceCheckHandler.check()) {
                recordTaskFail();
                return;
            }
        } catch (Exception e) {
            logger.error("error ", e);
            recordTaskFail();
            return;
        }

        textFileToExcel(outputFilePath);
    }

    /**
     * 对当前使用的配置进行自定义设置，适用于在构造函数中需要修改的场景
     *
     * @param configureWrapper
     */
    @Override
    protected void configConfigureWrapper(ConfigureWrapper configureWrapper) {
        // 使用固定app.name的H2数据库文件
        configureWrapper.useFixedAppNameH2Db();
    }

    /**
     * 需要生成excel文件
     *
     * @return
     */
    @Override
    protected boolean needGenerateExcel() {
        return true;
    }

    public void setSkipWriteDb(boolean skipWriteDb) {
        this.skipWriteDb = skipWriteDb;
    }
}

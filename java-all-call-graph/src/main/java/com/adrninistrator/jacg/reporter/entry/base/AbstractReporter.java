package com.adrninistrator.jacg.reporter.entry.base;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.extractor.dto.common.extract_file.AbstractCallGraphExtractedFile;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.writer.WriterSupportHeader;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/3/2
 * @description: 报告处理类基类
 */
public abstract class AbstractReporter {
    private static final Logger logger = LoggerFactory.getLogger(AbstractReporter.class);

    protected final ConfigureWrapper configureWrapper;

    // 保存结果文件的目录路径
    private final String reportDirPath;

    // 是否需要将调用堆栈文件拷贝到单独的目录
    protected final boolean copyStackFileInSeparateDir;

    // 是否跳过写数据库的步骤
    protected final boolean skipWriteDb;

    protected String appName;

    /**
     * 构造函数，生成结果文件在当前目录，使用配置文件中的参数
     */
    public AbstractReporter() {
        configureWrapper = new ConfigureWrapper(false);
        reportDirPath = "";
        copyStackFileInSeparateDir = false;
        skipWriteDb = false;
        initConfig();
    }

    /**
     * 构造函数，生成结果文件在指定目录，使用代码指定的参数
     *
     * @param configureWrapper           配置参数
     * @param reportDirPath              保存结果文件的目录路径
     * @param copyStackFileInSeparateDir 是否需要将调用堆栈文件拷贝到单独的目录
     * @param skipWriteDb                是否跳过写数据库的步骤
     */
    public AbstractReporter(ConfigureWrapper configureWrapper, String reportDirPath, boolean copyStackFileInSeparateDir, boolean skipWriteDb) {
        this.configureWrapper = configureWrapper;
        this.reportDirPath = reportDirPath;
        this.copyStackFileInSeparateDir = copyStackFileInSeparateDir;
        this.skipWriteDb = skipWriteDb;
        initConfig();
    }

    private void initConfig() {
        appName = configureWrapper.getMainConfig(ConfigKeyEnum.CKE_APP_NAME);
    }

    /**
     * 处理调用堆栈文件，拷贝到指定目录，返回新文件路径
     *
     * @param dirPath
     * @param callGraphExtractedFile
     * @return
     */
    protected String handleStackFile(String dirPath, AbstractCallGraphExtractedFile callGraphExtractedFile) {
        File srcStackFile = new File(callGraphExtractedFile.getStackFilePath());
        String newStackFilePath;
        if (copyStackFileInSeparateDir) {
            String newDirPath = dirPath + File.separator + appName;
            JACGFileUtil.isDirectoryExists(newDirPath);
            newStackFilePath = newDirPath + File.separator + srcStackFile.getName();
        } else {
            newStackFilePath = dirPath + File.separator + srcStackFile.getName();
        }
        // 拷贝文件到指定目录
        logger.info("拷贝文件到指定目录\n{}\n{}", srcStackFile, newStackFilePath);
        JACGFileUtil.copyFile(srcStackFile, new File(newStackFilePath));
        return newStackFilePath;
    }

    protected String genReportFilePath(String fileName) {
        if (reportDirPath.isEmpty()) {
            return fileName;
        }
        return reportDirPath + File.separator + fileName;
    }

    // 公共预处理
    protected boolean commonPreHandle() {
        // 添加所有预置的扩展类
        configureWrapper.addAllPreBuildExtensions();

        if (skipWriteDb) {
            logger.info("通过参数指定跳过写数据库步骤");
            return true;
        }

        logger.info("执行写数据库步骤");
        return new RunnerWriteDb().run(configureWrapper, configureWrapper.genJavaCGConfigureWrapper());
    }

    /**
     * 向文件中写入数据的公共操作
     *
     * @param writerSupportHeader
     * @param stringList
     * @throws IOException
     */
    protected void commonWriteData(WriterSupportHeader writerSupportHeader, List<String> stringList) throws IOException {
        writerSupportHeader.writeLine(StringUtils.join(stringList, JACGConstants.FLAG_TAB));
    }

    public String getReportDirPath() {
        logger.info("{} 当前结果文件输出的目录 {}", this.getClass().getSimpleName(), reportDirPath);
        return reportDirPath;
    }
}

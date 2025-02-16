package com.adrninistrator.jacg.handler.reporter;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.extractor.dto.common.extractfile.AbstractCallGraphExtractedFile;
import com.adrninistrator.jacg.runner.RunnerWriteDb;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.writer.WriterSupportHeader;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
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

    protected final JavaCG2ConfigureWrapper javaCG2ConfigureWrapper;

    protected final ConfigureWrapper configureWrapper;

    // 保存结果文件的目录路径
    protected final String reportDirPath;

    // 是否对生成的报告文件进行追加，true: 对报告文件进行追加（处理多个jar包时可生成到一个报告文件中）， false: 对报告文件覆盖
    protected final boolean appendReportFile;

    // 是否需要将调用堆栈文件拷贝到单独的目录
    protected final boolean copyStackFileInSeparateDir;

    protected String appName;

    /**
     * 构造函数，生成结果文件在指定目录，使用代码指定的参数
     *
     * @param configureWrapper 配置参数
     * @param reportDirPath    保存结果文件的目录路径
     * @param appendReportFile 是否对生成的报告文件进行追加，true: 对报告文件进行追加（处理多个jar包时可生成到一个报告文件中）， false: 对报告文件覆盖
     */
    public AbstractReporter(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper, ConfigureWrapper configureWrapper, String reportDirPath, boolean appendReportFile) {
        this(javaCG2ConfigureWrapper, configureWrapper, reportDirPath, appendReportFile, false);
    }

    /**
     * 构造函数，生成结果文件在指定目录，使用代码指定的参数
     *
     * @param configureWrapper           配置参数
     * @param reportDirPath              保存结果文件的目录路径
     * @param appendReportFile           是否对生成的报告文件进行追加，true: 对报告文件进行追加（处理多个jar包时可生成到一个报告文件中）， false: 对报告文件覆盖
     * @param copyStackFileInSeparateDir 是否需要将调用堆栈文件拷贝到单独的目录
     */
    public AbstractReporter(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper, ConfigureWrapper configureWrapper, String reportDirPath, boolean appendReportFile,
                            boolean copyStackFileInSeparateDir) {
        this.javaCG2ConfigureWrapper = javaCG2ConfigureWrapper;
        this.configureWrapper = configureWrapper;
        this.reportDirPath = reportDirPath;
        this.appendReportFile = appendReportFile;
        this.copyStackFileInSeparateDir = copyStackFileInSeparateDir;
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
            JavaCG2FileUtil.isDirectoryExists(newDirPath);
            newStackFilePath = newDirPath + File.separator + srcStackFile.getName();
        } else {
            newStackFilePath = dirPath + File.separator + srcStackFile.getName();
        }
        // 拷贝文件到指定目录
        logger.info("拷贝文件到指定目录\n{}\n{}", srcStackFile, newStackFilePath);
        JACGFileUtil.copyFile(srcStackFile, new File(newStackFilePath));
        return newStackFilePath;
    }

    // 生成报告文件路径
    protected String genReportFilePath(String fileName) {
        String reportFilePath = reportDirPath + File.separator + fileName;
        logger.info("使用的报告文件路径 {}", reportFilePath);
        return reportFilePath;
    }

    // 公共预处理，包含写数据库步骤
    protected boolean commonPreHandle() {
        logger.info("{} 当前结果文件输出的目录 {}", this.getClass().getSimpleName(), reportDirPath);
        if (StringUtils.isBlank(reportDirPath)) {
            logger.error("未指定当前结果文件输出的目录");
            return false;
        }

        if (!JavaCG2FileUtil.isDirectoryExists(reportDirPath)) {
            logger.error("当前结果文件输出的目录创建失败 {}", reportDirPath);
            return false;
        }

        logger.info("执行写数据库步骤");
        return new RunnerWriteDb(javaCG2ConfigureWrapper, configureWrapper).run();
    }

    /**
     * 向文件中写入数据的公共操作
     *
     * @param writerSupportHeader
     * @param stringList
     * @throws IOException
     */
    protected void commonWriteData(WriterSupportHeader writerSupportHeader, List<String> stringList) throws IOException {
        writerSupportHeader.writeLine(StringUtils.join(stringList, JavaCG2Constants.FLAG_TAB));
    }
}

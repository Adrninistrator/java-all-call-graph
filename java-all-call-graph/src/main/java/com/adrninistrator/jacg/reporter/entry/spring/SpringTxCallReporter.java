package com.adrninistrator.jacg.reporter.entry.spring;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.extractor.dto.common.extract.BaseCalleeExtractedMethod;
import com.adrninistrator.jacg.extractor.dto.spring_tx.entry_method.SpTxEntryMethodTxAnnotation;
import com.adrninistrator.jacg.extractor.dto.spring_tx.entry_method.SpTxEntryMethodTxTpl;
import com.adrninistrator.jacg.extractor.dto.spring_tx.extract_combined.SpTxCallCombined;
import com.adrninistrator.jacg.extractor.dto.spring_tx.extract_file.SpTxCallByAnnotationFile;
import com.adrninistrator.jacg.extractor.dto.spring_tx.extract_file.SpTxCallByTplFile;
import com.adrninistrator.jacg.extractor.entry.spring.SpringTxCallExtractor;
import com.adrninistrator.jacg.reporter.common.JACGReportConstants;
import com.adrninistrator.jacg.reporter.common.JACGSpReportConstants;
import com.adrninistrator.jacg.reporter.dto.spring_tx.SpringTxCallAnnotationReport;
import com.adrninistrator.jacg.reporter.dto.spring_tx.SpringTxCallTplReport;
import com.adrninistrator.jacg.reporter.entry.base.AbstractReporter;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.writer.WriterSupportHeader;
import com.adrninistrator.javacg.common.enums.JavaCGYesNoEnum;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/3/2
 * @description: 生成Spring事务发起的指定操作的报告
 */
public class SpringTxCallReporter extends AbstractReporter {
    private static final Logger logger = LoggerFactory.getLogger(SpringTxCallReporter.class);

    public static final String FILE_NAME_SP_TX_CALL_ANNOTATION = "Spring事务调用-使用注解@Transactional的情况.md";
    public static final String FILE_NAME_SP_TX_CALL_TPL = "Spring事务调用-使用事务模板TransactionTemplate的情况.md";

    public static final String[] FILE_HEADER_COLUMNS_COMMON = {
            JACGReportConstants.COLUMN_CALLEE_FULL_METHOD,
            JACGReportConstants.COLUMN_CALLEE_UPPER_FULL_METHOD,
            JACGReportConstants.COLUMN_RUN_IN_OTHER_THREAD,
            JACGReportConstants.COLUMN_DATA_SEQ,
            JACGReportConstants.COLUMN_LINE_NUMBER,
            JACGReportConstants.COLUMN_FILE_PATH
    };

    public static final String FILE_HEADER_ANNOTATION;
    public static final String FILE_HEADER_TPL;

    static {
        List<String> fileHeaderCommonList = Arrays.asList(FILE_HEADER_COLUMNS_COMMON);
        List<String> fileHeaderAnnotationList = new ArrayList<>();
        fileHeaderAnnotationList.add(JACGSpReportConstants.COLUMN_ANNOTATION_ENTRY_METHOD);
        fileHeaderAnnotationList.add(JACGSpReportConstants.COLUMN_ANNOTATION_METHOD_PROPAGATION);
        fileHeaderAnnotationList.addAll(fileHeaderCommonList);
        FILE_HEADER_ANNOTATION = StringUtils.join(fileHeaderAnnotationList, JACGConstants.FLAG_TAB);

        List<String> fileHeaderTplList = new ArrayList<>();
        fileHeaderTplList.add(JACGSpReportConstants.COLUMN_TPL_TYPE);
        fileHeaderTplList.add(JACGSpReportConstants.COLUMN_TPL_CALLEE_FULL_METHOD);
        fileHeaderTplList.add(JACGSpReportConstants.COLUMN_TPL_CALLER_FULL_METHOD);
        fileHeaderTplList.add(JACGSpReportConstants.COLUMN_TPL_CALLER_LINE_NUMBER);
        fileHeaderTplList.addAll(fileHeaderCommonList);
        FILE_HEADER_TPL = StringUtils.join(fileHeaderTplList, JACGConstants.FLAG_TAB);
    }

    public SpringTxCallReporter() {
        super();
    }

    public SpringTxCallReporter(ConfigureWrapper configureWrapper, String reportDirPath, boolean copyFileInSeparateDir, boolean skipWriteDb) {
        super(configureWrapper, reportDirPath, copyFileInSeparateDir, skipWriteDb);
    }

    /**
     * 生成报告
     *
     * @return
     */
    public boolean generate() {
        String reportDirPath = getReportDirPath();
        String reportDirPathTxAnnotation;
        String reportDirPathTxTpl;
        if (!reportDirPath.isEmpty()) {
            reportDirPathTxAnnotation = reportDirPath + File.separator + JACGSpReportConstants.DIR_NAME_SP_TX_USE_ANNOTATION;
            reportDirPathTxTpl = reportDirPath + File.separator + JACGSpReportConstants.DIR_NAME_SP_TX_USE_TPL;
        } else {
            reportDirPathTxAnnotation = JACGSpReportConstants.DIR_NAME_SP_TX_USE_ANNOTATION;
            reportDirPathTxTpl = JACGSpReportConstants.DIR_NAME_SP_TX_USE_TPL;
        }
        // 尝试创建结果文件目录
        if (!JACGFileUtil.isDirectoryExists(reportDirPathTxAnnotation) || !JACGFileUtil.isDirectoryExists(reportDirPathTxTpl)) {
            return false;
        }

        SpringTxCallExtractor springTxCallExtractor = new SpringTxCallExtractor();
        // 在需要处理的类名前缀中增加Spring事务模板类，需要在写入数据库之前设置
        springTxCallExtractor.setAllowedClassNamePrefix(configureWrapper);

        // 公共预处理
        if (!commonPreHandle()) {
            return false;
        }

        // 查找Spring事务嵌套的调用情况
        SpTxCallCombined spTxCallCombined = springTxCallExtractor.extract(configureWrapper);
        // 处理事务注解相关文件信息
        handleTxAnnotationFileInfo(spTxCallCombined.getSpTxCallByAnnotationFileList(), reportDirPathTxAnnotation);
        // 处理事务模板相关文件信息
        handleTxTplFileInfo(spTxCallCombined.getSpTxCallByTplFileList(), reportDirPathTxTpl);
        return true;
    }

    /**
     * 生成事务注解方式对应报告文件的Writer，可重载
     *
     * @return
     * @throws FileNotFoundException
     */
    protected WriterSupportHeader genWriter4Annotation() throws FileNotFoundException {
        String reportFilePath = genReportFilePath(FILE_NAME_SP_TX_CALL_ANNOTATION);
        return new WriterSupportHeader(JavaCGFileUtil.genBufferedWriter(reportFilePath), FILE_HEADER_ANNOTATION);
    }

    /**
     * 在事务注解方式对应报告文件中写入数据，可重载
     *
     * @param writerSupportHeader
     * @param stringList
     * @throws IOException
     */
    protected void writeData4Annotation(WriterSupportHeader writerSupportHeader, List<String> stringList) throws IOException {
        commonWriteData(writerSupportHeader, stringList);
    }

    /**
     * 关闭事务注解方式对应报告文件的Writer，可重载
     *
     * @param writerSupportHeader
     */
    protected void closeWriter4Annotation(WriterSupportHeader writerSupportHeader) {
        IOUtils.closeQuietly(writerSupportHeader);
    }

    // 处理事务注解相关文件信息
    protected void handleTxAnnotationFileInfo(List<SpTxCallByAnnotationFile> spTxCallByAnnotationFileList, String reportDirPathTxAnnotation) {
        if (JavaCGUtil.isCollectionEmpty(spTxCallByAnnotationFileList)) {
            return;
        }

        WriterSupportHeader writerSupportHeader = null;
        try {
            // 生成Writer
            writerSupportHeader = genWriter4Annotation();
            for (SpTxCallByAnnotationFile spTxCallByAnnotationFile : spTxCallByAnnotationFileList) {
                // 处理结果文件
                String newStackFilePath = handleStackFile(reportDirPathTxAnnotation, spTxCallByAnnotationFile);

                List<SpringTxCallAnnotationReport> springTxCallAnnotationReportList = genSpringTxCallAnnotationReport(spTxCallByAnnotationFile);
                for (SpringTxCallAnnotationReport springTxCallAnnotationReport : springTxCallAnnotationReportList) {
                    List<String> stringList = new ArrayList<>();
                    springTxCallAnnotationReport.setStackFilePath(newStackFilePath);
                    SpTxEntryMethodTxAnnotation spTxEntryMethodTxAnnotation = springTxCallAnnotationReport.getSpTxEntryMethodTxAnnotation();
                    stringList.add(spTxEntryMethodTxAnnotation.getCallFullMethod());
                    stringList.add(spTxEntryMethodTxAnnotation.getTxPropagation());
                    addColumn(stringList, springTxCallAnnotationReport.getCalleeExtractedMethod());
                    stringList.add(springTxCallAnnotationReport.getStackFilePath());

                    // 在报告文件写入一行
                    writeData4Annotation(writerSupportHeader, stringList);
                }
            }
        } catch (Exception e) {
            logger.error("error ", e);
        } finally {
            // 关闭Writer
            closeWriter4Annotation(writerSupportHeader);
        }
    }

    /**
     * 生成事务模板方式对应报告文件的Writer，可重载
     *
     * @return
     * @throws FileNotFoundException
     */
    protected WriterSupportHeader genWriter4Tpl() throws FileNotFoundException {
        String reportFilePath = genReportFilePath(FILE_NAME_SP_TX_CALL_TPL);
        return new WriterSupportHeader(JavaCGFileUtil.genBufferedWriter(reportFilePath), FILE_HEADER_TPL);
    }

    /**
     * 在事务模板方式对应报告文件中写入数据，可重载
     *
     * @param writerSupportHeader
     * @param stringList
     * @throws IOException
     */
    protected void writeData4Tpl(WriterSupportHeader writerSupportHeader, List<String> stringList) throws IOException {
        commonWriteData(writerSupportHeader, stringList);
    }

    /**
     * 关闭事务模板方式对应报告文件的Writer，可重载
     *
     * @param writerSupportHeader
     */
    protected void closeWriter4Tpl(WriterSupportHeader writerSupportHeader) {
        IOUtils.closeQuietly(writerSupportHeader);
    }

    // 处理事务模板相关文件信息
    protected void handleTxTplFileInfo(List<SpTxCallByTplFile> spTxCallByTplFileList, String reportDirPathTxTpl) {
        if (JavaCGUtil.isCollectionEmpty(spTxCallByTplFileList)) {
            return;
        }

        WriterSupportHeader writerSupportHeader = null;
        try {
            // 生成Writer
            writerSupportHeader = genWriter4Tpl();
            for (SpTxCallByTplFile spTxCallByTplFile : spTxCallByTplFileList) {
                // 处理结果文件
                String newStackFilePath = handleStackFile(reportDirPathTxTpl, spTxCallByTplFile);

                List<SpringTxCallTplReport> springTxCallTplReportList = genSpringTxCallTplReport(spTxCallByTplFile);
                for (SpringTxCallTplReport springTxCallTplReport : springTxCallTplReportList) {
                    List<String> stringList = new ArrayList<>();
                    springTxCallTplReport.setStackFilePath(newStackFilePath);
                    SpTxEntryMethodTxTpl spTxEntryMethodTxTpl = springTxCallTplReport.getSpTxEntryMethodTxTpl();
                    stringList.add(spTxEntryMethodTxTpl.getCallType().getDesc());
                    stringList.add(spTxEntryMethodTxTpl.getTxTplFullMethod());
                    stringList.add(spTxEntryMethodTxTpl.getCallerFullMethod());
                    stringList.add(String.valueOf(spTxEntryMethodTxTpl.getCallerLineNumber()));
                    addColumn(stringList, springTxCallTplReport.getCalleeExtractedMethod());
                    stringList.add(springTxCallTplReport.getStackFilePath());

                    // 在报告文件写入一行
                    writeData4Tpl(writerSupportHeader, stringList);
                }
            }
        } catch (Exception e) {
            logger.error("error ", e);
        } finally {
            // 关闭Writer
            closeWriter4Tpl(writerSupportHeader);
        }
    }

    protected List<SpringTxCallAnnotationReport> genSpringTxCallAnnotationReport(SpTxCallByAnnotationFile spTxCallByAnnotationFile) {
        List<BaseCalleeExtractedMethod> calleeExtractedMethodList = spTxCallByAnnotationFile.getCalleeExtractedMethodList();
        List<SpringTxCallAnnotationReport> springTxCallAnnotationReportList = new ArrayList<>(calleeExtractedMethodList.size());
        for (BaseCalleeExtractedMethod calleeExtractedMethod : calleeExtractedMethodList) {
            SpringTxCallAnnotationReport springTxCallAnnotationReport = new SpringTxCallAnnotationReport(spTxCallByAnnotationFile.getSpTxEntryMethodTxAnnotation(),
                    calleeExtractedMethod, spTxCallByAnnotationFile.getStackFilePath());
            springTxCallAnnotationReportList.add(springTxCallAnnotationReport);
        }
        return springTxCallAnnotationReportList;
    }

    protected List<SpringTxCallTplReport> genSpringTxCallTplReport(SpTxCallByTplFile spTxCallByTplFile) {
        List<BaseCalleeExtractedMethod> calleeExtractedMethodList = spTxCallByTplFile.getCalleeExtractedMethodList();
        List<SpringTxCallTplReport> springTxCallTplReportList = new ArrayList<>(calleeExtractedMethodList.size());
        for (BaseCalleeExtractedMethod calleeExtractedMethod : calleeExtractedMethodList) {
            SpringTxCallTplReport springTxCallTplReport = new SpringTxCallTplReport(spTxCallByTplFile.getSpTxEntryMethodTxTpl(),
                    calleeExtractedMethod, spTxCallByTplFile.getStackFilePath());
            springTxCallTplReportList.add(springTxCallTplReport);
        }
        return springTxCallTplReportList;
    }

    protected void addColumn(List<String> stringList, BaseCalleeExtractedMethod calleeExtractedMethod) {
        stringList.add(calleeExtractedMethod.getCalleeFullMethod());
        stringList.add(calleeExtractedMethod.getCalleeUpperFullMethod());
        stringList.add(JavaCGYesNoEnum.parseDesc(calleeExtractedMethod.isRunInOtherThread()));
        stringList.add(String.valueOf(calleeExtractedMethod.getDataSeq()));
        stringList.add(String.valueOf(calleeExtractedMethod.getLineNumber()));
    }
}

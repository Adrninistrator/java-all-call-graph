package com.adrninistrator.jacg.handler.spring.reporter;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.extractor.dto.common.extract.BaseCalleeExtractedMethod;
import com.adrninistrator.jacg.extractor.dto.springtx.entrymethod.SpTxEntryMethodTxAnnotation;
import com.adrninistrator.jacg.extractor.dto.springtx.entrymethod.SpTxEntryMethodTxTpl;
import com.adrninistrator.jacg.extractor.dto.springtx.extractcombined.SpTxCallCombined;
import com.adrninistrator.jacg.extractor.dto.springtx.extractfile.SpTxCallByAnnotationFile;
import com.adrninistrator.jacg.extractor.dto.springtx.extractfile.SpTxCallByTplFile;
import com.adrninistrator.jacg.extractor.entry.spring.SpringTxCallExtractor;
import com.adrninistrator.jacg.handler.common.JACGReportConstants;
import com.adrninistrator.jacg.handler.common.JACGSpReportConstants;
import com.adrninistrator.jacg.handler.dto.springtx.SpringTxCallAnnotationReport;
import com.adrninistrator.jacg.handler.dto.springtx.SpringTxCallTplReport;
import com.adrninistrator.jacg.handler.reporter.AbstractReporter;
import com.adrninistrator.jacg.writer.WriterSupportHeader;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.conf.JavaCG2ConfigureWrapper;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
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
            JACGReportConstants.COLUMN_LINE_NUMBER_IN_STACK_FILE,
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
        FILE_HEADER_ANNOTATION = StringUtils.join(fileHeaderAnnotationList, JavaCG2Constants.FLAG_TAB);

        List<String> fileHeaderTplList = new ArrayList<>();
        fileHeaderTplList.add(JACGSpReportConstants.COLUMN_TPL_TYPE);
        fileHeaderTplList.add(JACGSpReportConstants.COLUMN_TPL_CALLEE_FULL_METHOD);
        fileHeaderTplList.add(JACGSpReportConstants.COLUMN_TPL_CALLER_FULL_METHOD);
        fileHeaderTplList.add(JACGSpReportConstants.COLUMN_TPL_CALLER_LINE_NUMBER);
        fileHeaderTplList.addAll(fileHeaderCommonList);
        FILE_HEADER_TPL = StringUtils.join(fileHeaderTplList, JavaCG2Constants.FLAG_TAB);
    }

    public SpringTxCallReporter(JavaCG2ConfigureWrapper javaCG2ConfigureWrapper, ConfigureWrapper configureWrapper, String reportDirPath, boolean appendReportFile,
                                boolean copyFileInSeparateDir) {
        super(javaCG2ConfigureWrapper, configureWrapper, reportDirPath, appendReportFile, copyFileInSeparateDir);
    }

    /**
     * 选择需要使用的文件头，使用事务注解，可重载
     *
     * @return
     */
    protected String chooseFileHeader4TxAnnotation() {
        return FILE_HEADER_ANNOTATION;
    }

    /**
     * 选择需要使用的文件头，使用事务模板，可重载
     *
     * @return
     */
    protected String chooseFileHeader4TxTpl() {
        return FILE_HEADER_TPL;
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
     * 生成报告
     *
     * @return
     */
    public boolean generate() {
        SpringTxCallExtractor springTxCallExtractor = new SpringTxCallExtractor();

        // 公共预处理，包含写数据库步骤
        if (!commonPreHandle()) {
            return false;
        }

        String reportDirPathTxAnnotation = reportDirPath + File.separator + JACGSpReportConstants.DIR_NAME_SP_TX_USE_ANNOTATION;
        String reportDirPathTxTpl = reportDirPath + File.separator + JACGSpReportConstants.DIR_NAME_SP_TX_USE_TPL;
        // 尝试创建结果文件目录
        if (!JavaCG2FileUtil.isDirectoryExists(reportDirPathTxAnnotation) || !JavaCG2FileUtil.isDirectoryExists(reportDirPathTxTpl)) {
            return false;
        }

        // 查找Spring事务嵌套的调用情况
        SpTxCallCombined spTxCallCombined = springTxCallExtractor.extract(configureWrapper);
        if (!spTxCallCombined.getSpTxCallByAnnotationFileList().isSuccess()) {
            logger.error("处理事务注解失败");
            return false;
        }
        if (!spTxCallCombined.getSpTxCallByTplFileList().isSuccess()) {
            logger.error("处理事务模板失败");
            return false;
        }

        // 处理事务注解相关文件信息
        handleTxAnnotationFileInfo(spTxCallCombined.getSpTxCallByAnnotationFileList().getList(), reportDirPathTxAnnotation);
        // 处理事务模板相关文件信息
        handleTxTplFileInfo(spTxCallCombined.getSpTxCallByTplFileList().getList(), reportDirPathTxTpl);
        return true;
    }

    // 处理事务注解相关文件信息
    protected void handleTxAnnotationFileInfo(List<SpTxCallByAnnotationFile> spTxCallByAnnotationFileList, String reportDirPathTxAnnotation) {
        if (JavaCG2Util.isCollectionEmpty(spTxCallByAnnotationFileList)) {
            return;
        }

        String reportFilePath = genReportFilePath(FILE_NAME_SP_TX_CALL_ANNOTATION);
        try (WriterSupportHeader writerSupportHeader = new WriterSupportHeader(reportFilePath, chooseFileHeader4TxAnnotation(), appendReportFile)) {
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
        }
    }

    // 处理事务模板相关文件信息
    protected void handleTxTplFileInfo(List<SpTxCallByTplFile> spTxCallByTplFileList, String reportDirPathTxTpl) {
        if (JavaCG2Util.isCollectionEmpty(spTxCallByTplFileList)) {
            return;
        }

        String reportFilePath = genReportFilePath(FILE_NAME_SP_TX_CALL_TPL);
        try (WriterSupportHeader writerSupportHeader = new WriterSupportHeader(reportFilePath, chooseFileHeader4TxTpl(), appendReportFile)) {
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
        stringList.add(JavaCG2YesNoEnum.parseDesc(calleeExtractedMethod.isRunInOtherThread()));
        stringList.add(String.valueOf(calleeExtractedMethod.getDataSeq()));
        stringList.add(String.valueOf(calleeExtractedMethod.getLineNumber()));
    }
}

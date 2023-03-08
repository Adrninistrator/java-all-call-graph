package com.adrninistrator.jacg.reporter.entry.spring;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.YesNoEnum;
import com.adrninistrator.jacg.comparator.Comparator4AbstractCallGraphExtractedFile;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.extractor.common.enums.SpTxPropagationEnum;
import com.adrninistrator.jacg.extractor.common.enums.SpringTxTypeEnum;
import com.adrninistrator.jacg.extractor.dto.spring_tx.entry_method.SpTxEntryMethodTxAnnotation;
import com.adrninistrator.jacg.extractor.dto.spring_tx.entry_method.SpTxEntryMethodTxTpl;
import com.adrninistrator.jacg.extractor.dto.spring_tx.extract.SpTxCalleeInfo;
import com.adrninistrator.jacg.extractor.dto.spring_tx.extract_combined.SpTxNestedCombined;
import com.adrninistrator.jacg.extractor.dto.spring_tx.extract_file.SpTxNestedByAnnotationFile;
import com.adrninistrator.jacg.extractor.dto.spring_tx.extract_file.SpTxNestedByTplFile;
import com.adrninistrator.jacg.extractor.entry.spring.SpringTxNestedExtractor;
import com.adrninistrator.jacg.reporter.common.JACGReportConstants;
import com.adrninistrator.jacg.reporter.common.JACGSpReportConstants;
import com.adrninistrator.jacg.reporter.dto.spring_tx.SpringTxNestedAnnotationReport;
import com.adrninistrator.jacg.reporter.dto.spring_tx.SpringTxNestedTplReport;
import com.adrninistrator.jacg.reporter.entry.base.AbstractReporter;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.writer.WriterSupportHeader;
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
 * @date 2023/2/28
 * @description: 生成Spring事务嵌套的调用情况报告
 */
public class SpringTxNestedReporter extends AbstractReporter {
    private static final Logger logger = LoggerFactory.getLogger(SpringTxNestedReporter.class);

    public static final String FILE_NAME_SP_TX_NESTED_ANNOTATION = "Spring事务嵌套-使用注解@Transactional的情况.md";
    public static final String FILE_NAME_SP_TX_NESTED_TPL = "Spring事务嵌套-使用事务模板TransactionTemplate的情况.md";

    public static final String[] FILE_HEADER_COMMON = {
            "被调用" + JACGSpReportConstants.COLUMN_TX_TYPE,
            JACGReportConstants.COLUMN_CALLEE_FULL_METHOD,
            JACGReportConstants.COLUMN_CALLEE_UPPER_FULL_METHOD,
            "被调用Spring事务注解中的事务传播行为",
            "可能出现的问题（事务注解）",
            JACGReportConstants.COLUMN_RUN_IN_OTHER_THREAD,
            JACGReportConstants.COLUMN_DATA_SEQ,
            JACGReportConstants.COLUMN_LINE_NUMBER,
            JACGReportConstants.COLUMN_FILE_PATH
    };

    public static final String FILE_HEADER_ANNOTATION;
    public static final String FILE_HEADER_TPL;

    static {
        List<String> fileHeaderCommonList = Arrays.asList(FILE_HEADER_COMMON);
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

    public SpringTxNestedReporter() {
        super();
    }

    public SpringTxNestedReporter(ConfigureWrapper configureWrapper, String reportDirPath, boolean copyFileInSeparateDir, boolean skipWriteDb) {
        super(configureWrapper, reportDirPath, copyFileInSeparateDir, skipWriteDb);
    }

    /**
     * 生成报告
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

        SpringTxNestedExtractor springTxNestedExtractor = new SpringTxNestedExtractor();
        // 在需要处理的类名前缀中增加Spring事务模板类，需要在写入数据库之前设置
        springTxNestedExtractor.setAllowedClassNamePrefix(configureWrapper);

        // 公共预处理
        if (!commonPreHandle()) {
            return false;
        }

        // 查找Spring事务嵌套的调用情况
        SpTxNestedCombined spTxNestedCombined = springTxNestedExtractor.extract(configureWrapper);

        // 处理事务注解相关文件信息
        handleTxAnnotationFileInfo(spTxNestedCombined.getSpTxNestedByAnnotationFileList(), reportDirPathTxAnnotation);

        // 处理事务模板相关文件信息
        handleTxTplFileInfo(spTxNestedCombined.getSpTxNestedByTplFileList(), reportDirPathTxTpl);
        return true;
    }

    /**
     * 生成事务注解方式对应报告文件的Writer，可重载
     *
     * @return
     * @throws FileNotFoundException
     */
    protected WriterSupportHeader genWriter4Annotation() throws FileNotFoundException {
        String reportFilePath = genReportFilePath(FILE_NAME_SP_TX_NESTED_ANNOTATION);
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
    protected void handleTxAnnotationFileInfo(List<SpTxNestedByAnnotationFile> spTxNestedByAnnotationFileList, String reportDirPathTxAnnotation) {
        if (JavaCGUtil.isCollectionEmpty(spTxNestedByAnnotationFileList)) {
            return;
        }

        WriterSupportHeader writerSupportHeader = null;
        try {
            // 生成Writer
            writerSupportHeader = genWriter4Annotation();
            // 文件信息列表排序
            spTxNestedByAnnotationFileList.sort(Comparator4AbstractCallGraphExtractedFile.getInstance());
            for (SpTxNestedByAnnotationFile spTxNestedByAnnotationFile : spTxNestedByAnnotationFileList) {
                // 处理结果文件
                String newStackFilePath = handleStackFile(reportDirPathTxAnnotation, spTxNestedByAnnotationFile);

                List<SpringTxNestedAnnotationReport> springTxNestedAnnotationReportList = genSpringTxNestedAnnotationReport(spTxNestedByAnnotationFile);
                for (SpringTxNestedAnnotationReport springTxNestedAnnotationReport : springTxNestedAnnotationReportList) {
                    List<String> stringList = new ArrayList<>();
                    springTxNestedAnnotationReport.setStackFilePath(newStackFilePath);
                    SpTxEntryMethodTxAnnotation spTxEntryMethodTxAnnotation = springTxNestedAnnotationReport.getSpTxEntryMethodTxAnnotation();
                    stringList.add(spTxEntryMethodTxAnnotation.getCallFullMethod());
                    stringList.add(spTxEntryMethodTxAnnotation.getTxPropagation());
                    addColumn(stringList, springTxNestedAnnotationReport.getSpTxCalleeInfo(), spTxEntryMethodTxAnnotation.getTxPropagation());
                    stringList.add(springTxNestedAnnotationReport.getStackFilePath());

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
        String reportFilePath = genReportFilePath(FILE_NAME_SP_TX_NESTED_TPL);
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
    protected void handleTxTplFileInfo(List<SpTxNestedByTplFile> spTxNestedByTplFileList, String reportDirPathTxTpl) {
        if (JavaCGUtil.isCollectionEmpty(spTxNestedByTplFileList)) {
            return;
        }

        WriterSupportHeader writerSupportHeader = null;
        try {
            // 生成Writer
            writerSupportHeader = genWriter4Tpl();
            // 文件信息列表排序
            spTxNestedByTplFileList.sort(Comparator4AbstractCallGraphExtractedFile.getInstance());
            for (SpTxNestedByTplFile spTxNestedByAnnotationFile : spTxNestedByTplFileList) {
                // 处理结果文件
                String newStackFilePath = handleStackFile(reportDirPathTxTpl, spTxNestedByAnnotationFile);

                List<SpringTxNestedTplReport> springTxNestedTplReportList = genSpringTxNestedTplReport(spTxNestedByAnnotationFile);
                for (SpringTxNestedTplReport springTxNestedTplReport : springTxNestedTplReportList) {
                    List<String> stringList = new ArrayList<>();
                    springTxNestedTplReport.setStackFilePath(newStackFilePath);
                    SpTxEntryMethodTxTpl spTxEntryMethodTxTpl = springTxNestedTplReport.getSpTxEntryMethodTxTpl();
                    stringList.add(spTxEntryMethodTxTpl.getCallType().getDesc());
                    stringList.add(spTxEntryMethodTxTpl.getTxTplFullMethod());
                    stringList.add(spTxEntryMethodTxTpl.getCallerFullMethod());
                    stringList.add(String.valueOf(spTxEntryMethodTxTpl.getCallerLineNumber()));
                    addColumn(stringList, springTxNestedTplReport.getSpTxCalleeInfo(), null);
                    stringList.add(springTxNestedTplReport.getStackFilePath());

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

    protected List<SpringTxNestedAnnotationReport> genSpringTxNestedAnnotationReport(SpTxNestedByAnnotationFile spTxNestedByAnnotationFile) {
        List<SpTxCalleeInfo> spTxCalleeInfoList = spTxNestedByAnnotationFile.getSpTxCalleeInfoList();
        List<SpringTxNestedAnnotationReport> springTxNestedAnnotationReportList = new ArrayList<>(spTxCalleeInfoList.size());
        for (SpTxCalleeInfo spTxCalleeInfo : spTxCalleeInfoList) {
            SpringTxNestedAnnotationReport springTxNestedAnnotationReport = new SpringTxNestedAnnotationReport(spTxNestedByAnnotationFile.getSpTxEntryMethodTxAnnotation(),
                    spTxCalleeInfo, spTxNestedByAnnotationFile.getStackFilePath());
            springTxNestedAnnotationReportList.add(springTxNestedAnnotationReport);
        }
        return springTxNestedAnnotationReportList;
    }

    protected List<SpringTxNestedTplReport> genSpringTxNestedTplReport(SpTxNestedByTplFile spTxNestedByTplFile) {
        List<SpTxCalleeInfo> spTxCalleeInfoList = spTxNestedByTplFile.getSpTxCalleeInfoList();
        List<SpringTxNestedTplReport> springTxNestedTplReportList = new ArrayList<>(spTxCalleeInfoList.size());
        for (SpTxCalleeInfo spTxCalleeInfo : spTxCalleeInfoList) {
            SpringTxNestedTplReport springTxNestedTplReport = new SpringTxNestedTplReport(spTxNestedByTplFile.getSpTxEntryMethodTxTpl(),
                    spTxCalleeInfo, spTxNestedByTplFile.getStackFilePath());
            springTxNestedTplReportList.add(springTxNestedTplReport);
        }
        return springTxNestedTplReportList;
    }

    protected void addColumn(List<String> stringList, SpTxCalleeInfo spTxCalleeInfo, String txPropagation1) {
        stringList.add(spTxCalleeInfo.getSpringTxTypeEnum().getDesc());
        stringList.add(spTxCalleeInfo.getCalleeFullMethod());
        stringList.add(spTxCalleeInfo.getCalleeUpperFullMethod());
        stringList.add(spTxCalleeInfo.getTxPropagation());
        if (txPropagation1 != null && SpringTxTypeEnum.STTE_ANNOTATION == spTxCalleeInfo.getSpringTxTypeEnum()) {
            // 根据入口方法与被调用方法的事务注解中的事务传播行为生成对应的问题描述
            stringList.add(genSpTxNestedProblemDesc(txPropagation1, spTxCalleeInfo.getTxPropagation()));
        } else {
            stringList.add("");
        }
        stringList.add(YesNoEnum.parse(spTxCalleeInfo.isRunInOtherThread()));
        stringList.add(String.valueOf(spTxCalleeInfo.getDataSeq()));
        stringList.add(String.valueOf(spTxCalleeInfo.getLineNumber()));
    }

    // 根据入口方法与被调用方法的事务注解中的事务传播行为生成对应的问题描述
    protected String genSpTxNestedProblemDesc(String txPropagation1, String txPropagation2) {
        SpTxPropagationEnum spTxPropagationEnum1 = SpTxPropagationEnum.getFromPropagation(txPropagation1);
        SpTxPropagationEnum spTxPropagationEnum2 = SpTxPropagationEnum.getFromPropagation(txPropagation2);

        if (SpTxPropagationEnum.STPE_ILLEGAL == spTxPropagationEnum1 || SpTxPropagationEnum.STPE_ILLEGAL == spTxPropagationEnum2) {
            return "非法的Spring事务传播行为";
        }

        if (SpTxPropagationEnum.STPE_REQUIRED == spTxPropagationEnum1 && SpTxPropagationEnum.STPE_REQUIRED == spTxPropagationEnum2) {
            return "多个事务中的数据库操作会合并在一个事务中执行，范围可能会比预期大";
        }

        if ((SpTxPropagationEnum.STPE_REQUIRED == spTxPropagationEnum1 ||
                SpTxPropagationEnum.STPE_MANDATORY == spTxPropagationEnum1 ||
                SpTxPropagationEnum.STPE_REQUIRES_NEW == spTxPropagationEnum1) &&
                SpTxPropagationEnum.STPE_NEVER == spTxPropagationEnum2) {
            return "外层开启了事务，内层不支持事务";
        }
        return "";
    }
}

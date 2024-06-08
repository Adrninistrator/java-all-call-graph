package com.adrninistrator.jacg.handler.spring.reporter;

import com.adrninistrator.jacg.comparator.Comparator4AbstractCallGraphExtractedFile;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.extractor.common.enums.SpTxPropagationEnum;
import com.adrninistrator.jacg.extractor.common.enums.SpringTxTypeEnum;
import com.adrninistrator.jacg.extractor.dto.springtx.entrymethod.SpTxEntryMethodTxAnnotation;
import com.adrninistrator.jacg.extractor.dto.springtx.entrymethod.SpTxEntryMethodTxTpl;
import com.adrninistrator.jacg.extractor.dto.springtx.extract.SpTxCalleeInfo;
import com.adrninistrator.jacg.extractor.dto.springtx.extractcombined.SpTxNestedCombined;
import com.adrninistrator.jacg.extractor.dto.springtx.extractfile.SpTxNestedByAnnotationFile;
import com.adrninistrator.jacg.extractor.dto.springtx.extractfile.SpTxNestedByTplFile;
import com.adrninistrator.jacg.extractor.entry.spring.SpringTxNestedExtractor;
import com.adrninistrator.jacg.handler.common.JACGReportConstants;
import com.adrninistrator.jacg.handler.common.JACGSpReportConstants;
import com.adrninistrator.jacg.handler.dto.springtx.SpringTxNestedAnnotationReport;
import com.adrninistrator.jacg.handler.dto.springtx.SpringTxNestedTplReport;
import com.adrninistrator.jacg.handler.reporter.AbstractReporter;
import com.adrninistrator.jacg.writer.WriterSupportHeader;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGYesNoEnum;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
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
            JACGReportConstants.COLUMN_LINE_NUMBER_IN_STACK_FILE,
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
        FILE_HEADER_ANNOTATION = StringUtils.join(fileHeaderAnnotationList, JavaCGConstants.FLAG_TAB);

        List<String> fileHeaderTplList = new ArrayList<>();
        fileHeaderTplList.add(JACGSpReportConstants.COLUMN_TPL_TYPE);
        fileHeaderTplList.add(JACGSpReportConstants.COLUMN_TPL_CALLEE_FULL_METHOD);
        fileHeaderTplList.add(JACGSpReportConstants.COLUMN_TPL_CALLER_FULL_METHOD);
        fileHeaderTplList.add(JACGSpReportConstants.COLUMN_TPL_CALLER_LINE_NUMBER);
        fileHeaderTplList.addAll(fileHeaderCommonList);
        FILE_HEADER_TPL = StringUtils.join(fileHeaderTplList, JavaCGConstants.FLAG_TAB);
    }

    public SpringTxNestedReporter(ConfigureWrapper configureWrapper, String reportDirPath, boolean appendReportFile, boolean copyFileInSeparateDir) {
        super(configureWrapper, reportDirPath, appendReportFile, copyFileInSeparateDir);
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
     * 生成报告
     */
    public boolean generate() {
        SpringTxNestedExtractor springTxNestedExtractor = new SpringTxNestedExtractor();
        // 在需要处理的类名前缀中增加Spring事务模板类，需要在写入数据库之前设置
        springTxNestedExtractor.setAllowedClassNamePrefix(configureWrapper);

        // 公共预处理，包含写数据库步骤
        if (!commonPreHandle()) {
            return false;
        }

        String reportDirPathTxAnnotation = reportDirPath + File.separator + JACGSpReportConstants.DIR_NAME_SP_TX_USE_ANNOTATION;
        String reportDirPathTxTpl = reportDirPath + File.separator + JACGSpReportConstants.DIR_NAME_SP_TX_USE_TPL;
        // 尝试创建结果文件目录
        if (!JavaCGFileUtil.isDirectoryExists(reportDirPathTxAnnotation) || !JavaCGFileUtil.isDirectoryExists(reportDirPathTxTpl)) {
            return false;
        }

        // 查找Spring事务嵌套的调用情况
        SpTxNestedCombined spTxNestedCombined = springTxNestedExtractor.extract(configureWrapper);
        if (!spTxNestedCombined.getSpTxNestedByAnnotationFileList().isSuccess()) {
            logger.error("处理事务注解失败");
            return false;
        }
        if (!spTxNestedCombined.getSpTxNestedByTplFileList().isSuccess()) {
            logger.error("处理事务模板失败");
            return false;
        }

        // 处理事务注解相关文件信息
        handleTxAnnotationFileInfo(spTxNestedCombined.getSpTxNestedByAnnotationFileList().getList(), reportDirPathTxAnnotation);

        // 处理事务模板相关文件信息
        handleTxTplFileInfo(spTxNestedCombined.getSpTxNestedByTplFileList().getList(), reportDirPathTxTpl);
        return true;
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

    // 处理事务注解相关文件信息
    protected void handleTxAnnotationFileInfo(List<SpTxNestedByAnnotationFile> spTxNestedByAnnotationFileList, String reportDirPathTxAnnotation) {
        if (JavaCGUtil.isCollectionEmpty(spTxNestedByAnnotationFileList)) {
            return;
        }

        String reportFilePath = genReportFilePath(FILE_NAME_SP_TX_NESTED_ANNOTATION);
        try (WriterSupportHeader writerSupportHeader = new WriterSupportHeader(reportFilePath, chooseFileHeader4TxAnnotation(), appendReportFile)) {
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
        }
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

    // 处理事务模板相关文件信息
    protected void handleTxTplFileInfo(List<SpTxNestedByTplFile> spTxNestedByTplFileList, String reportDirPathTxTpl) {
        if (JavaCGUtil.isCollectionEmpty(spTxNestedByTplFileList)) {
            return;
        }

        String reportFilePath = genReportFilePath(FILE_NAME_SP_TX_NESTED_TPL);
        try (WriterSupportHeader writerSupportHeader = new WriterSupportHeader(reportFilePath, chooseFileHeader4TxTpl(), appendReportFile)) {
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
        stringList.add(JavaCGYesNoEnum.parseDesc(spTxCalleeInfo.isRunInOtherThread()));
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

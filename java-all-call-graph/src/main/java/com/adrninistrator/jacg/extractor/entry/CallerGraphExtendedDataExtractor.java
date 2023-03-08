package com.adrninistrator.jacg.extractor.entry;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.call_line.CallGraphLineParsed;
import com.adrninistrator.jacg.extractor.dto.common.extract.CallerExtractedLine;
import com.adrninistrator.jacg.extractor.dto.common.extract_file.CallerExtractedFile;
import com.adrninistrator.jacg.util.JACGCallGraphFileUtil;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2021/10/16
 * @description: 对调用链结果文件进行数据提取，向下的方法调用链，获取方法调用自定义数据
 */
public class CallerGraphExtendedDataExtractor extends CallerGraphBaseExtractor {
    private static final Logger logger = LoggerFactory.getLogger(CallerGraphExtendedDataExtractor.class);

    /**
     * 生成向下的完整调用链，根据关键字进行查找，获取方法调用自定义数据并返回
     *
     * @param extendedDataTypes 需要处理的方法调用自定义数据类型
     * @return
     */
    public List<CallerExtractedFile> extract(String... extendedDataTypes) {
        return extract(new ConfigureWrapper(), extendedDataTypes);
    }

    /**
     * 生成向下的完整调用链，根据关键字进行查找，获取方法调用自定义数据并返回
     *
     * @param configureWrapper
     * @param extendedDataTypes 需要处理的方法调用自定义数据类型
     * @return
     */
    public List<CallerExtractedFile> extract(ConfigureWrapper configureWrapper, String... extendedDataTypes) {
        if (ArrayUtils.isEmpty(extendedDataTypes)) {
            throw new JavaCGRuntimeException("未指定需要处理的方法调用自定义数据类型");
        }

        try {
            // 生成向下的方法完整调用链文件，并根据关键字生成调用堆栈文件
            List<String> stackFilePathList = genStackFiles(configureWrapper);
            if (stackFilePathList == null) {
                return null;
            }

            List<CallerExtractedFile> callerExtractedFileList = new ArrayList<>(stackFilePathList.size());
            for (String stackFilePath : stackFilePathList) {
                // 处理文件中的方法调用自定义数据
                CallerExtractedFile callerExtractedFile = handleStackFile(stackFilePath, extendedDataTypes);
                if (callerExtractedFile == null) {
                    return null;
                }
                if (!callerExtractedFile.getCallerExtractedLineList().isEmpty()) {
                    // 文件中存在指定类型的方法调用自定义数据时才添加
                    callerExtractedFileList.add(callerExtractedFile);
                }
            }
            logger.info("处理完毕");
            return callerExtractedFileList;
        } finally {
            // 关闭数据源
            closeDs();
        }
    }

    /**
     * 处理文件中的方法调用自定义数据
     *
     * @param stackFilePath     调用堆栈文件路径
     * @param extendedDataTypes 需要处理的方法调用自定义数据类型
     * @return
     */
    private CallerExtractedFile handleStackFile(String stackFilePath, String... extendedDataTypes) {
        if (StringUtils.isBlank(stackFilePath)) {
            throw new JavaCGRuntimeException("未指定文件路径");
        }

        if (ArrayUtils.isEmpty(extendedDataTypes)) {
            throw new JavaCGRuntimeException("未指定需要处理的方法调用自定义数据类型");
        }

        logger.info("处理文件中的方法调用自定义数据 {}", stackFilePath);

        int lineNumber = 0;
        String line = null;
        // 当前处理的数据序号
        int dataSeq = JACGConstants.DATA_SEQ_NONE;

        // 当前是否处理到一段数据
        boolean handleDataFlag = false;

        List<CallerExtractedLine> callerExtractedLineList = new ArrayList<>();
        try (BufferedReader br = JavaCGFileUtil.genBufferedReader(stackFilePath)) {
            String lastLine = null;
            while ((line = br.readLine()) != null) {
                lineNumber++;

                if (JACGCallGraphFileUtil.isDataSeqLine(line)) {
                    // 读取到#时，说明开始处理一段数据
                    dataSeq = JACGCallGraphFileUtil.getDataSeqFromLine(line);
                    if (dataSeq == JACGConstants.DATA_SEQ_NONE) {
                        return null;
                    }

                    // 标记开始处理数据
                    handleDataFlag = true;

                    // 清空上一行内容
                    lastLine = null;
                    continue;
                }

                if (!handleDataFlag) {
                    // 上一段数据处理完毕，还未开始处理下一段数据时，跳过
                    continue;
                }

                // 处理调用堆栈文件中的一行
                CallerExtractedLine callerExtractedLine = handleExtendedDataInLine(line, lastLine, dataSeq, lineNumber, extendedDataTypes);
                if (callerExtractedLine != null) {
                    // 当前数据处理完毕，等待后续数据进行处理
                    handleDataFlag = false;
                    // 添加当前行的自定义数据信息
                    callerExtractedLineList.add(callerExtractedLine);
                }

                // 记录上一行内容
                lastLine = line;
            }

            CallerExtractedFile callerExtractedFile = new CallerExtractedFile(callerExtractedLineList);
            // 处理调用堆栈文件信息
            fillExtractedFileInfo4Caller(stackFilePath, callerExtractedFile);
            return callerExtractedFile;
        } catch (Exception e) {
            logger.error("error 行号 {}\n{} ", lineNumber, line, e);
            return null;
        }
    }

    // 处理调用堆栈文件中的一行
    private CallerExtractedLine handleExtendedDataInLine(String line,
                                                         String lastLine,
                                                         int dataSeq,
                                                         int lineNumber,
                                                         String... extendedDataTypes) {
        if (!JACGCallGraphFileUtil.isCallGraphLine(line)) {
            // 当前行不是调用链对应的行
            return null;
        }

        // 当前行是调用链对应的行，解析当前行包含的内容
        CallGraphLineParsed callGraphLineParsed = JACGCallGraphFileUtil.parseCallGraphLine4er(line);
        if (callGraphLineParsed.getExtendedData() == null) {
            // 当前行不包含自定义数据
            return null;
        }

        if (!StringUtils.equalsAny(callGraphLineParsed.getExtendedData().getDataType(), extendedDataTypes)) {
            // 当前行包含的自定义数据不需要处理
            return null;
        }

        // 当前行包含方法调用自定义数据，且类型需要处理，进行处理
        // 生成向下的调用堆栈文件处理后对应行的信息
        return genCallerExtractedLine(line, lastLine, dataSeq, lineNumber, callGraphLineParsed, false);
    }

    @Override
    protected void handleCallStackData(int dataSeq, List<String> lineList, List<Integer> lineNumberList, boolean runInOtherThread) {
        // todo
        logger.error("实现方式待修改");
    }
}
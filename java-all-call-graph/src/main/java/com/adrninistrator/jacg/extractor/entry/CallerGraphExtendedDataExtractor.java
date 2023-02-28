package com.adrninistrator.jacg.extractor.entry;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.call_graph_result.CallGraphResultLineParsed;
import com.adrninistrator.jacg.extractor.dto.result.CallerGraphResultFileInfo;
import com.adrninistrator.jacg.extractor.dto.result.CallerGraphResultMethodInfo;
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
    public List<CallerGraphResultFileInfo> extract(String... extendedDataTypes) {
        return extract(new ConfigureWrapper(), extendedDataTypes);
    }

    /**
     * 生成向下的完整调用链，根据关键字进行查找，获取方法调用自定义数据并返回
     *
     * @param configureWrapper
     * @param extendedDataTypes 需要处理的方法调用自定义数据类型
     * @return
     */
    public List<CallerGraphResultFileInfo> extract(ConfigureWrapper configureWrapper, String... extendedDataTypes) {
        if (ArrayUtils.isEmpty(extendedDataTypes)) {
            throw new JavaCGRuntimeException("未指定需要处理的方法调用自定义数据类型");
        }

        try {
            // 生成向下的方法调用链并查找关键字，生成相关文件
            List<String> resultFilePathList = genCallGraphFiles(configureWrapper);
            if (resultFilePathList == null) {
                return null;
            }

            List<CallerGraphResultFileInfo> callerExtendedDataFileInfoList = new ArrayList<>(resultFilePathList.size());
            for (String resultFilePath : resultFilePathList) {
                // 处理文件中的方法调用自定义数据
                CallerGraphResultFileInfo callerExtendedDataFileInfo = parseExtendedDataInFile(resultFilePath, extendedDataTypes);
                if (callerExtendedDataFileInfo == null) {
                    return null;
                }
                if (!callerExtendedDataFileInfo.getCallerGraphResultMethodInfoList().isEmpty()) {
                    // 文件中存在指定类型的方法调用自定义数据时才添加
                    callerExtendedDataFileInfoList.add(callerExtendedDataFileInfo);
                }
            }
            logger.info("处理完毕");
            return callerExtendedDataFileInfoList;
        } finally {
            // 关闭数据源
            closeDs();
        }
    }

    /**
     * 处理文件中的方法调用自定义数据
     *
     * @param resultFilePath    文件路径
     * @param extendedDataTypes 需要处理的方法调用自定义数据类型
     * @return
     */
    private CallerGraphResultFileInfo parseExtendedDataInFile(String resultFilePath, String... extendedDataTypes) {
        if (StringUtils.isBlank(resultFilePath)) {
            throw new JavaCGRuntimeException("未指定文件路径");
        }

        if (ArrayUtils.isEmpty(extendedDataTypes)) {
            throw new JavaCGRuntimeException("未指定需要处理的方法调用自定义数据类型");
        }

        logger.info("处理文件中的方法调用自定义数据 {}", resultFilePath);

        int lineNumber = 0;
        String line = null;
        // 当前处理的数据序号
        int dataSeq = JACGConstants.DATA_SEQ_NONE;

        // 当前是否处理到一段数据
        boolean handleDataFlag = false;

        List<CallerGraphResultMethodInfo> callerExtendedDataInfoList = new ArrayList<>();
        try (BufferedReader br = JavaCGFileUtil.genBufferedReader(resultFilePath)) {
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

                // 处理方法完整调用链文件中的一行
                CallerGraphResultMethodInfo callerExtendedDataInfo = handleExtendedDataInLine(line, lastLine, dataSeq, lineNumber, extendedDataTypes);
                if (callerExtendedDataInfo != null) {
                    // 当前数据处理完毕，等待后续数据进行处理
                    handleDataFlag = false;
                    // 添加当前行的自定义数据信息
                    callerExtendedDataInfoList.add(callerExtendedDataInfo);
                }

                // 记录上一行内容
                lastLine = line;
            }

            CallerGraphResultFileInfo callerExtendedDataFileInfo = new CallerGraphResultFileInfo(callerExtendedDataInfoList);
            // 处理关键字搜索结果文件信息
            fillResultFileInfo4Caller(resultFilePath, callerExtendedDataFileInfo);
            return callerExtendedDataFileInfo;
        } catch (Exception e) {
            logger.error("error 行号 {}\n{} ", lineNumber, line, e);
            return null;
        }
    }

    // 处理方法完整调用链文件中的一行
    private CallerGraphResultMethodInfo handleExtendedDataInLine(String line,
                                                                 String lastLine,
                                                                 int dataSeq,
                                                                 int lineNumber,
                                                                 String... extendedDataTypes) {
        if (!JACGCallGraphFileUtil.isCallGraphLine(line)) {
            // 当前行不是调用链对应的行
            return null;
        }

        // 当前行是调用链对应的行，解析当前行包含的内容
        CallGraphResultLineParsed callGraphLineParsed = JACGCallGraphFileUtil.parseCallGraphLine4er(line);
        if (callGraphLineParsed.getExtendedData() == null) {
            // 当前行不包含自定义数据
            return null;
        }

        if (!StringUtils.equalsAny(callGraphLineParsed.getExtendedData().getDataType(), extendedDataTypes)) {
            // 当前行包含的自定义数据不需要处理
            return null;
        }

        // 当前行包含方法调用自定义数据，且类型需要处理，进行处理
        return genCallerGraphResultMethodInfo(line, lastLine, dataSeq, lineNumber, callGraphLineParsed);
    }
}
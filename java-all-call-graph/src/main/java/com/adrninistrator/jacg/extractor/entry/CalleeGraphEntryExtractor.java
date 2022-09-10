package com.adrninistrator.jacg.extractor.entry;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.extractor.dto.result.CalleeEntryMethodFile;
import com.adrninistrator.jacg.extractor.dto.result.CalleeEntryMethodInfo;
import com.adrninistrator.jacg.util.JACGCallGraphFileUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * @author adrninistrator
 * @date 2022/8/29
 * @description: 对调用链结果文件进行数据提取，向上的方法调用链，获取入口方法信息
 */
public class CalleeGraphEntryExtractor extends BaseExtractor {
    private static final Logger logger = LoggerFactory.getLogger(CalleeGraphEntryExtractor.class);

    /**
     * 生成向上的完整调用链，根据关键字进行查找，获取入口方法信息并返回
     *
     * @return
     */
    public List<CalleeEntryMethodFile> extract() {
        if (!init()) {
            return null;
        }

        // 添加额外关键字
        findKeywordCallGraph.addExtraKeyword(JACGConstants.CALLEE_FLAG_ENTRY);

        // 处理向上的方法调用链
        List<String> resultFilePathList = findKeywordCallGraph.find(true);
        if (resultFilePathList == null) {
            logger.error("生成向上的方法调用链及查找关键字失败");
            return null;
        }

        // 获取当前查找关键字对应的结果目录
        currentFindResultDirPath = findKeywordCallGraph.getCurrentDirPath();

        List<CalleeEntryMethodFile> calleeEntryMethodFileList = new ArrayList<>(resultFilePathList.size());

        for (String resultFilePath : resultFilePathList) {
            // 处理文件中的入口方法信息
            CalleeEntryMethodFile calleeEntryMethodFile = handleCallGraphResultInFile(resultFilePath);
            if (calleeEntryMethodFile == null) {
                return null;
            }

            calleeEntryMethodFileList.add(calleeEntryMethodFile);
        }

        return calleeEntryMethodFileList;
    }

    // 处理文件中的入口方法信息
    private CalleeEntryMethodFile handleCallGraphResultInFile(String filePath) {
        if (StringUtils.isBlank(filePath)) {
            logger.error("未指定文件路径");
            return null;
        }

        logger.info("处理文件中的入口方法信息 {}", filePath);

        int lineNumber = 0;

        // 当前处理的数据序号
        int dataSeq = JACGConstants.DATA_SEQ_NONE;

        // 当前是否处理到一段数据
        AtomicBoolean handleData = new AtomicBoolean(false);

        List<CalleeEntryMethodInfo> calleeEntryMethodInfoList = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(filePath), StandardCharsets.UTF_8))) {
            String line;
            List<String> lineList = new ArrayList<>(2);
            while ((line = br.readLine()) != null) {
                lineNumber++;

                if (JACGCallGraphFileUtil.isDataSeqLine(line)) {
                    // 读取到#时，说明开始处理一段数据
                    dataSeq = JACGCallGraphFileUtil.getDataSeqFromLine(line, lineNumber);
                    if (dataSeq == JACGConstants.DATA_SEQ_NONE) {
                        return null;
                    }

                    if (!lineList.isEmpty()) {
                        // 上一段数据只记录到一行，处理一个入口方法信息
                        handleOneEntryMethodInfo(lineList, dataSeq, lineNumber, calleeEntryMethodInfoList, handleData);
                    }

                    // 标记开始处理数据
                    handleData.set(true);
                    continue;
                }

                if (!handleData.get()) {
                    // 上一段数据处理完毕，还未开始处理下一段数据时，跳过
                    continue;
                }

                if (JACGCallGraphFileUtil.isCallGraphLine(line)) {
                    // 当前行为调用链数据，进行处理
                    lineList.add(line);

                    if (lineList.size() == 2) {
                        // 处理一个入口方法信息
                        handleOneEntryMethodInfo(lineList, dataSeq, lineNumber, calleeEntryMethodInfoList, handleData);
                    }
                }
            }

            if (!lineList.isEmpty()) {
                // 最后一段数据只记录到一行，处理一个入口方法信息
                handleOneEntryMethodInfo(lineList, dataSeq, lineNumber, calleeEntryMethodInfoList, handleData);
            }

            return genCallGraphFile(filePath, calleeEntryMethodInfoList);
        } catch (Exception e) {
            logger.error("error ", e);
            return null;
        }
    }


    // 处理一个入口方法信息
    private void handleOneEntryMethodInfo(List<String> lineList,
                                          int dataSeq,
                                          int lineNumber,
                                          List<CalleeEntryMethodInfo> calleeEntryMethodInfoList,
                                          AtomicBoolean handleData) {
        String line = lineList.get(0);
        CalleeEntryMethodInfo calleeEntryMethodInfo = new CalleeEntryMethodInfo();
        calleeEntryMethodInfo.setDataSeq(dataSeq);
        calleeEntryMethodInfo.setLineNumber(lineNumber);
        calleeEntryMethodInfo.setLineContent(line);
        if (lineList.size() > 1) {
            calleeEntryMethodInfo.setNextLineContent(lineList.get(1));
        }

        // 根据向上的调用链文件行内容获取调用方法
        String callerFullMethod = JACGCallGraphFileUtil.getCallerMethodFromCalleeGraph(line);
        calleeEntryMethodInfo.setFullMethod(callerFullMethod);

        calleeEntryMethodInfoList.add(calleeEntryMethodInfo);

        // 清空记录行的列表
        lineList.clear();

        // 当前数据处理完毕，等待后续数据进行处理
        handleData.set(false);
    }

    private CalleeEntryMethodFile genCallGraphFile(String filePath, List<CalleeEntryMethodInfo> calleeEntryMethodInfoList) {
        CalleeEntryMethodFile calleeEntryMethodFile = new CalleeEntryMethodFile();
        calleeEntryMethodFile.setCalleeEntryMethodInfoList(calleeEntryMethodInfoList);

        // 处理关键字搜索结果文件
        handleResultFile(filePath, calleeEntryMethodFile);
        if (calleeEntryMethodFile.isEmptyFile()) {
            return calleeEntryMethodFile;
        }

        // 根据被调用者完整方法HASH+长度，从方法调用表获取对应的完整方法
        String callerFullMethod = DbOperWrapper.getCalleeFullMethodFromHash(calleeEntryMethodFile.getMethodHash());
        calleeEntryMethodFile.setFullMethod(callerFullMethod);
        if (callerFullMethod != null) {
            calleeEntryMethodFile.setFullClassName(JACGUtil.getFullClassNameFromMethod(callerFullMethod));
        }
        return calleeEntryMethodFile;
    }
}

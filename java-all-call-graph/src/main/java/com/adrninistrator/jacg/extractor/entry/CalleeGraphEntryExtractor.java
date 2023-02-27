package com.adrninistrator.jacg.extractor.entry;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.extractor.dto.result.CalleeGraphResultFileInfo;
import com.adrninistrator.jacg.extractor.dto.result.CalleeGraphResultMethodInfo;
import com.adrninistrator.jacg.util.JACGCallGraphFileUtil;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.util.ArrayList;
import java.util.Collections;
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
    public List<CalleeGraphResultFileInfo> extract() {
        return extract(new ConfigureWrapper());
    }

    /**
     * 生成向上的完整调用链，根据关键字进行查找，获取入口方法信息并返回
     * 通过代码指定配置参数
     *
     * @param configureWrapper
     * @return
     */
    public List<CalleeGraphResultFileInfo> extract(ConfigureWrapper configureWrapper) {
        try {
            // 初始化
            init(configureWrapper);

            // 添加关键字，代表入口方法
            configureWrapper.addOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_KEYWORD_4CALLEE, Collections.singletonList(JACGConstants.CALLEE_FLAG_ENTRY));

            // 处理向上的方法调用链
            List<String> resultFilePathList = findKeywordCallGraph.find(chooseFindKeywordCallGraph(), configureWrapper);
            if (resultFilePathList == null) {
                logger.error("生成向上的方法调用链及查找关键字失败");
                return null;
            }

            // 创建数据库相关对象
            if (!genDbObject()) {
                return null;
            }

            // 记录当前查找关键字对应的结果目录
            recordCurrentFindResultDirPath();

            List<CalleeGraphResultFileInfo> calleeEntryMethodFileInfoList = new ArrayList<>(resultFilePathList.size());
            for (String resultFilePath : resultFilePathList) {
                // 处理文件中的入口方法信息
                CalleeGraphResultFileInfo calleeEntryMethodFileInfo = handleCallGraphResultInFile(resultFilePath);
                if (calleeEntryMethodFileInfo == null) {
                    return null;
                }
                calleeEntryMethodFileInfoList.add(calleeEntryMethodFileInfo);
            }
            logger.info("处理完毕");
            return calleeEntryMethodFileInfoList;
        } finally {
            // 关闭数据源
            closeDs();
        }
    }

    /**
     * 是否需要解析方法完整调用链文件中下一行的内容
     *
     * @return true: 解析 false: 不解析
     */
    protected boolean parseNextLine() {
        return false;
    }

    // 处理文件中的入口方法信息
    private CalleeGraphResultFileInfo handleCallGraphResultInFile(String resultFilePath) {
        if (StringUtils.isBlank(resultFilePath)) {
            logger.error("未指定文件路径");
            return null;
        }

        logger.info("处理文件中的入口方法信息 {}", resultFilePath);

        int lineNumber = 0;
        String line = null;
        // 当前处理的数据序号
        int dataSeq = JACGConstants.DATA_SEQ_NONE;

        // 当前是否处理到一段数据
        AtomicBoolean handleDataFlag = new AtomicBoolean(false);
        List<CalleeGraphResultMethodInfo> calleeEntryMethodInfoList = new ArrayList<>();
        try (BufferedReader br = JavaCGFileUtil.genBufferedReader(resultFilePath)) {
            List<String> lineList = new ArrayList<>(2);
            while ((line = br.readLine()) != null) {
                lineNumber++;

                if (JACGCallGraphFileUtil.isDataSeqLine(line)) {
                    // 读取到#时，说明开始处理一段数据
                    dataSeq = JACGCallGraphFileUtil.getDataSeqFromLine(line);
                    if (dataSeq == JACGConstants.DATA_SEQ_NONE) {
                        return null;
                    }

                    if (!lineList.isEmpty()) {
                        // 上一段数据只记录到一行，处理一个入口方法信息
                        handleOneEntryMethodInfo(lineList, dataSeq, lineNumber, calleeEntryMethodInfoList, handleDataFlag);
                    }

                    // 标记开始处理数据
                    handleDataFlag.set(true);
                    continue;
                }

                if (!handleDataFlag.get()) {
                    // 上一段数据处理完毕，还未开始处理下一段数据时，跳过
                    continue;
                }

                if (JACGCallGraphFileUtil.isCallGraphLine(line)) {
                    // 当前行为调用链数据，进行处理
                    lineList.add(line);

                    if (lineList.size() == 2) {
                        // 处理一个入口方法信息
                        handleOneEntryMethodInfo(lineList, dataSeq, lineNumber, calleeEntryMethodInfoList, handleDataFlag);
                    }
                }
            }

            if (!lineList.isEmpty()) {
                // 最后一段数据只记录到一行，处理一个入口方法信息
                handleOneEntryMethodInfo(lineList, dataSeq, lineNumber, calleeEntryMethodInfoList, handleDataFlag);
            }

            return genCallGraphFile(resultFilePath, calleeEntryMethodInfoList);
        } catch (Exception e) {
            logger.error("error 行号 {}\n{} ", lineNumber, line, e);
            return null;
        }
    }

    // 处理一个入口方法信息
    private void handleOneEntryMethodInfo(List<String> lineList,
                                          int dataSeq,
                                          int lineNumber,
                                          List<CalleeGraphResultMethodInfo> calleeEntryMethodInfoList,
                                          AtomicBoolean handleData) {
        String line = lineList.get(0);
        CalleeGraphResultMethodInfo calleeEntryMethodInfo = new CalleeGraphResultMethodInfo();
        calleeEntryMethodInfo.setDataSeq(dataSeq);
        calleeEntryMethodInfo.setLineNumber(lineNumber);
        calleeEntryMethodInfo.setLineContent(line);
        if (lineList.size() > 1) {
            String nextLine = lineList.get(1);
            calleeEntryMethodInfo.setNextLineContent(nextLine);
            if (parseNextLine()) {
                calleeEntryMethodInfo.setNextLineParsed(JACGCallGraphFileUtil.parseCallGraphLine4ee(nextLine));
            }
        }
        // 方法调用文件中每行解析后的内容
        calleeEntryMethodInfo.setCallGraphLineParsed(JACGCallGraphFileUtil.parseCallGraphLine4ee(line));

        calleeEntryMethodInfoList.add(calleeEntryMethodInfo);

        // 清空记录行的列表
        lineList.clear();

        // 当前数据处理完毕，等待后续数据进行处理
        handleData.set(false);
    }

    private CalleeGraphResultFileInfo genCallGraphFile(String resultFilePath, List<CalleeGraphResultMethodInfo> calleeEntryMethodInfoList) {
        CalleeGraphResultFileInfo calleeEntryMethodFileInfo = new CalleeGraphResultFileInfo(calleeEntryMethodInfoList);

        // 处理关键字搜索结果文件信息
        fillResultFileInfo4Callee(resultFilePath, calleeEntryMethodFileInfo);
        if (calleeEntryMethodFileInfo.isEmptyFile()) {
            return calleeEntryMethodFileInfo;
        }

        // 根据被调用者完整方法HASH+长度，从方法调用表获取对应的完整方法
        String callerFullMethod = dbOperWrapper.getCalleeFullMethodFromHash(calleeEntryMethodFileInfo.getMethodHash());
        calleeEntryMethodFileInfo.setFullMethod(callerFullMethod);
        if (callerFullMethod != null) {
            calleeEntryMethodFileInfo.setClassName(JACGClassMethodUtil.getClassNameFromMethod(callerFullMethod));
        }
        return calleeEntryMethodFileInfo;
    }

    @Override
    protected boolean chooseFindKeywordCallGraph() {
        // 向上
        return true;
    }
}

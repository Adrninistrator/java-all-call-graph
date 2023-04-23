package com.adrninistrator.jacg.extractor.entry;

import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.extractor.dto.common.extract.CalleeExtractedLine;
import com.adrninistrator.jacg.extractor.dto.common.extract_file.CalleeExtractedFile;
import com.adrninistrator.jacg.util.JACGCallGraphFileUtil;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/8/29
 * @description: 对调用链结果文件进行数据提取，向上的方法调用链
 */
public class CalleeGraphBaseExtractor extends BaseExtractor {
    private static final Logger logger = LoggerFactory.getLogger(CalleeGraphBaseExtractor.class);

    // 是否需要解析调用堆栈文件中下一行的内容
    private boolean parseNextLine;

    // 保存当前处理的调用堆栈文件行
    private List<CalleeExtractedLine> calleeExtractedLineList;

    /**
     * 生成向上的完整调用链，根据关键字进行查找，使用配置文件中的参数
     *
     * @return
     */
    public List<CalleeExtractedFile> baseExtract() {
        return baseExtract(new ConfigureWrapper(false));
    }

    /**
     * 生成向上的完整调用链，根据关键字进行查找，通过代码指定配置参数
     *
     * @param configureWrapper
     * @return
     */
    public List<CalleeExtractedFile> baseExtract(ConfigureWrapper configureWrapper) {
        List<String> keywordList = configureWrapper.getOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4EE, true);
        if (keywordList.isEmpty()) {
            logger.error("未在配置文件中指定生成方法调用堆栈时的搜索关键字 {}", OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4EE);
            return null;
        }

        try {
            // 根据关键字生成调用堆栈
            List<String> stackFilePathList = findStack(configureWrapper);
            if (stackFilePathList == null) {
                logger.error("生成向上的方法完整调用链文件，并根据关键字生成调用堆栈失败");
                return null;
            }

            // 创建数据库相关对象
            if (!genDbObject(configureWrapper)) {
                return null;
            }

            List<CalleeExtractedFile> calleeExtractedFileList = new ArrayList<>(stackFilePathList.size());
            for (String stackFilePath : stackFilePathList) {
                // 处理文件中包含指定关键字的方法信息
                CalleeExtractedFile calleeExtractedFile = handleStackFile(stackFilePath);
                if (calleeExtractedFile == null) {
                    return null;
                }
                calleeExtractedFileList.add(calleeExtractedFile);
            }
            logger.info("处理完毕");
            return calleeExtractedFileList;
        } finally {
            // 关闭数据源
            closeDs();
        }
    }

    // 处理调用堆栈文件中包含指定关键字的方法信息
    private CalleeExtractedFile handleStackFile(String stackFilePath) {
        calleeExtractedLineList = new ArrayList<>();

        // 解析调用堆栈文件
        if (!parseStackFilePath(stackFilePath)) {
            return null;
        }

        return genCalleeExtractedFile(stackFilePath, calleeExtractedLineList);
    }

    private CalleeExtractedFile genCalleeExtractedFile(String stackFilePath, List<CalleeExtractedLine> calleeExtractedLineList) {
        CalleeExtractedFile calleeExtractedFile = new CalleeExtractedFile(calleeExtractedLineList);

        // 处理调用堆栈结果文件信息
        fillExtractedFileInfo4Callee(stackFilePath, calleeExtractedFile);
        if (calleeExtractedFile.isEmptyStackFile()) {
            return calleeExtractedFile;
        }

        // 根据被调用者完整方法HASH+长度，从方法调用表获取对应的完整方法
        String callerFullMethod = dbOperWrapper.getCalleeFullMethodByHash(calleeExtractedFile.getMethodHash());
        calleeExtractedFile.setFullMethod(callerFullMethod);
        if (callerFullMethod != null) {
            calleeExtractedFile.setClassName(JACGClassMethodUtil.getClassNameFromMethod(callerFullMethod));
        }
        return calleeExtractedFile;
    }

    @Override
    protected boolean chooseOrder4ee() {
        return true;
    }

    @Override
    protected void handleCallStackData(int dataSeq, List<String> lineList, List<Integer> lineNumberList, boolean runInOtherThread, boolean runInTransaction) {
        // 获取包含指定关键字的方法，在每一段调用堆栈的第一行
        String line = lineList.get(0);
        int lineNumber = lineNumberList.get(0);

        CalleeExtractedLine calleeExtractedLine = new CalleeExtractedLine();
        calleeExtractedLine.setDataSeq(dataSeq);
        calleeExtractedLine.setLineNumber(lineNumber);
        calleeExtractedLine.setLineContent(line);
        if (lineList.size() > 1) {
            // 假如当前调用堆栈中的行数超过1行，则处理下一行数据
            String nextLine = lineList.get(1);
            calleeExtractedLine.setNextLineContent(nextLine);
            if (parseNextLine) {
                calleeExtractedLine.setNextLineParsed(JACGCallGraphFileUtil.parseCallGraphLine4ee(nextLine));
            }
        }
        // 方法调用文件中每行解析后的内容
        calleeExtractedLine.setCallGraphLineParsed(JACGCallGraphFileUtil.parseCallGraphLine4ee(line));
        calleeExtractedLine.setRunInOtherThread(runInOtherThread);
        calleeExtractedLine.setRunInTransaction(runInTransaction);

        calleeExtractedLineList.add(calleeExtractedLine);
    }

    public void setParseNextLine(boolean parseNextLine) {
        this.parseNextLine = parseNextLine;
    }
}

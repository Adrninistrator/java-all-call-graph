package com.adrninistrator.jacg.extractor.entry;

import com.adrninistrator.jacg.common.list.ListWithResult;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.dto.method.FullMethodWithReturnType;
import com.adrninistrator.jacg.extractor.callback.StackFileParsedCallback;
import com.adrninistrator.jacg.extractor.dto.common.extract.CalleeExtractedLine;
import com.adrninistrator.jacg.extractor.dto.common.extractfile.CalleeExtractedFile;
import com.adrninistrator.jacg.extractor.parser.StackFileParser;
import com.adrninistrator.jacg.util.JACGCallGraphFileUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/8/29
 * @description: 对向上的方法调用链文件进行数据提取，基础类
 */
public class CalleeGraphBaseExtractor extends BaseExtractor implements StackFileParsedCallback {
    private static final Logger logger = LoggerFactory.getLogger(CalleeGraphBaseExtractor.class);

    // 是否需要解析任务指定的被调用方法的直接调用方法所在行的内容
    private boolean parseDirectlyCallerLineP;

    /**
     * 生成向上的完整调用链，根据关键字进行查找，使用配置文件中的参数
     *
     * @return
     */
    public ListWithResult<CalleeExtractedFile> baseExtract() {
        return baseExtract(new ConfigureWrapper(false));
    }

    /**
     * 生成向上的完整调用链，根据关键字进行查找，通过代码指定配置参数
     *
     * @param configureWrapper
     * @return
     */
    public ListWithResult<CalleeExtractedFile> baseExtract(ConfigureWrapper configureWrapper) {
        List<String> keywordList = configureWrapper.getOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4EE);
        if (keywordList.isEmpty()) {
            logger.error("未在配置文件中指定生成方法调用堆栈时的搜索关键字 {}", OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4EE);
            return ListWithResult.genFail();
        }

        try {
            // 根据关键字生成调用堆栈
            ListWithResult<String> stackFilePathList = findStack(configureWrapper);
            if (!stackFilePathList.isSuccess()) {
                logger.error("根据关键字生成调用堆栈失败");
                return ListWithResult.genFail();
            }

            // 创建数据库相关对象
            genDbObject(configureWrapper);
            // 自定义初始化操作
            customInit();

            List<CalleeExtractedFile> calleeExtractedFileList = new ArrayList<>(stackFilePathList.getList().size());
            for (String stackFilePath : stackFilePathList.getList()) {
                // 处理文件中包含指定关键字的方法信息
                CalleeExtractedFile calleeExtractedFile = handleStackFile(stackFilePath);
                if (calleeExtractedFile == null) {
                    logger.error("处理文件中包含指定关键字的方法信息失败 {}", stackFilePath);
                    return ListWithResult.genFail();
                }
                calleeExtractedFileList.add(calleeExtractedFile);
            }
            logger.info("处理完毕");
            return new ListWithResult<>(calleeExtractedFileList);
        } finally {
            // 关闭数据源
            closeDs();
        }
    }

    /**
     * 自定义初始化操作
     */
    protected void customInit() {

    }

    /**
     * 对处理文件某行的数据进行自定义处理
     *
     * @param calleeExtractedLine
     */
    protected void customHandleCalleeExtractedLine(CalleeExtractedLine calleeExtractedLine) {

    }

    // 处理调用堆栈文件中包含指定关键字的方法信息
    private CalleeExtractedFile handleStackFile(String stackFilePath) {
        // 保存当前处理的调用堆栈文件行
        List<CalleeExtractedLine> calleeExtractedLineList = new ArrayList<>();

        // 解析调用堆栈文件
        if (!StackFileParser.parseStackFile(this, stackFilePath, calleeExtractedLineList)) {
            return null;
        }

        CalleeExtractedFile calleeExtractedFile = new CalleeExtractedFile(calleeExtractedLineList);
        // 处理调用堆栈结果文件信息
        fillExtractedFileInfo4Callee(stackFilePath, calleeExtractedFile);
        if (calleeExtractedFile.isEmptyStackFile()) {
            return calleeExtractedFile;
        }

        // 根据被调用方完整方法HASH+长度，从方法调用表获取对应的完整方法
        FullMethodWithReturnType callerMethod = methodCallHandler.queryCalleeFullMethodByHash(calleeExtractedFile.getMethodHash());
        if (callerMethod != null) {
            String callerFullMethod = callerMethod.getFullMethod();
            calleeExtractedFile.setFullMethod(callerFullMethod);
            calleeExtractedFile.setReturnType(callerMethod.getReturnType());
            calleeExtractedFile.setClassName(JavaCG2ClassMethodUtil.getClassNameFromMethod(callerFullMethod));
        }
        return calleeExtractedFile;
    }

    @Override
    protected boolean chooseOrder4ee() {
        return true;
    }

    @Override
    public void handleCallStackData(int dataSeq, List<String> lineList, List<Integer> lineNumberList, boolean runInOtherThread, boolean runInTransaction, Object... args) {
        List<CalleeExtractedLine> calleeExtractedLineList = JACGUtil.getArgAt(0, args);
        // 获取包含指定关键字的方法，在每一段调用堆栈的第一行
        String line = lineList.get(0);
        int lineNumber = lineNumberList.get(0);

        CalleeExtractedLine calleeExtractedLine = new CalleeExtractedLine();
        calleeExtractedLine.setDataSeq(dataSeq);
        calleeExtractedLine.setLineNumber(lineNumber);
        calleeExtractedLine.setLineContent(line);
        if (lineList.size() >= 2) {
            // 假如当前调用堆栈中的行数超过1行，则处理下一行数据
            String nextLine = lineList.get(lineList.size() - 2);
            calleeExtractedLine.setDirectlyCallerLineContent(nextLine);
            if (parseDirectlyCallerLineP) {
                calleeExtractedLine.setDirectlyCallerLineParsed(JACGCallGraphFileUtil.parseCallGraphLine4ee(nextLine));
            }
        }
        // 方法调用文件中每行解析后的内容
        calleeExtractedLine.setCallGraphLineParsed(JACGCallGraphFileUtil.parseCallGraphLine4ee(line));
        calleeExtractedLine.setRunInOtherThread(runInOtherThread);
        calleeExtractedLine.setRunInTransaction(runInTransaction);

        // 对处理文件某行的数据进行自定义处理
        customHandleCalleeExtractedLine(calleeExtractedLine);

        calleeExtractedLineList.add(calleeExtractedLine);
    }

    public void setParseDirectlyCallerLine(boolean parseDirectlyCallerLineP) {
        this.parseDirectlyCallerLineP = parseDirectlyCallerLineP;
    }
}

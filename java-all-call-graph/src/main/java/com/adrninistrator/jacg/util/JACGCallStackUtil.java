package com.adrninistrator.jacg.util;

import com.adrninistrator.jacg.common.list.ListWithResult;
import com.adrninistrator.jacg.dto.callstack.CalleeStackSummary;
import com.adrninistrator.jacg.extractor.callback.StackFileParsedCallback;
import com.adrninistrator.jacg.extractor.dto.common.extract.CalleeExtractedLine;
import com.adrninistrator.jacg.extractor.dto.common.extractfile.CalleeExtractedFile;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2025/5/16
 * @description: 处理调用堆栈工具类
 */
public class JACGCallStackUtil {

    private static final Logger logger = LoggerFactory.getLogger(JACGCallStackUtil.class);

    /**
     * 解析调用堆栈文件
     * 参数中的定义定义参数可在调用 handleCallStackData()方法时使用
     *
     * @param stackFileParsedCallback 处理解析后的调用堆栈文件的回调实现类
     * @param stackFilePath           调用堆栈文件路径
     * @param args                    自定义参数
     * @return
     */
    public static boolean parseStackFile(StackFileParsedCallback stackFileParsedCallback, String stackFilePath, Object... args) {
        if (stackFileParsedCallback == null) {
            logger.error("未指定 {} 实现类", StackFileParsedCallback.class.getName());
            return false;
        }

        if (StringUtils.isBlank(stackFilePath)) {
            logger.error("未指定调用堆栈文件路径");
            return false;
        }

        String stackFileParsedCallbackName = stackFileParsedCallback.getClass().getName();
        logger.info("处理调用堆栈文件 {} {}", stackFileParsedCallbackName, stackFilePath);
        // 当前处理的行号
        int lineNumber = 0;
        // 当前处理行的内容
        String line = null;
        // 调用堆栈数据对应的行号列表
        List<Integer> lineNumberList = new ArrayList<>();
        // 调用堆栈数据列表
        List<String> lineList = new ArrayList<>();
        // 当前处理的调用堆栈数据序号
        int dataSeq = 0;
        // 是否已开始处理调用堆栈数据
        boolean handleCallStack = false;
        // 是否在其他线程执行
        boolean runInOtherThread = false;
        // 是否在事务中执行
        boolean runInTransaction = false;

        try (BufferedReader br = JavaCG2FileUtil.genBufferedReader(stackFilePath)) {
            while ((line = br.readLine()) != null) {
                lineNumber++;

                if (JACGCallGraphFileUtil.isDataSeqLine(line)) {
                    // 读取到#时，说明开始处理一段数据
                    handleCallStack = true;
                    dataSeq++;
                    continue;
                }

                if (!handleCallStack) {
                    // 不处理数据，不执行后续处理
                    continue;
                }

                if (JACGCallGraphFileUtil.isMarkdownCodeLine(line) && !lineList.isEmpty()) {
                    // 当前行为markdown的代码行，且有记录调用堆栈数据列表
                    if (logger.isDebugEnabled()) {
                        StringBuilder lineAndNumber = new StringBuilder();
                        for (int i = 0; i < lineList.size(); i++) {
                            lineAndNumber.append(lineNumberList.get(i)).append(" ").append(lineList.get(i)).append("\n");
                        }
                        logger.debug("处理数据 {} {}\n{}\n{}", stackFileParsedCallbackName, stackFilePath, dataSeq, lineAndNumber);
                    }

                    // 对当前的调用堆栈数据进行处理
                    stackFileParsedCallback.handleCallStackData(dataSeq, lineList, lineNumberList, runInOtherThread, runInTransaction, args);

                    // 执行清理操作
                    lineList.clear();
                    lineNumberList.clear();
                    runInOtherThread = false;
                    runInTransaction = false;
                    continue;
                }

                if (JACGCallGraphFileUtil.isCallGraphLine(line)) {
                    // 当前行为调用堆栈数据，记录
                    lineList.add(line);
                    lineNumberList.add(lineNumber);

                    // 判断是否在其他线程执行
                    if (JACGCallGraphFileUtil.checkRunInOtherThread(line)) {
                        runInOtherThread = true;
                    }
                    // 判断是否在事务中执行
                    if (JACGCallGraphFileUtil.checkRunInTransaction(line)) {
                        runInTransaction = true;
                    }
                }
            }

            return true;
        } catch (Exception e) {
            logger.error("处理失败 {} 行号: {}\n行内容: [{}] ", stackFileParsedCallbackName, lineNumber, line, e);
            return false;
        }
    }

    /**
     * 生成向上的方法调用堆栈一行的字符串
     * 与下面的方法需要同步修改
     *
     * @param calleeStackSummary
     * @return
     */
    public static String formatCalleeStackSummaryLine(CalleeStackSummary calleeStackSummary) {
        return StringUtils.joinWith(
                JavaCG2Constants.FLAG_TAB,
                calleeStackSummary.getStackSeq(),
                calleeStackSummary.getCalleeMethod(),
                calleeStackSummary.getUpwardCallerMethod(),
                calleeStackSummary.getKeywordMethod(),
                calleeStackSummary.getKeywordMethodReturnType()
        );
    }

    /**
     * 通过一行的字符串解析向上的方法调用堆栈
     * 与上面的方法需要同步修改
     *
     * @param line
     * @return
     */
    public static CalleeStackSummary parseCalleeStackSummaryFromLine(String line) {
        String[] array = StringUtils.splitPreserveAllTokens(line, JavaCG2Constants.FLAG_TAB);
        String stackSeq = array[0];
        String calleeMethod = array[1];
        String upwardCallerMethod = array[2];
        String keywordMethod = array[3];
        String keywordMethodReturnType = array[4];
        return new CalleeStackSummary(stackSeq, calleeMethod, upwardCallerMethod, keywordMethod, keywordMethodReturnType);
    }

    /**
     * 通过对方法调用堆栈提取的结果，获得所有的入口完整方法
     *
     * @param listWithResult
     * @return
     */
    public static Set<String> getAllEntryFullMethod(ListWithResult<CalleeExtractedFile> listWithResult) {
        Set<String> entryFullMethodSet = new HashSet<>();
        if (!listWithResult.isSuccess()) {
            return entryFullMethodSet;
        }

        for (CalleeExtractedFile calleeExtractedFile : listWithResult.getList()) {
            for (CalleeExtractedLine calleeExtractedLine : calleeExtractedFile.getCalleeExtractedLineList()) {
                String fullMethod = calleeExtractedLine.getCallGraphLineParsed().getMethodDetail().getFullMethod();
                entryFullMethodSet.add(fullMethod);
            }
        }
        return entryFullMethodSet;
    }

    private JACGCallStackUtil() {
        throw new IllegalStateException("illegal");
    }
}

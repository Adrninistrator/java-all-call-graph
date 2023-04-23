package com.adrninistrator.jacg.extractor.entry;

import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.call_line.CallGraphLineParsed;
import com.adrninistrator.jacg.extractor.dto.common.extract.CallerExtractedLine;
import com.adrninistrator.jacg.extractor.dto.common.extract_file.CallerExtractedFile;
import com.adrninistrator.jacg.util.JACGCallGraphFileUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/1/28
 * @description: 对调用链结果文件进行数据提取，向下的方法调用链，基础类
 */
public class CallerGraphBaseExtractor extends BaseExtractor {
    private static final Logger logger = LoggerFactory.getLogger(CallerGraphBaseExtractor.class);

    // 保存当前处理的调用堆栈文件行
    protected List<CallerExtractedLine> callerExtractedLineList;

    /**
     * 生成向下的完整调用链，根据关键字进行查找，获取调用链结果文件信息并返回，使用配置文件中的参数
     *
     * @return
     */
    public List<CallerExtractedFile> baseExtract() {
        return baseExtract(new ConfigureWrapper(false), true);
    }

    /**
     * 生成向下的完整调用链，根据关键字进行查找，获取调用链结果文件信息并返回，使用代码指定的参数
     *
     * @param configureWrapper
     * @return
     */
    public List<CallerExtractedFile> baseExtract(ConfigureWrapper configureWrapper) {
        return baseExtract(configureWrapper, true);
    }

    /**
     * 生成向下的完整调用链，根据关键字进行查找，获取调用链结果文件信息并返回
     *
     * @param configureWrapper
     * @param needCloseDs      是否需要在执行完毕时关闭数据源
     * @return
     */
    protected List<CallerExtractedFile> baseExtract(ConfigureWrapper configureWrapper, boolean needCloseDs) {
        List<String> keywordList = configureWrapper.getOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4ER, true);
        if (keywordList.isEmpty()) {
            logger.error("未在配置文件中指定生成方法调用堆栈时的搜索关键字 {}", OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4ER);
            return null;
        }

        try {
            // 生成向下的方法完整调用链文件，并根据关键字生成调用堆栈文件
            List<String> stackFilePathList = genStackFiles(configureWrapper);
            if (stackFilePathList == null) {
                return null;
            }

            List<CallerExtractedFile> callerExtractedFileList = new ArrayList<>(stackFilePathList.size());
            for (String stackFilePath : stackFilePathList) {
                // 处理调用堆栈文件中的方法信息
                CallerExtractedFile callerExtractedFile = handleStackFile(stackFilePath);
                if (callerExtractedFile == null) {
                    return null;
                }
                callerExtractedFileList.add(callerExtractedFile);
            }
            logger.info("处理完毕");
            return callerExtractedFileList;
        } finally {
            if (needCloseDs) {
                // 关闭数据源
                closeDs();
            }
        }
    }

    // 生成向下的方法完整调用链文件，并根据关键字生成调用堆栈文件
    protected List<String> genStackFiles(ConfigureWrapper configureWrapper) {
        // 根据关键字生成调用堆栈
        List<String> stackFilePathList = findStack(configureWrapper);
        if (stackFilePathList == null) {
            logger.error("生成向下的方法完整调用链文件，并根据关键字生成调用堆栈失败");
            return null;
        }

        // 创建数据库相关对象
        if (!genDbObject(configureWrapper)) {
            return null;
        }

        return stackFilePathList;
    }

    // 处理调用堆栈文件中的方法信息
    // 生成向下的调用堆栈文件处理后对应行的信息
    protected CallerExtractedFile handleStackFile(String stackFilePath) {
        callerExtractedLineList = new ArrayList<>();

        // 解析调用堆栈文件
        if (!parseStackFilePath(stackFilePath)) {
            return null;
        }

        CallerExtractedFile callerExtractedFile = new CallerExtractedFile(callerExtractedLineList);
        // 处理调用堆栈文件信息
        fillExtractedFileInfo4Caller(stackFilePath, callerExtractedFile);
        return callerExtractedFile;
    }

    protected CallerExtractedLine genCallerExtractedLine(String line,
                                                         String lastLine,
                                                         int dataSeq,
                                                         int lineNumber,
                                                         CallGraphLineParsed callGraphLineParsed,
                                                         boolean runInOtherThread,
                                                         boolean runInTransaction) {
        CallerExtractedLine callerExtractedLine = new CallerExtractedLine();
        callerExtractedLine.setLastLineContent(lastLine);
        callerExtractedLine.setLastLineParsed(JACGCallGraphFileUtil.parseCallGraphLine4er(lastLine));

        callerExtractedLine.setDataSeq(dataSeq);
        callerExtractedLine.setLineNumber(lineNumber);
        callerExtractedLine.setLineContent(line);
        if (callGraphLineParsed != null) {
            callerExtractedLine.setCallGraphLineParsed(callGraphLineParsed);
        } else {
            callerExtractedLine.setCallGraphLineParsed(JACGCallGraphFileUtil.parseCallGraphLine4er(line));
        }
        callerExtractedLine.setRunInOtherThread(runInOtherThread);
        callerExtractedLine.setRunInTransaction(runInTransaction);
        return callerExtractedLine;
    }

    @Override
    protected boolean chooseOrder4ee() {
        return false;
    }

    @Override
    protected void handleCallStackData(int dataSeq, List<String> lineList, List<Integer> lineNumberList, boolean runInOtherThread, boolean runInTransaction) {
        // 获取调用堆栈最后一条记录的下标
        int listMaxIndex = lineList.size() - 1;
        String lastLine = null;
        if (lineList.size() > 1) {
            // 调用堆栈数据大于1条，处理上一行的数据
            lastLine = lineList.get(listMaxIndex - 1);
        }

        CallerExtractedLine callerExtractedLine = genCallerExtractedLine(lineList.get(listMaxIndex), lastLine, dataSeq, lineNumberList.get(listMaxIndex), null, runInOtherThread,
                runInTransaction);
        callerExtractedLineList.add(callerExtractedLine);
    }
}
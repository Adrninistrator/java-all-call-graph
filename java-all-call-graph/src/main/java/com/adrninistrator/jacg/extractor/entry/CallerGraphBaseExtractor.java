package com.adrninistrator.jacg.extractor.entry;

import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.call_graph_result.CallGraphResultLineParsed;
import com.adrninistrator.jacg.extractor.dto.result.CallerGraphResultFileInfo;
import com.adrninistrator.jacg.extractor.dto.result.CallerGraphResultMethodInfo;
import com.adrninistrator.jacg.util.JACGCallGraphFileUtil;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/1/28
 * @description: 对调用链结果文件进行数据提取，向下的方法调用链，基础类
 */
public class CallerGraphBaseExtractor extends BaseExtractor {
    private static final Logger logger = LoggerFactory.getLogger(CallerGraphBaseExtractor.class);

    /**
     * 生成向下的完整调用链，根据关键字进行查找，获取调用链结果文件信息并返回
     *
     * @return
     */
    public List<CallerGraphResultFileInfo> baseExtract() {
        return baseExtract(new ConfigureWrapper());
    }

    /**
     * 生成向下的完整调用链，根据关键字进行查找，获取调用链结果文件信息并返回
     *
     * @param configureWrapper
     * @return
     */
    public List<CallerGraphResultFileInfo> baseExtract(ConfigureWrapper configureWrapper) {
        try {
            // 生成向下的方法调用链并查找关键字，生成相关文件
            List<String> resultFilePathList = genCallGraphFiles(configureWrapper);
            if (resultFilePathList == null) {
                return null;
            }

            List<CallerGraphResultFileInfo> callGraphResultFileInfoList = new ArrayList<>(resultFilePathList.size());
            for (String resultFilePath : resultFilePathList) {
                // 处理关键字搜索结果文件中的方法信息
                CallerGraphResultFileInfo callGraphResultFileInfo = parseMethodInfoInFile(resultFilePath);
                if (callGraphResultFileInfo == null) {
                    return null;
                }
                callGraphResultFileInfoList.add(callGraphResultFileInfo);
            }
            logger.info("处理完毕");
            return callGraphResultFileInfoList;
        } finally {
            // 关闭数据源
            closeDs();
        }
    }

    /**
     * 是否需要解析方法完整调用链文件中上一行的内容
     *
     * @return true: 解析 false: 不解析
     */
    protected boolean parseLastLine() {
        return false;
    }

    // 生成向下的方法调用链并查找关键字，生成相关文件
    protected List<String> genCallGraphFiles(ConfigureWrapper configureWrapper) {
        // 初始化
        init(configureWrapper);

        if (!confInfo.isMultiImplGenInCurrentFile()) {
            logger.warn("若接口或父类存在多个实现类或子类，接口或父类方法调用多个实现类或子类方法的调用关系生成在当前文件中 {}", ConfigKeyEnum.CKE_MULTI_IMPL_GEN_IN_CURRENT_FILE.getKey());
            confInfo.setMultiImplGenInCurrentFile(false);
        }

        // 处理向下的方法调用链
        List<String> resultFilePathList = findKeywordCallGraph.find(chooseFindKeywordCallGraph(), configureWrapper);
        if (resultFilePathList == null) {
            logger.error("生成向下的方法调用链及查找关键字失败");
            return null;
        }

        // 创建数据库相关对象
        if (!genDbObject()) {
            return null;
        }

        // 记录当前查找关键字对应的结果目录
        recordCurrentFindResultDirPath();
        return resultFilePathList;
    }

    // 处理关键字搜索结果文件中的方法信息
    protected CallerGraphResultFileInfo parseMethodInfoInFile(String resultFilePath) {
        logger.info("处理关键字搜索结果文件中的方法信息 {}", resultFilePath);

        int lineNumber = 0;
        String line = null;

        List<CallerGraphResultMethodInfo> callerGraphResultMethodInfoList = new ArrayList<>();
        try (BufferedReader br = JavaCGFileUtil.genBufferedReader(resultFilePath)) {
            // 当前处理的数据序号
            int dataSeq = 0;
            // 上一行的数据
            String lastLine = null;
            // 上一行的上一行的数据
            String lastLine2 = null;
            while ((line = br.readLine()) != null) {
                lineNumber++;

                if (JACGCallGraphFileUtil.isDataSeqLine(line)) {
                    // 读取到#时，说明开始处理一段数据
                    dataSeq++;

                    // 清空上一行内容
                    lastLine = null;
                    lastLine2 = null;
                    continue;
                }

                if (JACGCallGraphFileUtil.isCallGraphLine(line)) {
                    // 当前行为调用链数据
                    // 记录上一行内容
                    lastLine2 = lastLine;
                    lastLine = line;
                    continue;
                }

                if (JACGCallGraphFileUtil.isMarkdownCodeLine(line) && lastLine != null) {
                    // 当前行为markdown的代码行，上一行即为需要处理的行
                    CallerGraphResultMethodInfo callerGraphResultMethodInfo = genCallerGraphResultMethodInfo(lastLine, lastLine2, dataSeq, lineNumber, null);
                    callerGraphResultMethodInfoList.add(callerGraphResultMethodInfo);
                }
            }

            CallerGraphResultFileInfo callerGraphResultFileInfo = new CallerGraphResultFileInfo(callerGraphResultMethodInfoList);
            // 处理关键字搜索结果文件信息
            fillResultFileInfo4Caller(resultFilePath, callerGraphResultFileInfo);
            return callerGraphResultFileInfo;
        } catch (Exception e) {
            logger.error("error 行号 {}\n{} ", lineNumber, line, e);
            return null;
        }
    }

    protected CallerGraphResultMethodInfo genCallerGraphResultMethodInfo(String line, String lastLine, int dataSeq, int lineNumber, CallGraphResultLineParsed callGraphLineParsed) {
        CallerGraphResultMethodInfo callerGraphResultMethodInfo = new CallerGraphResultMethodInfo();
        callerGraphResultMethodInfo.setLastLineContent(lastLine);
        if (parseLastLine()) {
            callerGraphResultMethodInfo.setLastLineParsed(JACGCallGraphFileUtil.parseCallGraphLine4er(lastLine));
        }

        callerGraphResultMethodInfo.setDataSeq(dataSeq);
        callerGraphResultMethodInfo.setLineNumber(lineNumber);
        callerGraphResultMethodInfo.setLineContent(line);
        if (callGraphLineParsed != null) {
            callerGraphResultMethodInfo.setCallGraphLineParsed(callGraphLineParsed);
        } else {
            callerGraphResultMethodInfo.setCallGraphLineParsed(JACGCallGraphFileUtil.parseCallGraphLine4er(line));
        }
        return callerGraphResultMethodInfo;
    }

    @Override
    protected boolean chooseFindKeywordCallGraph() {
        // 向下
        return false;
    }
}
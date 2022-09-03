package com.adrninistrator.jacg.find_keyword;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.keyword.FileContentNode;
import com.adrninistrator.jacg.extensions.find_filter.BaseFindKeywordFilter;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.jacg.runner.base.AbstractRunnerGenCallGraph;
import com.adrninistrator.jacg.util.JACGCallGraphFileUtil;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.jacg.writer.WriterSupportHeaderAndSkip;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2021/7/29
 * @description:
 */

public class FindKeywordCallGraph {

    private static final Logger logger = LoggerFactory.getLogger(FindKeywordCallGraph.class);

    public static final String GEN_FOR_CALLER_SUPPORT_IGNORE_KEY = "gen_for_caller_support_ignore";

    // 记录当前处理的目录
    private String currentDirPath;

    // 处理目录时，需要返回生成文件路径列表
    private boolean returnResultFileListKey = false;

    // 关键字搜索自定义过滤处理类
    private BaseFindKeywordFilter baseFindKeywordFilter;

    // 额外指定的关键字
    private final List<String> extraKeywordList = new ArrayList<>();

    // 读取文件内容时，上一行的节点
    private FileContentNode lastNode;

    // 设置生成向下的完整方法调用链时，支持忽略特定的包名、类、方法
    public static void setGenForCallerSupportIgnore() {
        System.setProperty(GEN_FOR_CALLER_SUPPORT_IGNORE_KEY, "1");
    }

    /**
     * 在生成的方法调用链文件中搜索指定关键字
     *
     * @param order4ee true: 处理向上的方法调用链 false: 处理向下的方法调用链
     * @return 生成的搜索结果文件的完整路径列表
     */
    public List<String> find(boolean order4ee) {
        // 先生成对应的方法完整调用链，再对生成目录的文件根据关键字生成到起始方法的调用链

        // 读取指定的关键字
        OtherConfigFileUseListEnum otherConfigFileUseListEnum = order4ee ? OtherConfigFileUseListEnum.OCFULE_FIND_KEYWORD_4CALLEE :
                OtherConfigFileUseListEnum.OCFULE_FIND_KEYWORD_4CALLER;
        String keywordConfigFilePath = otherConfigFileUseListEnum.getFileName();

        List<String> configKeywordList = ConfigureWrapper.getOtherConfigList(otherConfigFileUseListEnum);
        if (JACGUtil.isCollectionEmpty(configKeywordList)) {
            logger.error("请在配置文件中指定需要生成到起始方法之间调用链的关键字 {}", keywordConfigFilePath);
            return null;
        }

        // 这里需要新创建可写的List，从配置中获取的List可能是不可写的
        List<String> keywordList = new ArrayList<>(configKeywordList.size());
        for (String configKeyword : configKeywordList) {
            if (StringUtils.isNotBlank(configKeyword) &&
                    !StringUtils.startsWith(configKeyword, JACGConstants.FLAG_HASHTAG)
                    && !keywordList.contains(configKeyword)) {
                // 配置文件中被注释的行不处理，避免重复添加
                keywordList.add(configKeyword);
            }
        }

        // 添加额外关键字
        for (String extraKeyword : extraKeywordList) {
            if (StringUtils.isNotBlank(extraKeyword) && !keywordList.contains(extraKeyword)) {
                // 避免重复添加
                keywordList.add(extraKeyword);
            }
        }

        if (keywordList.isEmpty()) {
            logger.error("请在配置文件中指定需要生成到起始方法之间调用链的合法关键字 {}", keywordConfigFilePath);
            return null;
        }

        // 生成方法完整调用链
        AbstractRunnerGenCallGraph runnerGenCallGraph;

        if (order4ee) {
            runnerGenCallGraph = new RunnerGenAllGraph4Callee();
        } else {
            runnerGenCallGraph = new RunnerGenAllGraph4Caller();

            if (System.getProperty(GEN_FOR_CALLER_SUPPORT_IGNORE_KEY) != null) {
                ((RunnerGenAllGraph4Caller) runnerGenCallGraph).setSupportIgnore(true);
            }
        }

        boolean success = runnerGenCallGraph.run();
        String outputPath = runnerGenCallGraph.getSuccessOutputDir();

        if (!success || outputPath == null) {
            logger.error("生成方法完整调用链失败，请检查");
            return null;
        }

        // 处理目录
        return handleDir(outputPath, keywordList, order4ee);
    }

    // 返回当前处理的目录
    public String getCurrentDirPath() {
        return currentDirPath;
    }

    public void setBaseFindKeywordFilter(BaseFindKeywordFilter baseFindKeywordFilter) {
        this.baseFindKeywordFilter = baseFindKeywordFilter;
    }

    public void addExtraKeyword(String extraKeyword) {
        extraKeywordList.add(extraKeyword);
    }

    public void addExtraKeywords(String... extraKeyword) {
        extraKeywordList.addAll(Arrays.asList(extraKeyword));
    }

    // 设置处理目录时，需要返回生成文件路径列表
    public void setReturnResultFileList() {
        returnResultFileListKey = true;
    }

    private String genHeaderInfo(String filePath, List<String> keywordList, boolean order4ee) {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("- 处理文件: ").append(filePath).append(JACGConstants.NEW_LINE);

        if (!order4ee) {
            stringBuilder.append("- 查看方法向下调用链时使用，按层级增大方向打印").append(JACGConstants.NEW_LINE);
        } else {
            stringBuilder.append("- 查看方法向上调用链时使用，按层级减小方向打印").append(JACGConstants.NEW_LINE);
        }

        stringBuilder.append("- 查找关键字: ").append(JACGConstants.NEW_LINE).append(JACGConstants.FLAG_MD_CODE).append(JACGConstants.NEW_LINE);
        for (String keyword : keywordList) {
            stringBuilder.append(keyword).append(JACGConstants.NEW_LINE);
        }
        stringBuilder.append(JACGConstants.FLAG_MD_CODE);

        return stringBuilder.toString();
    }

    // 处理目录
    private List<String> handleDir(String srcDirPath, List<String> keywordList, boolean order4ee) {
        // 目录路径后增加分隔符
        String finalSrcDirPath = JACGUtil.addSeparator4FilePath(srcDirPath);

        Set<String> subDirPathSet = new HashSet<>();
        List<String> subFilePathList = new ArrayList<>();

        // 从目录中查找需要处理的文件
        JACGFileUtil.searchDir(finalSrcDirPath, subDirPathSet, subFilePathList, JACGConstants.EXT_TXT);

        if (subFilePathList.isEmpty()) {
            logger.error("{} 目录中未找到后缀为[{}]的文件", finalSrcDirPath, JACGConstants.EXT_TXT);
            return null;
        }

        // 记录当前处理的目录
        currentDirPath = finalSrcDirPath + JACGConstants.DIR_OUTPUT_FIND_KEYWORD;
        if (!JACGFileUtil.isDirectoryExists(currentDirPath)) {
            return null;
        }

        // 未搜索到关键字的文件保存目录
        String keyWordsNotFoundDirPath = currentDirPath + File.separator + JACGConstants.KEYWORDS_NOT_FOUND_DIR;

        // txt文件所在目录字符串长度，用于后续截取
        int finalSrcDirPathLength = finalSrcDirPath.length();

        // 创建md文件需要保存的目录
        for (String subDirPath : subDirPathSet) {
            if (subDirPath.equals(finalSrcDirPath)) {
                // 当前结果目录不需要处理
                continue;
            }

            String subDirName = subDirPath.substring(finalSrcDirPathLength);
            String newDirPath = currentDirPath + File.separator + subDirName;
            if (!JACGFileUtil.isDirectoryExists(newDirPath)) {
                return null;
            }
        }

        for (String subFilePath : subFilePathList) {
            String subFileName = JACGUtil.getFileNameFromPath(subFilePath);
            // 跳过合并文件，跳过映射文件
            if (StringUtils.startsWithAny(subFileName, JACGConstants.COMBINE_FILE_NAME_PREFIX, JACGConstants.FILE_MAPPING_NAME)) {
                continue;
            }

            logger.info("处理文件: {}", subFilePath);
            handleOneFile(finalSrcDirPathLength, keyWordsNotFoundDirPath, subFilePath, keywordList, order4ee);
        }

        if (!returnResultFileListKey) {
            // 不需要返回生成文件路径列表
            return null;
        }

        // 返回生成的结果文件路径列表
        List<String> mdFilePathList = new ArrayList<>();

        // 在生成.md文件的目录中搜索（合并文件在上面不会搜索，这里也不需要跳过，COMBINE_FILE_NAME_PREFIX = "_all"）
        JACGFileUtil.searchDir(currentDirPath, null, mdFilePathList, JACGConstants.EXT_MD);

        List<String> finalMdFilePathList = new ArrayList<>(mdFilePathList.size());
        for (String mdFilePath : mdFilePathList) {
            // 获取文件所在目录名
            String mdFileDirName = JACGUtil.getFileParentDirName(mdFilePath);

            if (JACGConstants.KEYWORDS_NOT_FOUND_DIR.equals(mdFileDirName)) {
                // 跳过未找到关键字的文件
                continue;
            }

            if (order4ee && !JACGConstants.DIR_OUTPUT_METHODS.equals(mdFileDirName)) {
                // 查找向上方法调用链文件时，对于不在方法目录中的文件跳过
                continue;
            }

            finalMdFilePathList.add(mdFilePath);
        }

        return finalMdFilePathList;
    }

    private boolean handleOneFile(int srcDirPathLength, String keyWordsNotFoundDirPath, String txtFilePath, List<String> keywordList, boolean order4ee) {
        // 获取txt文件去掉所在目录之后的文件名，可能包含中间的目录名
        String txtFileName = txtFilePath.substring(srcDirPathLength);
        String txtFileNameWithOutExt = JACGUtil.getFileNameWithOutExt(txtFileName);
        String mdFilePath = currentDirPath + File.separator + txtFileNameWithOutExt + JACGConstants.EXT_MD;

        // 生成文件头
        String headerInfo = genHeaderInfo(txtFilePath, keywordList, order4ee);
        try (BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream(txtFilePath), StandardCharsets.UTF_8));
             WriterSupportHeaderAndSkip out = WriterSupportHeaderAndSkip.genWriterSupportHeaderAndSkip(mdFilePath, headerInfo)) {
            String line;
            lastNode = null;
            StringBuilder foundCallGraph = new StringBuilder();
            // 文件行号
            int lineNum = 0;
            // 数据序号
            int dataSeq = 0;
            // 记录每个方法级别最后处理的文件内容节点
            List<FileContentNode> fileContentNodeList = new ArrayList<>(20);

            while ((line = in.readLine()) != null) {
                lineNum++;
                // 处理txt文件的一行
                if (!handleTxtFileOneLine(line, keywordList, order4ee, foundCallGraph, fileContentNodeList)) {
                    return false;
                }

                if (foundCallGraph.length() > 0) {
                    // 写入查找到的调用链
                    dataSeq++;
                    String writeData = JACGConstants.NEW_LINE + JACGConstants.NEW_LINE + JACGConstants.FLAG_HASHTAG + JACGConstants.FLAG_SPACE + dataSeq + JACGConstants.FLAG_DOT +
                            JACGConstants.FLAG_MD_LINE_NUMBER + lineNum + JACGConstants.NEW_LINE +
                            JACGConstants.FLAG_MD_CODE + JACGConstants.NEW_LINE;

                    out.write(writeData);
                    out.write(foundCallGraph.toString());
                    out.write(JACGConstants.FLAG_MD_CODE);

                    foundCallGraph.setLength(0);
                }
            }

            if (out.isFirstTime()) {
                // 未写入文件内容，在对应目录中生成空文件
                JACGFileUtil.createNewFile(keyWordsNotFoundDirPath + File.separator + txtFileNameWithOutExt + JACGConstants.EXT_MD);
            }

            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    // 处理txt文件的一行
    private boolean handleTxtFileOneLine(String line, List<String> keywordList, boolean order4ee, StringBuilder foundCallGraph, List<FileContentNode> fileContentNodeList) {
        if (!JACGCallGraphFileUtil.isCallGraphLine(line)) {
            // 不属于调用链信息的行，不处理
            // 对于向上的方法调用链，类对应的文件中包含多个方法的信息，需要将上次处理的数据清空
            lastNode = null;
            fileContentNodeList.clear();
            return true;
        }

        // 处理调用链信息的行
        // 获取方法级别
        int methodLevel = JACGCallGraphFileUtil.getCallGraphMethodLevel(line);
        if (methodLevel == JACGConstants.CALL_GRAPH_METHOD_LEVEL_ILLEGAL) {
            return false;
        }

        if (lastNode == null) {
            // 还未处理过文件内容节点
            if (methodLevel != JACGConstants.CALL_GRAPH_METHOD_LEVEL_START) {
                logger.error("还未处理过文件内容节点，方法级别非法 {} {}", methodLevel, line);
                return false;
            }

            // 根文件内容节点
            lastNode = FileContentNode.genInstance(null, JACGConstants.CALL_GRAPH_METHOD_LEVEL_START, line);

            // 记录每个方法级别最后处理的文件内容节点
            recordFileContentNodeInList(methodLevel, fileContentNodeList);

            // 生成当前节点到根节点的调用链
            genCallGraph(line, keywordList, foundCallGraph, order4ee);
            return true;
        }

        // 已经处理过文件内容节点
        if (methodLevel == JACGConstants.CALL_GRAPH_METHOD_LEVEL_START) {
            logger.error("已经处理过文件内容节点，方法级别非法 {} {}", methodLevel, line);
            return false;
        }

        FileContentNode parentNode;
        if (lastNode.getMethodLevel() == methodLevel) {
            // 上一行与当前行方法级别相同，父节点为上一行的父节点
            parentNode = lastNode.getParentNode();
        } else if (lastNode.getMethodLevel() < methodLevel) {
            // 上一行小于当前行方法级别，父节点为上一行
            parentNode = lastNode;
        } else {
            // 上一行大于当前行方法级别，父节点为上一方法级别最后处理的文件内容节点
            parentNode = fileContentNodeList.get(methodLevel - 1);
        }

        // 记录当前的文件内容节点
        lastNode = FileContentNode.genInstance(parentNode, methodLevel, line);

        // 记录每个方法级别最后处理的文件内容节点
        recordFileContentNodeInList(methodLevel, fileContentNodeList);

        // 生成当前节点到根节点的调用链
        genCallGraph(line, keywordList, foundCallGraph, order4ee);
        return true;
    }

    // 记录每个方法级别最后处理的文件内容节点
    private void recordFileContentNodeInList(int methodLevel, List<FileContentNode> fileContentNodeList) {
        if (fileContentNodeList.size() < methodLevel + 1) {
            // List中指定位置数据不存在则增加
            fileContentNodeList.add(lastNode);
            return;
        }

        // List中指定位置数据已存在则设置
        fileContentNodeList.set(methodLevel, lastNode);
    }

    // 生成当前节点到根节点的调用链
    private void genCallGraph(String line, List<String> keywordList, StringBuilder foundCallGraph, boolean order4ee) {
        // 查找关键字
        boolean containsKeyword = false;

        for (String keyword : keywordList) {
            /*
                使用配置文件中的关键字进行判断
                再判断是否有指定关键字搜索自定义过滤处理类，若有指定则调用
             */
            if (line.contains(keyword) &&
                    (baseFindKeywordFilter == null || baseFindKeywordFilter.filter(keyword, line))) {
                containsKeyword = true;
            }
        }

        if (!containsKeyword) {
            return;
        }

        // 当前行存在关键字，生成到根节点的调用链
        List<String> lineList = new ArrayList<>(20);

        FileContentNode tmpNode = lastNode;
        while (true) {
            lineList.add(tmpNode.getFileLineContent());

            if (tmpNode.getParentNode() == null) {
                break;
            }

            tmpNode = tmpNode.getParentNode();
        }

        if (!order4ee) {
            // 向下的调用链，逆序
            for (int i = lineList.size() - 1; i >= 0; i--) {
                foundCallGraph.append(lineList.get(i)).append(JACGConstants.NEW_LINE);
            }
        } else {
            // 向上的调用链，顺序
            for (String str : lineList) {
                foundCallGraph.append(str).append(JACGConstants.NEW_LINE);
            }
        }
    }
}

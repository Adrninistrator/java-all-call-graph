package com.adrninistrator.jacg.find_stack;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dto.call_line.CallGraphLineParsed;
import com.adrninistrator.jacg.dto.keyword.FileContentNode;
import com.adrninistrator.jacg.extensions.find_stack_filter.FindStackKeywordFilterInterface;
import com.adrninistrator.jacg.markdown.writer.MarkdownWriter;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.jacg.runner.base.AbstractRunnerGenCallGraph;
import com.adrninistrator.jacg.util.JACGCallGraphFileUtil;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2021/7/29
 * @description: 生成包含关键字的所有方法到起始方法之间的调用链
 */

public class FindCallStackTrace {
    private static final Logger logger = LoggerFactory.getLogger(FindCallStackTrace.class);

    private final String currentSimpleClassName = this.getClass().getSimpleName();

    private boolean inited = false;

    // 记录当前生成完整方法调用链的目录
    private String callGraphOutputDirPath;

    // 记录当前生成调用堆栈的目录
    private String stackOutputDirPath;

    // 根据关键字生成调用堆栈过滤器扩展类列表
    private List<FindStackKeywordFilterInterface> findStackKeywordFilterList;

    // 读取文件内容时，上一行的节点
    private FileContentNode lastNode;

    // 用于生成方法完整调用链的对象
    private AbstractRunnerGenCallGraph runnerGenCallGraph;

    /**
     * 初始化
     * 假如需要提前进行初始化，可在调用find方法前调用当前方法
     *
     * @param order4ee
     * @param configureWrapper
     * @return
     */
    public boolean init(boolean order4ee, ConfigureWrapper configureWrapper) {
        synchronized (this) {
            if (inited) {
                return true;
            }

            if (order4ee) {
                runnerGenCallGraph = new RunnerGenAllGraph4Callee();
            } else {
                runnerGenCallGraph = new RunnerGenAllGraph4Caller();
            }

            // 初始化根据关键字生成调用堆栈过滤器扩展类
            if (!initFindKeywordFilters(configureWrapper)) {
                return false;
            }

            inited = true;
            return true;
        }
    }

    // 初始化根据关键字生成调用堆栈过滤器扩展类
    private boolean initFindKeywordFilters(ConfigureWrapper configureWrapper) {
        List<String> findKeywordFilterClassList = configureWrapper.getOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_FIND_STACK_KEYWORD_FILTER, true);
        if (JavaCGUtil.isCollectionEmpty(findKeywordFilterClassList)) {
            return true;
        }

        findStackKeywordFilterList = new ArrayList<>(findKeywordFilterClassList.size());
        try {
            for (String extensionClass : findKeywordFilterClassList) {
                FindStackKeywordFilterInterface findStackKeywordFilter = JACGUtil.getClassObject(extensionClass, FindStackKeywordFilterInterface.class);
                if (findStackKeywordFilter == null) {
                    return false;
                }
                findStackKeywordFilterList.add(findStackKeywordFilter);
            }
            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    /**
     * 在生成的方法调用链文件中搜索指定关键字
     *
     * @param order4ee true: 处理向上的方法调用链 false: 处理向下的方法调用链
     * @return 生成的搜索结果文件的完整路径列表
     */
    public List<String> find(boolean order4ee) {
        return find(order4ee, new ConfigureWrapper(false));
    }

    /**
     * 在生成的方法调用链文件中搜索指定关键字，通过代码指定配置参数
     *
     * @param order4ee         true: 处理向上的方法调用链 false: 处理向下的方法调用链
     * @param configureWrapper
     * @return 生成的搜索结果文件的完整路径列表
     */
    public List<String> find(boolean order4ee, ConfigureWrapper configureWrapper) {
        // 记录入口简单类名
        configureWrapper.addEntryClass(currentSimpleClassName);

        if (!init(order4ee, configureWrapper)) {
            logger.error("初始化失败");
            return null;
        }

        // 读取指定的关键字
        OtherConfigFileUseListEnum otherConfigFileUseListEnum = order4ee ? OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4EE :
                OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4ER;

        List<String> configKeywordList = null;
        List<String> usedKeywordList = null;
        if (JavaCGUtil.isCollectionEmpty(findStackKeywordFilterList)) {
            // 未指定根据关键字生成调用堆栈过滤器扩展类时，使用指定的关键字
            configKeywordList = configureWrapper.getOtherConfigList(otherConfigFileUseListEnum, true);
            // 保存关键字的列表，这里需要新创建可写的List，从配置中获取的List可能是不可写的
            usedKeywordList = new ArrayList<>(configKeywordList.size());
        }

        // 处理关键字
        if (!handleKeywords(otherConfigFileUseListEnum, configKeywordList, usedKeywordList)) {
            return null;
        }

        // 生成完整方法调用链文件
        boolean success = runnerGenCallGraph.run(configureWrapper);
        callGraphOutputDirPath = runnerGenCallGraph.getCurrentOutputDirPath();
        if (!success || callGraphOutputDirPath == null) {
            logger.error("生成方法完整调用链失败，请检查");
            return null;
        }

        // 处理目录
        List<String> mdFilePathList = handleDir(usedKeywordList, order4ee);

        // 执行完毕时尝试打印当前使用的配置信息
        configureWrapper.tryPrintUsedConfigInfo(currentSimpleClassName, callGraphOutputDirPath);
        return mdFilePathList;
    }

    // 处理关键字
    private boolean handleKeywords(OtherConfigFileUseListEnum otherConfigFileUseListEnum,
                                   List<String> configKeywordList,
                                   List<String> usedKeywordList) {
        if (!JavaCGUtil.isCollectionEmpty(findStackKeywordFilterList)) {
            // 使用关键字过滤器扩展类
            logger.info("对方法完整调用链文件根据关键字生成调用堆栈文件的过滤器扩展类\n{}", StringUtils.join(usedKeywordList, "\n"));
            return true;
        }

        // 使用关键字
        for (String configKeyword : configKeywordList) {
            if (StringUtils.isBlank(configKeyword) ||
                    StringUtils.startsWith(configKeyword, JavaCGConstants.FLAG_HASHTAG) ||
                    usedKeywordList.contains(configKeyword)) {
                // 配置文件中被注释的行不处理，避免重复添加
                logger.warn("跳过以下关键字 {}", configKeyword);
                continue;
            }
            usedKeywordList.add(configKeyword);
        }

        if (usedKeywordList.isEmpty()) {
            logger.error("请在配置文件中指定需要生成到起始方法之间调用链的合法关键字 {}", otherConfigFileUseListEnum.getKey());
            return false;
        }

        logger.info("用于对方法完整调用链文件生成调用堆栈文件的关键字\n{}", StringUtils.join(usedKeywordList, "\n"));
        return true;
    }

    // 写入文件头信息
    private void writeHeaderInfo(MarkdownWriter markdownWriter, String txtFilePath, List<String> keywordList, boolean order4ee) throws IOException {
        markdownWriter.addList("处理调用链文件: " + txtFilePath);

        if (!order4ee) {
            markdownWriter.addList("方法向下调用链对应的调用堆栈，按层级增大方向打印");
        } else {
            markdownWriter.addList("方法向上调用链对应的调用堆栈，按层级减小方向打印");
        }

        List<String> usedKeywordList;
        if (!JavaCGUtil.isCollectionEmpty(findStackKeywordFilterList)) {
            usedKeywordList = new ArrayList<>(findStackKeywordFilterList.size());
            markdownWriter.addListWithNewLine("使用关键字过滤器扩展类: ");
            for (FindStackKeywordFilterInterface findStackKeywordFilter : findStackKeywordFilterList) {
                usedKeywordList.add(findStackKeywordFilter.getClass().getName());
            }
        } else {
            markdownWriter.addListWithNewLine("查找的关键字: ");
            usedKeywordList = keywordList;
        }
        markdownWriter.addCodeBlock();
        for (String keyword : usedKeywordList) {
            markdownWriter.addLine(keyword);
        }
        markdownWriter.addCodeBlock();
    }

    // 处理目录
    private List<String> handleDir(List<String> keywordList, boolean order4ee) {
        // 目录路径后增加分隔符
        String finalCallGraphDirPath = JavaCGUtil.addSeparator4FilePath(callGraphOutputDirPath);

        Set<String> subDirPathSet = new HashSet<>();
        List<String> subFilePathList = new ArrayList<>();

        // 从目录中查找需要处理的文件
        JACGFileUtil.searchDir(finalCallGraphDirPath, subDirPathSet, subFilePathList, JACGConstants.EXT_TXT);

        if (subFilePathList.isEmpty()) {
            logger.error("{} 目录中未找到后缀为[{}]的文件", finalCallGraphDirPath, JACGConstants.EXT_TXT);
            return null;
        }

        // 记录当前处理的目录
        stackOutputDirPath = finalCallGraphDirPath + JACGConstants.DIR_OUTPUT_STACK;
        if (!JACGFileUtil.isDirectoryExists(stackOutputDirPath)) {
            return null;
        }

        // 未搜索到关键字的文件保存目录
        String keyWordsNotFoundDirPath = stackOutputDirPath + File.separator + JACGConstants.DIR_KEYWORDS_NOT_FOUND;

        // txt文件所在目录字符串长度，用于后续截取
        int finalSrcDirPathLength = finalCallGraphDirPath.length();

        // 创建md文件需要保存的目录
        for (String subDirPath : subDirPathSet) {
            if (subDirPath.equals(finalCallGraphDirPath)) {
                // 当前结果目录不需要处理
                continue;
            }

            String subDirName = subDirPath.substring(finalSrcDirPathLength);
            String newDirPath = stackOutputDirPath + File.separator + subDirName;
            if (!JACGFileUtil.isDirectoryExists(newDirPath)) {
                return null;
            }
        }

        for (String subFilePath : subFilePathList) {
            String subFileName = JACGFileUtil.getFileNameFromPath(subFilePath);
            if (StringUtils.equalsAny(subFileName,
                    JACGConstants.FILE_JACG_ALL_CONFIG_MD,
                    JACGConstants.NOTICE_MULTI_ITF_MD,
                    JACGConstants.NOTICE_MULTI_SCC_MD,
                    JACGConstants.NOTICE_DISABLED_ITF_MD,
                    JACGConstants.NOTICE_DISABLED_SCC_MD
            )) {
                // 跳过自动生成的非调用链文件
                continue;
            }

            logger.info("根据调用链文件生成调用堆栈文件: {}", subFilePath);
            handleOneFile(finalSrcDirPathLength, keyWordsNotFoundDirPath, subFilePath, keywordList, order4ee);
        }

        // 生成结果信息
        return genMdFilePathList(order4ee);
    }

    // 生成结果信息
    private List<String> genMdFilePathList(boolean order4ee) {
        // 返回生成的结果文件路径列表
        List<String> mdFilePathList = new ArrayList<>();

        // 在生成.md文件的目录中搜索
        JACGFileUtil.searchDir(stackOutputDirPath, null, mdFilePathList, JACGConstants.EXT_MD);

        List<String> finalMdFilePathList = new ArrayList<>(mdFilePathList.size());
        for (String mdFilePath : mdFilePathList) {
            // 获取文件所在目录名
            String mdFileDirName = JACGFileUtil.getFileParentDirName(mdFilePath);

            if (JACGConstants.DIR_KEYWORDS_NOT_FOUND.equals(mdFileDirName)) {
                // 跳过未找到关键字的文件
                continue;
            }

            if (order4ee && !JACGConstants.DIR_OUTPUT_METHODS.equals(mdFileDirName)) {
                // 查找向上方法调用链文件时，对于不在方法目录中的文件跳过
                continue;
            }

            finalMdFilePathList.add(mdFilePath);
        }

        logger.info("处理完毕");
        return finalMdFilePathList;
    }

    private boolean handleOneFile(int srcDirPathLength,
                                  String keyWordsNotFoundDirPath,
                                  String txtFilePath,
                                  List<String> keywordList,
                                  boolean order4ee) {
        // 获取txt文件去掉所在目录之后的文件名，可能包含中间的目录名
        String txtFileName = txtFilePath.substring(srcDirPathLength);
        String txtFileNameWithOutExt = JACGFileUtil.getFileNameWithOutExt(txtFileName);
        String mdFilePath = stackOutputDirPath + File.separator + txtFileNameWithOutExt + JACGConstants.EXT_MD;

        try (BufferedReader br = JavaCGFileUtil.genBufferedReader(txtFilePath);
             MarkdownWriter markdownWriter = new MarkdownWriter(mdFilePath, true)) {
            // 写入文件头信息
            writeHeaderInfo(markdownWriter, txtFilePath, keywordList, order4ee);

            String line;
            lastNode = null;
            // 文件行号
            int lineNum = 0;
            // 记录每个方法级别最后处理的文件内容节点
            List<FileContentNode> fileContentNodeList = new ArrayList<>(20);

            while ((line = br.readLine()) != null) {
                lineNum++;
                // md文件中的标题内容
                String title = JACGConstants.FLAG_MD_LINE_NUMBER + lineNum;
                // 处理txt文件的一行
                if (!handleTxtFileOneLine(line, keywordList, order4ee, markdownWriter, fileContentNodeList, title)) {
                    return false;
                }
            }

            if (!markdownWriter.isWriteData()) {
                // 需要先结束写文件，否则后续无法移动
                markdownWriter.close();

                // 未写入文件内容，将当前md文件移动到代表空文件的目录中
                JACGFileUtil.renameFile(mdFilePath, keyWordsNotFoundDirPath + File.separator + txtFileNameWithOutExt + JACGConstants.EXT_MD);
            }

            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    // 处理txt文件的一行
    private boolean handleTxtFileOneLine(String line,
                                         List<String> keywordList,
                                         boolean order4ee,
                                         MarkdownWriter markdownWriter,
                                         List<FileContentNode> fileContentNodeList,
                                         String title) throws IOException {
        if (!JACGCallGraphFileUtil.isCallGraphLine(line)) {
            // 不属于调用链信息的行，不处理
            // 对于向上的方法调用链，类对应的文件中包含多个方法的信息，需要将上次处理的数据清空
            lastNode = null;
            fileContentNodeList.clear();
            return true;
        }

        // 处理调用链信息的行
        // 获取方法级别
        int methodLevel = JACGCallGraphFileUtil.getMethodLevel(line);
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
            lastNode = new FileContentNode(null, JACGConstants.CALL_GRAPH_METHOD_LEVEL_START, line);

            // 记录每个方法级别最后处理的文件内容节点
            recordFileContentNodeInList(methodLevel, fileContentNodeList);

            // 生成当前节点到根节点的调用堆栈
            genCallStack(line, keywordList, markdownWriter, order4ee, title);
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
        lastNode = new FileContentNode(parentNode, methodLevel, line);

        // 记录每个方法级别最后处理的文件内容节点
        recordFileContentNodeInList(methodLevel, fileContentNodeList);

        // 生成当前节点到根节点的调用堆栈
        genCallStack(line, keywordList, markdownWriter, order4ee, title);
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

    // 生成当前节点到根节点的调用堆栈
    private void genCallStack(String line, List<String> keywordList, MarkdownWriter markdownWriter, boolean order4ee, String title) throws IOException {
        // 在指定行中查找关键字
        if (!findKeyword(line, keywordList, order4ee)) {
            return;
        }

        // 当前行存在关键字，生成到根节点的调用堆栈
        List<String> lineList = new ArrayList<>(20);
        boolean runInOtherThread = false;
        boolean runInTransaction = false;
        FileContentNode tmpNode = lastNode;
        while (true) {
            String lineContent = tmpNode.getFileLineContent();
            lineList.add(lineContent);

            // 判断是否在其他线程执行
            if (JACGCallGraphFileUtil.checkRunInOtherThread(lineContent)) {
                runInOtherThread = true;
            }
            // 判断是否在事务中执行
            if (JACGCallGraphFileUtil.checkRunInTransaction(lineContent)) {
                runInTransaction = true;
            }

            if (tmpNode.getParentNode() == null) {
                break;
            }

            tmpNode = tmpNode.getParentNode();
        }

        // 在标题中增加在其他线程执行、在事务中执行标记
        String finalTitle = title;
        if (runInOtherThread) {
            finalTitle += JACGConstants.CALL_FLAG_RUN_IN_OTHER_THREAD;
        }
        if (runInTransaction) {
            finalTitle += JACGConstants.CALL_FLAG_RUN_IN_TRANSACTION;
        }
        // 添加标题
        markdownWriter.addTitle(1, finalTitle);

        markdownWriter.addCodeBlock();
        if (!order4ee) {
            // 向下的调用链，逆序
            for (int i = lineList.size() - 1; i >= 0; i--) {
                // 记录markdown已写入指定数据
                markdownWriter.setWriteData(true);
                markdownWriter.addLine(lineList.get(i));
            }
        } else {
            // 向上的调用链，顺序
            for (String str : lineList) {
                // 记录markdown已写入指定数据
                markdownWriter.setWriteData(true);
                markdownWriter.addLine(str);
            }
        }
        markdownWriter.addCodeBlock();
    }

    // 在指定行中查找关键字
    private boolean findKeyword(String line, List<String> keywordList, boolean order4ee) {
        if (JavaCGUtil.isCollectionEmpty(findStackKeywordFilterList)) {
            // 使用配置文件中的关键字进行判断
            for (String keyword : keywordList) {
                if (line.contains(keyword)) {
                    return true;
                }
            }
            return false;
        }

        // 使用根据关键字生成调用堆栈过滤器扩展类进行判断
        CallGraphLineParsed callGraphLineParsed = null;
        for (FindStackKeywordFilterInterface findStackKeywordFilter : findStackKeywordFilterList) {
            if (findStackKeywordFilter.filterByLine()) {
                // 通过当前行字符串判断
                if (findStackKeywordFilter.filter(line)) {
                    return true;
                }
                continue;
            }
            // 通过当前行解析后的内容判断
            if (callGraphLineParsed == null) {
                callGraphLineParsed = order4ee ? JACGCallGraphFileUtil.parseCallGraphLine4ee(line) : JACGCallGraphFileUtil.parseCallGraphLine4er(line);
            }
            if (findStackKeywordFilter.filter(callGraphLineParsed)) {
                return true;
            }
        }
        return false;
    }

    public String getCallGraphOutputDirPath() {
        return callGraphOutputDirPath;
    }
}

package com.adrninistrator.jacg.findstack;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.dto.callline.CallGraphLineParsed;
import com.adrninistrator.jacg.dto.callstack.CallStackFileResult;
import com.adrninistrator.jacg.dto.callstack.CalleeStackSummary;
import com.adrninistrator.jacg.dto.keyword.FileContentNode;
import com.adrninistrator.jacg.extensions.findstackfilter.FindStackKeywordFilterInterface;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Callee;
import com.adrninistrator.jacg.runner.RunnerGenAllGraph4Caller;
import com.adrninistrator.jacg.runner.base.AbstractExecutor;
import com.adrninistrator.jacg.runner.base.AbstractRunnerGenCallGraph;
import com.adrninistrator.jacg.util.JACGCallGraphFileUtil;
import com.adrninistrator.jacg.util.JACGCallStackUtil;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.writer.WriterSupportHeader;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import com.adrninistrator.javacg2.markdown.writer.MarkdownWriter;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.io.IOUtils;
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
import java.util.concurrent.atomic.AtomicReference;

/**
 * @author adrninistrator
 * @date 2021/7/29
 * @description: 生成包含指定关键字的方法到起始方法之间的调用堆栈
 */

public class FindCallStackTrace extends AbstractExecutor {
    private static final Logger logger = LoggerFactory.getLogger(FindCallStackTrace.class);

    public static final String[] FILE_HEADER_ARRAY_STACK_TABLE = new String[]{
            "调用堆栈在文件中的序号",
            "调用堆栈内部的序号",
            "完整调用方法/被调用方法",
            "调用方法代码行号"
    };

    public static final String[] FILE_HEADER_ARRAY_CALLEE_STACK_SUMMARY = new String[]{
            "调用堆栈在文件中的序号",
            "完整被调用方法",
            "上层完整调用方法",
            "向上通过关键字找到的完整方法",
            "向上通过关键字找到的方法返回类型",
    };

    public static final String[] FILE_HEADER_ARRAY_CALLER_STACK_SUMMARY = new String[]{
            "调用堆栈在文件中的序号",
            "完整调用方法",
            "向下通过关键字找到的完整方法",
            "向下通过关键字找到的方法返回类型",
    };

    public static final String FILE_HEADER_STACK_TABLE = StringUtils.join(FILE_HEADER_ARRAY_STACK_TABLE, JavaCG2Constants.FLAG_TAB);
    public static final String FILE_HEADER_CALLEE_STACK_SUMMARY = StringUtils.join(FILE_HEADER_ARRAY_CALLEE_STACK_SUMMARY, JavaCG2Constants.FLAG_TAB);
    public static final String FILE_HEADER_CALLER_STACK_SUMMARY = StringUtils.join(FILE_HEADER_ARRAY_CALLER_STACK_SUMMARY, JavaCG2Constants.FLAG_TAB);

    private final boolean order4ee;

    // 用于生成方法完整调用链的对象
    private final AbstractRunnerGenCallGraph runnerGenCallGraph;

    // 记录当前生成完整方法调用链的目录
    private String callGraphOutputDirPath;

    // 记录当前生成调用堆栈的目录
    private String stackOutputDirPath;

    // 未搜索到关键字的文件保存目录，以分隔符结束，分隔符使用/（后续需要判断.md文件路径是否以该目录开头）
    private String keyWordsNotFoundDirPath;

    // 是否需要生成其他形式的调用堆栈文件
    private boolean genStackOtherForms = false;

    // 创建保存其他形式的调用堆栈文件的目录
    private String otherFormsStackDirPath;

    // 根据关键字生成调用堆栈过滤器扩展类列表
    private List<FindStackKeywordFilterInterface> findStackKeywordFilterList;

    // 指定已存在的完整方法调用链文件保存目录（不生成完整方法调用链文件）
    private String existedCallGraphDirPath;

    /**
     * 使用配置文件中的参数
     *
     * @param order4ee true: 处理向上的方法调用链 false: 处理向下的方法调用链
     */
    public FindCallStackTrace(boolean order4ee) {
        this(order4ee, new ConfigureWrapper(false));
    }

    /**
     * 通过代码指定配置参数
     *
     * @param order4ee         true: 处理向上的方法调用链 false: 处理向下的方法调用链
     * @param configureWrapper 配置参数
     * @return
     */
    public FindCallStackTrace(boolean order4ee, ConfigureWrapper configureWrapper) {
        if (order4ee) {
            runnerGenCallGraph = new RunnerGenAllGraph4Callee(configureWrapper);
        } else {
            runnerGenCallGraph = new RunnerGenAllGraph4Caller(configureWrapper);
        }

        // 初始化根据关键字生成调用堆栈过滤器扩展类
        if (!initFindKeywordFilters(configureWrapper)) {
            throw new JavaCG2RuntimeException("初始化异常");
        }

        this.order4ee = order4ee;
        this.configureWrapper = configureWrapper;
    }

    // 初始化根据关键字生成调用堆栈过滤器扩展类
    private boolean initFindKeywordFilters(ConfigureWrapper configureWrapper) {
        List<String> findKeywordFilterClassList = configureWrapper.getOtherConfigList(OtherConfigFileUseListEnum.OCFULE_EXTENSIONS_FIND_STACK_KEYWORD_FILTER);
        if (JavaCG2Util.isCollectionEmpty(findKeywordFilterClassList)) {
            return true;
        }

        findStackKeywordFilterList = new ArrayList<>(findKeywordFilterClassList.size());
        try {
            for (String extensionClass : findKeywordFilterClassList) {
                FindStackKeywordFilterInterface findStackKeywordFilter = JACGClassMethodUtil.genClassObject(extensionClass, FindStackKeywordFilterInterface.class);
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
     * 在生成的方法调用链文件中搜索指定关键字，生成方法调用堆栈，通过代码指定配置参数
     *
     * @return 生成调用堆栈文件结果
     */
    public CallStackFileResult find() {
        if (Boolean.TRUE.equals(configureWrapper.getMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_GEN_STACK_OTHER_FORMS))) {
            if (!OutputDetailEnum.ODE_0.getDetail().equals(configureWrapper.getMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL))) {
                logger.warn("自动设置参数值为 {} {}", OutputDetailEnum.ODE_0.getDetail(), configureWrapper.genConfigUsage(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL));
                configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, OutputDetailEnum.ODE_0.getDetail());
            }
            logger.info("后续会生成其他形式的调用堆栈文件");
            genStackOtherForms = true;
        } else {
            logger.info("后续不会生成其他形式的调用堆栈文件");
        }

        if (!Boolean.TRUE.equals(configureWrapper.getMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE))) {
            logger.error("参数值需要设置为 {} {}", Boolean.TRUE, configureWrapper.genConfigUsage(ConfigKeyEnum.CKE_CALL_GRAPH_WRITE_TO_FILE));
            return CallStackFileResult.FAIL;
        }
        if (!Boolean.FALSE.equals(configureWrapper.getMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY))) {
            logger.error("参数值需要设置为 {} {}", Boolean.FALSE, configureWrapper.genConfigUsage(ConfigKeyEnum.CKE_CALL_GRAPH_RETURN_IN_MEMORY));
            return CallStackFileResult.FAIL;
        }

        // 读取指定的关键字
        OtherConfigFileUseListEnum otherConfigFileUseListEnum = order4ee ? OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4EE :
                OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4ER;

        List<String> configKeywordList = null;
        List<String> usedKeywordList = null;
        if (JavaCG2Util.isCollectionEmpty(findStackKeywordFilterList)) {
            // 未指定根据关键字生成调用堆栈过滤器扩展类时，使用指定的关键字
            configKeywordList = configureWrapper.getOtherConfigList(otherConfigFileUseListEnum);
            // 保存关键字的列表，这里需要新创建可写的List，从配置中获取的List可能是不可写的
            usedKeywordList = new ArrayList<>(configKeywordList.size());
        }

        // 处理关键字
        if (!handleKeywords(otherConfigFileUseListEnum, configKeywordList, usedKeywordList)) {
            return CallStackFileResult.FAIL;
        }

        if (StringUtils.isBlank(existedCallGraphDirPath)) {
            // 生成完整方法调用链文件
            boolean success = runnerGenCallGraph.run();
            callGraphOutputDirPath = runnerGenCallGraph.getCurrentOutputDirPath();
            if (!success) {
                logger.error("生成方法完整调用链失败，请检查");
                return CallStackFileResult.FAIL;
            }
        } else {
            logger.info("指定已存在的完整方法调用链文件保存目录（不生成完整方法调用链文件） {}", existedCallGraphDirPath);
            callGraphOutputDirPath = existedCallGraphDirPath;
        }

        // 处理目录
        CallStackFileResult callStackFileResult = handleDir(usedKeywordList);

        // 执行完毕时打印当前使用的配置信息
        configureWrapper.printUsedConfigInfo(currentSimpleClassName, callGraphOutputDirPath, JACGConstants.FILE_JACG_USED_CONFIG_MD);
        return callStackFileResult;
    }

    // 处理关键字
    private boolean handleKeywords(OtherConfigFileUseListEnum otherConfigFileUseListEnum,
                                   List<String> configKeywordList,
                                   List<String> usedKeywordList) {
        if (!JavaCG2Util.isCollectionEmpty(findStackKeywordFilterList)) {
            // 使用关键字过滤器扩展类
            logger.info("对方法完整调用链文件根据关键字生成调用堆栈文件的过滤器扩展类\n{}", StringUtils.join(usedKeywordList, "\n"));
            return true;
        }

        // 使用关键字
        for (String configKeyword : configKeywordList) {
            if (StringUtils.isBlank(configKeyword) ||
                    StringUtils.startsWith(configKeyword, JavaCG2Constants.FLAG_HASHTAG) ||
                    usedKeywordList.contains(configKeyword)) {
                // 配置文件中被注释的行不处理，避免重复添加
                logger.warn("跳过以下关键字 {}", configKeyword);
                continue;
            }
            usedKeywordList.add(configKeyword);
        }

        if (usedKeywordList.isEmpty()) {
            logger.error("请在配置文件中指定需要生成到起始方法之间调用链的合法关键字 {}", otherConfigFileUseListEnum.getConfigPrintInfo());
            return false;
        }

        logger.info("用于对方法完整调用链文件生成调用堆栈文件的关键字\n{}", StringUtils.join(usedKeywordList, "\n"));
        return true;
    }

    // 写入文件头信息
    private void writeHeaderInfo(MarkdownWriter markdownWriter, String txtFilePath, List<String> keywordList) throws IOException {
        markdownWriter.addList("处理调用链文件: " + txtFilePath);

        if (!order4ee) {
            markdownWriter.addList("方法向下调用链对应的调用堆栈，按层级增大方向打印");
        } else {
            markdownWriter.addList("方法向上调用链对应的调用堆栈，按层级减小方向打印");
        }

        List<String> usedKeywordList;
        if (!JavaCG2Util.isCollectionEmpty(findStackKeywordFilterList)) {
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
    private CallStackFileResult handleDir(List<String> keywordList) {
        // 目录路径后增加分隔符
        String finalCallGraphDirPath = JavaCG2Util.addSeparator4FilePath(callGraphOutputDirPath);

        // 记录当前处理的目录
        stackOutputDirPath = finalCallGraphDirPath + JACGConstants.DIR_OUTPUT_STACK;
        if (!JavaCG2FileUtil.isDirectoryExists(stackOutputDirPath)) {
            throw new JavaCG2RuntimeException("创建目录失败 " + stackOutputDirPath);
        }

        // 未搜索到关键字的文件保存目录
        keyWordsNotFoundDirPath = JavaCG2FileUtil.replaceFilePath2Slash(stackOutputDirPath + File.separator + JACGConstants.DIR_KEYWORDS_NOT_FOUND + File.separator);

        if (genStackOtherForms) {
            // 创建保存其他形式的调用堆栈文件的目录
            otherFormsStackDirPath = stackOutputDirPath + File.separator + JACGConstants.DIR_OUTPUT_STACK_OTHER_FORMS;
        }

        Set<String> subDirPathSet = new HashSet<>();
        List<String> subFilePathList = new ArrayList<>();

        // 从目录中查找需要处理的文件
        JACGFileUtil.searchDir(finalCallGraphDirPath, subDirPathSet, subFilePathList, JavaCG2Constants.EXT_TXT);

        if (subFilePathList.isEmpty()) {
            logger.warn("{} 目录中未找到后缀为[{}]的文件", finalCallGraphDirPath, JavaCG2Constants.EXT_TXT);
            return CallStackFileResult.EMPTY;
        }

        // 创建线程池
        createThreadPoolExecutor(subFilePathList.size());

        // txt文件所在目录字符串长度，用于后续截取
        int finalSrcDirPathLength = finalCallGraphDirPath.length();

        try {
            JavaCG2Counter failCounter = new JavaCG2Counter(0);
            for (String subFilePath : subFilePathList) {
                // 等待直到允许任务执行
                wait4TPEAllowExecute();
                executeByTPE(() -> {
                    if (!handleOneFile(finalSrcDirPathLength, subFilePath, keywordList)) {
                        logger.error("处理文件失败 {}", subFilePath);
                        failCounter.addAndGet();
                    }
                });
            }
            // 等待直到任务执行完毕
            wait4TPEDone();

            // 在生成调用堆栈文件的目录中搜索结果文件路径列表
            List<String> stackFilePathList = JACGFileUtil.findFilePathInCurrentDir(stackOutputDirPath, JavaCG2Constants.EXT_MD);
            List<String> otherFormsStackFilePathList = null;
            if (genStackOtherForms) {
                otherFormsStackFilePathList = JACGFileUtil.findDirPathInCurrentDir(otherFormsStackDirPath);
            }
            logger.info("处理完毕");
            if (failCounter.getCount() > 0) {
                return CallStackFileResult.FAIL;
            }
            return new CallStackFileResult(callGraphOutputDirPath, stackFilePathList, otherFormsStackFilePathList);
        } finally {
            // 关闭并等待线程池
            shutdownAndWaitTPE();
        }
    }

    private boolean handleOneFile(int srcDirPathLength, String txtFilePath, List<String> keywordList) {
        logger.info("根据调用链文件生成调用堆栈文件: {}", txtFilePath);
        // 获取txt文件去掉所在目录之后的文件名，可能包含中间的目录名
        String txtFileName = txtFilePath.substring(srcDirPathLength);
        String txtFileNameWithOutExt = JACGFileUtil.getFileNameWithOutExt(txtFileName);
        String stackFilePath = stackOutputDirPath + File.separator + txtFileNameWithOutExt + JavaCG2Constants.EXT_MD;
        String otherFormsDirPath = null;

        if (genStackOtherForms) {
            otherFormsDirPath = otherFormsStackDirPath + File.separator + txtFileNameWithOutExt;

            if (!JavaCG2FileUtil.isDirectoryExists(otherFormsDirPath)) {
                return false;
            }
        }

        // 用于写其他形式的调用堆栈文件
        WriterSupportHeader stackTableWriter = null;
        // 用于写汇总文件
        WriterSupportHeader stackSummaryWriter = null;
        try (BufferedReader br = JavaCG2FileUtil.genBufferedReader(txtFilePath);
             MarkdownWriter markdownWriter = new MarkdownWriter(stackFilePath, true)) {

            if (genStackOtherForms) {
                String otherFormsCallStackFilePath = otherFormsDirPath + File.separator + (order4ee ? JACGConstants.FILE_CALLEE_STACK_TABLE :
                        JACGConstants.FILE_CALLER_STACK_TABLE);
                stackTableWriter = new WriterSupportHeader(otherFormsCallStackFilePath, FILE_HEADER_STACK_TABLE);
                String summaryFilePath = otherFormsDirPath + File.separator + (order4ee ? JACGConstants.FILE_CALLEE_STACK_SUMMARY : JACGConstants.FILE_CALLER_STACK_SUMMARY);
                stackSummaryWriter = new WriterSupportHeader(summaryFilePath, order4ee ? FILE_HEADER_CALLEE_STACK_SUMMARY : FILE_HEADER_CALLER_STACK_SUMMARY);
            }

            // 写入文件头信息
            writeHeaderInfo(markdownWriter, txtFilePath, keywordList);

            String line;
            // 读取文件内容时，上一行的节点
            AtomicReference<FileContentNode> lastNodeReference = new AtomicReference<>(null);
            // 文件行号
            int lineNum = 0;
            // 记录每个方法级别最后处理的文件内容节点
            List<FileContentNode> fileContentNodeList = new ArrayList<>(20);

            JavaCG2Counter callStackCounter = new JavaCG2Counter(0);

            while ((line = br.readLine()) != null) {
                lineNum++;
                // md文件中的标题内容
                String title = JACGConstants.FLAG_MD_LINE_NUMBER + lineNum;
                // 处理txt文件的一行
                if (!handleTxtFileOneLine(line, keywordList, markdownWriter, fileContentNodeList, title, callStackCounter, stackTableWriter, stackSummaryWriter,
                        lastNodeReference)) {
                    return false;
                }
            }

            if (!markdownWriter.isWriteData()) {
                // 需要先结束写文件，否则后续无法移动
                markdownWriter.close();

                if (!JavaCG2FileUtil.isDirectoryExists(keyWordsNotFoundDirPath)) {
                    return false;
                }
                // 未写入文件内容，将当前md文件移动到代表空文件的目录中
                JACGFileUtil.renameFile(stackFilePath, keyWordsNotFoundDirPath + txtFileNameWithOutExt + JavaCG2Constants.EXT_MD);
            }

            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        } finally {
            if (genStackOtherForms) {
                if (stackTableWriter != null) {
                    IOUtils.closeQuietly(stackTableWriter);
                }
                if (stackSummaryWriter != null) {
                    IOUtils.closeQuietly(stackSummaryWriter);
                }
            }
        }
    }

    // 处理txt文件的一行
    private boolean handleTxtFileOneLine(String line, List<String> keywordList, MarkdownWriter markdownWriter, List<FileContentNode> fileContentNodeList, String title,
                                         JavaCG2Counter callStackCounter, WriterSupportHeader stackTableWriter, WriterSupportHeader stackSummaryWriter,
                                         AtomicReference<FileContentNode> lastNodeReference) throws IOException {
        if (!JACGCallGraphFileUtil.isCallGraphLine(line)) {
            // 不属于调用链信息的行，不处理
            // 对于向上的方法调用链，类对应的文件中包含多个方法的信息，需要将上次处理的数据清空
            lastNodeReference.set(null);
            fileContentNodeList.clear();
            return true;
        }

        // 处理调用链信息的行
        // 获取方法级别
        int methodLevel = JACGCallGraphFileUtil.getMethodLevel(line);
        if (methodLevel == JACGConstants.CALL_GRAPH_METHOD_LEVEL_ILLEGAL) {
            return false;
        }

        FileContentNode lastNode = lastNodeReference.get();
        if (lastNode == null) {
            // 还未处理过文件内容节点
            if (methodLevel != JACGConstants.CALL_GRAPH_METHOD_LEVEL_START) {
                logger.error("还未处理过文件内容节点，方法级别非法 {} {}", methodLevel, line);
                return false;
            }

            // 根文件内容节点
            lastNode = new FileContentNode(null, JACGConstants.CALL_GRAPH_METHOD_LEVEL_START, line);
            lastNodeReference.set(lastNode);

            // 记录每个方法级别最后处理的文件内容节点
            recordFileContentNodeInList(methodLevel, fileContentNodeList, lastNode);

            // 生成当前节点到根节点的调用堆栈
            genCallStack(line, keywordList, markdownWriter, title, callStackCounter, stackTableWriter, stackSummaryWriter, lastNode);
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
        lastNodeReference.set(lastNode);

        // 记录每个方法级别最后处理的文件内容节点
        recordFileContentNodeInList(methodLevel, fileContentNodeList, lastNode);

        // 生成当前节点到根节点的调用堆栈
        genCallStack(line, keywordList, markdownWriter, title, callStackCounter, stackTableWriter, stackSummaryWriter, lastNode);
        return true;
    }

    // 记录每个方法级别最后处理的文件内容节点
    private void recordFileContentNodeInList(int methodLevel, List<FileContentNode> fileContentNodeList, FileContentNode lastNode) {
        if (fileContentNodeList.size() < methodLevel + 1) {
            // List中指定位置数据不存在则增加
            fileContentNodeList.add(lastNode);
            return;
        }

        // List中指定位置数据已存在则设置
        fileContentNodeList.set(methodLevel, lastNode);
    }

    // 生成当前节点到根节点的调用堆栈
    private boolean genCallStack(String line, List<String> keywordList, MarkdownWriter markdownWriter, String title, JavaCG2Counter callStackCounter,
                                 WriterSupportHeader stackTableWriter, WriterSupportHeader stackSummaryWriter, FileContentNode lastNode) throws IOException {
        // 在指定行中查找关键字
        if (!findKeyword(line, keywordList)) {
            return true;
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

            FileContentNode parentNode = tmpNode.getParentNode();
            if (parentNode == null) {
                break;
            }

            tmpNode = parentNode;
        }

        // 在标题中增加在其他线程执行、在事务中执行标志
        String finalTitle = title;
        if (runInOtherThread) {
            finalTitle += JACGConstants.CALL_FLAG_RUN_IN_OTHER_THREAD;
        }
        if (runInTransaction) {
            finalTitle += JACGConstants.CALL_FLAG_RUN_IN_SPRING_TX;
        }
        // 添加标题
        markdownWriter.addTitle(1, finalTitle);
        markdownWriter.addCodeBlock();

        // 写调用堆栈文件
        if (!writeCallStackFile(callStackCounter, lineList, markdownWriter, stackTableWriter, stackSummaryWriter)) {
            return false;
        }

        markdownWriter.addCodeBlock();
        return true;
    }

    // 写调用堆栈文件
    private boolean writeCallStackFile(JavaCG2Counter callStackCounter, List<String> lineList, MarkdownWriter markdownWriter, WriterSupportHeader stackTableWriter,
                                       WriterSupportHeader stackSummaryWriter) {
        String seqInFile = null;
        try {
            if (genStackOtherForms) {
                seqInFile = String.format("%06x", callStackCounter.addAndGet());
            }
            // 生成表格形式的调用堆栈时，当前行所代表的层级
            int stackLevel = 1;
            int lineNumber = lineList.size();
            if (order4ee) {
                // 向上的调用链，顺序遍历
                // 被调用方法
                String calleeMethod = null;
                // 上层调用方法
                String callerMethod = null;
                // 关键字匹配的方法（如入口方法）
                String keywordMethod = null;
                // 关键字匹配的方法返回类型（如入口方法）
                String keywordMethodReturnType = null;
                for (int i = 0; i < lineNumber; i++) {
                    // 记录markdown已写入指定数据
                    String str = lineList.get(i);
                    markdownWriter.setWriteData(true);
                    markdownWriter.addLine(str);

                    if (genStackOtherForms) {
                        CallGraphLineParsed callGraphLineParsed = JACGCallGraphFileUtil.parseCallGraphLine4ee(str);
                        String fullMethod = callGraphLineParsed.getMethodDetail().getFullMethod();
                        String methodReturnType = callGraphLineParsed.getMethodDetail().getReturnType();
                        stackTableWriter.writeDataInLine(seqInFile, String.valueOf(stackLevel), fullMethod, callGraphLineParsed.getCallerLineNumberStr());
                        stackLevel++;

                        if (i == 0) {
                            keywordMethod = fullMethod;
                            keywordMethodReturnType = methodReturnType;
                        }
                        // 这里不用else
                        if (i == lineNumber - 2) {
                            // 倒数第二行
                            callerMethod = fullMethod;
                        } else if (i == lineNumber - 1) {
                            // 最后一行
                            calleeMethod = fullMethod;
                        }
                    }
                }
                if (genStackOtherForms) {
                    // 向上生成时输出的汇总文件
                    CalleeStackSummary calleeStackSummary = new CalleeStackSummary(seqInFile, calleeMethod, callerMethod, keywordMethod, keywordMethodReturnType);
                    stackSummaryWriter.writeDataInLine(JACGCallStackUtil.formatCalleeStackSummaryLine(calleeStackSummary));
                }
                return true;
            }
            // 向下的调用链，逆序遍历
            // 调用方法
            String callerMethod = null;
            // 关键字匹配的方法
            String keywordMethod = null;
            // 关键字匹配的方法返回类型
            String keywordMethodReturnType = null;
            // 记录上一行的调用方法
            String lastCallerMethod = null;
            for (int i = lineNumber - 1; i >= 0; i--) {
                // 记录markdown已写入指定数据
                String str = lineList.get(i);
                markdownWriter.setWriteData(true);
                markdownWriter.addLine(str);
                if (genStackOtherForms) {
                    CallGraphLineParsed callGraphLineParsed = JACGCallGraphFileUtil.parseCallGraphLine4er(str);
                    String fullMethod = callGraphLineParsed.getMethodDetail().getFullMethod();
                    String methodReturnType = callGraphLineParsed.getMethodDetail().getReturnType();
                    if (lastCallerMethod != null) {
                        stackTableWriter.writeDataInLine(seqInFile, String.valueOf(stackLevel), lastCallerMethod,
                                callGraphLineParsed.getCallerLineNumberStr());
                        stackLevel++;
                    }
                    lastCallerMethod = fullMethod;

                    // 逆序遍历，顺序是反的
                    if (i == 0) {
                        keywordMethod = fullMethod;
                        keywordMethodReturnType = methodReturnType;
                    } else if (i == lineNumber - 1) {
                        // 最后一行
                        callerMethod = fullMethod;
                    }
                }
            }
            if (genStackOtherForms) {
                // 表格形式的调用堆栈文件写入最后一行
                stackTableWriter.writeDataInLine(seqInFile, String.valueOf(stackLevel), lastCallerMethod, JavaCG2Constants.DEFAULT_LINE_NUMBER_STR);
                // 向上生成时输出的汇总文件
                stackSummaryWriter.writeDataInLine(seqInFile, callerMethod, keywordMethod, keywordMethodReturnType);
            }
            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    // 在指定行中查找关键字
    private boolean findKeyword(String line, List<String> keywordList) {
        if (JavaCG2Util.isCollectionEmpty(findStackKeywordFilterList)) {
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

    //
    public String getCallGraphOutputDirPath() {
        return callGraphOutputDirPath;
    }

    public ConfigureWrapper getConfigureWrapper() {
        return configureWrapper;
    }

    public void setExistedCallGraphDirPath(String existedCallGraphDirPath) {
        this.existedCallGraphDirPath = existedCallGraphDirPath;
    }
}

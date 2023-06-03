package com.adrninistrator.jacg.extractor.entry;

import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.dto.method.MethodInfoInFileName;
import com.adrninistrator.jacg.extractor.dto.common.extract_file.AbstractCallGraphExtractedFile;
import com.adrninistrator.jacg.find_stack.FindCallStackTrace;
import com.adrninistrator.jacg.util.JACGCallGraphFileUtil;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * @author adrninistrator
 * @date 2022/8/29
 * @description: 对调用堆栈结果文件进行数据提取的基类
 */
public abstract class BaseExtractor {
    private static final Logger logger = LoggerFactory.getLogger(BaseExtractor.class);

    protected DbOperator dbOperator;

    protected DbOperWrapper dbOperWrapper;

    protected AtomicBoolean runningFlag = new AtomicBoolean(false);

    protected final String currentSimpleClassName = this.getClass().getSimpleName();

    /**
     * 选择方法调用链方向
     *
     * @return true: 向上 false: 向下
     */
    protected abstract boolean chooseOrder4ee();

    /**
     * 对当前的调用堆栈数据进行处理
     *
     * @param dataSeq          调用堆栈文件中的数据序号
     * @param lineList         调用堆栈文件中的调用堆栈数据列表
     * @param lineNumberList   调用堆栈文件中的调用堆栈数据对应的行号列表
     * @param runInOtherThread 是否在其他线程执行
     * @param runInTransaction 是否在事务中执行
     */
    protected abstract void handleCallStackData(int dataSeq,
                                                List<String> lineList,
                                                List<Integer> lineNumberList,
                                                boolean runInOtherThread,
                                                boolean runInTransaction);

    /**
     * 根据关键字生成调用堆栈，并返回结果文件路径
     *
     * @param configureWrapper
     * @return
     */
    protected List<String> findStack(ConfigureWrapper configureWrapper) {
        // 记录入口简单类名
        configureWrapper.addEntryClass(currentSimpleClassName);

        // 判断生成调用链时的详细程度是否为最详细
        if (!OutputDetailEnum.ODE_1.getDetail().equals(configureWrapper.getMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, true))) {
            logger.warn("生成调用链时的详细程度自动设置为最详细 {} {}", ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL.getKey(), OutputDetailEnum.ODE_1.getDetail());
            configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, OutputDetailEnum.ODE_1.getDetail());
        }

        FindCallStackTrace findCallStackTrace = new FindCallStackTrace();
        findCallStackTrace.init(chooseOrder4ee(), configureWrapper);
        List<String> mdFilePathList = findCallStackTrace.find(chooseOrder4ee(), configureWrapper);

        // 执行完毕时尝试打印当前使用的配置信息
        configureWrapper.tryPrintUsedConfigInfo(currentSimpleClassName, findCallStackTrace.getCallGraphOutputDirPath());
        return mdFilePathList;
    }

    /**
     * 创建数据库相关对象
     *
     * @param configureWrapper
     */
    protected void genDbObject(ConfigureWrapper configureWrapper) {
        if (dbOperator != null) {
            return;
        }

        // 对需要使用的基础配置进行初始化
        dbOperWrapper = DbOperWrapper.genInstance(configureWrapper, currentSimpleClassName);
        dbOperator = dbOperWrapper.getDbOperator();
    }

    /**
     * 关闭数据源
     */
    protected void closeDs() {
        if (dbOperator != null) {
            dbOperator.closeDs();
        }
    }

    // 处理向上的调用堆栈结果文件信息
    protected void fillExtractedFileInfo4Callee(String stackFilePath, AbstractCallGraphExtractedFile callGraphExtractedFile) {
        fillExtractedFileInfo(stackFilePath, callGraphExtractedFile, true);
    }

    // 处理向下的调用堆栈结果文件信息
    protected void fillExtractedFileInfo4Caller(String stackFilePath, AbstractCallGraphExtractedFile callGraphExtractedFile) {
        fillExtractedFileInfo(stackFilePath, callGraphExtractedFile, false);
    }

    // 处理调用堆栈结果文件信息
    private void fillExtractedFileInfo(String stackFilePath, AbstractCallGraphExtractedFile callGraphExtractedFile, boolean order4ee) {
        callGraphExtractedFile.setStackFilePath(stackFilePath);

        String fileName = JACGFileUtil.getFileNameFromPath(stackFilePath);
        callGraphExtractedFile.setStackFileName(fileName);

        if (JACGCallGraphFileUtil.isEmptyCallGraphFileName(fileName)) {
            // 文件内容为空
            callGraphExtractedFile.setEmptyStackFile(true);
            return;
        }

        // 文件内容非空
        MethodInfoInFileName methodInfoInFileName = JACGCallGraphFileUtil.getMethodInfoFromFileName(fileName);
        if (methodInfoInFileName == null) {
            return;
        }

        callGraphExtractedFile.setSimpleClassName(methodInfoInFileName.getSimpleClassName());
        callGraphExtractedFile.setMethodName(methodInfoInFileName.getMethodName());
        callGraphExtractedFile.setMethodHash(methodInfoInFileName.getMethodHash());

        if (callGraphExtractedFile.isEmptyStackFile()) {
            return;
        }

        // 根据调用者完整方法HASH+长度，从方法调用表获取对应的完整方法
        String fullMethod;
        if (order4ee) {
            fullMethod = dbOperWrapper.getCalleeFullMethodByHash(callGraphExtractedFile.getMethodHash());
        } else {
            fullMethod = dbOperWrapper.getCallerFullMethodByHash(callGraphExtractedFile.getMethodHash());
        }
        callGraphExtractedFile.setFullMethod(fullMethod);
        if (fullMethod != null) {
            callGraphExtractedFile.setClassName(JACGClassMethodUtil.getClassNameFromMethod(fullMethod));
        }
    }

    /**
     * 解析调用堆栈文件
     *
     * @param stackFilePath
     * @return
     */
    protected boolean parseStackFilePath(String stackFilePath) {
        if (StringUtils.isBlank(stackFilePath)) {
            logger.error("未指定调用堆栈文件路径");
            return false;
        }

        if (!runningFlag.compareAndSet(false, true)) {
            // handleCallStackData()方法中会对类的字段进行修改，因此不能并发调用
            logger.error("当前类不允许并发调用，请创建新的实例");
            return false;
        }

        logger.info("处理调用堆栈文件 {}", stackFilePath);

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

        try (BufferedReader br = JavaCGFileUtil.genBufferedReader(stackFilePath)) {
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
                        logger.debug("处理数据 {}\n{}\n{}", stackFilePath, dataSeq, lineAndNumber);
                    }

                    // 对当前的调用堆栈数据进行处理
                    handleCallStackData(dataSeq, lineList, lineNumberList, runInOtherThread, runInTransaction);

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
            logger.error("error 行号 {}\n{} ", lineNumber, line, e);
            return false;
        } finally {
            runningFlag.set(false);
        }
    }
}

package com.adrninistrator.jacg.extractor.entry;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.common.list.ListWithResult;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.dboper.DbInitializer;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.dto.callstack.CallStackFileResult;
import com.adrninistrator.jacg.extractor.dto.common.extractfile.AbstractCallGraphExtractedFile;
import com.adrninistrator.jacg.findstack.FindCallStackTrace;
import com.adrninistrator.jacg.handler.methodcall.MethodCallHandler;
import com.adrninistrator.jacg.util.JACGCallGraphFileUtil;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/8/29
 * @description: 对调用堆栈结果文件进行数据提取的基类
 */
public abstract class BaseExtractor {
    private static final Logger logger = LoggerFactory.getLogger(BaseExtractor.class);

    protected DbOperator dbOperator;

    protected DbOperWrapper dbOperWrapper;

    protected MethodCallHandler methodCallHandler;

    protected final String currentSimpleClassName = this.getClass().getSimpleName();

    /**
     * 选择方法调用链方向
     *
     * @return true: 向上 false: 向下
     */
    protected abstract boolean chooseOrder4ee();

    /**
     * 根据关键字生成调用堆栈，并返回结果文件路径
     *
     * @param configureWrapper
     * @return
     */
    protected ListWithResult<String> findStack(ConfigureWrapper configureWrapper) {
        // 判断生成调用链时的详细程度是否为详细
        if (!OutputDetailEnum.ODE_1.getDetail().equals(configureWrapper.getMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL))) {
            logger.warn("生成调用链时的详细程度自动设置为详细 {} {}", ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL.getConfigPrintInfo(), OutputDetailEnum.ODE_1.getDetail());
            configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL, OutputDetailEnum.ODE_1.getDetail());
        }

        FindCallStackTrace findCallStackTrace = new FindCallStackTrace(chooseOrder4ee(), configureWrapper);
        CallStackFileResult callStackFileResult = findCallStackTrace.find();
        if (!callStackFileResult.isSuccess()) {
            return ListWithResult.genFail();
        }
        List<String> callStackFilePathList = callStackFileResult.getStackFilePathList();

        // 执行完毕时打印当前使用的配置信息
        // todo 检查效果
        configureWrapper.printUsedConfigInfo(currentSimpleClassName, findCallStackTrace.getCallGraphOutputDirPath(), JACGConstants.FILE_JACG_USED_CONFIG_MD);
        return new ListWithResult<>(callStackFilePathList);
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
        dbOperWrapper = DbInitializer.genDbOperWrapper(configureWrapper, this);
        dbOperator = dbOperWrapper.getDbOperator();
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
    }

    /**
     * 关闭数据源
     */
    protected void closeDs() {
        if (dbOperator != null) {
            dbOperator.closeDs(this);
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

        String fileName = JavaCG2FileUtil.getFileNameFromPath(stackFilePath);
        callGraphExtractedFile.setStackFileName(fileName);

        if (JACGCallGraphFileUtil.isEmptyCallGraphFileName(fileName) || JACGCallGraphFileUtil.isNotFoundCallGraphFileName(fileName)) {
            // 文件内容为空
            callGraphExtractedFile.setEmptyStackFile(true);
            return;
        }

        // 文件内容非空
        String methodHashInFileName = JACGCallGraphFileUtil.getMethodHashFromFileName(fileName);
        if (methodHashInFileName == null) {
            return;
        }

        callGraphExtractedFile.setMethodHash(methodHashInFileName);
        // 根据调用方完整方法HASH+长度，从方法调用表获取对应的完整方法
        String fullMethod;
        if (order4ee) {
            fullMethod = methodCallHandler.queryCalleeFullMethodByHash(callGraphExtractedFile.getMethodHash());
        } else {
            fullMethod = methodCallHandler.queryCallerFullMethodByHash(callGraphExtractedFile.getMethodHash());
        }
        callGraphExtractedFile.setFullMethod(fullMethod);
        if (fullMethod != null) {
            String className = JavaCG2ClassMethodUtil.getClassNameFromMethod(fullMethod);
            String simpleClassName = dbOperWrapper.querySimpleClassName(className);
            String methodName = JavaCG2ClassMethodUtil.getMethodNameFromFull(fullMethod);
            callGraphExtractedFile.setClassName(className);
            callGraphExtractedFile.setSimpleClassName(simpleClassName);
            callGraphExtractedFile.setMethodName(methodName);
        }
    }
}

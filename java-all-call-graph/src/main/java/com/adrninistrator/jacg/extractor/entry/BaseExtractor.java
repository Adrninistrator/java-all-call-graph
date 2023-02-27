package com.adrninistrator.jacg.extractor.entry;

import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.conf.ConfInfo;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.dto.call_graph_result.AbstractCallGraphResultFileInfo;
import com.adrninistrator.jacg.dto.method.MethodInfoInFileName;
import com.adrninistrator.jacg.find_keyword.FindKeywordCallGraph;
import com.adrninistrator.jacg.util.JACGCallGraphFileUtil;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author adrninistrator
 * @date 2022/8/29
 * @description: 对调用链结果文件进行数据提取的基类
 */
public abstract class BaseExtractor {
    private static final Logger logger = LoggerFactory.getLogger(BaseExtractor.class);

    protected boolean inited = false;

    // 保存当前查找关键字对应的结果目录
    protected String currentFindResultDirPath;

    protected ConfInfo confInfo;

    protected FindKeywordCallGraph findKeywordCallGraph;

    protected DbOperator dbOperator;

    protected DbOperWrapper dbOperWrapper;

    /**
     * 选择生成包含关键字的所有方法到起始方法之间的调用链方向
     *
     * @return
     */
    protected abstract boolean chooseFindKeywordCallGraph();

    // 初始化
    public void init(ConfigureWrapper configureWrapper) {
        synchronized (this) {
            if (inited) {
                logger.info("已完成初始化");
                return;
            }

            findKeywordCallGraph = new FindKeywordCallGraph();
            findKeywordCallGraph.init(chooseFindKeywordCallGraph(), configureWrapper);

            // 设置处理目录时，需要返回生成文件路径列表
            findKeywordCallGraph.setReturnResultFileList();

            confInfo = findKeywordCallGraph.getConfInfo();

            // 判断生成调用链时的详细程度是否为最详细
            if (!OutputDetailEnum.ODE_1.getDetail().equals(confInfo.getCallGraphOutputDetail())) {
                logger.warn("生成调用链时的详细程度自动设置为最详细 {} {}", ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL.getKey(), OutputDetailEnum.ODE_1.getDetail());
                confInfo.setCallGraphOutputDetail(OutputDetailEnum.ODE_1.getDetail());
            }

            inited = true;
        }
    }

    private void checkInited() {
        if (!inited) {
            logger.error("相关对象未完成初始化，请先调用init()方法");
            throw new JavaCGRuntimeException("相关对象未完成初始化，请先调用init()方法");
        }
    }

    /**
     * 创建数据库相关对象
     *
     * @return true: 创建成功 false: 创建失败
     */
    protected boolean genDbObject() {
        if (dbOperator != null) {
            return true;
        }

        dbOperator = DbOperator.genInstance(confInfo, this.getClass().getSimpleName());
        if (dbOperator == null) {
            return false;
        }

        dbOperWrapper = new DbOperWrapper(dbOperator);
        return true;
    }

    /**
     * 关闭数据源
     */
    protected void closeDs() {
        if (dbOperator != null) {
            dbOperator.closeDs();
        }
    }

    // 返回当前查找关键字对应的结果目录
    public String getCurrentFindResultDirPath() {
        return currentFindResultDirPath;
    }

    // 处理向上的方法完整调用关键字搜索结果文件信息
    protected void fillResultFileInfo4Callee(String resultFilePath, AbstractCallGraphResultFileInfo callGraphResultFileInfo) {
        fillResultFileInfo(resultFilePath, callGraphResultFileInfo, true);
    }

    // 处理向下的方法完整调用关键字搜索结果文件信息
    protected void fillResultFileInfo4Caller(String resultFilePath, AbstractCallGraphResultFileInfo callGraphResultFileInfo) {
        fillResultFileInfo(resultFilePath, callGraphResultFileInfo, false);
    }

    // 处理关键字搜索结果文件信息
    private void fillResultFileInfo(String resultFilePath, AbstractCallGraphResultFileInfo callGraphResultFileInfo, boolean order4ee) {
        callGraphResultFileInfo.setFilePath(resultFilePath);

        String fileName = JACGFileUtil.getFileNameFromPath(resultFilePath);
        callGraphResultFileInfo.setFileName(fileName);

        if (JACGCallGraphFileUtil.isEmptyCallGraphFileName(fileName)) {
            // 文件内容为空
            callGraphResultFileInfo.setEmptyFile(true);
            return;
        }

        // 文件内容非空
        MethodInfoInFileName methodInfoInFileName = JACGCallGraphFileUtil.getMethodInfoFromFileName(fileName);
        if (methodInfoInFileName == null) {
            return;
        }

        callGraphResultFileInfo.setSimpleClassName(methodInfoInFileName.getSimpleClassName());
        callGraphResultFileInfo.setMethodName(methodInfoInFileName.getMethodName());
        callGraphResultFileInfo.setMethodHash(methodInfoInFileName.getMethodHash());

        if (callGraphResultFileInfo.isEmptyFile()) {
            return;
        }

        // 根据调用者完整方法HASH+长度，从方法调用表获取对应的完整方法
        String fullMethod;
        if (order4ee) {
            fullMethod = dbOperWrapper.getCalleeFullMethodFromHash(callGraphResultFileInfo.getMethodHash());
        } else {
            fullMethod = dbOperWrapper.getCallerFullMethodFromHash(callGraphResultFileInfo.getMethodHash());
        }
        callGraphResultFileInfo.setFullMethod(fullMethod);
        if (fullMethod != null) {
            callGraphResultFileInfo.setClassName(JACGClassMethodUtil.getClassNameFromMethod(fullMethod));
        }
    }

    // 记录当前查找关键字对应的结果目录
    protected void recordCurrentFindResultDirPath() {
        currentFindResultDirPath = findKeywordCallGraph.getCurrentDirPath();
    }

    /**
     * 获取配置信息
     *
     * @return
     */
    public ConfInfo getConfInfo() {
        checkInited();
        return findKeywordCallGraph.getConfInfo();
    }
}

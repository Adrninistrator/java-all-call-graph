package com.adrninistrator.jacg.extractor.entry;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.common.enums.OutputDetailEnum;
import com.adrninistrator.jacg.conf.ConfInfo;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dboper.DbOperator;
import com.adrninistrator.jacg.dto.method.MethodInfoInFileName;
import com.adrninistrator.jacg.extensions.find_filter.BaseFindKeywordFilter;
import com.adrninistrator.jacg.extractor.dto.result.BaseResultFile;
import com.adrninistrator.jacg.find_keyword.FindKeywordCallGraph;
import com.adrninistrator.jacg.util.JACGCallGraphFileUtil;
import com.adrninistrator.jacg.util.JACGUtil;
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

    // 关键字搜索自定义过滤处理类
    protected BaseFindKeywordFilter baseFindKeywordFilter;

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
    public boolean init(ConfigureWrapper configureWrapper) {
        synchronized (this) {
            if (inited) {
                return true;
            }

            findKeywordCallGraph = new FindKeywordCallGraph();
            findKeywordCallGraph.init(chooseFindKeywordCallGraph(), configureWrapper);

            // 设置处理目录时，需要返回生成文件路径列表
            findKeywordCallGraph.setReturnResultFileList();

            // 添加关键字过滤处理类
            findKeywordCallGraph.setBaseFindKeywordFilter(baseFindKeywordFilter);

            confInfo = findKeywordCallGraph.getConfInfo();

            // 判断生成调用链时的详细程度是否为最详细
            if (!OutputDetailEnum.ODE_1.getDetail().equals(confInfo.getCallGraphOutputDetail())) {
                logger.error("生成调用链时的详细程度需要设置为最详细 {} {}", ConfigKeyEnum.CKE_CALL_GRAPH_OUTPUT_DETAIL.getKey(), OutputDetailEnum.ODE_1.getDetail());
                return false;
            }

            if (!confInfo.isShowCallerLineNum()) {
                logger.error("生成调用链时需要显示调用者源代码行号 {}", ConfigKeyEnum.CKE_SHOW_CALLER_LINE_NUM.getKey());
                return false;
            }

            inited = true;
            return true;
        }
    }

    private void checkInited() {
        if (!inited) {
            logger.error("相关对象未完成初始化，请先调用init()方法");
            throw new RuntimeException("相关对象未完成初始化，请先调用init()方法");
        }
    }

    /**
     * 创建数据库相关对象
     *
     * @return
     */
    protected boolean genDbObject() {
        dbOperator = DbOperator.genInstance(confInfo);
        if (dbOperator == null) {
            return false;
        }

        dbOperWrapper = new DbOperWrapper(dbOperator, confInfo.getAppName());
        return true;
    }

    /**
     * 关闭数据源
     */
    protected void closeDs() {
        dbOperator.closeDs();
    }

    // 返回当前查找关键字对应的结果目录
    public String getCurrentFindResultDirPath() {
        return currentFindResultDirPath;
    }

    // 设置关键字搜索自定义过滤处理类
    public void setBaseFindKeywordFilter(BaseFindKeywordFilter baseFindKeywordFilter) {
        this.baseFindKeywordFilter = baseFindKeywordFilter;
    }

    // 处理关键字搜索结果文件
    protected void handleResultFile(String filePath, BaseResultFile baseResultFile) {
        baseResultFile.setFilePath(filePath);

        String fileName = JACGUtil.getFileNameFromPath(filePath);
        baseResultFile.setFileName(fileName);
        if (fileName.endsWith(JACGConstants.EXT_EMPTY_MD)) {
            // 文件内容为空
            baseResultFile.setEmptyFile(true);
            return;
        }

        // 文件内容非空
        MethodInfoInFileName methodInfoInFileName = JACGCallGraphFileUtil.getMethodInfoFromFileName(fileName);
        if (methodInfoInFileName == null) {
            return;
        }

        baseResultFile.setClassName(methodInfoInFileName.getSimpleClassName());
        baseResultFile.setMethodName(methodInfoInFileName.getMethodName());
        baseResultFile.setMethodHash(methodInfoInFileName.getMethodHash());
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

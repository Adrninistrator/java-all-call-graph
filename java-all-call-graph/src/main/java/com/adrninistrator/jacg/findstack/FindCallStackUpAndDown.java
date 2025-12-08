package com.adrninistrator.jacg.findstack;

import com.adrninistrator.jacg.common.list.ListWithResult;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.dto.callstack.CallStackFileResult;
import com.adrninistrator.jacg.extractor.dto.common.extractfile.CalleeExtractedFile;
import com.adrninistrator.jacg.extractor.entry.CalleeGraphEntryExtractor;
import com.adrninistrator.jacg.util.JACGCallStackUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Set;

/**
 * @author adrninistrator
 * @date 2025/5/24
 * @description: 对指定的方法向上生成方法完整调用链并找到入口方法，并向下生成方法完整调用链并找到包含特定关键字的方法的调用堆栈
 */
public class FindCallStackUpAndDown {

    private static final Logger logger = LoggerFactory.getLogger(FindCallStackUpAndDown.class);

    protected final ConfigureWrapper configureWrapper;

    public FindCallStackUpAndDown(ConfigureWrapper configureWrapper) {
        this.configureWrapper = configureWrapper;
    }

    /**
     * 用于执行的方法
     *
     * @return
     */
    public CallStackFileResult find() {
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_FLAG, "_upward");
        // 对指定的方法向上生成方法完整调用链并找到入口方法
        CalleeGraphEntryExtractor calleeGraphEntryExtractor = new CalleeGraphEntryExtractor(configureWrapper);
        ListWithResult<CalleeExtractedFile> listWithResult = calleeGraphEntryExtractor.extract();
        if (!listWithResult.isSuccess()) {
            return CallStackFileResult.FAIL;
        }

        // 向下生成方法完整调用链并找到包含特定关键字的方法的调用堆栈
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_FLAG, "_downward");
        // 通过对方法调用堆栈提取的结果，获得所有的入口完整方法
        Set<String> entryFullMethodSet = JACGCallStackUtil.getAllEntryFullMethod(listWithResult);
        if (entryFullMethodSet.isEmpty()) {
            logger.error("未查询到对应的入口完整方法");
            return CallStackFileResult.FAIL;
        }
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER, entryFullMethodSet);
        FindCallStackTrace findCallStackTrace = new FindCallStackTrace(false, configureWrapper);
        return findCallStackTrace.find();
    }
}

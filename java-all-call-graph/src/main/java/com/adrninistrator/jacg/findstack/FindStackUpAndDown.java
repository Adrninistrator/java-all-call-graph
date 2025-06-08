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
 * @description: 对指定的方法向上生成完整方法调用链并找到入口方法，并向下生成完整方法调用链并找到包含特定关键字的方法的调用堆栈
 */
public class FindStackUpAndDown {

    private static final Logger logger = LoggerFactory.getLogger(FindStackUpAndDown.class);

    protected final ConfigureWrapper configureWrapper;

    public FindStackUpAndDown(ConfigureWrapper configureWrapper) {
        this.configureWrapper = configureWrapper;
    }

    public boolean find() {
        ConfigureWrapper configureWrapper1 = configureWrapper.copy();
        configureWrapper1.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_FLAG, "_upward");
        // 对指定的方法向上生成完整方法调用链并找到入口方法
        CalleeGraphEntryExtractor calleeGraphEntryExtractor = new CalleeGraphEntryExtractor(configureWrapper1);
        ListWithResult<CalleeExtractedFile> listWithResult = calleeGraphEntryExtractor.extract();
        if (!listWithResult.isSuccess()) {
            return false;
        }

        // 向下生成完整方法调用链并找到包含特定关键字的方法的调用堆栈
        ConfigureWrapper configureWrapper2 = configureWrapper.copy();
        configureWrapper2.setMainConfig(ConfigKeyEnum.CKE_OUTPUT_DIR_FLAG, "_downward");
        // 通过对方法调用堆栈提取的结果，获得所有的入口完整方法
        Set<String> entryFullMethodSet = JACGCallStackUtil.getAllEntryFullMethod(listWithResult);
        if (entryFullMethodSet.isEmpty()) {
            logger.error("未查询到对应的入口完整方法");
            return false;
        }
        configureWrapper2.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLER, entryFullMethodSet);
        FindCallStackTrace findCallStackTrace = new FindCallStackTrace(false, configureWrapper2);
        CallStackFileResult callStackFileResult = findCallStackTrace.find();
        return callStackFileResult.isSuccess();
    }
}

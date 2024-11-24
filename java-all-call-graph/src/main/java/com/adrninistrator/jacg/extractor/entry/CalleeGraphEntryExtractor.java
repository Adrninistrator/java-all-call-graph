package com.adrninistrator.jacg.extractor.entry;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.common.list.ListWithResult;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.extractor.dto.common.extractfile.CalleeExtractedFile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author adrninistrator
 * @date 2023/4/22
 * @description: 对向上的方法调用链文件进行数据提取，提取从指定被调用方法到对应的入口方法的信息
 */
public class CalleeGraphEntryExtractor extends CalleeGraphBaseExtractor {

    private static final Logger logger = LoggerFactory.getLogger(CalleeGraphEntryExtractor.class);

    /**
     * 生成向上的完整调用链，根据关键字进行查找，获取入口方法信息并返回，使用配置文件中的参数
     *
     * @return
     */
    public ListWithResult<CalleeExtractedFile> extract() {
        ConfigureWrapper configureWrapper = new ConfigureWrapper(false);
        // 指定生成方法调用堆栈时的关键字使用代表入口方法的标志
        if (!setFindStackKeyword4ee(configureWrapper)) {
            logger.warn("未查询到生成方法调用堆栈时的关键字");
            return ListWithResult.genEmpty();
        }
        return baseExtract(configureWrapper);
    }

    /**
     * 生成向上的完整调用链，根据关键字进行查找，获取入口方法信息并返回，通过代码指定配置参数
     *
     * @param configureWrapper
     * @return
     */
    public ListWithResult<CalleeExtractedFile> extract(ConfigureWrapper configureWrapper) {
        // 指定生成方法调用堆栈时的关键字使用代表入口方法的标志
        if (!setFindStackKeyword4ee(configureWrapper)) {
            return ListWithResult.genEmpty();
        }
        return baseExtract(configureWrapper);
    }

    /**
     * 指定生成方法调用堆栈时的关键字使用代表入口方法的标志
     *
     * @param configureWrapper
     * @return true: 关键字非空，需要查找 false: 关键字为空,不需要查找
     */
    protected boolean setFindStackKeyword4ee(ConfigureWrapper configureWrapper) {
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4EE, JACGConstants.CALLEE_FLAG_ENTRY);
        return true;
    }
}
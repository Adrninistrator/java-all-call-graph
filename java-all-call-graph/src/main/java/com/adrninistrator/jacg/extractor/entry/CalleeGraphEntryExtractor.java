package com.adrninistrator.jacg.extractor.entry;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.extractor.dto.common.extract_file.CalleeExtractedFile;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/4/22
 * @description: 对调用链结果文件进行数据提取，向上的方法调用链，提取从指定被调用方法对应的入口方法的信息
 */
public class CalleeGraphEntryExtractor extends CalleeGraphBaseExtractor {

    /**
     * 生成向上的完整调用链，根据关键字进行查找，获取入口方法信息并返回，使用配置文件中的参数
     *
     * @return
     */
    public List<CalleeExtractedFile> extract() {
        ConfigureWrapper configureWrapper = new ConfigureWrapper(false);
        // 指定生成方法调用堆栈时的关键字使用代表入口方法的标志
        setFindStackKeyword4ee(configureWrapper);
        return baseExtract(configureWrapper);
    }

    /**
     * 生成向上的完整调用链，根据关键字进行查找，获取入口方法信息并返回，通过代码指定配置参数
     *
     * @param configureWrapper
     * @return
     */
    public List<CalleeExtractedFile> extract(ConfigureWrapper configureWrapper) {
        // 指定生成方法调用堆栈时的关键字使用代表入口方法的标志
        setFindStackKeyword4ee(configureWrapper);
        return baseExtract(configureWrapper);
    }

    // 指定生成方法调用堆栈时的关键字使用代表入口方法的标志
    private void setFindStackKeyword4ee(ConfigureWrapper configureWrapper) {
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4EE, JACGConstants.CALLEE_FLAG_ENTRY);
    }
}
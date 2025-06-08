package com.adrninistrator.jacg.extractor.entry;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.handler.spring.SpringHandler;
import com.adrninistrator.javacg2.util.JavaCG2Util;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/11/19
 * @description: 对向上的方法调用链文件进行数据提取，提取从指定被调用方法到（可能的）Spring Controller文件上传方法的信息
 */
public class CalleeGraphSPCFileUploadExtractor extends CalleeGraphEntryExtractor {

    public CalleeGraphSPCFileUploadExtractor(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    // 指定生成方法调用堆栈时的关键字使用代表入口方法的标志
    @Override
    protected boolean setFindStackKeyword4ee() {
        try (SpringHandler springHandler = new SpringHandler(configureWrapper)) {
            List<String> fileUploadControllerFullMethodList = springHandler.queryFileUploadControllerOnlyMethod();
            // 对（可能的）Spring Controller文件上传方法的处理
            fileUploadControllerFullMethodList = handleFileUploadControllerFullMethodList(fileUploadControllerFullMethodList, configureWrapper);
            if (JavaCG2Util.isCollectionEmpty(fileUploadControllerFullMethodList)) {
                return false;
            }
            configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4EE, fileUploadControllerFullMethodList);
            return true;
        }
    }

    /**
     * 对（可能的）Spring Controller文件上传方法的处理，可重载进行自定义处理
     *
     * @param fileUploadControllerFullMethodList
     * @param configureWrapper
     * @return
     */
    protected List<String> handleFileUploadControllerFullMethodList(List<String> fileUploadControllerFullMethodList, ConfigureWrapper configureWrapper) {
        return fileUploadControllerFullMethodList;
    }
}
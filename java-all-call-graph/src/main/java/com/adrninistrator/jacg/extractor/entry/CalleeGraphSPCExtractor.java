package com.adrninistrator.jacg.extractor.entry;

import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.handler.spring.SpringHandler;
import com.adrninistrator.javacg2.util.JavaCG2Util;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/11/19
 * @description: 对向上的方法调用链文件进行数据提取，提取从指定被调用方法到Spring Controller方法的信息
 */
public class CalleeGraphSPCExtractor extends CalleeGraphEntryExtractor {

    public CalleeGraphSPCExtractor(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    // 指定生成方法调用堆栈时的关键字使用代表入口方法的标志
    @Override
    protected boolean setFindStackKeyword4ee() {
        try (SpringHandler springHandler = new SpringHandler(configureWrapper)) {
            List<String> list = springHandler.queryAllControllerFullMethod();
            if (JavaCG2Util.isCollectionEmpty(list)) {
                return false;
            }
            configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4EE, list);
            return true;
        }
    }
}
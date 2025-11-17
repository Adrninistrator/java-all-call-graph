package com.adrninistrator.jacg.extractor.entry;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.list.ListWithResult;
import com.adrninistrator.jacg.conf.ConfigureWrapper;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.dto.method.MethodDetail;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodInfo;
import com.adrninistrator.jacg.extractor.dto.common.extract.CalleeExtractedLine;
import com.adrninistrator.jacg.extractor.dto.common.extractfile.CalleeExtractedFile;
import com.adrninistrator.jacg.handler.method.MethodInfoHandler;
import com.adrninistrator.jacg.handler.methodcall.MethodCallHandler;

/**
 * @author adrninistrator
 * @date 2023/4/22
 * @description: 对向上的方法完整调用链文件进行数据提取，提取从指定被调用方法到对应的入口方法的信息
 */
public class CalleeGraphEntryExtractor extends CalleeGraphBaseExtractor {

    private MethodCallHandler methodCallHandler;
    private MethodInfoHandler methodInfoHandler;

    public CalleeGraphEntryExtractor(ConfigureWrapper configureWrapper) {
        super(configureWrapper);
    }

    /**
     * 生成向上的完整调用链，根据关键字进行查找，获取入口方法信息并返回，通过代码指定配置参数
     *
     * @return
     */
    public ListWithResult<CalleeExtractedFile> extract() {
        // 指定生成方法调用堆栈时的关键字使用代表入口方法的标志
        if (!setFindStackKeyword4ee()) {
            return ListWithResult.genEmpty();
        }
        return baseExtract();
    }

    /**
     * 自定义初始化操作
     */
    @Override
    protected void customInit() {
        methodCallHandler = new MethodCallHandler(dbOperWrapper);
        methodInfoHandler = new MethodInfoHandler(dbOperWrapper);
    }

    /**
     * 对处理文件某行的数据进行自定义处理
     *
     * @param calleeExtractedLine
     */
    @Override
    protected void customHandleCalleeExtractedLine(CalleeExtractedLine calleeExtractedLine) {
        MethodDetail methodDetail = calleeExtractedLine.getCallGraphLineParsed().getMethodDetail();
        String entryFullMethod = methodDetail.getFullMethod();
        String entryMethodReturnType = methodDetail.getReturnType();
        // 检查入口方法是否存在父类/接口调用子类/实现类的情况
        if (methodCallHandler.checkExistsSuperCallChild(entryFullMethod, entryMethodReturnType)) {
            calleeExtractedLine.setEntryMethodExistsSuperCallChild(true);
        }

        // 检查入口方法是否定义在父类或接口中（不在当前对应的类中）
        WriteDbData4MethodInfo entryMethodInfo = methodInfoHandler.queryMethodInfoByFullMethod(entryFullMethod, entryMethodReturnType);
        if (entryMethodInfo == null && methodInfoHandler.checkExistsMethodByFullMethodSuperInterface(entryFullMethod, entryMethodReturnType)) {
            calleeExtractedLine.setEntryMethodDefineInSuperInterface(true);
        }
    }

    /**
     * 指定生成方法调用堆栈时的关键字使用代表入口方法的标志
     *
     * @return true: 关键字非空，需要查找 false: 关键字为空,不需要查找
     */
    protected boolean setFindStackKeyword4ee() {
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4EE, JACGConstants.CALLEE_FLAG_ENTRY);
        return true;
    }
}
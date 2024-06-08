package com.adrninistrator.jacg.handler.dto.exception;

import com.adrninistrator.jacg.util.JACGMethodCallInfoUtil;

/**
 * @author adrninistrator
 * @date 2024/1/6
 * @description: 方法调用的被调用对象或参数使用catch的异常对象，被调用方法不符合预期
 */
public class MCEU4MethodCallUseE extends BaseMethodCatchExceptionUsage {

    // 使用异常对象在的调用方法代码行号
    protected int useECallerLineNumber;

    // 使用异常对象的被调用方完整方法
    protected String useECalleeFullMethod;

    // 异常对象在方法调用中的被调用对象或方法参数序号
    protected int useEObjArgSeq;

    @Override
    public String getUsageDescription() {
        if (!isUseEInExpectedMethodCall()) {
            return "方法调用的被调用对象或参数使用catch的异常对象，被调用方法不符合预期";
        }
        return "方法调用的被调用对象或参数使用catch的异常对象，被调用方法符合预期";
    }

    @Override
    public String getUsageDetail() {
        return "代码第 " + useECallerLineNumber +
                " 行，调用 " + useECalleeFullMethod +
                " 方法时，" + JACGMethodCallInfoUtil.genObjArgDesc(useEObjArgSeq) + "使用catch的异常对象";
    }

    public int getUseECallerLineNumber() {
        return useECallerLineNumber;
    }

    public void setUseECallerLineNumber(int useECallerLineNumber) {
        this.useECallerLineNumber = useECallerLineNumber;
    }

    public String getUseECalleeFullMethod() {
        return useECalleeFullMethod;
    }

    public void setUseECalleeFullMethod(String useECalleeFullMethod) {
        this.useECalleeFullMethod = useECalleeFullMethod;
    }

    public int getUseEObjArgSeq() {
        return useEObjArgSeq;
    }

    public void setUseEObjArgSeq(int useEObjArgSeq) {
        this.useEObjArgSeq = useEObjArgSeq;
    }
}

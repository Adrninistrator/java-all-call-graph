package com.adrninistrator.jacg.handler.dto.exception;

import com.adrninistrator.jacg.util.JACGMethodCallInfoUtil;

/**
 * @author adrninistrator
 * @date 2024/1/6
 * @description: throw的（构造函数）方法调用参数使用了catch的异常对象的方法调用返回值
 */
public class MCEU4ThrowMCUseEMCReturn extends MCEU4ThrowMCUseE {

    // 使用异常对象在的调用方法代码行号
    private int useECallerLineNumber;

    // 使用异常对象的被调用方完整方法
    private String useECalleeFullMethod;

    // 异常对象在方法调用中的被调用对象或方法参数序号
    private int useEObjArgSeq;

    @Override
    public String getUsageDescription() {
        return "throw的（构造函数）方法调用参数使用了catch的异常对象的方法调用返回值";
    }

    @Override
    public String getUsageDetail() {
        return "代码第 " + useECallerLineNumber + " 行调用 " +
                useECalleeFullMethod + " 方法时，" +
                JACGMethodCallInfoUtil.genObjArgDesc(useEObjArgSeq) + "使用catch的异常对象。" +
                "代码第 " + throwLineNumber + " 行调用 " +
                throwCalleeFullMethod + " 方法，将返回值作为异常抛出，" +
                JACGMethodCallInfoUtil.genObjArgDesc(throwExceptionObjArgSeq) + "使用以上方法调用返回值";
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

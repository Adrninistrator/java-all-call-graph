package com.adrninistrator.jacg.handler.dto.exception;

import com.adrninistrator.jacg.util.JACGMethodCallInfoUtil;

/**
 * @author adrninistrator
 * @date 2024/1/6
 * @description: 方法调用的被调用对象或参数使用catch的异常对象的方法调用返回值，外层被调用方法不符合预期
 */
public class MCEU4MethodCallUseEMCReturn extends MCEU4MethodCallUseE {

    // 使用异常的方法调用返回值作为被调用对象或参数的方法调用代码行号
    private int useEReturnCallerLineNumber;

    // 使用异常的方法调用返回值作为被调用对象或参数的被调用完整方法
    private String useEReturnCalleeFullMethod;

    // 调用指定方法时，使用异常的方法调用返回值作为被调用对象或参数序号
    private int useEReturnObjArgSeq;

    @Override
    public String getUsageDescription() {
        if (!isUseEInExpectedMethodCall()) {
            return "方法调用的被调用对象或参数使用catch的异常对象的方法调用返回值，外层被调用方法不符合预期";
        }
        return "方法调用的被调用对象或参数使用catch的异常对象的方法调用返回值，外层被调用方法符合预期";
    }

    @Override
    public String getUsageDetail() {
        return "第 " + useEReturnCallerLineNumber +
                " 行，调用 " + useEReturnCalleeFullMethod +
                " 方法时，" + JACGMethodCallInfoUtil.genObjArgDesc(useEReturnObjArgSeq) +
                "使用catch的异常对象。代码第 " + useECallerLineNumber +
                " 行，调用 " + useECalleeFullMethod +
                " 方法时，使用以上方法调用返回值作为" + JACGMethodCallInfoUtil.genObjArgDesc(useEObjArgSeq);
    }

    public int getUseEReturnCallerLineNumber() {
        return useEReturnCallerLineNumber;
    }

    public void setUseEReturnCallerLineNumber(int useEReturnCallerLineNumber) {
        this.useEReturnCallerLineNumber = useEReturnCallerLineNumber;
    }

    public String getUseEReturnCalleeFullMethod() {
        return useEReturnCalleeFullMethod;
    }

    public void setUseEReturnCalleeFullMethod(String useEReturnCalleeFullMethod) {
        this.useEReturnCalleeFullMethod = useEReturnCalleeFullMethod;
    }

    public int getUseEReturnObjArgSeq() {
        return useEReturnObjArgSeq;
    }

    public void setUseEReturnObjArgSeq(int useEReturnObjArgSeq) {
        this.useEReturnObjArgSeq = useEReturnObjArgSeq;
    }
}

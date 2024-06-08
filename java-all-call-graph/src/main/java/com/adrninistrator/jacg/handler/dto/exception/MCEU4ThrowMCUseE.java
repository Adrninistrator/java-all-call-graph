package com.adrninistrator.jacg.handler.dto.exception;

import com.adrninistrator.jacg.util.JACGMethodCallInfoUtil;

/**
 * @author adrninistrator
 * @date 2024/1/6
 * @description: throw的（构造函数）方法调用参数使用了catch的异常对象
 */
public class MCEU4ThrowMCUseE extends MCEU4ThrowE {

    // 通过throw抛出异常的被调用完整方法
    protected String throwCalleeFullMethod;

    // 异常对象在通过throw抛出异常的方法调用中的被调用对象或方法参数序号
    protected int throwExceptionObjArgSeq;

    @Override
    public String getUsageDescription() {
        return "throw的（构造函数）方法调用参数使用了catch的异常对象";
    }

    @Override
    public String getUsageDetail() {
        return "代码第 " + throwLineNumber + " 行调用 " +
                throwCalleeFullMethod + " 方法，将返回值作为异常抛出，" +
                JACGMethodCallInfoUtil.genObjArgDesc(throwExceptionObjArgSeq) + "使用catch的异常对象";
    }

    public String getThrowCalleeFullMethod() {
        return throwCalleeFullMethod;
    }

    public void setThrowCalleeFullMethod(String throwCalleeFullMethod) {
        this.throwCalleeFullMethod = throwCalleeFullMethod;
    }

    public int getThrowExceptionObjArgSeq() {
        return throwExceptionObjArgSeq;
    }

    public void setThrowExceptionObjArgSeq(int throwExceptionObjArgSeq) {
        this.throwExceptionObjArgSeq = throwExceptionObjArgSeq;
    }
}

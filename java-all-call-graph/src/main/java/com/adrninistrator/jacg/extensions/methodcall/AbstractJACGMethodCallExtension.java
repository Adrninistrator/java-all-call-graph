package com.adrninistrator.jacg.extensions.methodcall;

import com.adrninistrator.jacg.dboper.DbOperWrapper;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCall;

/**
 * @author adrninistrator
 * @date 2025/2/15
 * @description: 方法调用处理扩展类抽象父类
 */
public abstract class AbstractJACGMethodCallExtension {

    protected DbOperWrapper dbOperWrapper;

    AbstractJACGMethodCallExtension(DbOperWrapper dbOperWrapper) {
        this.dbOperWrapper = dbOperWrapper;
    }

    /**
     * 返回对应的占位的方法调用类型
     * 即 JavaCG2ObjInstanceMethodNameMCE 子类的 TYPE
     *
     * @return 方法调用类型
     */
    public abstract String getCallType();

    /**
     * 通过占位的方法调用类型，判断当前方法调用是否需要处理
     *
     * @param methodCall 方法调用信息
     * @return true: 需要处理当前方法调用 false: 不需要处理当前方法调用
     */
    public boolean checkNeedHandle(WriteDbData4MethodCall methodCall) {
        return getCallType().equals(methodCall.getCallType());
    }

    /**
     * 对方法调用进行处理
     *
     * @param methodCall
     * @return true: 有对当前方法调用进行处理 false: 未对当前方法调用进行处理
     */
    public abstract boolean handle(WriteDbData4MethodCall methodCall);
}

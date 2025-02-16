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
     * 对方法调用进行处理
     *
     * @param methodCall
     * @return true: 有对当前方法调用进行处理 false: 未对当前方法调用进行处理
     */
    public abstract boolean handle(WriteDbData4MethodCall methodCall);
}

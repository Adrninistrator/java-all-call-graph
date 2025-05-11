package com.adrninistrator.jacg.extractor.dto.springtx.entrymethod;

import com.adrninistrator.jacg.common.enums.SpecialCallTypeEnum;
import com.adrninistrator.jacg.dto.infowithhash.AbstractInfoWithMethodHash;

/**
 * @author adrninistrator
 * @date 2023/2/23
 * @description: Spring事务入口方法，使用事务模板
 */
public class SpTxEntryMethodTxTpl extends AbstractInfoWithMethodHash {
    // 调用类型：匿名内部类、Lambda表达式
    private final SpecialCallTypeEnum callType;

    // 事务模板中调用的完整方法
    private final String txTplFullMethod;

    // 事务模板中调用的方法返回类型
    private final String txTplReturnType;

    // 调用事务模板的完整方法
    private final String callerFullMethod;

    // 调用事务模板的代码行号
    private final int callerLineNumber;

    public SpTxEntryMethodTxTpl(SpecialCallTypeEnum callType, String txTplFullMethod, String txTplReturnType, String callerFullMethod, int callerLineNumber) {
        this.callType = callType;
        this.txTplFullMethod = txTplFullMethod;
        this.txTplReturnType = txTplReturnType;
        this.callerFullMethod = callerFullMethod;
        this.callerLineNumber = callerLineNumber;
    }

    public SpecialCallTypeEnum getCallType() {
        return callType;
    }

    public String getTxTplFullMethod() {
        return txTplFullMethod;
    }

    public String getTxTplReturnType() {
        return txTplReturnType;
    }

    public String getCallerFullMethod() {
        return callerFullMethod;
    }

    public int getCallerLineNumber() {
        return callerLineNumber;
    }

    @Override
    protected String chooseFullMethod() {
        return txTplFullMethod;
    }

    @Override
    protected String chooseMethodReturnType() {
        return txTplReturnType;
    }
}

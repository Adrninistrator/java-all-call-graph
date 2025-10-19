package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2025/9/21
 * @description: 用于写入数据库的数据，方法调用被调用对象的原始类型
 */
public class WriteDbData4MethodCallRawCallee implements BaseWriteDbData {
    private int callId;
    private String rawCalleeClassName;

    public int getCallId() {
        return callId;
    }

    public void setCallId(int callId) {
        this.callId = callId;
    }

    public String getRawCalleeClassName() {
        return rawCalleeClassName;
    }

    public void setRawCalleeClassName(String rawCalleeClassName) {
        this.rawCalleeClassName = rawCalleeClassName;
    }
}

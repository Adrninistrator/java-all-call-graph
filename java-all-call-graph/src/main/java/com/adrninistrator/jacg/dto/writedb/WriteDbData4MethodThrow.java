package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2024/1/2
 * @description: 用于写入数据库的数据，方法的throw信息
 */
public class WriteDbData4MethodThrow implements BaseWriteDbData {

    private int recordId;
    private String methodHash;
    private String simpleClassName;
    private int throwOffset;
    private int lineNumber;
    private int seq;
    private String throwExceptionType;
    private String throwFlag;
    private Integer catchStartOffset;
    private String catchExceptionVariableName;
    private Integer callId;
    private String fullMethod;

    public int getRecordId() {
        return recordId;
    }

    public void setRecordId(int recordId) {
        this.recordId = recordId;
    }

    public String getMethodHash() {
        return methodHash;
    }

    public void setMethodHash(String methodHash) {
        this.methodHash = methodHash;
    }

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public void setSimpleClassName(String simpleClassName) {
        this.simpleClassName = simpleClassName;
    }

    public int getThrowOffset() {
        return throwOffset;
    }

    public void setThrowOffset(int throwOffset) {
        this.throwOffset = throwOffset;
    }

    public int getLineNumber() {
        return lineNumber;
    }

    public void setLineNumber(int lineNumber) {
        this.lineNumber = lineNumber;
    }

    public int getSeq() {
        return seq;
    }

    public void setSeq(int seq) {
        this.seq = seq;
    }

    public String getThrowExceptionType() {
        return throwExceptionType;
    }

    public void setThrowExceptionType(String throwExceptionType) {
        this.throwExceptionType = throwExceptionType;
    }

    public String getThrowFlag() {
        return throwFlag;
    }

    public void setThrowFlag(String throwFlag) {
        this.throwFlag = throwFlag;
    }

    public Integer getCatchStartOffset() {
        return catchStartOffset;
    }

    public void setCatchStartOffset(Integer catchStartOffset) {
        this.catchStartOffset = catchStartOffset;
    }

    public String getCatchExceptionVariableName() {
        return catchExceptionVariableName;
    }

    public void setCatchExceptionVariableName(String catchExceptionVariableName) {
        this.catchExceptionVariableName = catchExceptionVariableName;
    }

    public Integer getCallId() {
        return callId;
    }

    public void setCallId(Integer callId) {
        this.callId = callId;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }
}

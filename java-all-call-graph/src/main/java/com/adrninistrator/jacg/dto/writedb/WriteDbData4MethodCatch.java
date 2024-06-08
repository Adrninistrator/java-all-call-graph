package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2023/12/2
 * @description: 用于写入数据库的数据，方法的catch信息
 */
public class WriteDbData4MethodCatch implements BaseWriteDbData {

    private int recordId;
    private String methodHash;
    private String simpleClassName;
    private String methodName;
    private String simpleCatchExceptionType;
    private String catchExceptionType;
    private String catchFlag;
    private int tryStartLineNumber;
    private int tryEndLineNumber;
    private int tryMinCallId;
    private int tryMaxCallId;
    private int catchStartOffset;
    private int catchEndOffset;
    private int catchStartLineNumber;
    private int catchEndLineNumber;
    private int catchMinCallId;
    private int catchMaxCallId;
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

    public String getMethodName() {
        return methodName;
    }

    public void setMethodName(String methodName) {
        this.methodName = methodName;
    }

    public String getSimpleCatchExceptionType() {
        return simpleCatchExceptionType;
    }

    public void setSimpleCatchExceptionType(String simpleCatchExceptionType) {
        this.simpleCatchExceptionType = simpleCatchExceptionType;
    }

    public String getCatchExceptionType() {
        return catchExceptionType;
    }

    public void setCatchExceptionType(String catchExceptionType) {
        this.catchExceptionType = catchExceptionType;
    }

    public String getCatchFlag() {
        return catchFlag;
    }

    public void setCatchFlag(String catchFlag) {
        this.catchFlag = catchFlag;
    }

    public int getTryStartLineNumber() {
        return tryStartLineNumber;
    }

    public void setTryStartLineNumber(int tryStartLineNumber) {
        this.tryStartLineNumber = tryStartLineNumber;
    }

    public int getTryEndLineNumber() {
        return tryEndLineNumber;
    }

    public void setTryEndLineNumber(int tryEndLineNumber) {
        this.tryEndLineNumber = tryEndLineNumber;
    }

    public int getTryMinCallId() {
        return tryMinCallId;
    }

    public void setTryMinCallId(int tryMinCallId) {
        this.tryMinCallId = tryMinCallId;
    }

    public int getTryMaxCallId() {
        return tryMaxCallId;
    }

    public void setTryMaxCallId(int tryMaxCallId) {
        this.tryMaxCallId = tryMaxCallId;
    }

    public int getCatchStartOffset() {
        return catchStartOffset;
    }

    public void setCatchStartOffset(int catchStartOffset) {
        this.catchStartOffset = catchStartOffset;
    }

    public int getCatchEndOffset() {
        return catchEndOffset;
    }

    public void setCatchEndOffset(int catchEndOffset) {
        this.catchEndOffset = catchEndOffset;
    }

    public int getCatchStartLineNumber() {
        return catchStartLineNumber;
    }

    public void setCatchStartLineNumber(int catchStartLineNumber) {
        this.catchStartLineNumber = catchStartLineNumber;
    }

    public int getCatchEndLineNumber() {
        return catchEndLineNumber;
    }

    public void setCatchEndLineNumber(int catchEndLineNumber) {
        this.catchEndLineNumber = catchEndLineNumber;
    }

    public int getCatchMinCallId() {
        return catchMinCallId;
    }

    public void setCatchMinCallId(int catchMinCallId) {
        this.catchMinCallId = catchMinCallId;
    }

    public int getCatchMaxCallId() {
        return catchMaxCallId;
    }

    public void setCatchMaxCallId(int catchMaxCallId) {
        this.catchMaxCallId = catchMaxCallId;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }
}

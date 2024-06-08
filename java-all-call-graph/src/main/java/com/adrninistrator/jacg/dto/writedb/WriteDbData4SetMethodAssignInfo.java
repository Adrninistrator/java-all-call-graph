package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData;

/**
 * @author adrninistrator
 * @date 2023/12/5
 * @description: 用于写入数据库的数据，dto的set方法被调用时的赋值信息
 */
public class WriteDbData4SetMethodAssignInfo implements BaseWriteDbData {

    private int setRecordId;
    private int setMethodCallId;
    private int seq;
    private int step;
    private Integer fldRelationshipId;
    private int currCallId;
    private String callerMethodHash;
    private String callerFullMethod;
    private int callerLineNumber;
    private String calleeFullMethod;
    private String setMethodHash;
    private String setFullMethod;
    private int setMethodInSuper;
    private String flag;
    private String flagDesc;
    private String assignInfo;
    private int equivalentConversion;

    public int getSetRecordId() {
        return setRecordId;
    }

    public void setSetRecordId(int setRecordId) {
        this.setRecordId = setRecordId;
    }

    public int getSetMethodCallId() {
        return setMethodCallId;
    }

    public void setSetMethodCallId(int setMethodCallId) {
        this.setMethodCallId = setMethodCallId;
    }

    public int getSeq() {
        return seq;
    }

    public void setSeq(int seq) {
        this.seq = seq;
    }

    public int getStep() {
        return step;
    }

    public void setStep(int step) {
        this.step = step;
    }

    public Integer getFldRelationshipId() {
        return fldRelationshipId;
    }

    public void setFldRelationshipId(Integer fldRelationshipId) {
        this.fldRelationshipId = fldRelationshipId;
    }

    public int getCurrCallId() {
        return currCallId;
    }

    public void setCurrCallId(int currCallId) {
        this.currCallId = currCallId;
    }

    public String getCallerMethodHash() {
        return callerMethodHash;
    }

    public void setCallerMethodHash(String callerMethodHash) {
        this.callerMethodHash = callerMethodHash;
    }

    public String getCallerFullMethod() {
        return callerFullMethod;
    }

    public void setCallerFullMethod(String callerFullMethod) {
        this.callerFullMethod = callerFullMethod;
    }

    public int getCallerLineNumber() {
        return callerLineNumber;
    }

    public void setCallerLineNumber(int callerLineNumber) {
        this.callerLineNumber = callerLineNumber;
    }

    public String getCalleeFullMethod() {
        return calleeFullMethod;
    }

    public void setCalleeFullMethod(String calleeFullMethod) {
        this.calleeFullMethod = calleeFullMethod;
    }

    public String getSetMethodHash() {
        return setMethodHash;
    }

    public void setSetMethodHash(String setMethodHash) {
        this.setMethodHash = setMethodHash;
    }

    public String getSetFullMethod() {
        return setFullMethod;
    }

    public void setSetFullMethod(String setFullMethod) {
        this.setFullMethod = setFullMethod;
    }

    public int getSetMethodInSuper() {
        return setMethodInSuper;
    }

    public void setSetMethodInSuper(int setMethodInSuper) {
        this.setMethodInSuper = setMethodInSuper;
    }

    public String getFlag() {
        return flag;
    }

    public void setFlag(String flag) {
        this.flag = flag;
    }

    public String getFlagDesc() {
        return flagDesc;
    }

    public void setFlagDesc(String flagDesc) {
        this.flagDesc = flagDesc;
    }

    public String getAssignInfo() {
        return assignInfo;
    }

    public void setAssignInfo(String assignInfo) {
        this.assignInfo = assignInfo;
    }

    public int getEquivalentConversion() {
        return equivalentConversion;
    }

    public void setEquivalentConversion(int equivalentConversion) {
        this.equivalentConversion = equivalentConversion;
    }

    @Override
    public String toString() {
        return "WriteDbData4SetMethodAssignInfo{" +
                "setRecordId=" + setRecordId +
                ", setMethodCallId=" + setMethodCallId +
                ", seq=" + seq +
                ", step=" + step +
                ", fldRelationshipId=" + fldRelationshipId +
                ", currCallId=" + currCallId +
                ", callerMethodHash='" + callerMethodHash + '\'' +
                ", callerFullMethod='" + callerFullMethod + '\'' +
                ", callerLineNumber=" + callerLineNumber +
                ", calleeFullMethod='" + calleeFullMethod + '\'' +
                ", setMethodHash='" + setMethodHash + '\'' +
                ", setFullMethod='" + setFullMethod + '\'' +
                ", setMethodInSuper=" + setMethodInSuper +
                ", flag='" + flag + '\'' +
                ", flagDesc='" + flagDesc + '\'' +
                ", assignInfo='" + assignInfo + '\'' +
                ", equivalentConversion=" + equivalentConversion +
                '}';
    }
}

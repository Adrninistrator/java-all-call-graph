package com.adrninistrator.jacg.dto;

/**
 * @author adrninistrator
 * @date 2021/6/23
 * @description:
 */

public class MethodCallEntity {

    private Integer id;
    private String callType;
    private int enabled;
    private String callerMethodHash;
    private String callerFullMethod;
    private String callerMethodName;
    private String callerFullClassName;
    private String callerFullOrSimpleClassName;
    private int callerLineNum;
    private String calleeMethodHash;
    private String finalCalleeFullMethod;
    private String calleeMethodName;
    private String calleeFullClassName;
    private String calleeFullOrSimpleClassName;

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public String getCallType() {
        return callType;
    }

    public void setCallType(String callType) {
        this.callType = callType;
    }

    public int getEnabled() {
        return enabled;
    }

    public void setEnabled(int enabled) {
        this.enabled = enabled;
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

    public String getCallerMethodName() {
        return callerMethodName;
    }

    public void setCallerMethodName(String callerMethodName) {
        this.callerMethodName = callerMethodName;
    }

    public String getCallerFullClassName() {
        return callerFullClassName;
    }

    public void setCallerFullClassName(String callerFullClassName) {
        this.callerFullClassName = callerFullClassName;
    }

    public String getCallerFullOrSimpleClassName() {
        return callerFullOrSimpleClassName;
    }

    public void setCallerFullOrSimpleClassName(String callerFullOrSimpleClassName) {
        this.callerFullOrSimpleClassName = callerFullOrSimpleClassName;
    }

    public int getCallerLineNum() {
        return callerLineNum;
    }

    public void setCallerLineNum(int callerLineNum) {
        this.callerLineNum = callerLineNum;
    }

    public String getCalleeMethodHash() {
        return calleeMethodHash;
    }

    public void setCalleeMethodHash(String calleeMethodHash) {
        this.calleeMethodHash = calleeMethodHash;
    }

    public String getFinalCalleeFullMethod() {
        return finalCalleeFullMethod;
    }

    public void setFinalCalleeFullMethod(String finalCalleeFullMethod) {
        this.finalCalleeFullMethod = finalCalleeFullMethod;
    }

    public String getCalleeMethodName() {
        return calleeMethodName;
    }

    public void setCalleeMethodName(String calleeMethodName) {
        this.calleeMethodName = calleeMethodName;
    }

    public String getCalleeFullClassName() {
        return calleeFullClassName;
    }

    public void setCalleeFullClassName(String calleeFullClassName) {
        this.calleeFullClassName = calleeFullClassName;
    }

    public String getCalleeFullOrSimpleClassName() {
        return calleeFullOrSimpleClassName;
    }

    public void setCalleeFullOrSimpleClassName(String calleeFullOrSimpleClassName) {
        this.calleeFullOrSimpleClassName = calleeFullOrSimpleClassName;
    }
}

package com.adrninistrator.jacg.dto.entity;

/**
 * @author adrninistrator
 * @date 2021/6/23
 * @description:
 */

public class MethodCallEntity {

    private Integer id;
    private String callType;
    private int enabled;
    private String callerJarNum;
    private String callerMethodHash;
    private String callerFullMethod;
    private String callerMethodName;
    private String callerFullClassName;
    private String callerClassName;
    private int callerLineNum;
    private String calleeMethodHash;
    private String calleeFullMethod;
    private String calleeMethodName;
    private String calleeFullClassName;
    private String calleeClassName;

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

    public String getCallerJarNum() {
        return callerJarNum;
    }

    public void setCallerJarNum(String callerJarNum) {
        this.callerJarNum = callerJarNum;
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

    public String getCallerClassName() {
        return callerClassName;
    }

    public void setCallerClassName(String callerClassName) {
        this.callerClassName = callerClassName;
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

    public String getCalleeFullMethod() {
        return calleeFullMethod;
    }

    public void setCalleeFullMethod(String calleeFullMethod) {
        this.calleeFullMethod = calleeFullMethod;
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

    public String getCalleeClassName() {
        return calleeClassName;
    }

    public void setCalleeClassName(String calleeClassName) {
        this.calleeClassName = calleeClassName;
    }
}

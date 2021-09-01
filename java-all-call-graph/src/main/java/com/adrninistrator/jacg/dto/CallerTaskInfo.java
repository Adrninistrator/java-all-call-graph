package com.adrninistrator.jacg.dto;

/**
 * @author adrninistrator
 * @date 2021/8/31
 * @description:
 */
public class CallerTaskInfo {

    private String callerClassName;

    private String callerMethodName;

    private int lineNumStart;

    private int lineNumEnd;

    public String getCallerClassName() {
        return callerClassName;
    }

    public void setCallerClassName(String callerClassName) {
        this.callerClassName = callerClassName;
    }

    public String getCallerMethodName() {
        return callerMethodName;
    }

    public void setCallerMethodName(String callerMethodName) {
        this.callerMethodName = callerMethodName;
    }

    public int getLineNumStart() {
        return lineNumStart;
    }

    public void setLineNumStart(int lineNumStart) {
        this.lineNumStart = lineNumStart;
    }

    public int getLineNumEnd() {
        return lineNumEnd;
    }

    public void setLineNumEnd(int lineNumEnd) {
        this.lineNumEnd = lineNumEnd;
    }
}

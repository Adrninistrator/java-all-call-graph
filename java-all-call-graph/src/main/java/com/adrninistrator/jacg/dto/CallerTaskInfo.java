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

    private String saveDirPath;

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

    public String getSaveDirPath() {
        return saveDirPath;
    }

    public void setSaveDirPath(String saveDirPath) {
        this.saveDirPath = saveDirPath;
    }

    @Override
    public String toString() {
        return "CallerTaskInfo{" +
                "callerClassName='" + callerClassName + '\'' +
                ", callerMethodName='" + callerMethodName + '\'' +
                ", lineNumStart=" + lineNumStart +
                ", lineNumEnd=" + lineNumEnd +
                ", saveDirPath='" + saveDirPath + '\'' +
                '}';
    }
}

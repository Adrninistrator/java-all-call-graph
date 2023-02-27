package com.adrninistrator.jacg.dto.task;

/**
 * @author adrninistrator
 * @date 2021/8/31
 * @description: 生成向下调用链时，在配置文件中指定的任务信息
 */
public class CallerTaskInfo {
    // 配置文件中指定的任务原始文本
    private String origText;

    // 调用者类名，简单类名或完整类名
    private String callerSimpleClassName;

    // 调用者方法名，可包括参数
    private String callerMethodName;

    // 调用者方法代码行号
    private int methodLineNumber;

    // 调用者方法起始代码行号
    private int lineNumStart;

    // 调用者方法结束代码行号
    private int lineNumEnd;

    // 当前任务需要保存的目录
    private String saveDirPath;

    public String getOrigText() {
        return origText;
    }

    public void setOrigText(String origText) {
        this.origText = origText;
    }

    public String getCallerSimpleClassName() {
        return callerSimpleClassName;
    }

    public void setCallerSimpleClassName(String callerSimpleClassName) {
        this.callerSimpleClassName = callerSimpleClassName;
    }

    public String getCallerMethodName() {
        return callerMethodName;
    }

    public void setCallerMethodName(String callerMethodName) {
        this.callerMethodName = callerMethodName;
    }

    public int getMethodLineNumber() {
        return methodLineNumber;
    }

    public void setMethodLineNumber(int methodLineNumber) {
        this.methodLineNumber = methodLineNumber;
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
}

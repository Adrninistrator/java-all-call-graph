package com.adrninistrator.jacg.dto.callstack;

/**
 * @author adrninistrator
 * @date 2025/5/15
 * @description: 生成向上的方法调用堆栈的汇总信息
 */
public class CalleeStackSummary {

    // 调用堆栈在文件中的序号
    private String stackSeq;

    // 完整被调用方法
    private String calleeMethod;

    // 上层完整调用方法
    private String upwardCallerMethod;

    // 向上通过关键字找到的完整方法
    private String keywordMethod;

    // 向上通过关键字找到的方法返回类型
    private String keywordMethodReturnType;

    public CalleeStackSummary() {
    }

    public CalleeStackSummary(String stackSeq, String calleeMethod, String upwardCallerMethod, String keywordMethod, String keywordMethodReturnType) {
        this.stackSeq = stackSeq;
        this.calleeMethod = calleeMethod;
        this.upwardCallerMethod = upwardCallerMethod;
        this.keywordMethod = keywordMethod;
        this.keywordMethodReturnType = keywordMethodReturnType;
    }

    public String getStackSeq() {
        return stackSeq;
    }

    public void setStackSeq(String stackSeq) {
        this.stackSeq = stackSeq;
    }

    public String getCalleeMethod() {
        return calleeMethod;
    }

    public void setCalleeMethod(String calleeMethod) {
        this.calleeMethod = calleeMethod;
    }

    public String getUpwardCallerMethod() {
        return upwardCallerMethod;
    }

    public void setUpwardCallerMethod(String upwardCallerMethod) {
        this.upwardCallerMethod = upwardCallerMethod;
    }

    public String getKeywordMethod() {
        return keywordMethod;
    }

    public void setKeywordMethod(String keywordMethod) {
        this.keywordMethod = keywordMethod;
    }

    public String getKeywordMethodReturnType() {
        return keywordMethodReturnType;
    }

    public void setKeywordMethodReturnType(String keywordMethodReturnType) {
        this.keywordMethodReturnType = keywordMethodReturnType;
    }
}

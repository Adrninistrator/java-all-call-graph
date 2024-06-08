package com.adrninistrator.jacg.handler.dto.exception;

/**
 * @author adrninistrator
 * @date 2024/1/6
 * @description: 方法中catch的异常对象使用情况
 */
public abstract class BaseMethodCatchExceptionUsage {

    // catch的异常对象变量名称
    private String catchExceptionVariableName;

    // 是否使用了预期的被调用方法（仅当在方法调用中使用catch异常对象时有效）
    private boolean useEInExpectedMethodCall;

    /**
     * 返回当前类对应catch的异常对象的使用方式描述
     *
     * @return
     */
    public abstract String getUsageDescription();

    /**
     * 返回当前类对应catch的异常对象的使用详情
     *
     * @return
     */
    public abstract String getUsageDetail();

    public String getCatchExceptionVariableName() {
        return catchExceptionVariableName;
    }

    public void setCatchExceptionVariableName(String catchExceptionVariableName) {
        this.catchExceptionVariableName = catchExceptionVariableName;
    }

    public boolean isUseEInExpectedMethodCall() {
        return useEInExpectedMethodCall;
    }

    public void setUseEInExpectedMethodCall(boolean useEInExpectedMethodCall) {
        this.useEInExpectedMethodCall = useEInExpectedMethodCall;
    }
}

package com.adrninistrator.jacg.handler.dto.string.element;

/**
 * @author adrninistrator
 * @date 2025/9/21
 * @description: 字符串元素，获取类名
 */
public class StringElementClassGetName extends BaseStringElement {

    // 类名
    private String className;

    // 获取类名的方法名
    private String methodName;

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }

    public String getMethodName() {
        return methodName;
    }

    public void setMethodName(String methodName) {
        this.methodName = methodName;
    }
}

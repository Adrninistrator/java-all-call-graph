package com.adrninistrator.jacg.handler.dto.string.element;

import java.util.Objects;

/**
 * @author adrninistrator
 * @date 2025/9/19
 * @description: 字符串元素，静态字段（包括枚举常量）的方法调用返回值
 */
public class StringElementStaticFieldMethodCallReturn extends BaseStringElement {

    // 字段所在的类名
    private String className;

    // 字段类型
    private String fieldType;

    // 字段名称
    private String fieldName;

    // 完整方法
    private String fullMethod;

    // 方法返回类型
    private String methodReturnType;

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }

    public String getFieldType() {
        return fieldType;
    }

    public void setFieldType(String fieldType) {
        this.fieldType = fieldType;
    }

    public String getFieldName() {
        return fieldName;
    }

    public void setFieldName(String fieldName) {
        this.fieldName = fieldName;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }

    public String getMethodReturnType() {
        return methodReturnType;
    }

    public void setMethodReturnType(String methodReturnType) {
        this.methodReturnType = methodReturnType;
    }

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof StringElementStaticFieldMethodCallReturn)) return false;
        StringElementStaticFieldMethodCallReturn that = (StringElementStaticFieldMethodCallReturn) o;
        return Objects.equals(className, that.className) && Objects.equals(fieldType, that.fieldType) && Objects.equals(fieldName, that.fieldName) && Objects.equals(fullMethod, that.fullMethod) && Objects.equals(methodReturnType, that.methodReturnType);
    }

    @Override
    public int hashCode() {
        return Objects.hash(className, fieldType, fieldName, fullMethod, methodReturnType);
    }
}

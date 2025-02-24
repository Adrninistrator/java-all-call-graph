package com.adrninistrator.jacg.handler.dto.methodcall;

import com.adrninistrator.javacg2.common.enums.JavaCG2MethodCallInfoTypeEnum;

/**
 * @author adrninistrator
 * @date 2025/2/21
 * @description: 方法调用中被调用对象或参数的值
 */
public class MethodCallObjArgValue {

    // 方法调用中被调用对象或参数的类型
    private JavaCG2MethodCallInfoTypeEnum typeEnum;

    // 值的类型，如String、int等
    private String valueType;

    // 值的String格式
    private String value;

    public JavaCG2MethodCallInfoTypeEnum getTypeEnum() {
        return typeEnum;
    }

    public void setTypeEnum(JavaCG2MethodCallInfoTypeEnum typeEnum) {
        this.typeEnum = typeEnum;
    }

    public String getValueType() {
        return valueType;
    }

    public void setValueType(String valueType) {
        this.valueType = valueType;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }
}

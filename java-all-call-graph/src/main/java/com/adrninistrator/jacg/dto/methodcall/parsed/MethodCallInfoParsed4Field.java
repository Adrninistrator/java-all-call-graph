package com.adrninistrator.jacg.dto.methodcall.parsed;

/**
 * @author adrninistrator
 * @date 2023/11/9
 * @description: 方法调用信息，解析后的数据，变量，使用字段
 */
public class MethodCallInfoParsed4Field extends AbstractMethodCallInfoParsed {

    // 字段的名称
    private String fieldName;

    // 字段的类型（与参数定义的类型不相同时才非空）
    private String fieldType;

    public String getFieldName() {
        return fieldName;
    }

    public void setFieldName(String fieldName) {
        this.fieldName = fieldName;
    }

    public String getFieldType() {
        return fieldType;
    }

    public void setFieldType(String fieldType) {
        this.fieldType = fieldType;
    }

    @Override
    public String toString() {
        return "MethodCallInfoParsed4Field{" +
                "fieldName='" + fieldName + '\'' +
                ", fieldType='" + fieldType + '\'' +
                '}';
    }
}

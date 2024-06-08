package com.adrninistrator.jacg.dto.methodcall.parsed;

/**
 * @author adrninistrator
 * @date 2023/11/9
 * @description: 方法调用信息，解析后的数据，变量，使用静态字段
 */
public class MethodCallInfoParsed4StaticField extends AbstractMethodCallInfoParsed {

    // 类与静态字段的名称
    private String classFieldName;

    // 字段的类型（与参数定义的类型不相同时才非空）
    private String fieldType;

    public String getClassFieldName() {
        return classFieldName;
    }

    public void setClassFieldName(String classFieldName) {
        this.classFieldName = classFieldName;
    }

    public String getFieldType() {
        return fieldType;
    }

    public void setFieldType(String fieldType) {
        this.fieldType = fieldType;
    }

    @Override
    public String toString() {
        return "MethodCallInfoParsed4StaticField{" +
                "classFieldName='" + classFieldName + '\'' +
                ", fieldType='" + fieldType + '\'' +
                '}';
    }
}

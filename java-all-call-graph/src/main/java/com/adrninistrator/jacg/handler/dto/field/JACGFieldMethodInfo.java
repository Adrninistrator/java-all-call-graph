package com.adrninistrator.jacg.handler.dto.field;

/**
 * @author adrninistrator
 * @date 2023/7/24
 * @description: 字段及方法信息
 */
public class JACGFieldMethodInfo {

    // 字段名称
    private final String fieldName;

    // get/set方法名称
    private final String methodName;

    // 字段是否在当前类中（否则字段在超类中）
    private final boolean inCurrentClass;

    // 字段名称是否通过@JsonProperty注解属性指定
    private final boolean useJsonProperty;

    public JACGFieldMethodInfo(String fieldName, String methodName, boolean inCurrentClass, boolean useJsonProperty) {
        this.fieldName = fieldName;
        this.methodName = methodName;
        this.inCurrentClass = inCurrentClass;
        this.useJsonProperty = useJsonProperty;
    }

    public String getFieldName() {
        return fieldName;
    }

    public String getMethodName() {
        return methodName;
    }

    public boolean isInCurrentClass() {
        return inCurrentClass;
    }

    public boolean isUseJsonProperty() {
        return useJsonProperty;
    }

    @Override
    public String toString() {
        return "JACGFieldInfo{" +
                "fieldName='" + fieldName + '\'' +
                ", getSetMethodName='" + methodName + '\'' +
                ", inCurrentClass=" + inCurrentClass +
                ", useJsonProperty=" + useJsonProperty +
                '}';
    }
}

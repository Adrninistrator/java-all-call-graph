package com.adrninistrator.jacg.handler.dto.field;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4FieldInfo;
import com.adrninistrator.javacg2.dto.field.FieldTypeAndName;
import com.adrninistrator.javacg2.util.JavaCG2Util;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/10/24
 * @description: 指定类中的常用数据类型字段信息，不包含静态字段，可以包含超类中的字段，或自定义类型字段中定义的字段，或字段的泛型类型中自定义类型中的字段
 */
public class CommonFieldInfoInClass {

    // 字段信息
    private WriteDbData4FieldInfo fieldInfo;

    // 字段所在的类（指定的类或超类）
    private String classNameOrSuper;

    // 字段在类中定义的层级，0代表直接在当前类定义，大于1的值代表在在自定义类型的字段中定义，或在字段的泛型类型中自定义类型中定义
    private int fieldLevel;

    // 字段在类中定义的路径（直接在当前类定义，或在自定义类型的字段中定义，或在字段的泛型类型中自定义类型中定义）
    private List<FieldTypeAndName> fieldTypeAndNameList;

    // 使用jackson @JsonProperty注解指定的JSON字段别名
    private String jsonAlias;

    public static CommonFieldInfoInClass genInstance(WriteDbData4FieldInfo writeDbData4FieldInfo) {
        CommonFieldInfoInClass commonFieldInfoInClass = new CommonFieldInfoInClass();
        commonFieldInfoInClass.setFieldInfo(writeDbData4FieldInfo);
        return commonFieldInfoInClass;
    }

    @Override
    public String toString() {
        return classNameOrSuper + " " + fieldLevel + " " + jsonAlias + " " + super.toString();
    }

    /**
     * 显示字段在类中定义的路径
     *
     * @return 返回格式为： 字段类型 字段名称->字段类型 字段名称
     */
    public String printFieldTypeAndNameList() {
        if (JavaCG2Util.isCollectionEmpty(fieldTypeAndNameList)) {
            return "";
        }
        StringBuilder stringBuilder = new StringBuilder();
        for (FieldTypeAndName fieldTypeAndName : fieldTypeAndNameList) {
            if (stringBuilder.length() > 0) {
                stringBuilder.append(" -> ");
            }
            stringBuilder.append(fieldTypeAndName.getFieldType()).append(" ").append(fieldTypeAndName.getFieldName());
        }
        return stringBuilder.toString();
    }

    public WriteDbData4FieldInfo getFieldInfo() {
        return fieldInfo;
    }

    public void setFieldInfo(WriteDbData4FieldInfo fieldInfo) {
        this.fieldInfo = fieldInfo;
    }

    public String getClassNameOrSuper() {
        return classNameOrSuper;
    }

    public void setClassNameOrSuper(String classNameOrSuper) {
        this.classNameOrSuper = classNameOrSuper;
    }

    public int getFieldLevel() {
        return fieldLevel;
    }

    public void setFieldLevel(int fieldLevel) {
        this.fieldLevel = fieldLevel;
    }

    public List<FieldTypeAndName> getFieldTypeAndNameList() {
        return fieldTypeAndNameList;
    }

    public void setFieldTypeAndNameList(List<FieldTypeAndName> fieldTypeAndNameList) {
        this.fieldTypeAndNameList = fieldTypeAndNameList;
    }

    public String getJsonAlias() {
        return jsonAlias;
    }

    public void setJsonAlias(String jsonAlias) {
        this.jsonAlias = jsonAlias;
    }
}

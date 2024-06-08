package com.adrninistrator.jacg.handler.dto.field;

import com.adrninistrator.jacg.util.JACGJsonUtil;
import com.adrninistrator.javacg.util.JavaCGClassMethodUtil;
import com.fasterxml.jackson.annotation.JsonIgnore;
import org.apache.commons.lang3.StringUtils;

/**
 * @author adrninistrator
 * @date 2023/7/29
 * @description: 字段行为基类，包含类名、字段名及对应的get/set方法名
 */
public class FieldBehavior {

    // 代表不存在可填充字段行为的类型
    public static final String TYPE = "None";

    @JsonIgnore
    // 类型
    protected final String type;

    @JsonIgnore
    // 类名
    protected String className;

    @JsonIgnore
    // 字段名
    protected String fieldName;

    @JsonIgnore
    // 字段类型
    protected String fieldType;

    @JsonIgnore
    // get方法名
    protected String getMethodName;

    @JsonIgnore
    // set方法名
    protected String setMethodName;

    @JsonIgnore
    // 是否对应get方法
    protected boolean getOrSet;

    @JsonIgnore
    // 字段关联关系标志
    protected int relationShipFlags;

    @JsonIgnore
    // 是否为自定义类型中的字段
    protected boolean nestedField;

    public FieldBehavior() {
        this.type = TYPE;
    }

    public FieldBehavior(String type, String className, String fieldName, String fieldType, String getMethodName, String setMethodName, boolean getOrSet, int relationShipFlags,
                         boolean nestedField) {
        this.type = type;
        this.className = className;
        this.fieldName = fieldName;
        this.fieldType = fieldType;
        this.getMethodName = getMethodName;
        this.setMethodName = setMethodName;
        this.getOrSet = getOrSet;
        this.relationShipFlags = relationShipFlags;
        this.nestedField = nestedField;
    }

    public FieldBehavior(String className, String fieldName, String fieldType, String getMethodName, String setMethodName, boolean getOrSet, int relationShipFlags) {
        this(TYPE, className, fieldName, fieldType, getMethodName, setMethodName, getOrSet, relationShipFlags, false);
    }

    /**
     * 生成类名+字段名
     *
     * @return
     */
    @JsonIgnore
    public String genClassAndFieldName() {
        return JavaCGClassMethodUtil.genClassAndField(className, fieldName);
    }

    /**
     * 生成用于在文件中以表格格式显示成一行的内容
     * 第1列： 类名
     * 第2列： 字段名
     * 第3列： 额外的属性，JSON格式，内容为""
     *
     * @param separator 分隔符
     * @return
     */
    @JsonIgnore
    public String genLineContent(String separator) {
        return StringUtils.joinWith(separator, className, fieldName, fieldType, type, JACGJsonUtil.getJsonStr(this));
    }

    public String getType() {
        return type;
    }

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }

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

    public String getGetMethodName() {
        return getMethodName;
    }

    public void setGetMethodName(String getMethodName) {
        this.getMethodName = getMethodName;
    }

    public String getSetMethodName() {
        return setMethodName;
    }

    public void setSetMethodName(String setMethodName) {
        this.setMethodName = setMethodName;
    }

    public boolean isGetOrSet() {
        return getOrSet;
    }

    public void setGetOrSet(boolean getOrSet) {
        this.getOrSet = getOrSet;
    }

    public int getRelationShipFlags() {
        return relationShipFlags;
    }

    public void setRelationShipFlags(int relationShipFlags) {
        this.relationShipFlags = relationShipFlags;
    }

    public boolean isNestedField() {
        return nestedField;
    }

    public void setNestedField(boolean nestedField) {
        this.nestedField = nestedField;
    }

    @Override
    public String toString() {
        return "FieldBehavior{" +
                "type='" + type + '\'' +
                ", className='" + className + '\'' +
                ", fieldName='" + fieldName + '\'' +
                ", fieldType='" + fieldType + '\'' +
                ", getMethodName='" + getMethodName + '\'' +
                ", setMethodName='" + setMethodName + '\'' +
                ", getOrSet=" + getOrSet +
                ", relationShipFlags=" + relationShipFlags +
                ", nestedField=" + nestedField +
                '}';
    }
}

package com.adrninistrator.jacg.dto.methodcall;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author adrninistrator
 * @date 2022/12/8
 * @description: 方法调用中使用的相关信息
 */
public class MethodCallInfo {
    // 可能的类型
    @JsonProperty("t")
    private String type;

    // 可能的值的类型
    @JsonProperty("vt")
    private String valueType;

    // 可能的值
    @JsonProperty("v")
    private String value;

    // 可能的静态字段
    @JsonProperty("sf")
    private String staticField;

    // 可能的静态字段的方法调用
    @JsonProperty("sfm")
    private String staticFieldMethodCall;

    // 可能的字段名称
    @JsonProperty("nof")
    private String nameOfField;

    // 可能的变量名称
    @JsonProperty("nov")
    private String nameOfVariable;

    // 可能的方法调用使用的调用方法参数序号
    @JsonProperty("mas")
    private Integer methodArgSeq;

    // 可能的方法调用返回的call_id
    @JsonProperty("mci")
    private String methodCallReturnId;

    // 可能的等值转换前方法调用使用的调用方法参数序号
    @JsonProperty("mas_eqc")
    private Integer methodArgSeqEQC;

    // 可能的等值转换前方法调用返回的call_id
    @JsonProperty("mci_eqc")
    private String methodCallReturnIdEQC;

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
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

    public String getStaticField() {
        return staticField;
    }

    public void setStaticField(String staticField) {
        this.staticField = staticField;
    }

    public String getStaticFieldMethodCall() {
        return staticFieldMethodCall;
    }

    public void setStaticFieldMethodCall(String staticFieldMethodCall) {
        this.staticFieldMethodCall = staticFieldMethodCall;
    }

    public String getNameOfField() {
        return nameOfField;
    }

    public void setNameOfField(String nameOfField) {
        this.nameOfField = nameOfField;
    }

    public String getNameOfVariable() {
        return nameOfVariable;
    }

    public void setNameOfVariable(String nameOfVariable) {
        this.nameOfVariable = nameOfVariable;
    }

    public Integer getMethodArgSeq() {
        return methodArgSeq;
    }

    public void setMethodArgSeq(Integer methodArgSeq) {
        this.methodArgSeq = methodArgSeq;
    }

    public String getMethodCallReturnId() {
        return methodCallReturnId;
    }

    public void setMethodCallReturnId(String methodCallReturnId) {
        this.methodCallReturnId = methodCallReturnId;
    }

    public Integer getMethodArgSeqEQC() {
        return methodArgSeqEQC;
    }

    public void setMethodArgSeqEQC(Integer methodArgSeqEQC) {
        this.methodArgSeqEQC = methodArgSeqEQC;
    }

    public String getMethodCallReturnIdEQC() {
        return methodCallReturnIdEQC;
    }

    public void setMethodCallReturnIdEQC(String methodCallReturnIdEQC) {
        this.methodCallReturnIdEQC = methodCallReturnIdEQC;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("MethodCallInfo{");
        if (type != null) {
            sb.append("type='").append(type).append('\'');
        }
        if (value != null) {
            sb.append(", value='").append(value).append('\'');
        }
        if (staticField != null) {
            sb.append(", staticField='").append(staticField).append('\'');
        }
        if (staticFieldMethodCall != null) {
            sb.append(", staticFieldMethodCall='").append(staticFieldMethodCall).append('\'');
        }
        if (nameOfField != null) {
            sb.append(", nameOfField='").append(nameOfField).append('\'');
        }
        if (nameOfVariable != null) {
            sb.append(", nameOfVariable='").append(nameOfVariable).append('\'');
        }
        if (methodArgSeq != 0) {
            sb.append(", methodArgSeq='").append(methodArgSeq).append('\'');
        }
        if (methodCallReturnId != null) {
            sb.append(", methodCallReturnId='").append(methodCallReturnId).append('\'');
        }
        if (methodArgSeqEQC != 0) {
            sb.append(", methodArgSeqEQC='").append(methodArgSeqEQC).append('\'');
        }
        if (methodCallReturnIdEQC != null) {
            sb.append(", methodCallReturnIdEQC='").append(methodCallReturnIdEQC).append('\'');
        }
        sb.append('}');
        return sb.toString();
    }
}

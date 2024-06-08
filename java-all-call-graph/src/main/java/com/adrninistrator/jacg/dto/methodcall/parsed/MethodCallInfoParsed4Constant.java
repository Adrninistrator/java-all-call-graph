package com.adrninistrator.jacg.dto.methodcall.parsed;

/**
 * @author adrninistrator
 * @date 2023/11/9
 * @description: 方法调用信息，解析后的数据，常量
 */
public class MethodCallInfoParsed4Constant extends AbstractMethodCallInfoParsed {

    // 常量的类型
    private String constType;

    // 常量的值
    private String constValue;

    // 保存常量值的本地变量名称（可能为空）
    private String localVariableName;

    public String getConstType() {
        return constType;
    }

    public void setConstType(String constType) {
        this.constType = constType;
    }

    public String getConstValue() {
        return constValue;
    }

    public void setConstValue(String constValue) {
        this.constValue = constValue;
    }

    public String getLocalVariableName() {
        return localVariableName;
    }

    public void setLocalVariableName(String localVariableName) {
        this.localVariableName = localVariableName;
    }

    @Override
    public String toString() {
        return "MethodCallInfoParsed4Constant{" +
                "constType='" + constType + '\'' +
                ", constValue='" + constValue + '\'' +
                ", localVariableName='" + localVariableName + '\'' +
                '}';
    }
}

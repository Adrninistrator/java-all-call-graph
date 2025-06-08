package com.adrninistrator.jacg.dto.methodcall.parsed;

/**
 * @author adrninistrator
 * @date 2023/11/9
 * @description: 方法调用信息，解析后的数据，使用方法调用返回值
 */
public class MethodCallInfoParsed4MCReturnCallId extends AbstractMethodCallInfoParsed {

    // 方法调用序号
    private int methodCallId;

    // 方法调用返回值保存到本地变量的名称（可能为空）
    private String localVariableName;

    // 对类的静态字段进行方法调用的完整方法
    private String staticFieldFullMethod;

    public MethodCallInfoParsed4MCReturnCallId(boolean equivalentConversion) {
        super(equivalentConversion);
    }

    public int getMethodCallId() {
        return methodCallId;
    }

    public void setMethodCallId(int methodCallId) {
        this.methodCallId = methodCallId;
    }

    public String getLocalVariableName() {
        return localVariableName;
    }

    public void setLocalVariableName(String localVariableName) {
        this.localVariableName = localVariableName;
    }

    public String getStaticFieldFullMethod() {
        return staticFieldFullMethod;
    }

    public void setStaticFieldFullMethod(String staticFieldFullMethod) {
        this.staticFieldFullMethod = staticFieldFullMethod;
    }

    @Override
    public String toString() {
        return "MethodCallInfoParsed4MCReturnCallId{" +
                "equivalentConversion=" + equivalentConversion +
                ", methodCallId=" + methodCallId +
                ", localVariableName='" + localVariableName + '\'' +
                ", staticFieldFullMethod='" + staticFieldFullMethod + '\'' +
                '}';
    }
}

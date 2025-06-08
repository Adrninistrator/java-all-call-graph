package com.adrninistrator.jacg.dto.methodcall.parsed;

/**
 * @author adrninistrator
 * @date 2023/11/9
 * @description: 方法调用信息，解析后的数据，使用方法的参数
 */
public class MethodCallInfoParsed4MethodArg extends AbstractMethodCallInfoParsed {

    // 方法参数序号，从1开始
    private int methodArgSeq;

    // 方法参数名称
    private String methodArgName;

    // 方法参数类型
    private String methodArgType;

    public MethodCallInfoParsed4MethodArg(boolean equivalentConversion) {
        super(equivalentConversion);
    }

    public int getMethodArgSeq() {
        return methodArgSeq;
    }

    public void setMethodArgSeq(int methodArgSeq) {
        this.methodArgSeq = methodArgSeq;
    }

    public String getMethodArgName() {
        return methodArgName;
    }

    public void setMethodArgName(String methodArgName) {
        this.methodArgName = methodArgName;
    }

    public String getMethodArgType() {
        return methodArgType;
    }

    public void setMethodArgType(String methodArgType) {
        this.methodArgType = methodArgType;
    }

    @Override
    public String toString() {
        return "MethodCallInfoParsed4MethodArg{" +
                "equivalentConversion=" + equivalentConversion +
                ", methodArgSeq=" + methodArgSeq +
                ", methodArgName='" + methodArgName + '\'' +
                ", methodArgType='" + methodArgType + '\'' +
                '}';
    }
}

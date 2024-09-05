package com.adrninistrator.jacg.dto.methodcall;

import com.adrninistrator.javacg2.common.enums.JavaCG2MethodCallInfoTypeEnum;

/**
 * @author adrninistrator
 * @date 2023/7/3
 * @description: 用于人工查看的方法调用中使用的相关信息
 */
public class MethodCallInfo4Read {

    // 方法调用信息的类型
    private final JavaCG2MethodCallInfoTypeEnum methodCallInfoTypeEnum;

    // 方法调用信息的值
    private final String info;

    public MethodCallInfo4Read(JavaCG2MethodCallInfoTypeEnum methodCallInfoTypeEnum, String info) {
        this.methodCallInfoTypeEnum = methodCallInfoTypeEnum;
        this.info = info;
    }

    public JavaCG2MethodCallInfoTypeEnum getMethodCallInfoTypeEnum() {
        return methodCallInfoTypeEnum;
    }

    public String getInfo() {
        return info;
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("MethodCallInfo4Read{");
        sb.append("methodCallInfoTypeEnum=").append(methodCallInfoTypeEnum);
        sb.append(", info='").append(info).append('\'');
        sb.append('}');
        return sb.toString();
    }
}

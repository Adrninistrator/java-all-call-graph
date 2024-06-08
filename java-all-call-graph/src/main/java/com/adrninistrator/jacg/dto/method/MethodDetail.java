package com.adrninistrator.jacg.dto.method;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2022/12/10
 * @description: 方法详细信息
 */
public class MethodDetail extends ClassAndMethodName {
    // 完整方法
    private final String fullMethod;

    // 参数类型字符串（不包含括号）
    private final String argTypeStr;

    // 参数类型数组（不可修改）
    private final List<String> argTypeList;

    public MethodDetail(String fullMethod, String className, String methodName, String argTypeStr, List<String> argTypeList) {
        super(className, methodName);
        this.fullMethod = fullMethod;
        this.argTypeStr = argTypeStr;
        this.argTypeList = argTypeList;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public String getArgTypeStr() {
        return argTypeStr;
    }

    public List<String> getArgTypeList() {
        return argTypeList;
    }

    @Override
    public String toString() {
        return fullMethod;
    }
}

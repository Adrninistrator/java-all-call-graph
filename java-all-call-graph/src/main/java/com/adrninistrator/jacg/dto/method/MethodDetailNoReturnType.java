package com.adrninistrator.jacg.dto.method;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/4/5
 * @description: 方法详细信息，不包含返回类型
 */
public class MethodDetailNoReturnType extends ClassAndMethodName {
    // 完整方法
    private String fullMethod;

    // 参数类型字符串（不包含括号）
    private String argTypeStr;

    // 参数类型数组（不可修改）
    private List<String> argTypeList;

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }

    public String getArgTypeStr() {
        return argTypeStr;
    }

    public void setArgTypeStr(String argTypeStr) {
        this.argTypeStr = argTypeStr;
    }

    public List<String> getArgTypeList() {
        return argTypeList;
    }

    public void setArgTypeList(List<String> argTypeList) {
        this.argTypeList = argTypeList;
    }

    @Override
    public String toString() {
        return fullMethod;
    }
}

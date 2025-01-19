package com.adrninistrator.jacg.handler.dto.methodreturn;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/12/3
 * @description: 方法返回类型，包含泛型类型
 */
public class MethodReturnTypeWithGenerics {

    // 方法返回类型
    private String methodReturnType;

    // 方法返回类型中Object类型的字段名称
    private List<String> objectFieldNameList;

    // 方法返回的泛型类型
    private List<String> methodReturnGenericsTypeList;

    public String getMethodReturnType() {
        return methodReturnType;
    }

    public void setMethodReturnType(String methodReturnType) {
        this.methodReturnType = methodReturnType;
    }

    public List<String> getMethodReturnGenericsTypeList() {
        return methodReturnGenericsTypeList;
    }

    public List<String> getObjectFieldNameList() {
        return objectFieldNameList;
    }

    public void setObjectFieldNameList(List<String> objectFieldNameList) {
        this.objectFieldNameList = objectFieldNameList;
    }

    public void setMethodReturnGenericsTypeList(List<String> methodReturnGenericsTypeList) {
        this.methodReturnGenericsTypeList = methodReturnGenericsTypeList;
    }
}

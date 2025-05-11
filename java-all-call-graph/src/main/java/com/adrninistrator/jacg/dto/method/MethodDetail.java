package com.adrninistrator.jacg.dto.method;

import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;

/**
 * @author adrninistrator
 * @date 2022/12/10
 * @description: 方法详细信息
 */
public class MethodDetail extends MethodDetailNoReturnType {

    // 方法返回类型
    private String returnType;

    public String getFullMethodWithReturnType() {
        return JavaCG2ClassMethodUtil.genFullMethodWithReturnType(getFullMethod(), returnType);
    }

    //
    public String getReturnType() {
        return returnType;
    }

    public void setReturnType(String returnType) {
        this.returnType = returnType;
    }

    @Override
    public String toString() {
        return getFullMethodWithReturnType();
    }
}

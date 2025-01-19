package com.adrninistrator.jacg.handler.dto.spring;

import com.adrninistrator.jacg.handler.dto.methodreturn.MethodReturnTypeWithGenerics;

/**
 * @author adrninistrator
 * @date 2024/12/3
 * @description: Spring Controller信息，及方法返回类型，包含泛型类型
 */
public class SpringControllerReturnTypeWithGenerics {

    private SpringControllerInfo springControllerInfo;

    private MethodReturnTypeWithGenerics methodReturnTypeWithGenerics;

    public SpringControllerInfo getSpringControllerInfo() {
        return springControllerInfo;
    }

    public void setSpringControllerInfo(SpringControllerInfo springControllerInfo) {
        this.springControllerInfo = springControllerInfo;
    }

    public MethodReturnTypeWithGenerics getMethodReturnTypeWithGenerics() {
        return methodReturnTypeWithGenerics;
    }

    public void setMethodReturnTypeWithGenerics(MethodReturnTypeWithGenerics methodReturnTypeWithGenerics) {
        this.methodReturnTypeWithGenerics = methodReturnTypeWithGenerics;
    }
}

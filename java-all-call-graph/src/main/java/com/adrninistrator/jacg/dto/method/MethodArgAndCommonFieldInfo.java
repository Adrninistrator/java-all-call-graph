package com.adrninistrator.jacg.dto.method;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodArgument;
import com.adrninistrator.jacg.handler.dto.field.CommonFieldInfoInClass;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/11/21
 * @description: 方法参数，及方法参数类型或方法参数泛型类型中的常用数据类型字段信息
 */
public class MethodArgAndCommonFieldInfo {

    // 方法参数
    private WriteDbData4MethodArgument methodArgument;

    // 方法参数类型或方法参数泛型类型中的常用数据类型字段信息，可能为null，代表只有方法参数
    private List<CommonFieldInfoInClass> commonFieldInfoInClassList;

    public WriteDbData4MethodArgument getMethodArgument() {
        return methodArgument;
    }

    public void setMethodArgument(WriteDbData4MethodArgument methodArgument) {
        this.methodArgument = methodArgument;
    }

    public List<CommonFieldInfoInClass> getCommonFieldInfoInClassList() {
        return commonFieldInfoInClassList;
    }

    public void setCommonFieldInfoInClassList(List<CommonFieldInfoInClass> commonFieldInfoInClassList) {
        this.commonFieldInfoInClassList = commonFieldInfoInClassList;
    }
}

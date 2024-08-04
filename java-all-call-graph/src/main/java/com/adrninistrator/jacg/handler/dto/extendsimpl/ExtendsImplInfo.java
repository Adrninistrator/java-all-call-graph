package com.adrninistrator.jacg.handler.dto.extendsimpl;

import com.adrninistrator.jacg.handler.common.enums.ClassInterfaceEnum;
import com.adrninistrator.jacg.handler.dto.classes.ClassNameAndType;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/7/18
 * @description: 类的继承与实现信息
 */
public class ExtendsImplInfo {

    // 当前类/接口的类型
    private ClassInterfaceEnum classType;

    // 向下的子类与实现类信息，或向上的父类、实现的接口信息
    private List<ClassNameAndType> extendsImplClassInfoList;

    public ClassInterfaceEnum getClassType() {
        return classType;
    }

    public void setClassType(ClassInterfaceEnum classType) {
        this.classType = classType;
    }

    public List<ClassNameAndType> getExtendsImplClassInfoList() {
        return extendsImplClassInfoList;
    }

    public void setExtendsImplClassInfoList(List<ClassNameAndType> extendsImplClassInfoList) {
        this.extendsImplClassInfoList = extendsImplClassInfoList;
    }
}

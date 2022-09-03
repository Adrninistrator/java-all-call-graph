package com.adrninistrator.jacg.extensions.code_parser;

import com.adrninistrator.javacg.dto.classes.ClassInterfaceMethodInfo;
import com.adrninistrator.javacg.dto.classes.ExtendsClassMethodInfo;
import com.adrninistrator.javacg.extensions.code_parser.CustomCodeParserInterface;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/8/30
 * @description: 用于处理继承及实现关系的抽象父类
 */
public abstract class AbstractHandleExtendsOrImplCodeParser implements CustomCodeParserInterface {

    protected Map<String, ExtendsClassMethodInfo> extendsClassMethodInfoMap;

    protected Map<String, ClassInterfaceMethodInfo> classInterfaceMethodInfoMap;

    protected Map<String, String> interfaceExtendsMap;

    @Override
    public void setExtendsClassMethodInfoMap(Map<String, ExtendsClassMethodInfo> extendsClassMethodInfoMap) {
        this.extendsClassMethodInfoMap = extendsClassMethodInfoMap;
    }

    @Override
    public void setClassInterfaceMethodInfoMap(Map<String, ClassInterfaceMethodInfo> classInterfaceMethodInfoMap) {
        this.classInterfaceMethodInfoMap = classInterfaceMethodInfoMap;
    }

    @Override
    public void setInterfaceExtendsMap(Map<String, String> interfaceExtendsMap) {
        this.interfaceExtendsMap = interfaceExtendsMap;
    }
}

package com.adrninistrator.jacg.extensions.dto.multi_impl;

/**
 * @author adrninistrator
 * @date 2021/11/10
 * @description: 存在多个实现类的接口或父类方法自定义数据
 */
public class MultiImplMethodData {
    // 接口或父类方法
    private String interfaceOrSuperMethodName;

    // 实现类序号
    private int implSeq;

    // 实现类
    private String implMethodName;

    // 对应的实现类.md文件路径
    private String implMethodMdFilePath;

    public String getInterfaceOrSuperMethodName() {
        return interfaceOrSuperMethodName;
    }

    public void setInterfaceOrSuperMethodName(String interfaceOrSuperMethod) {
        this.interfaceOrSuperMethodName = interfaceOrSuperMethod;
    }

    public int getImplSeq() {
        return implSeq;
    }

    public void setImplSeq(int implSeq) {
        this.implSeq = implSeq;
    }

    public String getImplMethodName() {
        return implMethodName;
    }

    public void setImplMethodName(String implMethod) {
        this.implMethodName = implMethod;
    }

    public String getImplMethodMdFilePath() {
        return implMethodMdFilePath;
    }

    public void setImplMethodMdFilePath(String implMethodMdFilePath) {
        this.implMethodMdFilePath = implMethodMdFilePath;
    }
}

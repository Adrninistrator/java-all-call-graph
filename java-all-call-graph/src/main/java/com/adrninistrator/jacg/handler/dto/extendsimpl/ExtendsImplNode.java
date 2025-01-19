package com.adrninistrator.jacg.handler.dto.extendsimpl;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2025/1/5
 * @description: 处理继承与实现时使用的栈节点
 */
public class ExtendsImplNode {

    // 类的父类或实现的接口的类名列表
    private List<String> extendsImplClassNameList;

    // 以上列表当前处理的序号
    private int listIndex;

    /**
     * 将列表的序号加1
     */
    public void addListIndex() {
        listIndex++;
    }

    /**
     * 获取列表中当前的类名
     *
     * @return
     */
    public String getCurrentClassName() {
        if (listIndex >= extendsImplClassNameList.size()) {
            return null;
        }
        return extendsImplClassNameList.get(listIndex);
    }

    //
    public List<String> getExtendsImplClassNameList() {
        return extendsImplClassNameList;
    }

    public void setExtendsImplClassNameList(List<String> extendsImplClassNameList) {
        this.extendsImplClassNameList = extendsImplClassNameList;
    }

    public int getListIndex() {
        return listIndex;
    }

    public void setListIndex(int listIndex) {
        this.listIndex = listIndex;
    }
}

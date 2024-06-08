package com.adrninistrator.jacg.dto.callgraph;

/**
 * @author adrninistrator
 * @date 2022/12/6
 * @description: SCC父类方法调用子类方法对应的信息
 */
public class SuperCallChildInfo {
    // 被调用方法的节点层级（对应子类）
    private final int childCalleeNodeLevel;

    // 被调用类唯一类名（对应子类）
    private final String childCalleeSimpleClassName;

    // 被调用类类名（对应子类）
    private final String childCalleeClassName;

    // 被调用方法（对应子类）
    private final String childCalleeFullMethod;

    public SuperCallChildInfo(int childCalleeNodeLevel, String childCalleeSimpleClassName, String childCalleeClassName, String childCalleeFullMethod) {
        this.childCalleeNodeLevel = childCalleeNodeLevel;
        this.childCalleeSimpleClassName = childCalleeSimpleClassName;
        this.childCalleeClassName = childCalleeClassName;
        this.childCalleeFullMethod = childCalleeFullMethod;
    }

    public int getChildCalleeNodeLevel() {
        return childCalleeNodeLevel;
    }

    public String getChildCalleeSimpleClassName() {
        return childCalleeSimpleClassName;
    }

    public String getChildCalleeClassName() {
        return childCalleeClassName;
    }

    public String getChildCalleeFullMethod() {
        return childCalleeFullMethod;
    }

    @Override
    public String toString() {
        return childCalleeNodeLevel + " " + childCalleeFullMethod;
    }
}

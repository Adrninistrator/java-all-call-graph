package com.adrninistrator.jacg.dto.call_graph;

/**
 * @author adrninistrator
 * @date 2022/12/2
 * @description: CCS子类方法调用父类方法对应的信息
 */
public class ChildCallSuperInfo {
    // 调用方法的节点层级（对应子类）
    private final int childCallerNodeLevel;

    // 调用类唯一类名（对应子类）
    private final String childCallerSimpleClassName;

    // 调用类类名（对应子类）
    private final String childCallerClassName;

    // 调用方法（对应子类）
    private final String childCallerFullMethod;

    public ChildCallSuperInfo(int childCallerNodeLevel, String childCallerSimpleClassName, String childCallerClassName, String childCallerFullMethod) {
        this.childCallerNodeLevel = childCallerNodeLevel;
        this.childCallerSimpleClassName = childCallerSimpleClassName;
        this.childCallerClassName = childCallerClassName;
        this.childCallerFullMethod = childCallerFullMethod;
    }

    public int getChildCallerNodeLevel() {
        return childCallerNodeLevel;
    }

    public String getChildCallerSimpleClassName() {
        return childCallerSimpleClassName;
    }

    public String getChildCallerClassName() {
        return childCallerClassName;
    }

    public String getChildCallerFullMethod() {
        return childCallerFullMethod;
    }

    @Override
    public String toString() {
        return childCallerNodeLevel + " " + childCallerFullMethod;
    }
}

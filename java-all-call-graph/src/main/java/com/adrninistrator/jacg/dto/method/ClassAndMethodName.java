package com.adrninistrator.jacg.dto.method;

import java.util.Objects;

/**
 * @author adrninistrator
 * @date 2023/3/3
 * @description: 类名与方法名
 */
public class ClassAndMethodName {

    // 完整类名
    protected final String className;

    // 方法名
    protected final String methodName;

    public ClassAndMethodName(String className, String methodName) {
        this.className = className;
        this.methodName = methodName;
    }

    public String getClassName() {
        return className;
    }

    public String getMethodName() {
        return methodName;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ClassAndMethodName that = (ClassAndMethodName) o;
        return Objects.equals(className, that.className) && Objects.equals(methodName, that.methodName);
    }

    @Override
    public int hashCode() {
        return Objects.hash(className, methodName);
    }
}

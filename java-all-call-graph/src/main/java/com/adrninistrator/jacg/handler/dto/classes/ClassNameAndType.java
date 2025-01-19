package com.adrninistrator.jacg.handler.dto.classes;

import com.adrninistrator.jacg.handler.common.enums.ClassInterfaceEnum;

import java.util.Objects;

/**
 * @author adrninistrator
 * @date 2024/7/17
 * @description: 类名及类型
 */
public class ClassNameAndType {
    // 类名
    private final String className;

    // 类的类型
    private final ClassInterfaceEnum classType;

    public ClassNameAndType(String className, ClassInterfaceEnum classType) {
        this.className = className;
        this.classType = classType;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ClassNameAndType that = (ClassNameAndType) o;
        return className.equals(that.className);
    }

    @Override
    public int hashCode() {
        return Objects.hash(className);
    }

    @Override
    public String toString() {
        return "ClassNameAndType{" +
                "className='" + className + '\'' +
                ", classType=" + classType +
                '}';
    }

    public String getClassName() {
        return className;
    }

    public ClassInterfaceEnum getClassType() {
        return classType;
    }
}

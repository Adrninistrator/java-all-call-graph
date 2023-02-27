package com.adrninistrator.jacg.dto.classes;

import java.util.Objects;

/**
 * @author adrninistrator
 * @date 2022/11/19
 * @description: 类名及access_flags
 */
public class ClassNameAndAccessFlags {
    private final String simpleClassName;

    private final String className;

    private final int accessFlags;

    public ClassNameAndAccessFlags(String simpleClassName, String className, int accessFlags) {
        this.simpleClassName = simpleClassName;
        this.className = className;
        this.accessFlags = accessFlags;
    }

    public String getSimpleClassName() {
        return simpleClassName;
    }

    public String getClassName() {
        return className;
    }

    public int getAccessFlags() {
        return accessFlags;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ClassNameAndAccessFlags that = (ClassNameAndAccessFlags) o;
        return className.equals(that.className);
    }

    @Override
    public int hashCode() {
        return Objects.hash(className);
    }

    @Override
    public String toString() {
        return "ClassNameAndAccessFlags{" +
                "className='" + className + '\'' +
                ", accessFlags=" + accessFlags +
                '}';
    }
}

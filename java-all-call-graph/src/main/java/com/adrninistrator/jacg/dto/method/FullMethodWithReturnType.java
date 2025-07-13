package com.adrninistrator.jacg.dto.method;

import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;

import java.util.Objects;

/**
 * @author adrninistrator
 * @date 2025/4/5
 * @description: 完整方法与返回类型
 */
public class FullMethodWithReturnType {

    // 完整方法
    private String fullMethod;

    // 返回类型（包含数组标志）
    private String returnType;

    public FullMethodWithReturnType() {
    }

    public FullMethodWithReturnType(String fullMethod, String returnType) {
        this.fullMethod = fullMethod;
        this.returnType = returnType;
    }

    public String genFullMethodWithReturnType() {
        return JavaCG2ClassMethodUtil.genFullMethodWithReturnType(fullMethod, returnType);
    }

    public String genMethodHash() {
        return JACGUtil.genHashWithLen(genFullMethodWithReturnType());
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }

    public String getReturnType() {
        return returnType;
    }

    public void setReturnType(String returnType) {
        this.returnType = returnType;
    }

    @Override
    public boolean equals(Object object) {
        if (this == object) return true;
        if (!(object instanceof FullMethodWithReturnType)) return false;
        FullMethodWithReturnType that = (FullMethodWithReturnType) object;
        return Objects.equals(fullMethod, that.fullMethod) && Objects.equals(returnType, that.returnType);
    }

    @Override
    public int hashCode() {
        return Objects.hash(fullMethod, returnType);
    }

    @Override
    public String toString() {
        return "FullMethodWithReturnType{" +
                "fullMethod='" + fullMethod + '\'' +
                ", returnType='" + returnType + '\'' +
                '}';
    }
}

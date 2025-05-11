package com.adrninistrator.jacg.comparator;

import com.adrninistrator.jacg.dto.method.FullMethodWithReturnType;

import java.util.Comparator;

/**
 * @author adrninistrator
 * @date 2025/4/5
 * @description:
 */
public class Comparator4FullMethodWithReturnType implements Comparator<FullMethodWithReturnType> {
    private static final Comparator4FullMethodWithReturnType INSTANCE = new Comparator4FullMethodWithReturnType();

    public static Comparator4FullMethodWithReturnType getInstance() {
        return INSTANCE;
    }

    private Comparator4FullMethodWithReturnType() {
    }

    @Override
    public int compare(FullMethodWithReturnType o1, FullMethodWithReturnType o2) {
        int result1 = o1.getFullMethod().compareTo(o2.getFullMethod());
        if (result1 != 0) {
            return result1;
        }
        return o1.getReturnType().compareTo(o2.getReturnType());
    }
}

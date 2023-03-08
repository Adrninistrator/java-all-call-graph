package com.adrninistrator.jacg.comparator;

import com.adrninistrator.jacg.dto.method_call.MethodCallPair;

import java.util.Comparator;

/**
 * @author adrninistrator
 * @date 2023/3/6
 * @description:
 */
public class Comparator4MethodCallPairByCaller implements Comparator<MethodCallPair> {
    private static final Comparator4MethodCallPairByCaller INSTANCE = new Comparator4MethodCallPairByCaller();

    public static Comparator4MethodCallPairByCaller getInstance() {
        return INSTANCE;
    }

    private Comparator4MethodCallPairByCaller() {
    }

    @Override
    public int compare(MethodCallPair o1, MethodCallPair o2) {
        int result = o1.getCallerFullMethod().compareTo(o2.getCallerFullMethod());
        if (result != 0) {
            return result;
        }

        return o1.getCallerLineNumber() - o2.getCallerLineNumber();
    }
}

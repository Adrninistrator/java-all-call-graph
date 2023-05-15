package com.adrninistrator.jacg.comparator;

import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodCall;

import java.util.Comparator;

/**
 * @author adrninistrator
 * @date 2023/3/6
 * @description:
 */
public class Comparator4MethodCallByCaller1 implements Comparator<WriteDbData4MethodCall> {
    private static final Comparator4MethodCallByCaller1 INSTANCE = new Comparator4MethodCallByCaller1();

    public static Comparator4MethodCallByCaller1 getInstance() {
        return INSTANCE;
    }

    private Comparator4MethodCallByCaller1() {
    }

    @Override
    public int compare(WriteDbData4MethodCall o1, WriteDbData4MethodCall o2) {
        int result = o1.getCallerFullMethod().compareTo(o2.getCallerFullMethod());
        if (result != 0) {
            return result;
        }

        return o1.getCallerLineNumber() - o2.getCallerLineNumber();
    }
}

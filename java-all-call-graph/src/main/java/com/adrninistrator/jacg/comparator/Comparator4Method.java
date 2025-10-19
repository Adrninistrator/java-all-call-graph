package com.adrninistrator.jacg.comparator;

import java.lang.reflect.Method;
import java.util.Comparator;

/**
 * @author adrninistrator
 * @date 2025/9/26
 * @description:
 */
public class Comparator4Method implements Comparator<Method> {
    private static final Comparator4Method INSTANCE = new Comparator4Method();

    public static Comparator4Method getInstance() {
        return INSTANCE;
    }

    private Comparator4Method() {
    }

    @Override
    public int compare(Method o1, Method o2) {
        int result1 = o1.getName().compareTo(o2.getName());
        if (result1 != 0) {
            return result1;
        }

        return o1.getReturnType().getName().compareTo(o2.getReturnType().getName());
    }
}
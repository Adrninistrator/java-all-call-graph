package com.adrninistrator.jacg.comparator;

import com.adrninistrator.jacg.handler.dto.spring.SpringControllerInfo;

import java.util.Comparator;

/**
 * @author adrninistrator
 * @date 2023/3/26
 * @description:
 */
public class Comparator4SpringControllerInfo implements Comparator<SpringControllerInfo> {
    private static final Comparator4SpringControllerInfo INSTANCE = new Comparator4SpringControllerInfo();

    public static Comparator4SpringControllerInfo getInstance() {
        return INSTANCE;
    }

    private Comparator4SpringControllerInfo() {
    }

    @Override
    public int compare(SpringControllerInfo o1, SpringControllerInfo o2) {
        return o1.getFullMethod().compareTo(o2.getFullMethod());
    }
}


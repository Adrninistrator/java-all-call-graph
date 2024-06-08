package com.adrninistrator.jacg.comparator;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData4GetSetMethod;

import java.util.Comparator;

/**
 * @author adrninistrator
 * @date 2023/8/9
 * @description:
 */
public class Comparator4GetSetMethod implements Comparator<BaseWriteDbData4GetSetMethod> {
    private static final Comparator4GetSetMethod INSTANCE = new Comparator4GetSetMethod();

    public static Comparator4GetSetMethod getInstance() {
        return INSTANCE;
    }

    private Comparator4GetSetMethod() {
    }

    @Override
    public int compare(BaseWriteDbData4GetSetMethod o1, BaseWriteDbData4GetSetMethod o2) {
        return o1.getRecordId() - o2.getRecordId();
    }
}


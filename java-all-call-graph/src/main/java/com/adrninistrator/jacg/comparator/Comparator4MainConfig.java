package com.adrninistrator.jacg.comparator;

import com.adrninistrator.jacg.common.enums.interfaces.MainConfigInterface;

import java.util.Comparator;

/**
 * @author adrninistrator
 * @date 2024/8/30
 * @description:
 */
public class Comparator4MainConfig implements Comparator<MainConfigInterface> {
    private static final Comparator4MainConfig INSTANCE = new Comparator4MainConfig();

    public static Comparator4MainConfig getInstance() {
        return INSTANCE;
    }

    private Comparator4MainConfig() {
    }

    @Override
    public int compare(MainConfigInterface o1, MainConfigInterface o2) {
        int r1 = o1.getFileName().compareTo(o2.getFileName());
        if (r1 != 0) {
            return r1;
        }

        return o1.getKey().compareTo(o2.getKey());
    }
}

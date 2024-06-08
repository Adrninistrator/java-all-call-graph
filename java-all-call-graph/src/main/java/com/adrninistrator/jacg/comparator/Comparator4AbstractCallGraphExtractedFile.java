package com.adrninistrator.jacg.comparator;

import com.adrninistrator.jacg.extractor.dto.common.extractfile.AbstractCallGraphExtractedFile;

import java.util.Comparator;

/**
 * @author adrninistrator
 * @date 2023/2/28
 * @description:
 */
public class Comparator4AbstractCallGraphExtractedFile implements Comparator<AbstractCallGraphExtractedFile> {
    private static final Comparator4AbstractCallGraphExtractedFile INSTANCE = new Comparator4AbstractCallGraphExtractedFile();

    public static Comparator4AbstractCallGraphExtractedFile getInstance() {
        return INSTANCE;
    }

    private Comparator4AbstractCallGraphExtractedFile() {
    }

    @Override
    public int compare(AbstractCallGraphExtractedFile o1, AbstractCallGraphExtractedFile o2) {
        return o1.getFullMethod().compareTo(o2.getFullMethod());
    }
}

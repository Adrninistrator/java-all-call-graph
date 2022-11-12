package com.adrninistrator.jacg.dto.multiple;

import com.adrninistrator.javacg.enums.CallTypeEnum;

/**
 * @author adrninistrator
 * @date 2021/11/9
 * @description:
 */
public class MultiImplMethodInfo {

    private final CallTypeEnum multiImplMethodCallType;

    private final String dirPath;

    public MultiImplMethodInfo(CallTypeEnum multiImplMethodCallType, String dirPath) {
        this.multiImplMethodCallType = multiImplMethodCallType;
        this.dirPath = dirPath;
    }

    public CallTypeEnum getMultiImplMethodCallType() {
        return multiImplMethodCallType;
    }

    public String getDirPath() {
        return dirPath;
    }
}

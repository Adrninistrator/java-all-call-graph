package com.adrninistrator.jacg.dto.multiple;

import com.adrninistrator.javacg.enums.CallTypeEnum;

/**
 * @author adrninistrator
 * @date 2021/11/9
 * @description:
 */
public class MultiImplMethodInfo {

    private CallTypeEnum multiImplMethodCallType;

    private String dirPath;

    public CallTypeEnum getMultiImplMethodCallType() {
        return multiImplMethodCallType;
    }

    public void setMultiImplMethodCallType(CallTypeEnum multiImplMethodCallType) {
        this.multiImplMethodCallType = multiImplMethodCallType;
    }

    public String getDirPath() {
        return dirPath;
    }

    public void setDirPath(String dirPath) {
        this.dirPath = dirPath;
    }
}

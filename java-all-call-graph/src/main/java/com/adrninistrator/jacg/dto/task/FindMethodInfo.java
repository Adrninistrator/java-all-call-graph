package com.adrninistrator.jacg.dto.task;

/**
 * @author adrninistrator
 * @date 2022/4/29
 * @description:
 */
public class FindMethodInfo {
    // 是否出现错误
    private boolean error;

    // 是否需要生成空文件（只要不是无法执行下去的错误，都生成空文件，避免终止处理）
    private boolean genEmptyFile;

    // 完整方法HASH+长度
    private String methodHash;

    // 完整方法信息
    private String fullMethod;

    public static FindMethodInfo genFindMethodInfoFail() {
        return genFindMethodInfo(true, false, null, null);
    }

    public static FindMethodInfo genFindMethodInfoGenEmptyFile() {
        return genFindMethodInfo(false, true, null, null);
    }

    public static FindMethodInfo genFindMethodInfoSuccess(String methodHash, String fullMethod) {
        return genFindMethodInfo(false, false, methodHash, fullMethod);
    }

    private static FindMethodInfo genFindMethodInfo(boolean error, boolean genEmptyFile, String methodHash, String fullMethod) {
        FindMethodInfo findMethodInfo = new FindMethodInfo();
        findMethodInfo.setError(error);
        findMethodInfo.setGenEmptyFile(genEmptyFile);
        findMethodInfo.setMethodHash(methodHash);
        findMethodInfo.setFullMethod(fullMethod);
        return findMethodInfo;
    }

    //
    public boolean isError() {
        return error;
    }

    public void setError(boolean error) {
        this.error = error;
    }

    public boolean isGenEmptyFile() {
        return genEmptyFile;
    }

    public void setGenEmptyFile(boolean genEmptyFile) {
        this.genEmptyFile = genEmptyFile;
    }

    public String getMethodHash() {
        return methodHash;
    }

    public void setMethodHash(String methodHash) {
        this.methodHash = methodHash;
    }

    public String getFullMethod() {
        return fullMethod;
    }

    public void setFullMethod(String fullMethod) {
        this.fullMethod = fullMethod;
    }
}

package com.adrninistrator.jacg.dto.task;

/**
 * @author adrninistrator
 * @date 2022/4/29
 * @description:
 */
public class FindMethodTaskInfo {
    // 是否出现错误
    private boolean error;

    // 是否需要生成空文件（只要不是无法执行下去的错误，都生成空文件，避免终止处理）
    private boolean genEmptyFile;

    // 完整方法HASH+长度
    private String methodHash;

    // 完整方法信息
    private String fullMethod;

    // 方法调用的标志
    private int callFlags;

    public static FindMethodTaskInfo genFindMethodInfoFail() {
        return genFindMethodInfo(true, false, null, null, 0);
    }

    public static FindMethodTaskInfo genFindMethodInfoGenEmptyFile() {
        return genFindMethodInfo(false, true, null, null, 0);
    }

    public static FindMethodTaskInfo genFindMethodInfoSuccess(String methodHash, String fullMethod, int callFlags) {
        return genFindMethodInfo(false, false, methodHash, fullMethod, callFlags);
    }

    private static FindMethodTaskInfo genFindMethodInfo(boolean error, boolean genEmptyFile, String methodHash, String fullMethod, int callFlags) {
        FindMethodTaskInfo findMethodTaskInfo = new FindMethodTaskInfo();
        findMethodTaskInfo.setError(error);
        findMethodTaskInfo.setGenEmptyFile(genEmptyFile);
        findMethodTaskInfo.setMethodHash(methodHash);
        findMethodTaskInfo.setFullMethod(fullMethod);
        findMethodTaskInfo.setCallFlags(callFlags);
        return findMethodTaskInfo;
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

    public int getCallFlags() {
        return callFlags;
    }

    public void setCallFlags(int callFlags) {
        this.callFlags = callFlags;
    }
}

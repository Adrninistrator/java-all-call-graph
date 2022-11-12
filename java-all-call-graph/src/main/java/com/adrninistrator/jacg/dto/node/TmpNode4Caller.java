package com.adrninistrator.jacg.dto.node;

/**
 * @author adrninistrator
 * @date 2021/6/18
 * @description: 生成指定类调用的所有向下的调用关系时，使用的临时节点
 */

public class TmpNode4Caller {

    private String currentCalleeMethodHash;

    private int currentCalleeMethodId;

    public TmpNode4Caller(String currentCalleeMethodHash, int currentCalleeMethodId) {
        this.currentCalleeMethodHash = currentCalleeMethodHash;
        this.currentCalleeMethodId = currentCalleeMethodId;
    }

    //
    public String getCurrentCalleeMethodHash() {
        return currentCalleeMethodHash;
    }

    public void setCurrentCalleeMethodHash(String currentCalleeMethodHash) {
        this.currentCalleeMethodHash = currentCalleeMethodHash;
    }

    public int getCurrentCalleeMethodId() {
        return currentCalleeMethodId;
    }

    public void setCurrentCalleeMethodId(int currentCalleeMethodId) {
        this.currentCalleeMethodId = currentCalleeMethodId;
    }
}

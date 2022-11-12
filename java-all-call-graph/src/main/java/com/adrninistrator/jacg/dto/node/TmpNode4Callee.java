package com.adrninistrator.jacg.dto.node;

/**
 * @author adrninistrator
 * @date 2021/6/18
 * @description: 生成调用指定类的所有向上的调用关系时，使用的临时节点
 */

public class TmpNode4Callee {

    private String currentCalleeMethodHash;

    private String currentCallerMethodHash;

    public TmpNode4Callee(String currentCalleeMethodHash, String currentCallerMethodHash) {
        this.currentCalleeMethodHash = currentCalleeMethodHash;
        this.currentCallerMethodHash = currentCallerMethodHash;
    }

    //
    public String getCurrentCalleeMethodHash() {
        return currentCalleeMethodHash;
    }

    public void setCurrentCalleeMethodHash(String currentCalleeMethodHash) {
        this.currentCalleeMethodHash = currentCalleeMethodHash;
    }

    public String getCurrentCallerMethodHash() {
        return currentCallerMethodHash;
    }

    public void setCurrentCallerMethodHash(String currentCallerMethodHash) {
        this.currentCallerMethodHash = currentCallerMethodHash;
    }
}

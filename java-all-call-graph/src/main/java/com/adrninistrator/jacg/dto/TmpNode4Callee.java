package com.adrninistrator.jacg.dto;

/**
 * @author adrninistrator
 * @date 2021/6/18
 * @description: 生成调用指定类的所有向上的调用关系时，使用的临时节点
 */

public class TmpNode4Callee {

    private String currentCalleeMethodHash;

    private String currentCallerMethodHash;

    public static TmpNode4Callee genNode(String currentCalleeMethodHash, String currentCallerMethodHash) {
        TmpNode4Callee node = new TmpNode4Callee();
        node.setCurrentCalleeMethodHash(currentCalleeMethodHash);
        node.setCurrentCallerMethodHash(currentCallerMethodHash);
        return node;
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

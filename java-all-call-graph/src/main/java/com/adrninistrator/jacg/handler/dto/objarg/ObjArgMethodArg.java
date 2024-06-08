package com.adrninistrator.jacg.handler.dto.objarg;

/**
 * @author adrninistrator
 * @date 2023/10/31
 * @description: 方法调用中被调用对象或参数的信息，对应调用方法的参数
 */
public class ObjArgMethodArg extends AbstractObjArgInfo {
    // 参数序号
    private final int argSeq;

    public ObjArgMethodArg(int argSeq) {
        this.argSeq = argSeq;
    }

    public int getArgSeq() {
        return argSeq;
    }
}

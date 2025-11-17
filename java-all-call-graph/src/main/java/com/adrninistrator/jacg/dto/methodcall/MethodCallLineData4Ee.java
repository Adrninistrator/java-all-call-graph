package com.adrninistrator.jacg.dto.methodcall;

/**
 * @author adrninistrator
 * @date 2025/2/24
 * @description: 方法完整调用链当前行的数据，对应生成向下的完整调用链
 */
public class MethodCallLineData4Ee extends MethodCallLineData {

    // 是否入口方法
    private boolean entryMethod;

    public MethodCallLineData4Ee(int methodCallLevel, String callerSimpleClassName, Integer callerLineNumber, String actualFullMethod, String rawMethodHash,
                                 String actualMethodHash, String methodReturnType, int methodCallId, Integer callFlags, String callType) {
        super(methodCallLevel, callerSimpleClassName, callerLineNumber, actualFullMethod, rawMethodHash, actualMethodHash, methodReturnType, methodCallId, callFlags, callType);
    }

    public boolean isEntryMethod() {
        return entryMethod;
    }

    public void setEntryMethod(boolean entryMethod) {
        this.entryMethod = entryMethod;
    }
}

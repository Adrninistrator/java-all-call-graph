package com.adrninistrator.jacg.neo4j.common;

/**
 * @author adrninistrator
 * @date 2024/7/23
 * @description:
 */
public class Neo4jColumnConstants {

    public static final String MC_METHOD_HASH = "methodHash";
    public static final String MC_FULL_METHOD = "fullMethod";
    public static final String MC_RETURN_TYPE = "returnType";
    public static final String MC_CALL_FLAGS = "callFlags";
    public static final String MC_CALL_ID = "callId";
    public static final String MC_CALL_TYPE = "callType";
    public static final String MC_CALL_ENABLED = "enabled";
    public static final String MC_CALLEE_FULL_METHOD = "calleeFullMethod";
    public static final String MC_CALLEE_METHOD_HASH = "calleeMethodHash";
    public static final String MC_CALLER_SIMPLE_CLASS_NAME = "callerSimpleClassName";
    public static final String MC_CALLER_LINE_NUMBER = "callerLineNumber";
    public static final String MC_CALLER_RETURN_TYPE = "callerReturnType";
    public static final String MC_RAW_RETURN_TYPE = "rawReturnType";

    private Neo4jColumnConstants() {
        throw new IllegalStateException("illegal");
    }
}

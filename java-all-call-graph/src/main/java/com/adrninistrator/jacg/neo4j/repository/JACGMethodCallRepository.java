package com.adrninistrator.jacg.neo4j.repository;

import com.adrninistrator.jacg.neo4j.domain.relationship.JACGMethodCall;
import org.springframework.data.neo4j.annotation.Query;
import org.springframework.data.neo4j.repository.Neo4jRepository;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2024/7/21
 * @description:
 */
public interface JACGMethodCallRepository extends Neo4jRepository<JACGMethodCall, String> {

    // 使用自定义类型DTO会失败
    @Query("MATCH (caller:jacg_method_in_method_call)-[mc:jacg_method_call]->(callee:jacg_method_in_method_call) " +
            "WHERE caller.appName = $appName AND caller.simpleClassName = $simpleClassName AND caller.fullMethod STARTS WITH $fullMethodPrefix " +
            "RETURN DISTINCT caller.methodHash AS methodHash, caller.fullMethod AS fullMethod, caller.returnType AS returnType, mc.callFlags AS callFlags")
    List<Map<String, Object>> queryCallerMethodByName(@Param("appName") String appName, @Param("simpleClassName") String simpleClassName,
                                                      @Param("fullMethodPrefix") String fullMethodPrefix);

    @Query("MATCH (caller:jacg_method_in_method_call)-[mc:jacg_method_call]->(callee:jacg_method_in_method_call) " +
            "WHERE caller.appName = $appName AND caller.methodHash = $callerMethodHash AND mc.callId > $callId " +
            "RETURN {callId: mc.callId, callType: mc.callType, enabled: mc.enabled, " +
            "calleeFullMethod: callee.fullMethod, calleeMethodHash: callee.methodHash, " +
            "callerSimpleClassName: caller.simpleClassName, callerLineNumber: mc.callerLineNumber, callerReturnType: caller.returnType, callFlags: mc.callFlags, rawReturnType: callee.returnType} AS result " +
            "ORDER BY mc.callId ASC " +
            "LIMIT 1")
    Map<String, Object> queryOneCalleeMethod(@Param("appName") String appName, @Param("callerMethodHash") String callerMethodHash, @Param("callId") int callId);

    @Query("MATCH (caller:jacg_method_in_method_call)-[mc:jacg_method_call]->(callee:jacg_method_in_method_call) " +
            "WHERE caller.appName = $appName AND caller.methodHash = $callerMethodHash AND mc.callId > $callId " +
            "AND mc.callerLineNumber >= $lineNumStart AND mc.callerLineNumber <= $lineNumEnd " +
            "RETURN {callId: mc.callId, callType: mc.callType, enabled: mc.enabled, " +
            "calleeFullMethod: callee.fullMethod, calleeMethodHash: callee.methodHash, " +
            "callerSimpleClassName: caller.simpleClassName, callerLineNumber: mc.callerLineNumber, callerReturnType: caller.returnType, callFlags: mc.callFlags, rawReturnType: callee.returnType} AS result " +
            "ORDER BY mc.callId ASC " +
            "LIMIT 1")
    Map<String, Object> queryOneCalleeMethodByLine(@Param("appName") String appName, @Param("callerMethodHash") String callerMethodHash, @Param("callId") int callId,
                                                   @Param("lineNumStart") int lineNumStart, @Param("lineNumEnd") int lineNumEnd);

    @Query("MATCH (caller:jacg_method_in_method_call)-[mc:jacg_method_call]->(callee:jacg_method_in_method_call) " +
            "WHERE callee.appName = $appName AND callee.methodHash = $methodHash " +
            "RETURN {callerReturnType: caller.returnType, callFlags: mc.callFlags, rawReturnType: callee.returnType} AS result " +
            "LIMIT 1")
    Map<String, Object> queryMethodCallExtraInfoByCallee(@Param("appName") String appName, @Param("methodHash") String methodHash);


    @Query("MATCH (caller:jacg_method_in_method_call)-[mc:jacg_method_call]->(callee:jacg_method_in_method_call) " +
            "WHERE caller.appName = $appName AND caller.methodHash = $methodHash " +
            "RETURN {callerReturnType: caller.returnType, callFlags: mc.callFlags, rawReturnType: callee.returnType} AS result " +
            "LIMIT 1")
    Map<String, Object> queryMethodCallExtraInfoByCaller(@Param("appName") String appName, @Param("methodHash") String methodHash);
}

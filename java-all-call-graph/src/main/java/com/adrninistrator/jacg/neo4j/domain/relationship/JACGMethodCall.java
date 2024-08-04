package com.adrninistrator.jacg.neo4j.domain.relationship;

import com.adrninistrator.jacg.neo4j.domain.node.JACGMethodInMC;
import com.adrninistrator.jacg.neo4j.idstrategy.JACGIdStrategy;
import org.neo4j.ogm.annotation.EndNode;
import org.neo4j.ogm.annotation.GeneratedValue;
import org.neo4j.ogm.annotation.Id;
import org.neo4j.ogm.annotation.RelationshipEntity;
import org.neo4j.ogm.annotation.StartNode;

/**
 * @author adrninistrator
 * @date 2024/7/21
 * @description:
 */
@RelationshipEntity(type = "jacg_method_call")
public class JACGMethodCall {
    @Id
    @GeneratedValue(strategy = JACGIdStrategy.class)
    private String id;
    private Integer callId;
    private String callType;
    private String calleeObjType;
    private Integer enabled;
    private Integer callerLineNumber;
    private Integer callFlags = 0;
    private String actualReturnType;

    @StartNode
    private JACGMethodInMC callerMethod;

    @EndNode
    private JACGMethodInMC calleeMethod;

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public Integer getCallId() {
        return callId;
    }

    public void setCallId(Integer callId) {
        this.callId = callId;
    }

    public String getCallType() {
        return callType;
    }

    public void setCallType(String callType) {
        this.callType = callType;
    }

    public String getCalleeObjType() {
        return calleeObjType;
    }

    public void setCalleeObjType(String calleeObjType) {
        this.calleeObjType = calleeObjType;
    }

    public Integer getEnabled() {
        return enabled;
    }

    public void setEnabled(Integer enabled) {
        this.enabled = enabled;
    }

    public Integer getCallerLineNumber() {
        return callerLineNumber;
    }

    public void setCallerLineNumber(Integer callerLineNumber) {
        this.callerLineNumber = callerLineNumber;
    }

    public Integer getCallFlags() {
        return callFlags;
    }

    public void setCallFlags(Integer callFlags) {
        this.callFlags = callFlags;
    }

    public String getActualReturnType() {
        return actualReturnType;
    }

    public void setActualReturnType(String actualReturnType) {
        this.actualReturnType = actualReturnType;
    }

    public JACGMethodInMC getCallerMethod() {
        return callerMethod;
    }

    public void setCallerMethod(JACGMethodInMC callerMethod) {
        this.callerMethod = callerMethod;
    }

    public JACGMethodInMC getCalleeMethod() {
        return calleeMethod;
    }

    public void setCalleeMethod(JACGMethodInMC calleeMethod) {
        this.calleeMethod = calleeMethod;
    }
}

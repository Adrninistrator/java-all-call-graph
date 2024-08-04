package com.adrninistrator.jacg.neo4j.idstrategy;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.neo4j.domain.node.JACGMethodInMC;
import com.adrninistrator.jacg.util.JACGUtil;
import org.neo4j.ogm.id.IdStrategy;

import java.util.UUID;

/**
 * @author adrninistrator
 * @date 2024/7/21
 * @description:
 */
public class JACGIdStrategy implements IdStrategy {
    @Override
    public Object generateId(Object entity) {
        if (entity instanceof JACGMethodInMC) {
            JACGMethodInMC methodInMethodCall = (JACGMethodInMC) entity;
            return JACGUtil.genHashWithLen(methodInMethodCall.getAppName() + JACGConstants.FLAG_AT + methodInMethodCall.getFullMethod());
        }

        return UUID.randomUUID().toString();
    }
}

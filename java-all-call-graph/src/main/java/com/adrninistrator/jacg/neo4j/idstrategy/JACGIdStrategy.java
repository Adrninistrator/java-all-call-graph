package com.adrninistrator.jacg.neo4j.idstrategy;

import com.adrninistrator.jacg.neo4j.domain.node.JACGMethodInMC;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
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
            return JACGUtil.genHashWithLen(methodInMethodCall.getAppName()
                    + JavaCG2Constants.FLAG_COLON + methodInMethodCall.getFullMethod()
                    + JavaCG2Constants.FLAG_COLON + methodInMethodCall.getReturnType());
        }

        return UUID.randomUUID().toString();
    }
}

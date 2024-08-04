package com.adrninistrator.jacg.neo4j.domain.node;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodLineNumber;
import com.adrninistrator.jacg.neo4j.annoataion.Neo4jIndex;
import com.adrninistrator.jacg.neo4j.annoataion.Neo4jIndexes;
import com.adrninistrator.jacg.neo4j.idstrategy.JACGIdStrategy;
import org.neo4j.ogm.annotation.GeneratedValue;
import org.neo4j.ogm.annotation.Id;
import org.neo4j.ogm.annotation.NodeEntity;

/**
 * @author adrninistrator
 * @date 2024/7/24
 * @description:
 */
@NodeEntity(label = "jacg_method_line_number")
@Neo4jIndexes(indexes = {
        @Neo4jIndex(properties = {"appName", "simpleClassName"})
})
public class JACGMethodLineNumber extends WriteDbData4MethodLineNumber {

    @Id
    @GeneratedValue(strategy = JACGIdStrategy.class)
    private String id;
    private String appName;

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getAppName() {
        return appName;
    }

    public void setAppName(String appName) {
        this.appName = appName;
    }
}

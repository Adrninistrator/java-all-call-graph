package com.adrninistrator.jacg.neo4j.domain.node;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassInfo;
import com.adrninistrator.jacg.neo4j.annoataion.Neo4jIndex;
import com.adrninistrator.jacg.neo4j.annoataion.Neo4jIndexes;
import com.adrninistrator.jacg.neo4j.idstrategy.JACGIdStrategy;
import org.neo4j.ogm.annotation.GeneratedValue;
import org.neo4j.ogm.annotation.Id;
import org.neo4j.ogm.annotation.NodeEntity;

/**
 * @author adrninistrator
 * @date 2024/7/23
 * @description:
 */
// 父类也会变成neo4j的节点，是neo4j的默认行为
@NodeEntity(label = "jacg_class_info")
@Neo4jIndexes(indexes = {
        @Neo4jIndex(properties = {"appName", "simpleClassName"}),
        @Neo4jIndex(properties = {"appName", "className"})
})
public class JACGClassInfo extends WriteDbData4ClassInfo {

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

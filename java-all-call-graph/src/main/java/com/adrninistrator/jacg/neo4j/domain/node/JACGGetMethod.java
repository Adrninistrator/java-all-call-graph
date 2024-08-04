package com.adrninistrator.jacg.neo4j.domain.node;

import com.adrninistrator.jacg.dto.writedb.base.BaseWriteDbData4GetSetMethod;
import com.adrninistrator.jacg.neo4j.annoataion.Neo4jIndex;
import com.adrninistrator.jacg.neo4j.annoataion.Neo4jIndexes;
import com.adrninistrator.jacg.neo4j.idstrategy.JACGIdStrategy;
import org.neo4j.ogm.annotation.GeneratedValue;
import org.neo4j.ogm.annotation.Id;
import org.neo4j.ogm.annotation.NodeEntity;

/**
 * @author adrninistrator
 * @date 2024/7/25
 * @description:
 */
// 若继承子类WriteDbData4GetMethod则无法向neo4j写入父类BaseWriteDbData4GetSetMethod的字段
@NodeEntity(label = "jacg_get_method")
@Neo4jIndexes(indexes = {
        @Neo4jIndex(properties = {"appName"})
})
public class JACGGetMethod extends BaseWriteDbData4GetSetMethod {

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

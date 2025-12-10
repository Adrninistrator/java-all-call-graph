package test.neo4j.runner;

import com.adrninistrator.jacg.common.enums.DefaultBusinessDataTypeEnum;
import com.adrninistrator.jacg.conf.enums.ConfigKeyEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.neo4j.runner.Neo4jRunnerGenAllGraph4Caller;
import org.junit.Assert;
import org.junit.Test;
import test.annotation.JACGExample;
import test.neo4j.base.TestNeo4jBase;

/**
 * @author adrninistrator
 * @date 2024/7/23
 * @description:
 */
@JACGExample(title = "根据Neo4j的数据生成向下的方法完整调用链",
        desc = {})
public class TestNeo4jRunnerGenAllGraph4Caller extends TestNeo4jBase {

    @Test
    public void test() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFULE_BUSINESS_DATA_TYPE_SHOW_4ER,
                DefaultBusinessDataTypeEnum.BDTE_METHOD_CALL_INFO.getType(),
                DefaultBusinessDataTypeEnum.BDTE_MYBATIS_MYSQL_TABLE.getType(),
                DefaultBusinessDataTypeEnum.BDTE_MYBATIS_MYSQL_WRITE_TABLE.getType(),
                DefaultBusinessDataTypeEnum.BDTE_METHOD_ARG_GENERICS_TYPE.getType(),
                DefaultBusinessDataTypeEnum.BDTE_METHOD_RETURN_GENERICS_TYPE.getType()
        );
        configureWrapper.setMainConfig(ConfigKeyEnum.CKE_CALL_GRAPH_GEN_JSON_CALLER, Boolean.TRUE.toString());
        Assert.assertTrue(new Neo4jRunnerGenAllGraph4Caller(configureWrapper).run());
    }
}

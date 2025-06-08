package test.runbycode.mybatis;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.common.enums.DefaultBusinessDataTypeEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseListEnum;
import com.adrninistrator.jacg.conf.enums.OtherConfigFileUseSetEnum;
import com.adrninistrator.jacg.findstack.FindStackUpAndDown;
import org.junit.Assert;
import org.junit.Test;
import test.callgraph.mybatis.dao.TestTable2Mapper;
import test.callgraph.mybatis.dao.TestTableMapper;
import test.runbycode.base.TestRunByCodeBase;

/**
 * @author adrninistrator
 * @date 2025/5/24
 * @description:
 */
public class TestMyBatisMapperFindStackUpAndDown extends TestRunByCodeBase {

    @Test
    public void $test0WriteDb() {
        commonWriteDbForce();
    }

    @Test
    public void testCallStackUpAndDown() {
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFUSE_METHOD_CLASS_4CALLEE,
                TestTableMapper.class.getName(),
                TestTable2Mapper.class.getName()
        );
        configureWrapper.setOtherConfigSet(OtherConfigFileUseSetEnum.OCFULE_BUSINESS_DATA_TYPE_SHOW_4ER,
                DefaultBusinessDataTypeEnum.BDTE_MYBATIS_MYSQL_TABLE.getType()
        );
        configureWrapper.setOtherConfigList(OtherConfigFileUseListEnum.OCFULE_FIND_STACK_KEYWORD_4ER,
                JACGConstants.CALL_FLAG_BUSINESS_DATA + DefaultBusinessDataTypeEnum.BDTE_MYBATIS_MYSQL_TABLE.getType()
        );
        FindStackUpAndDown findStackUpAndDown = new FindStackUpAndDown(configureWrapper);
        Assert.assertTrue(findStackUpAndDown.find());
    }
}

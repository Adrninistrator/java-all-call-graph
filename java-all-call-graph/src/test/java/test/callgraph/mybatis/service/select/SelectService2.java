package test.callgraph.mybatis.service.select;

import com.adrninistrator.jacg.util.JACGJsonUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import test.callgraph.mybatis.dao.TestTableGjcusdMapper;
import test.callgraph.mybatis.entity.TestTableGjcusd;

/**
 * @author adrninistrator
 * @date 2023/10/23
 * @description: select语句对应的返回字段
 */
@Service
public class SelectService2 {
    private static final Logger logger = LoggerFactory.getLogger(SelectService2.class);

    private String strField1 = "aaa";

    private static String STR_FIELD_2 = "bbb";

    private TestTableGjcusdMapper testTableGjcusdMapper;

    public String useInOtherMethod1(String data) {
        String value = testTableGjcusdMapper.selectString1(data);
        logger.info("{}", value);
        return value;
    }

    public void useInCurrentMethodEntity() {
        TestTableGjcusd testTableGjcusd = testTableGjcusdMapper.selectByPrimaryKey("useInCurrentMethodString");
        logger.info("{}", JACGJsonUtil.getJsonStr(testTableGjcusd));

        useInCurrentMethodEntityObject(testTableGjcusd.getIdC());
    }

    public void useInCurrentMethodEntityObject(String data1) {
        TestTableGjcusd testTableGjcusd = testTableGjcusdMapper.selectObject("useInCurrentMethodString");
        logger.info("{}", JACGJsonUtil.getJsonStr(testTableGjcusd));
    }

    private String getData() {
        TestTableGjcusd testTableGjcusd = new TestTableGjcusd();
        return testTableGjcusd.getFlag2C();
    }
}

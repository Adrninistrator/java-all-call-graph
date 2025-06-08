package test.callgraph.mybatis.service.select;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import test.callgraph.mybatis.dao.TestTableGjcusdMapper;

/**
 * @author adrninistrator
 * @date 2023/10/23
 * @description: select语句对应的返回字段
 */
@Service
public class SelectService1 {
    private static final Logger logger = LoggerFactory.getLogger(SelectService1.class);

    private SelectService2 selectService2;

    private SelectService3 selectService3;

    private TestTableGjcusdMapper testTableGjcusdMapper;

    public void useInCurrentMethodString() {
        String value = testTableGjcusdMapper.selectString1("useInCurrentMethodString");
        logger.info("{}", value);
    }

    public void useInOtherMethod1() {
        String value = selectService2.useInOtherMethod1("useInOtherMethod1");
        logger.info("{}", value);
    }

    public void useInOtherMethod2() {
        String value = selectService3.useInOtherMethod1("useInOtherMethod2");
        logger.info("{}", value);
    }
}

package test.callgraph.mybatis.service.impl;

import org.springframework.stereotype.Service;
import test.callgraph.mybatis.dao.TestTableMapper;
import test.callgraph.mybatis.service.AbstractService1;

/**
 * @author adrninistrator
 * @date 2021/11/9
 * @description:
 */
@Service(Service1AImpl.BEAN_NAME)
public class Service1AImpl extends AbstractService1 {

    public static final String BEAN_NAME = "test.callgraph.mybatis.service.impl.Service1AImpl";

    private TestTableMapper testTableMapper;

    @Override
    public void test1() {
        testTableMapper.deleteByPrimaryKey("test");
    }
}

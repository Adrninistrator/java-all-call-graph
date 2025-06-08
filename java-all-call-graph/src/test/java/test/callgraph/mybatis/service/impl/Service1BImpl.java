package test.callgraph.mybatis.service.impl;

import org.springframework.stereotype.Service;
import test.callgraph.mybatis.dao.TestTable2Mapper;
import test.callgraph.mybatis.service.AbstractService1;

/**
 * @author adrninistrator
 * @date 2021/11/9
 * @description:
 */
@Service(Service1BImpl.BEAN_NAME)
public class Service1BImpl extends AbstractService1 {

    public static final String BEAN_NAME = "test.callgraph.mybatis.service.impl.Service1BImpl";

    private TestTable2Mapper testTable2Mapper;

    @Override
    public void test1() {
        testTable2Mapper.selectByOtherTable("test");
    }
}

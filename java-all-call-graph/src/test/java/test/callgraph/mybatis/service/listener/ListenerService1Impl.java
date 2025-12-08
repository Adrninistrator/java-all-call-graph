package test.callgraph.mybatis.service.listener;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import test.callgraph.mybatis.dao.TestTable2Mapper;
import test.callgraph.mybatis.dao.TestTableMapper;
import test.callgraph.mybatis.service.select.SelectService1;

/**
 * @author adrninistrator
 * @date 2021/10/15
 * @description:
 */
@Service
public class ListenerService1Impl implements ListenerService1 {
    private TestTableMapper testTableMapper;

    private TestTable2Mapper testTable2Mapper;

    @Autowired
    private SelectService1 selectService1;

    @Override
    public void test1() {
        testTableMapper.select1();

        testTableMapper.selectByPrimaryKey("");
        testTableMapper.selectByPrimaryKey("");

        testTableMapper.selectByPrimaryKey("");
        testTable2Mapper.insert(null);

        testTable2Mapper.updateByPrimaryKeySelective(null);

        testTable2Mapper.deleteByPrimaryKey(null);
    }

    public void test2() {
        selectService1.useInCurrentMethodString();
        selectService1.useInOtherMethod1();
    }
}

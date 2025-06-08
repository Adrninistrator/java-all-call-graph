package test.callgraph.mybatis.dao;

import test.callgraph.mybatis.entity.TestTable2;

public interface TestTable2Mapper {
    int deleteByPrimaryKey(String id);

    int insert(TestTable2 record);

    int insertSelective(TestTable2 record);

    TestTable2 selectByPrimaryKey(String id);

    int updateByPrimaryKeySelective(TestTable2 record);

    int updateByPrimaryKey(TestTable2 record);

    //
    TestTable2 selectByOtherTable(String id);
}
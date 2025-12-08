package test.callgraph.mybatis.dao;

import test.callgraph.mybatis.entity.TestTable;

public interface TestTableMapper {
    int deleteByPrimaryKey(String id);

    int insert(TestTable record);

    int insertSelective(TestTable record);

    TestTable selectByPrimaryKey(String id);

    int updateByPrimaryKeySelective(TestTable record);

    int updateByPrimaryKey(TestTable record);

    //
    int select1();
}
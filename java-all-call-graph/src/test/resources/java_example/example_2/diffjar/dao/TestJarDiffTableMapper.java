package test.diffjar.dao;

import test.callgraph.mybatis.entity.TestTable;

public interface TestJarDiffTableMapper {
    TestTable selectByPrimaryKey(String id);

    TestTable selectByPrimaryKeySame(String id);

    TestTable selectByPrimaryKey2(String id);

    TestTable select2(String id);

    int deleteByFlag(String id);
}
package test.callgraph.mybatis.dao;

public interface BaseMapper<T> {

    int insertObject(T record);

    T selectObject(String id, String key1);

    T selectObject(String id);

    int updateObject(T record);
}
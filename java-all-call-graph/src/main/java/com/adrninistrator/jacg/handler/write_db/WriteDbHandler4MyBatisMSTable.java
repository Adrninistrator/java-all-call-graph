package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MyBatisMSTable;

import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/3/14
 * @description: 写入数据库，MyBatis数据库表信息（使用MySQL）
 */
public class WriteDbHandler4MyBatisMSTable extends AbstractWriteDbHandler<WriteDbData4MyBatisMSTable> {

    // 保存MyBatis Mapper类名
    private Set<String> myBatisMapperSet;

    @Override
    protected WriteDbData4MyBatisMSTable genData(String line) {
        String[] array = splitEquals(line, 5);

        String mapperClassName = array[0];
        // 根据完整方法前缀判断是否需要处理
        if (!isAllowedClassPrefix(mapperClassName)) {
            return null;
        }

        // 记录MyBatis Mapper类名
        myBatisMapperSet.add(mapperClassName);
        String mapperMethodName = array[1];
        String sqlStatement = array[2];
        int tableSeq = Integer.parseInt(array[3]);
        String tableName = array[4];
        String mapperSimpleClassName = dbOperWrapper.getSimpleClassName(mapperClassName);
        return new WriteDbData4MyBatisMSTable(
                mapperSimpleClassName,
                mapperMethodName,
                sqlStatement,
                tableSeq,
                tableName,
                mapperClassName
        );
    }

    @Override
    protected DbTableInfoEnum chooseDbTableInfo() {
        return DbTableInfoEnum.DTIE_MYBATIS_MS_TABLE;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MyBatisMSTable data) {
        return new Object[]{
                genNextRecordId(),
                data.getMapperSimpleClassName(),
                data.getMapperMethodName(),
                data.getSqlStatement(),
                data.getTableSeq(),
                data.getTableName(),
                data.getMapperClassName()
        };
    }

    public void setMyBatisMapperSet(Set<String> myBatisMapperSet) {
        this.myBatisMapperSet = myBatisMapperSet;
    }
}

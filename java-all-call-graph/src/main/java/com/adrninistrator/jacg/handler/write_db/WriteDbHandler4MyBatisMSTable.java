package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MyBatisMSTable;
import com.adrninistrator.jacg.extensions.code_parser.jar_entry_other_file.MyBatisMySqlSqlInfoCodeParser;

import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/3/14
 * @description: 写入数据库，MyBatis数据库表信息（使用MySQL）
 */
@JACGWriteDbHandler(
        readFile = true,
        otherFileName = MyBatisMySqlSqlInfoCodeParser.FILE_NAME,
        minColumnNum = 5,
        maxColumnNum = 5,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_MYBATIS_MS_TABLE
)
public class WriteDbHandler4MyBatisMSTable extends AbstractWriteDbHandler<WriteDbData4MyBatisMSTable> {

    // 保存MyBatis Mapper类名
    private Set<String> myBatisMapperSet;

    @Override
    protected WriteDbData4MyBatisMSTable genData(String[] array) {
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

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "MyBatis Mapper完整类名",
                "MyBatis Mapper方法名",
                "sql语句类型",
                "数据库表序号",
                "数据库表名"
        };
    }

    @Override
    public String chooseOtherFileDesc() {
        return "MyBatis数据库表信息（使用MySQL）";
    }

    @Override
    public String[] chooseOtherFileDetailInfo() {
        return new String[]{
                "使用MySQL时，MyBatis的Mapper接口方法中涉及到的数据库表信息"
        };
    }

    public void setMyBatisMapperSet(Set<String> myBatisMapperSet) {
        this.myBatisMapperSet = myBatisMapperSet;
    }
}

package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MyBatisMSWriteTable;
import com.adrninistrator.jacg.extensions.code_parser.jar_entry_other_file.MyBatisMySqlWriteSqlInfoCodeParser;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;

import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/3/14
 * @description: 写入数据库，MyBatis写数据库表信息（使用MySQL）
 */
@JACGWriteDbHandler(
        readFile = true,
        otherFileName = MyBatisMySqlWriteSqlInfoCodeParser.FILE_NAME,
        minColumnNum = 4,
        maxColumnNum = 4,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_MYBATIS_MS_WRITE_TABLE
)
public class WriteDbHandler4MyBatisMSWriteTable extends AbstractWriteDbHandler<WriteDbData4MyBatisMSWriteTable> {

    // 保存MyBatis写数据库的Mapper方法
    private Set<String> myBatisMapperMethodWriteSet;

    @Override
    protected WriteDbData4MyBatisMSWriteTable genData(String[] array) {
        String mapperClassName = array[0];
        // 根据完整方法前缀判断是否需要处理
        if (!isAllowedClassPrefix(mapperClassName)) {
            return null;
        }

        String mapperMethodName = array[1];
        String sqlStatement = array[2];
        String tableName = array[3];
        String mapperSimpleClassName = dbOperWrapper.getSimpleClassName(mapperClassName);

        // 记录MyBatis写数据库的Mapper方法
        myBatisMapperMethodWriteSet.add(JACGClassMethodUtil.getClassAndMethodName(mapperClassName, mapperMethodName));
        return new WriteDbData4MyBatisMSWriteTable(
                mapperSimpleClassName,
                mapperMethodName,
                sqlStatement,
                tableName,
                mapperClassName
        );
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MyBatisMSWriteTable data) {
        return new Object[]{
                genNextRecordId(),
                data.getMapperSimpleClassName(),
                data.getMapperMethodName(),
                data.getSqlStatement(),
                data.getTableName(),
                data.getMapperClassName()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "MyBatis Mapper完整类名",
                "MyBatis Mapper方法名",
                "写操作sql语句类型",
                "数据库表名"
        };
    }

    @Override
    public String chooseOtherFileDesc() {
        return "MyBatis写数据库表信息（使用MySQL）";
    }


    @Override
    public String[] chooseOtherFileDetailInfo() {
        return new String[]{
                "使用MySQL时，MyBatis的Mapper接口方法中进行写操作的数据库表信息"
        };
    }

    public void setMyBatisMapperMethodWriteSet(Set<String> myBatisMapperMethodWriteSet) {
        this.myBatisMapperMethodWriteSet = myBatisMapperMethodWriteSet;
    }
}

package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MyBatisMSWriteTable;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.MyBatisMySqlWriteSqlInfoCodeParser;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGFileUtil;

import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/3/14
 * @description: 写入数据库，MyBatis Mapper方法写的数据库表信息（使用MySQL）
 */
@JACGWriteDbHandler(
        readFile = true,
        otherFileName = MyBatisMySqlWriteSqlInfoCodeParser.FILE_NAME,
        minColumnNum = 5,
        maxColumnNum = 5,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_MYBATIS_MS_WRITE_TABLE
)
public class WriteDbHandler4MyBatisMSWriteTable extends AbstractWriteDbHandler<WriteDbData4MyBatisMSWriteTable> {

    // 保存MyBatis写数据库的Mapper方法
    private Set<String> myBatisMapperMethodWriteSet;

    public WriteDbHandler4MyBatisMSWriteTable(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MyBatisMSWriteTable genData(String[] array) {
        String mapperClassName = array[0];
        String mapperMethodName = array[1];
        String sqlStatement = array[2];
        String tableName = array[3];
        String xmlFilePath = array[4];
        String xmlFileName = JACGFileUtil.getFileNameFromPathInJar(xmlFilePath);
        String mapperSimpleClassName = dbOperWrapper.querySimpleClassName(mapperClassName);

        // 记录MyBatis写数据库的Mapper方法
        myBatisMapperMethodWriteSet.add(JACGClassMethodUtil.genClassAndMethodName(mapperClassName, mapperMethodName));
        WriteDbData4MyBatisMSWriteTable writeDbData4MyBatisMSWriteTable = new WriteDbData4MyBatisMSWriteTable();
        writeDbData4MyBatisMSWriteTable.setRecordId(genNextRecordId());
        writeDbData4MyBatisMSWriteTable.setMapperSimpleClassName(mapperSimpleClassName);
        writeDbData4MyBatisMSWriteTable.setMapperMethodName(mapperMethodName);
        writeDbData4MyBatisMSWriteTable.setSqlStatement(sqlStatement);
        writeDbData4MyBatisMSWriteTable.setTableName(tableName);
        writeDbData4MyBatisMSWriteTable.setMapperClassName(mapperClassName);
        writeDbData4MyBatisMSWriteTable.setXmlFileName(xmlFileName);
        writeDbData4MyBatisMSWriteTable.setXmlFilePath(xmlFilePath);
        return writeDbData4MyBatisMSWriteTable;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MyBatisMSWriteTable data) {
        return new Object[]{
                data.getRecordId(),
                data.getMapperSimpleClassName(),
                data.getMapperMethodName(),
                data.getSqlStatement(),
                data.getTableName(),
                data.getMapperClassName(),
                data.getXmlFileName(),
                data.getXmlFilePath()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "MyBatis Mapper接口类名",
                "MyBatis Mapper方法名",
                "写操作sql语句类型",
                "数据库表名",
                "MyBatis XML文件路径"
        };
    }

    @Override
    public String chooseNotMainFileDesc() {
        return "MyBatis Mapper方法写的数据库表信息（使用MySQL）";
    }


    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "使用MySQL时，MyBatis的Mapper接口方法中进行写操作的数据库表信息"
        };
    }

    public void setMyBatisMapperMethodWriteSet(Set<String> myBatisMapperMethodWriteSet) {
        this.myBatisMapperMethodWriteSet = myBatisMapperMethodWriteSet;
    }
}

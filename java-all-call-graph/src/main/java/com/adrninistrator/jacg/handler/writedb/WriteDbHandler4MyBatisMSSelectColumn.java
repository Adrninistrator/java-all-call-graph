package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MyBatisMSSelectColumn;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.MyBatisMySqlSelectColumnCodeParser;
import com.adrninistrator.jacg.util.JACGFileUtil;

/**
 * @author adrninistrator
 * @date 2023/10/12
 * @description: 写入数据库，MyBatis的XML中select的字段信息（使用MySQL）
 */
@JACGWriteDbHandler(
        readFile = true,
        otherFileName = MyBatisMySqlSelectColumnCodeParser.FILE_NAME,
        minColumnNum = 6,
        maxColumnNum = 6,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_MYBATIS_MS_SELECT_COLUMN
)
public class WriteDbHandler4MyBatisMSSelectColumn extends AbstractWriteDbHandler<WriteDbData4MyBatisMSSelectColumn> {

    public WriteDbHandler4MyBatisMSSelectColumn(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MyBatisMSSelectColumn genData(String[] array) {
        String mapperClassName = array[0];
        // 根据类名前缀判断是否需要处理
        if (!isAllowedClassPrefix(mapperClassName)) {
            return null;
        }
        String mapperMethodName = array[1];
        String tableName = array[2];
        String columnName = array[3];
        String columnAlias = array[4];
        String xmlFilePath = array[5];
        String xmlFileName = JACGFileUtil.getFileNameFromPathInJar(xmlFilePath);
        String mapperSimpleClassName = dbOperWrapper.getSimpleClassName(mapperClassName);
        WriteDbData4MyBatisMSSelectColumn writeDbData4MyBatisMSSelectColumn = new WriteDbData4MyBatisMSSelectColumn();
        writeDbData4MyBatisMSSelectColumn.setRecordId(genNextRecordId());
        writeDbData4MyBatisMSSelectColumn.setMapperSimpleClassName(mapperSimpleClassName);
        writeDbData4MyBatisMSSelectColumn.setMapperMethodName(mapperMethodName);
        writeDbData4MyBatisMSSelectColumn.setTableName(tableName);
        writeDbData4MyBatisMSSelectColumn.setColumnName(columnName);
        writeDbData4MyBatisMSSelectColumn.setColumnAlias(columnAlias);
        writeDbData4MyBatisMSSelectColumn.setMapperClassName(mapperClassName);
        writeDbData4MyBatisMSSelectColumn.setXmlFileName(xmlFileName);
        writeDbData4MyBatisMSSelectColumn.setXmlFilePath(xmlFilePath);
        return writeDbData4MyBatisMSSelectColumn;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MyBatisMSSelectColumn data) {
        return new Object[]{
                data.getRecordId(),
                data.getMapperSimpleClassName(),
                data.getMapperMethodName(),
                data.getTableName(),
                data.getColumnName(),
                data.getColumnAlias(),
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
                "数据库表名",
                "数据库字段名",
                "数据库字段赋值的参数名称",
                "MyBatis XML文件路径"
        };
    }

    @Override
    public String chooseNotMainFileDesc() {
        return "MyBatis的XML中select的字段信息（使用MySQL）";
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "使用MySQL时，MyBatis的XML文件中select的字段及相关的表名与别名"
        };
    }
}

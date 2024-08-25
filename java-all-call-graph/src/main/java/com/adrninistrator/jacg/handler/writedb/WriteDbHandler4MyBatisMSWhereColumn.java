package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MyBatisMSWhereColumn;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.MyBatisMySqlWhereColumnCodeParser;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.mybatismysqltableparser.dto.ParameterName;
import com.adrninistrator.mybatismysqltableparser.util.MyBatisTableParserUtil;

/**
 * @author adrninistrator
 * @date 2023/10/8
 * @description: 写入数据库，MyBatis的XML中where子句的字段信息（使用MySQL）
 */
@JACGWriteDbHandler(
        readFile = true,
        otherFileName = MyBatisMySqlWhereColumnCodeParser.FILE_NAME,
        minColumnNum = 8,
        maxColumnNum = 8,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_MYBATIS_MS_WHERE_COLUMN
)
public class WriteDbHandler4MyBatisMSWhereColumn extends AbstractWriteDbHandler<WriteDbData4MyBatisMSWhereColumn> {

    public WriteDbHandler4MyBatisMSWhereColumn(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MyBatisMSWhereColumn genData(String[] array) {
        String mapperClassName = array[0];
        // 根据类名前缀判断是否需要处理
        if (!isAllowedClassPrefix(mapperClassName)) {
            return null;
        }
        String mapperMethodName = array[1];
        String tableName = array[2];
        String columnName = array[3];
        String operation = array[4];
        String paramRawName = array[5];
        String paramType = array[6];
        String xmlFilePath = array[7];
        String xmlFileName = JACGFileUtil.getFileNameFromPathInJar(xmlFilePath);
        String mapperSimpleClassName = dbOperWrapper.querySimpleClassName(mapperClassName);
        ParameterName parameterName = MyBatisTableParserUtil.genParameterName(paramRawName);
        WriteDbData4MyBatisMSWhereColumn writeDbData4MyBatisMSWhereColumn = new WriteDbData4MyBatisMSWhereColumn();
        writeDbData4MyBatisMSWhereColumn.setRecordId(genNextRecordId());
        writeDbData4MyBatisMSWhereColumn.setMapperSimpleClassName(mapperSimpleClassName);
        writeDbData4MyBatisMSWhereColumn.setMapperMethodName(mapperMethodName);
        writeDbData4MyBatisMSWhereColumn.setTableName(tableName);
        writeDbData4MyBatisMSWhereColumn.setColumnName(columnName);
        writeDbData4MyBatisMSWhereColumn.setOperation(operation);
        writeDbData4MyBatisMSWhereColumn.setParamObjName(parameterName.getParamObjName());
        writeDbData4MyBatisMSWhereColumn.setParamName(parameterName.getParamName());
        writeDbData4MyBatisMSWhereColumn.setParamRawName(paramRawName);
        writeDbData4MyBatisMSWhereColumn.setParamType(paramType);
        writeDbData4MyBatisMSWhereColumn.setMapperClassName(mapperClassName);
        writeDbData4MyBatisMSWhereColumn.setXmlFileName(xmlFileName);
        writeDbData4MyBatisMSWhereColumn.setXmlFilePath(xmlFilePath);
        return writeDbData4MyBatisMSWhereColumn;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MyBatisMSWhereColumn data) {
        return new Object[]{
                data.getRecordId(),
                data.getMapperSimpleClassName(),
                data.getMapperMethodName(),
                data.getTableName(),
                data.getColumnName(),
                data.getOperation(),
                data.getParamObjName(),
                data.getParamName(),
                data.getParamRawName(),
                data.getParamType(),
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
                "数据库字段进行比较的方式",
                "数据库字段用于比较的参数名",
                "数据库字段用于比较的参数的使用方式，#/$",
                "MyBatis XML文件路径"
        };
    }

    @Override
    public String chooseNotMainFileDesc() {
        return "MyBatis的XML中where子句的字段信息（使用MySQL）";
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "使用MySQL时，MyBatis的XML文件中where子句中的字段与用于比较的参数情况"
        };
    }
}

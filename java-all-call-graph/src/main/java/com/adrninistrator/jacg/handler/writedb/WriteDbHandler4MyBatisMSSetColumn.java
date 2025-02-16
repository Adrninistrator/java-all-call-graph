package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MyBatisMSSetColumn;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.MyBatisMySqlSetColumnCodeParser;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.mybatismysqltableparser.dto.ParameterName;
import com.adrninistrator.mybatismysqltableparser.util.MyBatisTableParserUtil;

/**
 * @author adrninistrator
 * @date 2023/10/8
 * @description: 写入数据库，MyBatis的XML中update set子句的字段信息（使用MySQL）
 */
@JACGWriteDbHandler(
        readFile = true,
        otherFileName = MyBatisMySqlSetColumnCodeParser.FILE_NAME,
        minColumnNum = 6,
        maxColumnNum = 6,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_MYBATIS_MS_SET_COLUMN
)
public class WriteDbHandler4MyBatisMSSetColumn extends AbstractWriteDbHandler<WriteDbData4MyBatisMSSetColumn> {

    public WriteDbHandler4MyBatisMSSetColumn(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MyBatisMSSetColumn genData(String[] array) {
        String mapperClassName = array[0];
        String mapperMethodName = array[1];
        String tableName = array[2];
        String columnName = array[3];
        String paramRawName = array[4];
        String xmlFilePath = array[5];
        String xmlFileName = JACGFileUtil.getFileNameFromPathInJar(xmlFilePath);
        String mapperSimpleClassName = dbOperWrapper.querySimpleClassName(mapperClassName);
        ParameterName parameterName = MyBatisTableParserUtil.genParameterName(paramRawName);

        WriteDbData4MyBatisMSSetColumn writeDbData4MyBatisMSSetColumn = new WriteDbData4MyBatisMSSetColumn();
        writeDbData4MyBatisMSSetColumn.setRecordId(genNextRecordId());
        writeDbData4MyBatisMSSetColumn.setMapperSimpleClassName(mapperSimpleClassName);
        writeDbData4MyBatisMSSetColumn.setMapperMethodName(mapperMethodName);
        writeDbData4MyBatisMSSetColumn.setTableName(tableName);
        writeDbData4MyBatisMSSetColumn.setColumnName(columnName);
        writeDbData4MyBatisMSSetColumn.setParamObjName(parameterName.getParamObjName());
        writeDbData4MyBatisMSSetColumn.setParamName(parameterName.getParamName());
        writeDbData4MyBatisMSSetColumn.setParamRawName(paramRawName);
        writeDbData4MyBatisMSSetColumn.setMapperClassName(mapperClassName);
        writeDbData4MyBatisMSSetColumn.setXmlFileName(xmlFileName);
        writeDbData4MyBatisMSSetColumn.setXmlFilePath(xmlFilePath);
        return writeDbData4MyBatisMSSetColumn;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MyBatisMSSetColumn data) {
        return new Object[]{
                data.getRecordId(),
                data.getMapperSimpleClassName(),
                data.getMapperMethodName(),
                data.getTableName(),
                data.getColumnName(),
                data.getParamObjName(),
                data.getParamName(),
                data.getParamRawName(),
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
        return "MyBatis的XML中update set子句的字段信息（使用MySQL）";
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "使用MySQL时，MyBatis的XML文件中update语句的set子句中的字段与用于赋值的参数情况"
        };
    }
}

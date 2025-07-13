package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MybatisMSColumn;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.MyBatisMySqlColumnInfoCodeParser;
import com.adrninistrator.jacg.util.JACGFileUtil;

/**
 * @author adrninistrator
 * @date 2023/7/24
 * @description: 写入数据库，MyBatis的Entity与数据库字段名信息（使用MySQL）
 */
@JACGWriteDbHandler(
        readFile = true,
        otherFileName = MyBatisMySqlColumnInfoCodeParser.FILE_NAME,
        minColumnNum = 6,
        maxColumnNum = 6,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_MYBATIS_MS_COLUMN
)
public class WriteDbHandler4MybatisMSColumn extends AbstractWriteDbHandler<WriteDbData4MybatisMSColumn> {

    public WriteDbHandler4MybatisMSColumn(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MybatisMSColumn genData(String[] array) {
        String entityClassName = readLineData();
        String resultMapId = readLineData();
        String entityColumnName = readLineData();
        String columnName = readLineData();
        String columnType = readLineData();
        String xmlFilePath = readLineData();
        String xmlFileName = JACGFileUtil.getFileNameFromPathInJar(xmlFilePath);
        String entitySimpleClassName = dbOperWrapper.querySimpleClassName(entityClassName);
        WriteDbData4MybatisMSColumn writeDbData4MybatisMSColumn = new WriteDbData4MybatisMSColumn();
        writeDbData4MybatisMSColumn.setRecordId(genNextRecordId());
        writeDbData4MybatisMSColumn.setResultMapId(resultMapId);
        writeDbData4MybatisMSColumn.setEntitySimpleClassName(entitySimpleClassName);
        writeDbData4MybatisMSColumn.setEntityFieldName(entityColumnName);
        writeDbData4MybatisMSColumn.setColumnName(columnName);
        writeDbData4MybatisMSColumn.setColumnType(columnType);
        writeDbData4MybatisMSColumn.setEntityClassName(entityClassName);
        writeDbData4MybatisMSColumn.setXmlFileName(xmlFileName);
        writeDbData4MybatisMSColumn.setXmlFilePath(xmlFilePath);
        return writeDbData4MybatisMSColumn;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MybatisMSColumn data) {
        return new Object[]{
                data.getRecordId(),
                data.getResultMapId(),
                data.getEntitySimpleClassName(),
                data.getEntityFieldName(),
                data.getColumnName(),
                data.getColumnType(),
                data.getEntityClassName(),
                data.getXmlFileName(),
                data.getXmlFilePath()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "XML的resultMap ID",
                "MyBatis的Entity类名",
                "Entity中的字段名称",
                "数据库表中的字段名称",
                "数据库字段类型",
                "MyBatis XML文件路径"
        };
    }

    @Override
    public String chooseNotMainFileDesc() {
        return "MyBatis的resultMap中的信息（使用MySQL）";
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "使用MySQL时，MyBatis的resultMap中的信息，包括Entity与数据库字段名对应关系信息"
        };
    }
}

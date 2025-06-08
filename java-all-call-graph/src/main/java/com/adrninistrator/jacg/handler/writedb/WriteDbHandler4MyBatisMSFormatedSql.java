package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MyBatisMSFormatedSql;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.MyBatisMySqlFormatedSqlCodeParser;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGUtil;

/**
 * @author adrninistrator
 * @date 2025/2/24
 * @description: 写入数据库，MyBatis XML中格式化后的sql文本（使用MySQL）
 */
@JACGWriteDbHandler(
        readFile = true,
        otherFileName = MyBatisMySqlFormatedSqlCodeParser.FILE_NAME,
        minColumnNum = 6,
        maxColumnNum = 6,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_MYBATIS_MS_FORMATED_SQL
)
public class WriteDbHandler4MyBatisMSFormatedSql extends AbstractWriteDbHandler<WriteDbData4MyBatisMSFormatedSql> {

    public WriteDbHandler4MyBatisMSFormatedSql(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MyBatisMSFormatedSql genData(String[] array) {
        String mapperInterfaceName = readLineData();
        String sqlId = readLineData();
        int sqlSeq = Integer.parseInt(readLineData());
        String xmlElementName = readLineData();
        String formatedSql = readLineData();
        String xmlFilePath = readLineData();

        String xmlFileName = JACGFileUtil.getFileNameFromPathInJar(xmlFilePath);
        String mapperSimpleInterfaceName = dbOperWrapper.querySimpleClassName(mapperInterfaceName);

        WriteDbData4MyBatisMSFormatedSql writeDbData4MyBatisMSFormatedSql = new WriteDbData4MyBatisMSFormatedSql();
        writeDbData4MyBatisMSFormatedSql.setRecordId(genNextRecordId());
        writeDbData4MyBatisMSFormatedSql.setXmlFileName(xmlFileName);
        writeDbData4MyBatisMSFormatedSql.setSqlId(sqlId);
        writeDbData4MyBatisMSFormatedSql.setSqlSeq(sqlSeq);
        writeDbData4MyBatisMSFormatedSql.setXmlElementName(xmlElementName);
        writeDbData4MyBatisMSFormatedSql.setFormatedSql(formatedSql);
        writeDbData4MyBatisMSFormatedSql.setSqlHash(JACGUtil.genHashWithLen(formatedSql));
        writeDbData4MyBatisMSFormatedSql.setMapperSimpleClassName(mapperSimpleInterfaceName);
        writeDbData4MyBatisMSFormatedSql.setMapperClassName(mapperInterfaceName);
        writeDbData4MyBatisMSFormatedSql.setXmlFilePath(xmlFilePath);
        return writeDbData4MyBatisMSFormatedSql;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MyBatisMSFormatedSql data) {
        return new Object[]{
                data.getRecordId(),
                data.getXmlFileName(),
                data.getSqlId(),
                data.getSqlSeq(),
                data.getXmlElementName(),
                data.getFormatedSql(),
                data.getSqlHash(),
                data.getMapperSimpleClassName(),
                data.getMapperClassName(),
                data.getXmlFilePath()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "MyBatis Mapper接口类名",
                "MyBatis XML sql Id",
                "sql文本序号，从0开始",
                "XML元素名称，如select、insert、update等",
                "格式化后的sql文本",
                "MyBatis XML文件路径"
        };
    }

    @Override
    public String chooseNotMainFileDesc() {
        return "MyBatis XML中格式化后的sql文本（使用MySQL）";
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "使用MySQL时，MyBatis的XML中格式化后的sql文本，包括XML文件路径、Mapper类名等"
        };
    }
}

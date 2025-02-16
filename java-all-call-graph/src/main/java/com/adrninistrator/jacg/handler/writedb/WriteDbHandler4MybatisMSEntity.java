package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MybatisMSEntity;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.MyBatisMySqlEntityInfoCodeParser;
import com.adrninistrator.jacg.util.JACGFileUtil;

/**
 * @author adrninistrator
 * @date 2023/7/24
 * @description: 写入数据库，MyBatis的Entity与Mapper、表名（使用MySQL）
 */
@JACGWriteDbHandler(
        readFile = true,
        otherFileName = MyBatisMySqlEntityInfoCodeParser.FILE_NAME,
        minColumnNum = 4,
        maxColumnNum = 4,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_MYBATIS_MS_ENTITY
)
public class WriteDbHandler4MybatisMSEntity extends AbstractWriteDbHandler<WriteDbData4MybatisMSEntity> {

    public WriteDbHandler4MybatisMSEntity(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MybatisMSEntity genData(String[] array) {
        String mapperInterfaceName = array[0];
        String entityClassName = array[1];
        String tableName = array[2];
        String xmlFilePath = array[3];
        String xmlFileName = JACGFileUtil.getFileNameFromPathInJar(xmlFilePath);
        String mapperSimpleInterfaceName = dbOperWrapper.querySimpleClassName(mapperInterfaceName);
        String entitySimpleClassName = dbOperWrapper.querySimpleClassName(entityClassName);

        WriteDbData4MybatisMSEntity writeDbData4MybatisMSEntity = new WriteDbData4MybatisMSEntity();
        writeDbData4MybatisMSEntity.setRecordId(genNextRecordId());
        writeDbData4MybatisMSEntity.setMapperSimpleClassName(mapperSimpleInterfaceName);
        writeDbData4MybatisMSEntity.setEntitySimpleClassName(entitySimpleClassName);
        writeDbData4MybatisMSEntity.setTableName(tableName);
        writeDbData4MybatisMSEntity.setMapperClassName(mapperInterfaceName);
        writeDbData4MybatisMSEntity.setEntityClassName(entityClassName);
        writeDbData4MybatisMSEntity.setXmlFileName(xmlFileName);
        writeDbData4MybatisMSEntity.setXmlFilePath(xmlFilePath);
        return writeDbData4MybatisMSEntity;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MybatisMSEntity data) {
        return new Object[]{
                data.getRecordId(),
                data.getMapperSimpleClassName(),
                data.getEntitySimpleClassName(),
                data.getTableName(),
                data.getMapperClassName(),
                data.getEntityClassName(),
                data.getXmlFileName(),
                data.getXmlFilePath()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "MyBatis Mapper接口类名",
                "MyBatis的Entity类名",
                "数据库表名",
                "MyBatis XML文件路径"
        };
    }

    @Override
    public String chooseNotMainFileDesc() {
        return "MyBatis的Entity与Mapper、表名（使用MySQL）";
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "使用MySQL时，MyBatis的Entity类名与Mapper类名、数据库表名"
        };
    }
}

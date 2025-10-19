package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4PropertiesConf;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.PropertiesConfCodeParser;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.util.JavaCG2Util;

/**
 * @author adrninistrator
 * @date 2023/9/18
 * @description: 写入数据库，properties文件配置内容
 */
@JACGWriteDbHandler(
        readFile = true,
        otherFileName = PropertiesConfCodeParser.FILE_NAME,
        minColumnNum = 4,
        maxColumnNum = 4,
        // 读取文件时允许最后一列出现TAB
        allowTabInLastColumn = true,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_PROPERTIES_CONF
)
public class WriteDbHandler4PropertiesConf extends AbstractWriteDbHandler<WriteDbData4PropertiesConf> {

    public WriteDbHandler4PropertiesConf(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4PropertiesConf genData(String[] array) {
        String propertiesFilePath = array[0];
        boolean useBase64 = JavaCG2YesNoEnum.isYes(array[1]);
        String propertiesKey = array[2];
        String propertiesFileName = JACGFileUtil.getFileNameFromPathInJar(propertiesFilePath);
        String propertiesValue = array[3];
        if (useBase64) {
            propertiesValue = JavaCG2Util.base64Decode(propertiesValue);
        }
        WriteDbData4PropertiesConf writeDbData4PropertiesConf = new WriteDbData4PropertiesConf();
        writeDbData4PropertiesConf.setRecordId(genNextRecordId());
        writeDbData4PropertiesConf.setPropertiesKey(propertiesKey);
        writeDbData4PropertiesConf.setPropertiesFilePath(propertiesFilePath);
        writeDbData4PropertiesConf.setPropertiesFileName(propertiesFileName);
        writeDbData4PropertiesConf.setPropertiesValue(propertiesValue);
        return writeDbData4PropertiesConf;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4PropertiesConf data) {
        return new Object[]{
                data.getRecordId(),
                data.getPropertiesKey(),
                data.getPropertiesFilePath(),
                data.getPropertiesFileName(),
                data.getPropertiesValue()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "properties配置文件路径",
                "properties配置内容是否经过BASE64编码，1:是，0:否",
                "properties配置名称",
                "properties配置内容"
        };
    }

    @Override
    public String chooseNotMainFileDesc() {
        return "properties文件配置内容";
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{"保存properties文件名，及其中的配置参数"};
    }
}

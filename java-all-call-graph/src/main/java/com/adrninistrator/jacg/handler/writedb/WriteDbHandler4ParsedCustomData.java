package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ParsedCustomData;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile.AbstractCodeParserWithCustomData;

/**
 * @author adrninistrator
 * @date 2025/6/6
 * @description: 写入数据库，解析jar文件时获取的自定义数据
 */
@JACGWriteDbHandler(
        readFile = true,
        otherFileName = AbstractCodeParserWithCustomData.FILE_NAME,
        minColumnNum = 3,
        maxColumnNum = 3,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_PARSED_CUSTOM_DATA
)
public class WriteDbHandler4ParsedCustomData extends AbstractWriteDbHandler<WriteDbData4ParsedCustomData> {

    public WriteDbHandler4ParsedCustomData(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4ParsedCustomData genData(String[] array) {
        WriteDbData4ParsedCustomData parsedCustomData = new WriteDbData4ParsedCustomData();
        parsedCustomData.setRecordId(genNextRecordId());
        parsedCustomData.setDataType(readLineData());
        parsedCustomData.setDataKey(readLineData());
        parsedCustomData.setDataValue(readLineData());
        return parsedCustomData;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4ParsedCustomData data) {
        return new Object[]{
                data.getRecordId(),
                data.getDataType(),
                data.getDataKey(),
                data.getDataValue()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "数据类型，代表当前数据的类型，格式没有限制",
                "数据的key，格式没有限制",
                "数据内容，格式没有限制"
        };
    }

    @Override
    public String chooseNotMainFileDesc() {
        return "解析jar文件时获取的自定义数据";
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{"可按照需要获取其他解析到的数据，写入到数据库表后可读取"};
    }
}

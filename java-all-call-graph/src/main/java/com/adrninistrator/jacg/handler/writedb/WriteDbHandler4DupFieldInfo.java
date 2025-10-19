package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2024/1/11
 * @description:
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_DUP_FIELD_INFO,
        minColumnNum = 13,
        maxColumnNum = 13,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_DUP_FIELD_INFO
)
public class WriteDbHandler4DupFieldInfo extends WriteDbHandler4FieldInfo {
    public WriteDbHandler4DupFieldInfo(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    public String[] chooseFileDetailInfo() {
        String[] parentInfo = super.chooseFileDetailInfo();
        String[] result = new String[parentInfo.length + 1];

        result[0] = "重复同名类的字段信息";
        System.arraycopy(parentInfo, 0, result, 1, parentInfo.length);
        return result;
    }
}

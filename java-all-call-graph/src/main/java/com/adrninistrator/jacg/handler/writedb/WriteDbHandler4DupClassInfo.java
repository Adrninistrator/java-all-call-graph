package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2024/12/5
 * @description: 写入数据库，重复同名类的信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_DUP_CLASS_INFO,
        minColumnNum = 5,
        maxColumnNum = 5,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_DUP_CLASS_INFO
)
public class WriteDbHandler4DupClassInfo extends WriteDbHandler4ClassInfo {

    public WriteDbHandler4DupClassInfo(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    public String[] chooseFileDetailInfo() {
        String[] parentInfo = super.chooseFileDetailInfo();
        String[] result = new String[parentInfo.length + 1];

        result[0] = "重复同名类的信息";
        System.arraycopy(parentInfo, 0, result, 1, parentInfo.length);
        return result;
    }
}

package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2025/8/10
 * @description: 写入数据库，重复同名类引用的类
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_DUP_CLASS_REFERENCE,
        minColumnNum = 3,
        maxColumnNum = 3,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_DUP_CLASS_REFERENCE
)
public class WriteDbHandler4DupClassReference extends WriteDbHandler4ClassReference {

    public WriteDbHandler4DupClassReference(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "重复同名类所引用的其他类关系"
        };
    }
}

package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2024/11/17
 * @description: 写入数据库，java-callgraph2组件使用的配置参数
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_JAVACG2_CONFIG,
        minColumnNum = 4,
        maxColumnNum = 4,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_CONFIG
)
public class WriteDbHandler4JavaCG2Config extends AbstractWriteDbHandler4Config {
    public WriteDbHandler4JavaCG2Config(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "java-callgraph2组件使用的配置参数，包括，包括配置参数名、参数值、参数类型等"
        };
    }
}

package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.runner.RunnerWriteCallGraphFile;

/**
 * @author adrninistrator
 * @date 2025/8/20
 * @description: 写入数据库，java-all-call-graph组件使用的配置参数
 */
@JACGWriteDbHandler(
        readFile = true,
        otherFileName = RunnerWriteCallGraphFile.JACG_CONFIG_FILE_NAME,
        minColumnNum = 4,
        maxColumnNum = 4,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_CONFIG
)
public class WriteDbHandler4JACGConfig extends AbstractWriteDbHandler4Config {
    public WriteDbHandler4JACGConfig(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    public String chooseNotMainFileDesc() {
        return "java-all-call-graph组件使用的配置参数";
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "java-all-call-graph组件使用的配置参数，包括配置参数名、参数值、参数类型等"
        };
    }
}

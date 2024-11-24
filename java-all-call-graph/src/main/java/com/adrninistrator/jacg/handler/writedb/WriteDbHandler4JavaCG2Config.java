package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4JavaCG2Config;
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
        dbTableInfoEnum = DbTableInfoEnum.DTIE_JAVACG2_CONFIG
)
public class WriteDbHandler4JavaCG2Config extends AbstractWriteDbHandler<WriteDbData4JavaCG2Config> {
    public WriteDbHandler4JavaCG2Config(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4JavaCG2Config genData(String[] array) {
        String configFileName = readLineData();
        String configKey = readLineData();
        String configValue = readLineData();
        String configType = readLineData();

        WriteDbData4JavaCG2Config writeDbData4JavaCG2Config = new WriteDbData4JavaCG2Config();
        writeDbData4JavaCG2Config.setConfigFileName(configFileName);
        writeDbData4JavaCG2Config.setConfigKey(configKey);
        writeDbData4JavaCG2Config.setConfigValue(configValue);
        writeDbData4JavaCG2Config.setConfigType(configType);
        return writeDbData4JavaCG2Config;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4JavaCG2Config data) {
        return new Object[]{
                data.getConfigFileName(),
                data.getConfigKey(),
                data.getConfigValue(),
                data.getConfigType()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "配置文件名",
                "配置参数名，List/Set类型的参数代表序号",
                "配置参数值",
                "配置参数类型"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "java-callgraph2组件使用的配置参数信息表"
        };
    }
}

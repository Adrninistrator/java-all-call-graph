package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4Config;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;

/**
 * @author adrninistrator
 * @date 2025/8/20
 * @description: 写入数据库，使用的配置参数，基类
 */
public abstract class AbstractWriteDbHandler4Config extends AbstractWriteDbHandler<WriteDbData4Config> {
    public AbstractWriteDbHandler4Config(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4Config genData(String[] array) {
        String configFileName = readLineData();
        String configKey = readLineData();
        String configValue = readLineData();
        String configType = readLineData();

        WriteDbData4Config writeDbData4Config = new WriteDbData4Config();
        writeDbData4Config.setConfigFileName(configFileName);
        writeDbData4Config.setConfigKey(configKey);
        writeDbData4Config.setConfigValue(configValue);
        writeDbData4Config.setConfigType(configType);
        return writeDbData4Config;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4Config data) {
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
}

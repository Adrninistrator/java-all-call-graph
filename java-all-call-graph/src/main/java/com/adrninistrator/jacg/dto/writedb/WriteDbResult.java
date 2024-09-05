package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.handler.writedb.AbstractWriteDbHandler;
import com.adrninistrator.javacg2.dto.counter.JavaCG2Counter;

import java.util.HashMap;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2024/5/16
 * @description: 写数据库的结果信息
 */
public class WriteDbResult {

    // 记录写数据库记录的次数
    private final Map<String, JavaCG2Counter> writeDbNumMap = new HashMap<>();

    // 记录写文件记录的次数
    private final Map<String, JavaCG2Counter> writeFileNumMap = new HashMap<>();

    // 记录处理失败的次数
    private final Map<String, JavaCG2Counter> failNumMap = new HashMap<>();

    // 记录写入数据库的类
    private final Map<String, AbstractWriteDbHandler<?>> writeDbHandlerMap = new HashMap<>();

    public Map<String, JavaCG2Counter> getWriteDbNumMap() {
        return writeDbNumMap;
    }

    public Map<String, JavaCG2Counter> getWriteFileNumMap() {
        return writeFileNumMap;
    }

    public Map<String, JavaCG2Counter> getFailNumMap() {
        return failNumMap;
    }

    public Map<String, AbstractWriteDbHandler<?>> getWriteDbHandlerMap() {
        return writeDbHandlerMap;
    }
}

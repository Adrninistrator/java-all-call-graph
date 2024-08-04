package com.adrninistrator.jacg.dto.writedb;

import com.adrninistrator.jacg.handler.writedb.AbstractWriteDbHandler;
import com.adrninistrator.javacg.dto.counter.JavaCGCounter;

import java.util.HashMap;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2024/5/16
 * @description: 写数据库的结果信息
 */
public class WriteDbResult {

    // 记录写数据库记录的次数
    private final Map<String, JavaCGCounter> writeDbNumMap = new HashMap<>();

    // 记录写文件记录的次数
    private final Map<String, JavaCGCounter> writeFileNumMap = new HashMap<>();

    // 记录处理失败的次数
    private final Map<String, JavaCGCounter> failNumMap = new HashMap<>();

    // 记录写入数据库的类
    private final Map<String, AbstractWriteDbHandler<?>> writeDbHandlerMap = new HashMap<>();

    public Map<String, JavaCGCounter> getWriteDbNumMap() {
        return writeDbNumMap;
    }

    public Map<String, JavaCGCounter> getWriteFileNumMap() {
        return writeFileNumMap;
    }

    public Map<String, JavaCGCounter> getFailNumMap() {
        return failNumMap;
    }

    public Map<String, AbstractWriteDbHandler<?>> getWriteDbHandlerMap() {
        return writeDbHandlerMap;
    }
}

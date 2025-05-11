package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4GetMethod;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/7/19
 * @description: 写入数据库，get方法
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_GET_METHOD,
        minColumnNum = 8,
        maxColumnNum = 8,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_GET_METHOD
)
public class WriteDbHandler4GetMethod extends AbstractWriteDbHandler4GetSetMethod<WriteDbData4GetMethod> {
    /*
        get方法对应的信息
        key
            唯一类名
        value
            get方法名称Set
    */
    private Map<String, Set<String>> getMethodSimpleClassMap;

    public WriteDbHandler4GetMethod(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4GetMethod genData(String[] array) {
        WriteDbData4GetMethod writeDbData4GetMethod = new WriteDbData4GetMethod();
        // 读取文件内容并填充对象
        if (!fillInBaseWriteDbData4GetSetMethod(writeDbData4GetMethod, getMethodSimpleClassMap)) {
            return null;
        }
        return writeDbData4GetMethod;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4GetMethod data) {
        return genObjectArrayBase(data);
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return chooseFileColumnDescBase();
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "dto的get方法的信息，包含对应的字段信息"
        };
    }

    public void setGetMethodSimpleClassMap(Map<String, Set<String>> getMethodSimpleClassMap) {
        this.getMethodSimpleClassMap = getMethodSimpleClassMap;
    }
}


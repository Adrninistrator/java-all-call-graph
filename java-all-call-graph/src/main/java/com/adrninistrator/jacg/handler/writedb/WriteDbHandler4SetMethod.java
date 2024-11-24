package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4SetMethod;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/7/19
 * @description: 写入数据库，set方法
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_SET_METHOD,
        minColumnNum = 7,
        maxColumnNum = 7,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_SET_METHOD
)
public class WriteDbHandler4SetMethod extends AbstractWriteDbHandler4GetSetMethod<WriteDbData4SetMethod> {
    /*
        set方法对应的信息
        key
            唯一类名
        value
            set方法名称Set
    */
    private Map<String, Set<String>> setMethodSimpleClassMap;

    public WriteDbHandler4SetMethod(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4SetMethod genData(String[] array) {
        WriteDbData4SetMethod writeDbData4SetMethod = new WriteDbData4SetMethod();
        // 读取文件内容并填充对象
        if (!fillInBaseWriteDbData4GetSetMethod(writeDbData4SetMethod, setMethodSimpleClassMap)) {
            return null;
        }
        return writeDbData4SetMethod;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4SetMethod data) {
        return genObjectArrayBase(data);
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return chooseFileColumnDescBase();
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "dto的set方法的信息，包含对应的字段信息"
        };
    }

    public void setSetMethodSimpleClassMap(Map<String, Set<String>> setMethodSimpleClassMap) {
        this.setMethodSimpleClassMap = setMethodSimpleClassMap;
    }
}


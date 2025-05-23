package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4InnerClass;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

import java.util.HashSet;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/3/25
 * @description: 写入数据库，内部类信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_INNER_CLASS,
        minColumnNum = 3,
        maxColumnNum = 3,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_INNER_CLASS
)
public class WriteDbHandler4InnerClassInfo extends AbstractWriteDbHandler<WriteDbData4InnerClass> {
    private final Set<String> handledClassNameSet = new HashSet<>();

    public WriteDbHandler4InnerClassInfo(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4InnerClass genData(String[] array) {
        String innerClassName = array[0];
        if (!handledClassNameSet.add(innerClassName)) {
            // 根据类名判断者已处理过则不处理
            return null;
        }

        String outerClassName = array[1];
        int anonymousClass = Integer.parseInt(array[2]);
        return new WriteDbData4InnerClass(
                dbOperWrapper.querySimpleClassName(innerClassName),
                innerClassName,
                dbOperWrapper.querySimpleClassName(outerClassName),
                outerClassName,
                anonymousClass);
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4InnerClass data) {
        return new Object[]{
                data.getInnerSimpleClassName(),
                data.getInnerClassName(),
                data.getOuterSimpleClassName(),
                data.getOuterClassName(),
                data.getAnonymousClass()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "内部类完整类名",
                "外部类完整类名",
                "是否为匿名内部类，1:是，0:否"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "内部类相关的信息，包括内部类与对应的外部类"
        };
    }
}

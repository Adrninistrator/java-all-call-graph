package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4InnerClass;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;

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
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_INNER_CLASS,
        minColumnNum = 3,
        maxColumnNum = 3,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_INNER_CLASS
)
public class WriteDbHandler4InnerClassInfo extends AbstractWriteDbHandler<WriteDbData4InnerClass> {
    private final Set<String> handledClassNameSet = new HashSet<>();

    @Override
    protected WriteDbData4InnerClass genData(String[] array) {
        String innerClassName = array[0];
        if (!isAllowedClassPrefix(innerClassName) ||
                !handledClassNameSet.add(innerClassName)) {
            /*
                根据类名前缀判断不需要处理时则不处理
                或者已处理过则不处理
             */
            return null;
        }

        String outerClassName = array[1];
        int anonymousClass = Integer.parseInt(array[2]);
        return new WriteDbData4InnerClass(
                dbOperWrapper.getSimpleClassName(innerClassName),
                innerClassName,
                dbOperWrapper.getSimpleClassName(outerClassName),
                outerClassName,
                anonymousClass);
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4InnerClass data) {
        return new Object[]{
                data.getSimpleInnerClassName(),
                data.getInnerClassName(),
                data.getSimpleOuterClassName(),
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
    public String[] chooseOtherFileDetailInfo() {
        return new String[]{
                "内部类相关的信息，包括内部类与对应的外部类"
        };
    }
}

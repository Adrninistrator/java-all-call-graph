package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4InnerClass;

import java.util.HashSet;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/3/25
 * @description: 写入数据库，内部类信息
 */
public class WriteDbHandler4InnerClassInfo extends AbstractWriteDbHandler<WriteDbData4InnerClass> {
    private final Set<String> handledClassNameSet = new HashSet<>();

    @Override
    protected WriteDbData4InnerClass genData(String line) {
        String[] array = splitEquals(line, 3);

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
    protected DbTableInfoEnum chooseDbTableInfo() {
        return DbTableInfoEnum.DTIE_INNER_CLASS;
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
}

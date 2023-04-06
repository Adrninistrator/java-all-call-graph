package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4ClassName;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;

import java.util.HashSet;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 写入数据库，引用的类
 */
public class WriteDbHandler4ClassName extends AbstractWriteDbHandler<WriteDbData4ClassName> {
    private final Set<String> handledClassNameSet = new HashSet<>();

    @Override
    protected WriteDbData4ClassName genData(String line) {
        if (JavaCGConstants.FLAG_HASHTAG.equals(line)) {
            return null;
        }

        // line为类名
        if (!isAllowedClassPrefix(line) ||
                !handledClassNameSet.add(line)) {
              /*
                根据类名前缀判断不需要处理时则不处理
                或者已处理过则不处理
             */
            return null;
        }

        String simpleClassName = JACGClassMethodUtil.getSimpleClassNameFromFull(line);
        return new WriteDbData4ClassName(line, simpleClassName);
    }

    @Override
    protected DbTableInfoEnum chooseDbTableInfo() {
        return DbTableInfoEnum.DTIE_CLASS_NAME;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4ClassName data) {
        return new Object[]{
                genNextRecordId(),
                data.getClassName(),
                data.getSimpleClassName()
        };
    }
}

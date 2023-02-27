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
        // line为类名
        if (JavaCGConstants.FLAG_HASHTAG.equals(line)) {
            return null;
        }

        // 根据类名前缀判断是否需要处理
        if (!isAllowedClassPrefix(line)) {
            return null;
        }

        if (!handledClassNameSet.add(line)) {
            // 已处理过则返回
            return null;
        }

        String simpleClassName = JACGClassMethodUtil.getSimpleClassNameFromFull(line);
        return new WriteDbData4ClassName(genNextRecordId(), line, simpleClassName);
    }

    @Override
    protected DbTableInfoEnum chooseDbTableInfo() {
        return DbTableInfoEnum.DTIE_CLASS_NAME;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4ClassName data) {
        return new Object[]{
                data.getRecordId(),
                data.getClassName(),
                data.getSimpleClassName()
        };
    }
}

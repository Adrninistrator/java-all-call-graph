package com.adrninistrator.jacg.handler.write_db;


import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4ClassInfo;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 写入数据库，类的信息
 */
public class WriteDbHandler4ClassInfo extends AbstractWriteDbHandler<WriteDbData4ClassInfo> {
    @Override
    protected WriteDbData4ClassInfo genData(String line) {
        String[] array = splitEquals(line, 2);

        String className = array[0];
        // 根据类名前缀判断是否需要处理
        if (!isAllowedClassPrefix(className)) {
            return null;
        }

        String accessFlags = array[1];
        return new WriteDbData4ClassInfo(genNextRecordId(), dbOperWrapper.getSimpleClassName(className), Integer.parseInt(accessFlags), className);
    }

    @Override
    protected DbTableInfoEnum chooseDbTableInfo() {
        return DbTableInfoEnum.DTIE_CLASS_INFO;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4ClassInfo data) {
        return new Object[]{
                data.getRecordId(),
                data.getSimpleClassName(),
                data.getAccessFlags(),
                data.getClassName()
        };
    }
}

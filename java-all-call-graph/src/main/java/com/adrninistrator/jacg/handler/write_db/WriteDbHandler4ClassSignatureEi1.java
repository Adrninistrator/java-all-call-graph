package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4ClassSignatureEi1;

/**
 * @author adrninistrator
 * @date 2022/12/7
 * @description: 写入数据库，类的签名中涉及继承与实现的信息1
 */
public class WriteDbHandler4ClassSignatureEi1 extends AbstractWriteDbHandler<WriteDbData4ClassSignatureEi1> {
    @Override
    protected WriteDbData4ClassSignatureEi1 genData(String line) {
        String[] array = splitEquals(line, 5);

        String className = array[0];
        String type = array[1];
        String superItfClassName = array[2];
        int seq = Integer.parseInt(array[3]);
        String signClassName = array[4];

        // 根据类名前缀判断是否需要处理
        if (!isAllowedClassPrefix(className)) {
            return null;
        }

        WriteDbData4ClassSignatureEi1 writeDbData4ClassSignatureEi1 = new WriteDbData4ClassSignatureEi1();
        writeDbData4ClassSignatureEi1.setRecordId(genNextRecordId());
        writeDbData4ClassSignatureEi1.setSimpleClassName(dbOperWrapper.getSimpleClassName(className));
        writeDbData4ClassSignatureEi1.setType(type);
        writeDbData4ClassSignatureEi1.setSuperItfClassName(superItfClassName);
        writeDbData4ClassSignatureEi1.setSeq(seq);
        writeDbData4ClassSignatureEi1.setSignClassName(signClassName);
        writeDbData4ClassSignatureEi1.setClassName(className);

        return writeDbData4ClassSignatureEi1;
    }

    @Override
    protected DbTableInfoEnum chooseDbTableInfo() {
        return DbTableInfoEnum.DTIE_CLASS_SIGNATURE_EI1;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4ClassSignatureEi1 data) {
        return new Object[]{
                data.getRecordId(),
                data.getSimpleClassName(),
                data.getType(),
                data.getSuperItfClassName(),
                data.getSeq(),
                data.getSignClassName(),
                data.getClassName()
        };
    }
}

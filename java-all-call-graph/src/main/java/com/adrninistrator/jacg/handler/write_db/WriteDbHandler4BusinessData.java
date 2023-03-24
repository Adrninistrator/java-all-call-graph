package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4BusinessData;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;

/**
 * @author adrninistrator
 * @date 2022/12/9
 * @description: 写入数据库，方法调用业务功能数据
 */
public class WriteDbHandler4BusinessData extends AbstractWriteDbHandler<WriteDbData4BusinessData> {
    @Override
    protected WriteDbData4BusinessData genData(String line) {
        throw new JavaCGRuntimeException("不会调用当前方法");
    }

    @Override
    protected DbTableInfoEnum chooseDbTableInfo() {
        return DbTableInfoEnum.DTIE_BUSINESS_DATA;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4BusinessData data) {
        return new Object[]{
                data.getCallId(),
                data.getDataType(),
                data.getDataValue()
        };
    }
}

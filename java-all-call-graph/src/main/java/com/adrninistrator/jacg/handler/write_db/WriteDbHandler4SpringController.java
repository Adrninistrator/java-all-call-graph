package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4SpringController;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;

/**
 * @author adrninistrator
 * @date 2022/11/17
 * @description: 写入数据库，Spring Controller信息
 */
public class WriteDbHandler4SpringController extends AbstractWriteDbHandler<WriteDbData4SpringController> {
    @Override
    protected WriteDbData4SpringController genData(String line) {
        throw new JavaCGRuntimeException("不会调用当前方法");
    }

    @Override
    protected DbTableInfoEnum chooseDbTableInfo() {
        return DbTableInfoEnum.DTIE_SPRING_CONTROLLER;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4SpringController data) {
        return new Object[]{
                data.getMethodHash(),
                data.getSeq(),
                data.getShowUri(),
                data.getClassPath(),
                data.getMethodPath(),
                data.getAnnotationName(),
                data.getSimpleClassName(),
                data.getFullMethod()
        };
    }
}

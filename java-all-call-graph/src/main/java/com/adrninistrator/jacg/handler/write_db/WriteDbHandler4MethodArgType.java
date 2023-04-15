package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodArgType;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;

/**
 * @author adrninistrator
 * @date 2023/4/12
 * @description: 写入数据库，方法的参数类型
 */
public class WriteDbHandler4MethodArgType extends AbstractWriteDbHandler<WriteDbData4MethodArgType> {
    @Override
    protected WriteDbData4MethodArgType genData(String line) {
        throw new JavaCGRuntimeException("不会调用当前方法");
    }

    @Override
    protected DbTableInfoEnum chooseDbTableInfo() {
        return DbTableInfoEnum.DTIE_METHOD_ARG_TYPE;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodArgType data) {
        return new Object[]{
                data.getMethodHash(),
                data.getArgSeq(),
                data.getSimpleArgType(),
                data.getArgType(),
                data.getSimpleClassName(),
                data.getFullMethod()
        };
    }
}

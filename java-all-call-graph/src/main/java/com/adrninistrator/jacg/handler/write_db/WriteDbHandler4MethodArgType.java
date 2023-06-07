package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodArgType;

/**
 * @author adrninistrator
 * @date 2023/4/12
 * @description: 写入数据库，方法的参数类型
 */
@JACGWriteDbHandler(
        readFile = false,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_ARG_TYPE
)
public class WriteDbHandler4MethodArgType extends AbstractWriteDbHandler<WriteDbData4MethodArgType> {

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

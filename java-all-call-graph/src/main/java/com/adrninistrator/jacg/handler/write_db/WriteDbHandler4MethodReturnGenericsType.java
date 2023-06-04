package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodReturnGenericsType;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;
import com.adrninistrator.javacg.dto.output.JavaCGOutputInfo;

/**
 * @author adrninistrator
 * @date 2023/4/13
 * @description:
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_METHOD_RETURN_GENERICS_TYPE,
        minColumnNum = 4,
        maxColumnNum = 4,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_RETURN_GENERICS_TYPE
)
public class WriteDbHandler4MethodReturnGenericsType extends AbstractWriteDbHandler<WriteDbData4MethodReturnGenericsType> {
    public WriteDbHandler4MethodReturnGenericsType(JavaCGOutputInfo javaCGOutputInfo) {
        super(javaCGOutputInfo);
    }

    @Override
    protected WriteDbData4MethodReturnGenericsType genData(String[] array) {
        String fullMethod = array[0];
        // 根据完整方法前缀判断是否需要处理
        if (!isAllowedClassPrefix(fullMethod)) {
            return null;
        }

        String className = JACGClassMethodUtil.getClassNameFromMethod(fullMethod);
        String simpleClassName = dbOperWrapper.getSimpleClassName(className);
        String methodHash = JACGUtil.genHashWithLen(fullMethod);
        String type = array[1];
        int typeSeq = Integer.parseInt(array[2]);
        String genericsType = array[3];

        return new WriteDbData4MethodReturnGenericsType(methodHash,
                simpleClassName,
                type,
                typeSeq,
                dbOperWrapper.getSimpleClassName(genericsType),
                genericsType,
                fullMethod);
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodReturnGenericsType data) {
        return new Object[]{
                genNextRecordId(),
                data.getMethodHash(),
                data.getSimpleClassName(),
                data.getType(),
                data.getTypeSeq(),
                data.getSimpleGenericsType(),
                data.getGenericsType(),
                data.getFullMethod()
        };
    }
}

package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodArgGenericsType;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGUtil;

import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/3/20
 * @description: 写入数据库，方法参数泛型类型
 */
public class WriteDbHandler4MethodArgGenericsType extends AbstractWriteDbHandler<WriteDbData4MethodArgGenericsType> {
    // 方法参数存在泛型类型的方法HASH+长度
    private Set<String> withGenericsTypeMethodHash;

    @Override
    protected WriteDbData4MethodArgGenericsType genData(String line) {
        String[] array = splitEquals(line, 5);
        String fullMethod = array[0];
        // 根据完整方法前缀判断是否需要处理
        if (!isAllowedClassPrefix(fullMethod)) {
            return null;
        }

        String className = JACGClassMethodUtil.getClassNameFromMethod(fullMethod);
        String simpleClassName = dbOperWrapper.getSimpleClassName(className);
        String methodHash = JACGUtil.genHashWithLen(fullMethod);
        int argSeq = Integer.parseInt(array[1]);
        String type = array[2];
        int typeSeq = Integer.parseInt(array[3]);
        String genericsType = array[4];

        withGenericsTypeMethodHash.add(methodHash);
        return new WriteDbData4MethodArgGenericsType(methodHash,
                simpleClassName,
                argSeq,
                type,
                typeSeq,
                dbOperWrapper.getSimpleClassName(genericsType),
                genericsType,
                fullMethod);
    }

    @Override
    protected DbTableInfoEnum chooseDbTableInfo() {
        return DbTableInfoEnum.DTIE_METHOD_ARG_GENERICS_TYPE;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodArgGenericsType data) {
        return new Object[]{
                genNextRecordId(),
                data.getMethodHash(),
                data.getSimpleClassName(),
                data.getArgSeq(),
                data.getType(),
                data.getTypeSeq(),
                data.getSimpleGenericsType(),
                data.getGenericsType(),
                data.getFullMethod()
        };
    }

    public void setWithGenericsTypeMethodHash(Set<String> withGenericsTypeMethodHash) {
        this.withGenericsTypeMethodHash = withGenericsTypeMethodHash;
    }
}

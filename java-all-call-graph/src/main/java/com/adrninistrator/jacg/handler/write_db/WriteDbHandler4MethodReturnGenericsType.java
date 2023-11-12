package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4MethodReturnGenericsType;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;

import java.util.Set;

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
    // 方法返回存在泛型类型的方法HASH+长度
    private Set<String> withReturnGenericsTypeMethodHash;

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

        withReturnGenericsTypeMethodHash.add(methodHash);
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

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整方法（类名+方法名+参数）",
                "类型，t:参数类型，gt:参数泛型类型",
                "类型序号，参数类型固定为0，参数泛型类型从0开始",
                "泛型类型或参数类型类名"
        };
    }

    @Override
    public String[] chooseOtherFileDetailInfo() {
        return new String[]{
                "方法返回类型使用的泛型类型信息",
                "示例：”Map<Integer, String> test2()“",
                "对于以上示例，会记录对应的方法，以及方法参数中涉及泛型的Map、Integer、String"
        };
    }

    public void setWithReturnGenericsTypeMethodHash(Set<String> withReturnGenericsTypeMethodHash) {
        this.withReturnGenericsTypeMethodHash = withReturnGenericsTypeMethodHash;
    }
}

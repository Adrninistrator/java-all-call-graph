package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodArgGenericsType;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;

import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/3/20
 * @description: 写入数据库，方法参数泛型类型
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_METHOD_ARG_GENERICS_TYPE,
        minColumnNum = 5,
        maxColumnNum = 5,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_ARG_GENERICS_TYPE
)
public class WriteDbHandler4MethodArgGenericsType extends AbstractWriteDbHandler<WriteDbData4MethodArgGenericsType> {
    // 方法参数存在泛型类型的方法HASH+长度
    private Set<String> withArgsGenericsTypeMethodHash;

    public WriteDbHandler4MethodArgGenericsType(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MethodArgGenericsType genData(String[] array) {
        String fullMethod = array[0];
        // 根据完整方法前缀判断是否需要处理
        if (!isAllowedClassPrefix(fullMethod)) {
            return null;
        }

        String className = JACGClassMethodUtil.getClassNameFromMethod(fullMethod);
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        String methodHash = JACGUtil.genHashWithLen(fullMethod);
        int argSeq = Integer.parseInt(array[1]);
        String type = array[2];
        int typeSeq = Integer.parseInt(array[3]);
        String genericsType = array[4];

        withArgsGenericsTypeMethodHash.add(methodHash);

        WriteDbData4MethodArgGenericsType writeDbData4MethodArgGenericsType = new WriteDbData4MethodArgGenericsType();
        writeDbData4MethodArgGenericsType.setRecordId(genNextRecordId());
        writeDbData4MethodArgGenericsType.setMethodHash(methodHash);
        writeDbData4MethodArgGenericsType.setSimpleClassName(simpleClassName);
        writeDbData4MethodArgGenericsType.setArgSeq(argSeq);
        writeDbData4MethodArgGenericsType.setType(type);
        writeDbData4MethodArgGenericsType.setTypeSeq(typeSeq);
        writeDbData4MethodArgGenericsType.setSimpleGenericsType(dbOperWrapper.querySimpleClassName(genericsType));
        writeDbData4MethodArgGenericsType.setGenericsType(genericsType);
        writeDbData4MethodArgGenericsType.setFullMethod(fullMethod);
        return writeDbData4MethodArgGenericsType;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodArgGenericsType data) {
        return new Object[]{
                data.getRecordId(),
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

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整方法（类名+方法名+参数）",
                "参数序号，从0开始",
                "类型，t:参数类型，gt:参数泛型类型",
                "类型序号，参数类型固定为0，参数泛型类型从0开始",
                "泛型类型或参数类型类名"
        };
    }


    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "方法参数中的集合中涉及的泛型类型",
                "示例：”TestArgumentGenerics1.testAll(int i, List<TestArgument1> list)“",
                "对于以上示例，会记录对应的方法，方法参数的集合类型，和其中涉及的泛型类型 List、TestArgument1"
        };
    }

    public void setWithArgsGenericsTypeMethodHash(Set<String> withArgsGenericsTypeMethodHash) {
        this.withArgsGenericsTypeMethodHash = withArgsGenericsTypeMethodHash;
    }
}

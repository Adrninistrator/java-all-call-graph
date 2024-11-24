package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodArgGenericsType;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/3/20
 * @description: 写入数据库，方法参数泛型类型
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_ARG_GENERICS_TYPE,
        minColumnNum = 10,
        maxColumnNum = 10,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_ARG_GENERICS_TYPE
)
public class WriteDbHandler4MethodArgGenericsType extends AbstractWriteDbHandler<WriteDbData4MethodArgGenericsType> {
    // 方法参数存在泛型类型的方法HASH+长度
    private Set<String> withArgsGenericsTypeMethodHashSet;

    public WriteDbHandler4MethodArgGenericsType(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MethodArgGenericsType genData(String[] array) {
        String fullMethod = readLineData();
        // 根据完整方法前缀判断是否需要处理
        if (!isAllowedClassPrefix(fullMethod)) {
            return null;
        }

        String className = JACGClassMethodUtil.getClassNameFromMethod(fullMethod);
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        String methodHash = JACGUtil.genHashWithLen(fullMethod);
        int seq = Integer.parseInt(readLineData());
        String type = readLineData();
        int typeSeq = Integer.parseInt(readLineData());
        String genericsType = readLineData();
        String simpleGenericsType = dbOperWrapper.querySimpleClassName(genericsType);
        int genericsArrayDimensions = Integer.parseInt(readLineData());
        String typeVariablesName = readLineData();
        String wildcard = readLineData();
        String referenceType = readLineData();
        String genericsCategory = readLineData();

        withArgsGenericsTypeMethodHashSet.add(methodHash);

        WriteDbData4MethodArgGenericsType writeDbData4MethodArgGenericsType = new WriteDbData4MethodArgGenericsType();
        writeDbData4MethodArgGenericsType.setRecordId(genNextRecordId());
        writeDbData4MethodArgGenericsType.setMethodHash(methodHash);
        writeDbData4MethodArgGenericsType.setSimpleClassName(simpleClassName);
        writeDbData4MethodArgGenericsType.setSeq(seq);
        writeDbData4MethodArgGenericsType.setType(type);
        writeDbData4MethodArgGenericsType.setTypeSeq(typeSeq);
        writeDbData4MethodArgGenericsType.setSimpleGenericsType(simpleGenericsType);
        writeDbData4MethodArgGenericsType.setGenericsArrayDimensions(genericsArrayDimensions);
        writeDbData4MethodArgGenericsType.setTypeVariablesName(typeVariablesName);
        writeDbData4MethodArgGenericsType.setWildcard(wildcard);
        writeDbData4MethodArgGenericsType.setReferenceType(referenceType);
        writeDbData4MethodArgGenericsType.setGenericsCategory(genericsCategory);
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
                data.getSeq(),
                data.getType(),
                data.getTypeSeq(),
                data.getSimpleGenericsType(),
                data.getGenericsArrayDimensions(),
                data.getTypeVariablesName(),
                data.getWildcard(),
                data.getReferenceType(),
                data.getGenericsCategory(),
                data.getGenericsType(),
                data.getFullMethod()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整方法（类名+方法名+参数）",
                "参数序号，从0开始",
                "类型，t:参数类型，gt:参数的泛型类型",
                "类型序号，参数类型固定为0，参数中的泛型类型从0开始",
                "方法参数类型或其中的泛型类型类名",
                "方法参数中的泛型数组类型的维度，为0代表不是数组类型",
                "方法参数中的泛型类型变量名称",
                "方法参数中的泛型通配符",
                "方法参数中的泛型通配符引用的类型",
                "方法参数中的泛型类型分类，J:JDK中的类型，C:自定义类型"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "方法参数中的泛型类型",
                "示例：”TestArgumentGenerics1.testAll(int i, List<TestArgument1> list)“",
                "对于以上示例，会记录对应的方法，方法参数的类型，和其中涉及的泛型类型 List、TestArgument1"
        };
    }

    public void setWithArgsGenericsTypeMethodHashSet(Set<String> withArgsGenericsTypeMethodHashSet) {
        this.withArgsGenericsTypeMethodHashSet = withArgsGenericsTypeMethodHashSet;
    }
}

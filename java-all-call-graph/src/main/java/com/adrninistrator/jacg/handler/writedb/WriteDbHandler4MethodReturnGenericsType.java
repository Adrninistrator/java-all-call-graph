package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodReturnGenericsType;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;

import java.util.Set;

/**
 * @author adrninistrator
 * @date 2023/4/13
 * @description: 写入数据库，方法返回泛型类型
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_RETURN_GENERICS_TYPE,
        minColumnNum = 9,
        maxColumnNum = 9,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_RETURN_GENERICS_TYPE
)
public class WriteDbHandler4MethodReturnGenericsType extends AbstractWriteDbHandler<WriteDbData4MethodReturnGenericsType> {
    // 方法返回存在泛型类型的方法HASH+长度
    private Set<String> withReturnGenericsTypeMethodHash;

    public WriteDbHandler4MethodReturnGenericsType(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MethodReturnGenericsType genData(String[] array) {
        String fullMethod = readLineData();
        String className = JavaCG2ClassMethodUtil.getClassNameFromMethod(fullMethod);
        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        String methodHash = JACGUtil.genHashWithLen(fullMethod);
        String type = readLineData();
        int typeSeq = Integer.parseInt(readLineData());
        String genericsType = readLineData();
        String simpleGenericsType = dbOperWrapper.querySimpleClassName(genericsType);
        int genericsArrayDimensions = Integer.parseInt(readLineData());
        String typeVariablesName = readLineData();
        String wildcard = readLineData();
        String referenceType = readLineData();
        String genericsCategory = readLineData();

        withReturnGenericsTypeMethodHash.add(methodHash);
        WriteDbData4MethodReturnGenericsType writeDbData4MethodReturnGenericsType = new WriteDbData4MethodReturnGenericsType();
        writeDbData4MethodReturnGenericsType.setRecordId(genNextRecordId());
        writeDbData4MethodReturnGenericsType.setMethodHash(methodHash);
        writeDbData4MethodReturnGenericsType.setSimpleClassName(simpleClassName);
        writeDbData4MethodReturnGenericsType.setType(type);
        writeDbData4MethodReturnGenericsType.setTypeSeq(typeSeq);
        writeDbData4MethodReturnGenericsType.setSimpleGenericsType(simpleGenericsType);
        writeDbData4MethodReturnGenericsType.setGenericsArrayDimensions(genericsArrayDimensions);
        writeDbData4MethodReturnGenericsType.setTypeVariablesName(typeVariablesName);
        writeDbData4MethodReturnGenericsType.setWildcard(wildcard);
        writeDbData4MethodReturnGenericsType.setReferenceType(referenceType);
        writeDbData4MethodReturnGenericsType.setGenericsCategory(genericsCategory);
        writeDbData4MethodReturnGenericsType.setGenericsType(genericsType);
        writeDbData4MethodReturnGenericsType.setFullMethod(fullMethod);
        return writeDbData4MethodReturnGenericsType;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodReturnGenericsType data) {
        return new Object[]{
                data.getRecordId(),
                data.getMethodHash(),
                data.getSimpleClassName(),
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
                "类型，t:方法返回类型，gt:方法返回类型中的泛型类型",
                "类型序号，方法返回类型固定为0，方法返回类型中的泛型类型从0开始",
                "方法返回类型或其中的泛型类型类名",
                "方法返回类型中的泛型数组类型的维度，为0代表不是数组类型",
                "方法返回类型中的泛型类型变量名称",
                "方法返回类型中的泛型通配符",
                "方法返回类型中的泛型通配符引用的类型",
                "方法返回类型中的泛型类型分类，J:JDK中的类型，C:自定义类型",
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "方法返回类型中涉及的泛型类型",
                "示例：”Map<Integer, String> test2()“",
                "对于以上示例，会记录对应的方法，以及方法返回的类型，和其中涉及的泛型类型 Map、Integer、String"
        };
    }

    public void setWithReturnGenericsTypeMethodHash(Set<String> withReturnGenericsTypeMethodHash) {
        this.withReturnGenericsTypeMethodHash = withReturnGenericsTypeMethodHash;
    }
}

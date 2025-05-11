package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodArgument;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;

/**
 * @author adrninistrator
 * @date 2023/4/12
 * @description: 写入数据库，方法的参数类型
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_ARGUMENT,
        minColumnNum = 8,
        maxColumnNum = 8,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_ARGUMENT
)
public class WriteDbHandler4MethodArgument extends AbstractWriteDbHandler<WriteDbData4MethodArgument> {

    public WriteDbHandler4MethodArgument(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MethodArgument genData(String[] array) {
        String fullMethod = readLineData();
        String returnType = readLineData();
        String className = JavaCG2ClassMethodUtil.getClassNameFromMethod(fullMethod);
        String methodHash = JACGClassMethodUtil.genMethodHashWithLen(fullMethod, returnType);
        int argSeq = Integer.parseInt(readLineData());
        String argName = readLineData();
        String argTypeNad = readLineData();
        int arrayDimensions = Integer.parseInt(readLineData());
        String argCategory = readLineData();
        int existsGenericsType = Integer.parseInt(readLineData());

        WriteDbData4MethodArgument writeDbData4MethodArgument = new WriteDbData4MethodArgument();
        writeDbData4MethodArgument.setRecordId(genNextRecordId());
        writeDbData4MethodArgument.setMethodHash(methodHash);
        writeDbData4MethodArgument.setArgSeq(argSeq);
        writeDbData4MethodArgument.setSimpleArgTypeNad(dbOperWrapper.querySimpleClassName(argTypeNad));
        writeDbData4MethodArgument.setArgName(argName);
        writeDbData4MethodArgument.setArgTypeNad(argTypeNad);
        writeDbData4MethodArgument.setArrayDimensions(arrayDimensions);
        writeDbData4MethodArgument.setArgCategory(argCategory);
        writeDbData4MethodArgument.setExistsGenericsType(existsGenericsType);
        writeDbData4MethodArgument.setSimpleClassName(dbOperWrapper.querySimpleClassName(className));
        writeDbData4MethodArgument.setFullMethod(fullMethod);
        writeDbData4MethodArgument.setReturnType(returnType);
        return writeDbData4MethodArgument;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodArgument data) {
        return new Object[]{
                data.getRecordId(),
                data.getMethodHash(),
                data.getArgSeq(),
                data.getSimpleArgTypeNad(),
                data.getArgName(),
                data.getArgTypeNad(),
                data.getArrayDimensions(),
                data.getArgCategory(),
                data.getExistsGenericsType(),
                data.getSimpleClassName(),
                data.getFullMethod(),
                data.getReturnType()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整方法（类名+方法名+参数）",
                "方法返回类型，包含数组标志",
                "参数序号，从0开始",
                "参数名称，可能为空",
                "参数类型（不包含数组标志）",
                "参数数组类型的维度，为0代表不是数组类型",
                "参数类型分类，J:JDK中的类型，C:自定义类型",
                "是否存在泛型类型，1:是，0:否"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "方法参数的类型及名称等，参数名称可能为空"
        };
    }
}

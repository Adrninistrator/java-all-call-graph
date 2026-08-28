package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4MethodCallFieldActualType;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;

/**
 * @author adrninistrator
 * @date 2026/6/21
 * @description: 写入数据库，方法调用被调用对象为非静态字段时的实际类型（运行时多态）
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_METHOD_CALL_FIELD_ACTUAL_TYPE,
        minColumnNum = 5,
        maxColumnNum = 5,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_METHOD_CALL_FIELD_ACTUAL_TYPE
)
public class WriteDbHandler4MethodCallFieldActualType extends AbstractWriteDbHandler<WriteDbData4MethodCallFieldActualType> {

    public WriteDbHandler4MethodCallFieldActualType(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4MethodCallFieldActualType genData(String[] array) {
        String callerFullMethod = readLineData();
        int callerLineNumber = Integer.parseInt(readLineData());
        String fieldType = readLineData();
        String fieldName = readLineData();
        String fieldActualType = readLineData();

        // 推导调用方完整类名、唯一类名与方法名（不在文件中，不计入文件列数）
        String callerClassName = JavaCG2ClassMethodUtil.getClassNameFromMethod(callerFullMethod);
        String callerSimpleClassName = dbOperWrapper.querySimpleClassName(callerClassName);
        String callerMethodName = JavaCG2ClassMethodUtil.getMethodNameFromFull(callerFullMethod);

        WriteDbData4MethodCallFieldActualType writeDbData4MethodCallFieldActualType = new WriteDbData4MethodCallFieldActualType();
        writeDbData4MethodCallFieldActualType.setRecordId(genNextRecordId());
        writeDbData4MethodCallFieldActualType.setCallerSimpleClassName(callerSimpleClassName);
        writeDbData4MethodCallFieldActualType.setCallerClassName(callerClassName);
        writeDbData4MethodCallFieldActualType.setCallerMethodName(callerMethodName);
        writeDbData4MethodCallFieldActualType.setCallerFullMethod(callerFullMethod);
        writeDbData4MethodCallFieldActualType.setCallerLineNumber(callerLineNumber);
        writeDbData4MethodCallFieldActualType.setFieldType(fieldType);
        writeDbData4MethodCallFieldActualType.setFieldName(fieldName);
        writeDbData4MethodCallFieldActualType.setFieldActualType(fieldActualType);
        return writeDbData4MethodCallFieldActualType;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4MethodCallFieldActualType data) {
        return new Object[]{
                data.getRecordId(),
                data.getCallerSimpleClassName(),
                data.getCallerClassName(),
                data.getCallerMethodName(),
                data.getCallerFullMethod(),
                data.getCallerLineNumber(),
                data.getFieldType(),
                data.getFieldName(),
                data.getFieldActualType()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "调用方，完整方法（类名+方法名+参数）",
                "方法调用指令对应的代码行号",
                "被调用方字段声明类型",
                "被调用方字段名称",
                "被调用方字段实际类型（运行时多态，与声明类型不同才记录，多个实际类型每个一行）"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "方法调用被调用对象为非静态字段时的实际类型（运行时多态），当字段声明类型与实际类型不同时记录，每个实际类型生成一行"
        };
    }
}

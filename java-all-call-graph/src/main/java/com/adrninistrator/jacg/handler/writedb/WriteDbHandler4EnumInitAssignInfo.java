package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4EnumInitAssignInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.util.JavaCG2Util;

/**
 * @author adrninistrator
 * @date 2025/1/17
 * @description: 写入数据库，枚举类初始化赋值信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_ENUM_INIT_ASSIGN_INFO,
        minColumnNum = 7,
        maxColumnNum = 7,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_ENUM_INIT_ASSIGN_INFO
)
public class WriteDbHandler4EnumInitAssignInfo extends AbstractWriteDbHandler<WriteDbData4EnumInitAssignInfo> {

    public WriteDbHandler4EnumInitAssignInfo(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4EnumInitAssignInfo genData(String[] array) {
        String fullMethod = readLineData();
        // 根据完整方法前缀判断是否需要处理
        if (!isAllowedClassPrefix(fullMethod)) {
            return null;
        }
        String className = JACGClassMethodUtil.getClassNameFromMethod(fullMethod);
        String constName = readLineData();
        int ordinal = Integer.parseInt(readLineData());
        int argSeq = Integer.parseInt(readLineData());
        String fieldType = readLineData();
        boolean isBase64Value = JavaCG2YesNoEnum.isYes(readLineData());
        String fieldValue = readLineData();
        if (isBase64Value) {
            fieldValue = JavaCG2Util.base64Decode(fieldValue);
        }

        WriteDbData4EnumInitAssignInfo enumInitAssignInfo = new WriteDbData4EnumInitAssignInfo();
        enumInitAssignInfo.setRecordId(genNextRecordId());
        enumInitAssignInfo.setSimpleClassName(dbOperWrapper.querySimpleClassName(className));
        enumInitAssignInfo.setConstName(constName);
        enumInitAssignInfo.setOrdinal(ordinal);
        enumInitAssignInfo.setArgSeq(argSeq);
        enumInitAssignInfo.setFieldType(fieldType);
        enumInitAssignInfo.setFieldValue(fieldValue);
        enumInitAssignInfo.setClassName(className);
        enumInitAssignInfo.setFullMethod(fullMethod);
        return enumInitAssignInfo;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4EnumInitAssignInfo data) {
        return new Object[]{
                data.getRecordId(),
                data.getSimpleClassName(),
                data.getConstName(),
                data.getOrdinal(),
                data.getArgSeq(),
                data.getFieldType(),
                data.getFieldValue(),
                data.getClassName(),
                data.getFullMethod()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "枚举类构造函数完整方法（类名+方法名+参数）",
                "枚举常量名称",
                "枚举字段序号",
                "通过枚举类构造函数被赋值的参数序号（从1开始，最小为3）",
                "通过枚举类构造函数被赋值的字段类型",
                "通过枚举类构造函数被赋值的字段值是否有进行BASE64编码，1:是，0:否",
                "通过枚举类构造函数被赋值的字段值"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "枚举类初始化赋值信息，即枚举类中的每个常量在初始化时指定的参数信息"
        };
    }
}

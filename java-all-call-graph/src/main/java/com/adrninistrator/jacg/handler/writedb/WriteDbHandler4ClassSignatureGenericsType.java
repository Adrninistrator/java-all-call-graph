package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassSignatureGenericsType;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2024/6/20
 * @description: 写入数据库，类的签名中的泛型信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_CLASS_SIGNATURE_GENERICS_TYPE,
        minColumnNum = 4,
        maxColumnNum = 4,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_CLASS_SIGNATURE_GENERICS_TYPE
)
public class WriteDbHandler4ClassSignatureGenericsType extends AbstractWriteDbHandler<WriteDbData4ClassSignatureGenericsType> {

    public WriteDbHandler4ClassSignatureGenericsType(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4ClassSignatureGenericsType genData(String[] array) {
        String className = readLineData();
        // 根据类名前缀判断是否需要处理
        if (!isAllowedClassPrefix(className)) {
            return null;
        }
        int seq = Integer.parseInt(readLineData());
        String typeVariablesName = readLineData();
        String genericsExtendsClassName = readLineData();

        WriteDbData4ClassSignatureGenericsType writeDbData4ClassSignatureGenericsType = new WriteDbData4ClassSignatureGenericsType();
        writeDbData4ClassSignatureGenericsType.setRecordId(genNextRecordId());
        writeDbData4ClassSignatureGenericsType.setSimpleClassName(dbOperWrapper.querySimpleClassName(className));
        writeDbData4ClassSignatureGenericsType.setSeq(seq);
        writeDbData4ClassSignatureGenericsType.setTypeVariablesName(typeVariablesName);
        writeDbData4ClassSignatureGenericsType.setGenericsExtendsClassName(genericsExtendsClassName);
        writeDbData4ClassSignatureGenericsType.setClassName(className);
        return writeDbData4ClassSignatureGenericsType;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4ClassSignatureGenericsType data) {
        return new Object[]{
                data.getRecordId(),
                data.getSimpleClassName(),
                data.getSeq(),
                data.getTypeVariablesName(),
                data.getGenericsExtendsClassName(),
                data.getClassName()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整类名",
                "序号，从0开始",
                "类的签名中的泛型类型变量名称",
                "类的签名中的泛型的父类类名"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "类的签名中的泛型信息",
                "例如：“interface CommonMapper<S, T1> extends BaseMapper<T1>”，对应信息中会包含 S、T1"
        };
    }
}

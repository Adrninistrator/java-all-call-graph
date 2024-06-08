package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassSignatureEi1;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2022/12/7
 * @description: 写入数据库，类的签名中涉及继承与实现的信息1
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_CLASS_SIGNATURE_EI1,
        minColumnNum = 5,
        maxColumnNum = 5,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_CLASS_SIGNATURE_EI1
)
public class WriteDbHandler4ClassSignatureEi1 extends AbstractWriteDbHandler<WriteDbData4ClassSignatureEi1> {

    public WriteDbHandler4ClassSignatureEi1(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4ClassSignatureEi1 genData(String[] array) {
        String className = array[0];
        String type = array[1];
        String superItfClassName = array[2];
        int seq = Integer.parseInt(array[3]);
        String signClassName = array[4];

        // 根据类名前缀判断是否需要处理
        if (!isAllowedClassPrefix(className)) {
            return null;
        }
        return new WriteDbData4ClassSignatureEi1(dbOperWrapper.getSimpleClassName(className), type, superItfClassName, seq, signClassName, className);
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4ClassSignatureEi1 data) {
        return new Object[]{
                genNextRecordId(),
                data.getSimpleClassName(),
                data.getType(),
                data.getSuperItfClassName(),
                data.getSeq(),
                data.getSignClassName(),
                data.getClassName()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整类名",
                "类型，e:继承，i:实现",
                "父类或接口的类名",
                "序号，从0开始",
                "签名中的完整类名"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "类在继承类或实现接口时，在签名中指定的泛型类型",
                "例如：“TestClassWithSignature1 implements TestInterfaceWithSignature<String, Integer>”，包含 String、Integer",
                "例如：“TestClassWithSignature2 extends TestAbstractClassWithSignature<TestArgument1, TestArgument2>”，包含 TestArgument1、TestArgument2"
        };
    }
}

package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassReference;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;

/**
 * @author adrninistrator
 * @date 2024/8/17
 * @description: 写入数据库，引用的类
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_CLASS_REFERENCE,
        minColumnNum = 2,
        maxColumnNum = 2,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_CLASS_REFERENCE
)
public class WriteDbHandler4ClassReference extends AbstractWriteDbHandler<WriteDbData4ClassReference> {
    private WriteDbHandler4ClassName writeDbHandler4ClassName;

    public WriteDbHandler4ClassReference(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    public void afterHandle() {
        super.afterHandle();
        writeDbHandler4ClassName.afterHandle();
    }

    @Override
    protected WriteDbData4ClassReference genData(String[] array) {
        String className = array[0];
        String referenceClassName = array[1];

        if (!isAllowedClassPrefix(className) || !isAllowedClassPrefix(referenceClassName)) {
            // 根据类名前缀判断不需要处理
            return null;
        }

        String simpleClassName = JavaCG2ClassMethodUtil.getSimpleClassNameFromFull(className);
        String referencedSimpleClassName = JavaCG2ClassMethodUtil.getSimpleClassNameFromFull(referenceClassName);
        WriteDbData4ClassReference writeDbData4ClassReference = new WriteDbData4ClassReference();
        writeDbData4ClassReference.setRecordId(genNextRecordId());
        writeDbData4ClassReference.setClassName(className);
        writeDbData4ClassReference.setSimpleClassName(simpleClassName);
        writeDbData4ClassReference.setReferencedClassName(referenceClassName);
        writeDbData4ClassReference.setReferencedSimpleClassName(referencedSimpleClassName);

        writeDbHandler4ClassName.addClassReference(writeDbData4ClassReference);
        return writeDbData4ClassReference;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4ClassReference data) {
        return new Object[]{
                data.getRecordId(),
                data.getClassName(),
                data.getSimpleClassName(),
                data.getReferencedClassName(),
                data.getReferencedSimpleClassName()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "引用的完整类名",
                "被引用的完整类名"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "项目中所有的类所引用的其他类关系"
        };
    }

    public void setWriteDbHandler4ClassName(WriteDbHandler4ClassName writeDbHandler4ClassName) {
        this.writeDbHandler4ClassName = writeDbHandler4ClassName;
    }
}

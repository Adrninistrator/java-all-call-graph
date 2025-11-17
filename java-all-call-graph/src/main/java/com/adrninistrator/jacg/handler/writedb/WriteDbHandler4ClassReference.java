package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassReference;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2024/8/17
 * @description: 写入数据库，引用的类
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_CLASS_REFERENCE,
        minColumnNum = 3,
        maxColumnNum = 3,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_CLASS_REFERENCE
)
public class WriteDbHandler4ClassReference extends AbstractWriteDbHandler<WriteDbData4ClassReference> {
    private WriteDbHandler4ClassName writeDbHandler4ClassName;

    // 是否仅处理类名
    private boolean onlyHandleClassName;

    public WriteDbHandler4ClassReference(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    public void afterHandle() {
        super.afterHandle();
        if (onlyHandleClassName) {
            writeDbHandler4ClassName.afterHandle();
        }
    }

    @Override
    protected WriteDbData4ClassReference genData(String[] array) {
        String className = readLineData();
        String referenceClassName = readLineData();

        if (onlyHandleClassName) {
            writeDbHandler4ClassName.addClassReference(className, referenceClassName);
            writeDbHandler4ClassName.tryInsertDb();
            return null;
        }

        String simpleClassName = dbOperWrapper.querySimpleClassName(className);
        String referencedSimpleClassName = dbOperWrapper.querySimpleClassName(referenceClassName);
        int jarNum = Integer.parseInt(readLineData());
        WriteDbData4ClassReference writeDbData4ClassReference = new WriteDbData4ClassReference();
        writeDbData4ClassReference.setRecordId(genNextRecordId());
        writeDbData4ClassReference.setClassName(className);
        writeDbData4ClassReference.setSimpleClassName(simpleClassName);
        writeDbData4ClassReference.setReferencedClassName(referenceClassName);
        writeDbData4ClassReference.setReferencedSimpleClassName(referencedSimpleClassName);
        writeDbData4ClassReference.setJarNum(jarNum);
        return writeDbData4ClassReference;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4ClassReference data) {
        return new Object[]{
                data.getRecordId(),
                data.getClassName(),
                data.getSimpleClassName(),
                data.getReferencedClassName(),
                data.getReferencedSimpleClassName(),
                data.getJarNum()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "引用的完整类名",
                "被引用的完整类名",
                "类所在的jar文件序号"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "类所引用的其他类关系"
        };
    }

    public void setOnlyHandleClassName(boolean onlyHandleClassName) {
        this.onlyHandleClassName = onlyHandleClassName;
    }

    public void setWriteDbHandler4ClassName(WriteDbHandler4ClassName writeDbHandler4ClassName) {
        this.writeDbHandler4ClassName = writeDbHandler4ClassName;
    }
}

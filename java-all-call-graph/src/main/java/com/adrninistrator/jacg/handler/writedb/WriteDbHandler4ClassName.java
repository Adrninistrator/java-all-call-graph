package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassName;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassReference;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.util.JavaCG2ClassMethodUtil;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 写入数据库，类名
 */
@JACGWriteDbHandler(
        readFile = false,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_CLASS_NAME,
        dependsWriteDbTableEnums = {DbTableInfoEnum.DTIE_CLASS_REFERENCE}
)
public class WriteDbHandler4ClassName extends AbstractWriteDbHandler<WriteDbData4ClassName> {
    public WriteDbHandler4ClassName(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    private final Set<String> classNameSet = new HashSet<>();

    // 增加数据
    public void addClassReference(WriteDbData4ClassReference data) {
        classNameSet.add(data.getClassName());
        classNameSet.add(data.getReferencedClassName());
    }

    @Override
    public void afterHandle() {
        List<String> classNameList = new ArrayList<>(classNameSet);
        Collections.sort(classNameList);
        for (String className : classNameList) {
            WriteDbData4ClassName writeDbData4ClassName = new WriteDbData4ClassName();
            writeDbData4ClassName.setRecordId(genNextRecordId());
            writeDbData4ClassName.setClassName(className);
            writeDbData4ClassName.setSimpleClassName(JavaCG2ClassMethodUtil.getSimpleClassNameFromFull(className));
            writeDbData4ClassName.setDuplicateClass(JavaCG2YesNoEnum.NO.getIntValue());
            dataList.add(writeDbData4ClassName);
            tryInsertDb();
        }

        super.afterHandle();
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4ClassName data) {
        return new Object[]{
                data.getRecordId(),
                data.getClassName(),
                data.getSimpleClassName(),
                data.getDuplicateClass()
        };
    }
}

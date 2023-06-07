package com.adrninistrator.jacg.handler.write_db;


import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4ClassInfo;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 写入数据库，类的信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_CLASS_INFO,
        minColumnNum = 2,
        maxColumnNum = 2,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_CLASS_INFO
)
public class WriteDbHandler4ClassInfo extends AbstractWriteDbHandler<WriteDbData4ClassInfo> {

    @Override
    protected WriteDbData4ClassInfo genData(String[] array) {
        String className = array[0];
        // 根据类名前缀判断是否需要处理
        if (!isAllowedClassPrefix(className)) {
            return null;
        }

        String accessFlags = array[1];
        return new WriteDbData4ClassInfo(dbOperWrapper.getSimpleClassName(className), Integer.parseInt(accessFlags), className);
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4ClassInfo data) {
        return new Object[]{
                genNextRecordId(),
                data.getSimpleClassName(),
                data.getAccessFlags(),
                data.getClassName()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整类名",
                "类的access_flags"
        };
    }

    @Override
    public String[] chooseOtherFileDetailInfo() {
        return new String[]{
                "类的信息，主要是access_flags"
        };
    }
}

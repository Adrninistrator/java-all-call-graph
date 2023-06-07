package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4ClassName;
import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;
import com.adrninistrator.javacg.common.enums.JavaCGYesNoEnum;

import java.util.HashSet;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/11/16
 * @description: 写入数据库，引用的类
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_CLASS_NAME,
        minColumnNum = 1,
        maxColumnNum = 1,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_CLASS_NAME
)
public class WriteDbHandler4ClassName extends AbstractWriteDbHandler<WriteDbData4ClassName> {
    private final Set<String> handledClassNameSet = new HashSet<>();

    @Override
    protected WriteDbData4ClassName genData(String[] array) {
        String line = array[0];
        if (JavaCGConstants.FLAG_HASHTAG.equals(line)) {
            return null;
        }

        // line为类名
        if (!isAllowedClassPrefix(line) ||
                !handledClassNameSet.add(line)) {
             /*
                根据类名前缀判断不需要处理时则不处理
                或者已处理过则不处理
             */
            return null;
        }

        String simpleClassName = JACGClassMethodUtil.getSimpleClassNameFromFull(line);
        return new WriteDbData4ClassName(line, simpleClassName, JavaCGYesNoEnum.NO.getIntValue());
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4ClassName data) {
        return new Object[]{
                genNextRecordId(),
                data.getClassName(),
                data.getSimpleClassName(),
                data.getDuplicateClass()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "完整类名"
        };
    }

    @Override
    public String[] chooseOtherFileDetailInfo() {
        return new String[]{
                "项目中所有引用的类",
                "文件中内容为#的行将各个类中引用的类分隔开；第一个类代表引用其他类的类，后面的类代表被引用的类",
                "可能出现重复值"
        };
    }
}

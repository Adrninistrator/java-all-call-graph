package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4ClassInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.handler.classes.ClassInfoHandler;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author adrninistrator
 * @date 2024/12/5
 * @description: 写入数据库，重复同名类的信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_DUP_CLASS_INFO,
        minColumnNum = 5,
        maxColumnNum = 5,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_DUP_CLASS_INFO,
        dependsWriteDbTableEnums = {DbTableInfoEnum.DTIE_CLASS_INFO}
)
public class WriteDbHandler4DupClassInfo extends WriteDbHandler4ClassInfo {

    private static final Logger logger = LoggerFactory.getLogger(WriteDbHandler4DupClassInfo.class);
    private ClassInfoHandler classInfoHandler;

    public WriteDbHandler4DupClassInfo(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    public void init(WriteDbResult writeDbResult) {
        super.init(writeDbResult);
        classInfoHandler = new ClassInfoHandler(dbOperWrapper);
    }

    @Override
    protected WriteDbData4ClassInfo genData(String[] array) {
        WriteDbData4ClassInfo dupClassInfo = super.genData(array);
        /*
            检查重复同名类与原始类是否在同一个jar文件中，且MD5相同，若是则跳过，不写入数据库
            jar文件中可以出现重复的同名文件，例如 xmlbeans-2.6.0.jar\org\apache\xmlbeans\xml\stream\Location.class
         */
        WriteDbData4ClassInfo classInfo = classInfoHandler.queryClassInfoByClassName(dupClassInfo.getClassName());
        if (dupClassInfo.getJarNum() == classInfo.getJarNum()) {
            if (dupClassInfo.getClassFileHash().equals(classInfo.getClassFileHash())) {
                logger.warn("在同一个jar文件中出现了重复同名类，且文件HASH相同，跳过写入数据库 {} {}", dupClassInfo.getClassName(), dupClassInfo.getClassPathInJar());
                return null;
            }
            logger.warn("在同一个jar文件中出现了重复同名类，且文件HASH不同 {} {}", dupClassInfo.getClassName(), dupClassInfo.getClassPathInJar());
        }
        return dupClassInfo;
    }

    @Override
    public String[] chooseFileDetailInfo() {
        String[] parentInfo = super.chooseFileDetailInfo();
        String[] result = new String[parentInfo.length + 1];

        result[0] = "重复同名类的信息";
        System.arraycopy(parentInfo, 0, result, 1, parentInfo.length);
        return result;
    }
}

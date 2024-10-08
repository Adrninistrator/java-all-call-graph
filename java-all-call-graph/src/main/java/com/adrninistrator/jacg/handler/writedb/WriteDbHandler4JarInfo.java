package com.adrninistrator.jacg.handler.writedb;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4JarInfo;
import com.adrninistrator.jacg.dto.writedb.WriteDbResult;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2OutPutFileTypeEnum;
import com.adrninistrator.javacg2.exceptions.JavaCG2RuntimeException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Date;

/**
 * @author adrninistrator
 * @date 2022/11/15
 * @description: 写入数据库，jar包信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_JAR_INFO,
        minColumnNum = 3,
        maxColumnNum = 3,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_JAR_INFO
)
public class WriteDbHandler4JarInfo extends AbstractWriteDbHandler<WriteDbData4JarInfo> {
    private static final Logger logger = LoggerFactory.getLogger(WriteDbHandler4JarInfo.class);

    public WriteDbHandler4JarInfo(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4JarInfo genData(String[] array) {
        String jarType = array[0];
        String jarNumStr = array[1];
        String jarFilePath = array[2];
        String jarFileName = JACGFileUtil.getFileNameFromPath(jarFilePath);
        String lastModifiedTime = "";
        String jarFileHash = "";

        if (JavaCG2Constants.FILE_KEY_JAR_INFO_PREFIX.equals(jarType)) {
            if (!JACGFileUtil.isFileExists(jarFilePath)) {
                logger.error("jar包文件不存在: {}", jarFilePath);
                throw new JavaCG2RuntimeException("jar包文件不存在");
            }

            // 为jar包时，获取文件修改时间及HASH
            lastModifiedTime = JACGFileUtil.getFileLastModifiedTime(jarFilePath);
            jarFileHash = JACGFileUtil.getFileMd5(jarFilePath);
        }

        WriteDbData4JarInfo writeDbData4JarInfo = new WriteDbData4JarInfo();
        writeDbData4JarInfo.setJarNum(Integer.parseInt(jarNumStr));
        writeDbData4JarInfo.setJarType(jarType);
        writeDbData4JarInfo.setJarPathHash(JACGUtil.genHashWithLen(jarFilePath));
        writeDbData4JarInfo.setJarFullPath(jarFilePath);
        writeDbData4JarInfo.setJarFileName(jarFileName);
        writeDbData4JarInfo.setJarFileNameHead(JACGFileUtil.getJarFileHead(jarFileName));
        writeDbData4JarInfo.setJarFileNameExt(JACGFileUtil.getFileNameExt(jarFileName));
        writeDbData4JarInfo.setLastModifiedTime(lastModifiedTime);
        writeDbData4JarInfo.setJarFileHash(jarFileHash);
        writeDbData4JarInfo.setImportTime(new Date());
        return writeDbData4JarInfo;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4JarInfo data) {
        return new Object[]{
                data.getJarNum(),
                data.getJarType(),
                data.getJarPathHash(),
                data.getJarFullPath(),
                data.getJarFileName(),
                data.getJarFileNameHead(),
                data.getJarFileNameExt(),
                data.getLastModifiedTime(),
                data.getJarFileHash(),
                data.getImportTime()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "Jar包类型，J: jar包，D: 目录，R: 解析结果文件保存目录",
                "Jar包序号",
                "Jar包完整路径"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "解析的Jar包或目录相关的信息"
        };
    }
}

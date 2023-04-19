package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4JarInfo;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author adrninistrator
 * @date 2022/11/15
 * @description: 写入数据库，jar包信息
 */
public class WriteDbHandler4JarInfo extends AbstractWriteDbHandler<WriteDbData4JarInfo> {
    private static final Logger logger = LoggerFactory.getLogger(WriteDbHandler4JarInfo.class);

    @Override
    protected WriteDbData4JarInfo genData(String line) {
        String[] array = splitEquals(line, 3);

        String jarType = array[0];
        String jarNumStr = array[1];
        String jarFilePath = array[2];
        String jarFileName = JACGFileUtil.getFileNameFromPath(jarFilePath);
        String lastModified = "";
        String jarFileHash = "";

        if (JavaCGConstants.FILE_KEY_JAR_INFO_PREFIX.equals(jarType)) {
            if (!JACGFileUtil.isFileExists(jarFilePath)) {
                logger.error("jar包文件不存在: {}", jarFilePath);
                throw new JavaCGRuntimeException("jar包文件不存在");
            }

            // 为jar包时，获取文件修改时间及HASH
            lastModified = String.valueOf(JACGFileUtil.getFileLastModified(jarFilePath));
            jarFileHash = JACGFileUtil.getFileMd5(jarFilePath);
        }
        return new WriteDbData4JarInfo(Integer.parseInt(jarNumStr),
                jarType,
                JACGUtil.genHashWithLen(jarFilePath),
                jarFilePath,
                jarFileName,
                lastModified,
                jarFileHash);
    }

    @Override
    protected DbTableInfoEnum chooseDbTableInfo() {
        return DbTableInfoEnum.DTIE_JAR_INFO;
    }

    @Override
    protected Object[] genObjectArray(WriteDbData4JarInfo data) {
        return new Object[]{
                data.getJarNum(),
                data.getJarType(),
                data.getJarPathHash(),
                data.getJarFullPath(),
                data.getJarFileName(),
                data.getLastModified(),
                data.getJarHash()
        };
    }
}

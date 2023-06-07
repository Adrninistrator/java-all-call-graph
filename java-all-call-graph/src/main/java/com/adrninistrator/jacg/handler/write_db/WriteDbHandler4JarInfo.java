package com.adrninistrator.jacg.handler.write_db;

import com.adrninistrator.jacg.common.annotations.JACGWriteDbHandler;
import com.adrninistrator.jacg.common.enums.DbTableInfoEnum;
import com.adrninistrator.jacg.dto.write_db.WriteDbData4JarInfo;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.common.enums.JavaCGOutPutFileTypeEnum;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author adrninistrator
 * @date 2022/11/15
 * @description: 写入数据库，jar包信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCGOutPutFileTypeEnum.OPFTE_JAR_INFO,
        minColumnNum = 3,
        maxColumnNum = 3,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_JAR_INFO
)
public class WriteDbHandler4JarInfo extends AbstractWriteDbHandler<WriteDbData4JarInfo> {
    private static final Logger logger = LoggerFactory.getLogger(WriteDbHandler4JarInfo.class);

    @Override
    protected WriteDbData4JarInfo genData(String[] array) {
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

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "Jar包类型，J: jar包，D: 目录",
                "Jar包序号",
                "Jar包完整路径"
        };
    }

    @Override
    public String[] chooseOtherFileDetailInfo() {
        return new String[]{
                "解析的Jar包或目录相关的信息"
        };
    }
}

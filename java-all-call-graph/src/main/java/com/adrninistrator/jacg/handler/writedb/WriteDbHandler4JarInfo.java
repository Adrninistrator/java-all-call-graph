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
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.javacg2.util.JavaCG2JarUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Date;

/**
 * @author adrninistrator
 * @date 2022/11/15
 * @description: 写入数据库，jar文件信息
 */
@JACGWriteDbHandler(
        readFile = true,
        mainFile = true,
        mainFileTypeEnum = JavaCG2OutPutFileTypeEnum.OPFTE_JAR_INFO,
        minColumnNum = 4,
        maxColumnNum = 4,
        dbTableInfoEnum = DbTableInfoEnum.DTIE_JAR_INFO
)
public class WriteDbHandler4JarInfo extends AbstractWriteDbHandler<WriteDbData4JarInfo> {
    private static final Logger logger = LoggerFactory.getLogger(WriteDbHandler4JarInfo.class);

    public WriteDbHandler4JarInfo(WriteDbResult writeDbResult) {
        super(writeDbResult);
    }

    @Override
    protected WriteDbData4JarInfo genData(String[] array) {
        String jarType = readLineData();
        String jarNumStr = readLineData();
        String jarFilePath = readLineData();
        String innerJarFilePath = readLineData();
        String jarFileName = JavaCG2FileUtil.getFileNameFromPath(jarFilePath);
        String lastModifiedTime = "";
        String jarFileHash = "";

        if (JavaCG2Constants.FILE_KEY_JAR.equals(jarType)) {
            if (!JavaCG2FileUtil.isFileExists(jarFilePath)) {
                logger.error("jar/war文件不存在: {}", jarFilePath);
                throw new JavaCG2RuntimeException("jar/war文件不存在");
            }

            // 为jar包时，获取文件修改时间及HASH
            lastModifiedTime = JACGFileUtil.getFileLastModifiedTime(jarFilePath);
            jarFileHash = JACGFileUtil.getFileMd5(jarFilePath);
        }

        String innerJarFileName = "";
        if (StringUtils.isNotEmpty(innerJarFilePath)) {
            innerJarFileName = JavaCG2JarUtil.getJarEntryNameFromPath(innerJarFilePath);
        }
        WriteDbData4JarInfo writeDbData4JarInfo = new WriteDbData4JarInfo();
        writeDbData4JarInfo.setJarNum(Integer.parseInt(jarNumStr));
        writeDbData4JarInfo.setJarType(jarType);
        writeDbData4JarInfo.setJarPathHash(JACGUtil.genHashWithLen(jarFilePath));
        writeDbData4JarInfo.setJarFullPath(jarFilePath);
        writeDbData4JarInfo.setJarFileName(jarFileName);
        writeDbData4JarInfo.setJarFileNameHead(JACGFileUtil.getJarFileHead(jarFileName));
        writeDbData4JarInfo.setJarFileNameExt(JavaCG2FileUtil.getFileExt(jarFileName));
        writeDbData4JarInfo.setLastModifiedTime(lastModifiedTime);
        writeDbData4JarInfo.setJarFileHash(jarFileHash);
        writeDbData4JarInfo.setInnerJarPath(innerJarFilePath);
        writeDbData4JarInfo.setInnerJarFileName(innerJarFileName);
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
                data.getInnerJarPath(),
                data.getInnerJarFileName(),
                data.getImportTime()
        };
    }

    @Override
    public String[] chooseFileColumnDesc() {
        return new String[]{
                "jar文件类型，J: jar/war文件，D: 目录，JIJ: jar/war文件中的jar，R: 解析结果文件保存目录，FJ: 最终解析的jar文件",
                "jar文件序号",
                "外层jar文件完整路径",
                "jar/war文件中的jar文件路径"
        };
    }

    @Override
    public String[] chooseFileDetailInfo() {
        return new String[]{
                "解析的Jar/war文件或目录相关的信息"
        };
    }
}

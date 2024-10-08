package com.adrninistrator.jacg.unpacker.targz;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import net.lingala.zip4j.io.inputstream.ZipInputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.apache.commons.lang3.StringUtils;
import org.apache.tools.tar.TarEntry;
import org.apache.tools.tar.TarInputStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2024/1/18
 * @description:
 */
public abstract class BaseTarGzUnpacker {

    private static final Logger logger = LoggerFactory.getLogger(BaseTarGzUnpacker.class);

    // 需要解压的tar.gz文件路径
    protected final String tarGzFilePath;

    // 需要解压的tar.gz文件名
    protected String tarGzFileName;

    public BaseTarGzUnpacker(String tarGzFilePath) {
        this.tarGzFilePath = tarGzFilePath;
    }

    /**
     * 执行解压的方法
     *
     * @return true: 成功 false: 失败
     */
    public boolean unpack() {
        logger.info("开始处理文件 {}", tarGzFilePath);
        File tarGzFile = new File(tarGzFilePath);
        if (!tarGzFile.exists()) {
            logger.error("文件不存在 {}", tarGzFilePath);
            return false;
        }
        tarGzFileName = tarGzFile.getName();

        // 开始执行前的处理
        if (!beforeStart()) {
            return false;
        }

        try (TarInputStream tarInput = new TarInputStream(new GzipCompressorInputStream(new BufferedInputStream(new FileInputStream(tarGzFile))), 1024 * 32)) {
            TarEntry tarEntry;
            while ((tarEntry = tarInput.getNextEntry()) != null) {
                if (!tarEntry.isFile()) {
                    continue;
                }

                String tarEntryName = tarEntry.getName();
                if (checkJarFileType(tarEntryName)) {
                    // 解析.tar.gz中的.jar
                    // 以下创建的JarInputStream不能使用try with resource的方式，否则对应的TarInputStream tarInput ，后续无法再使用
                    handleJarFileInTarWar(new ZipInputStream(tarInput), null, tarEntryName);
                } else if (checkWarFileType(tarEntryName)) {
                    // 解析.tar.gz中的.war
                    handleWarFileInTar(new ZipInputStream(tarInput), tarEntry);
                } else {
                    // 解析.tar.gz中的其他文件
                    handleOtherFileInTar(tarInput, tarEntry);
                }
            }
            return true;
        } catch (Exception e) {
            logger.error("error {} ", tarGzFileName, e);
            return false;
        } finally {
            // 执行完成后的处理
            afterDone();
        }
    }

    /**
     * 开始执行前的处理
     *
     * @return
     */
    protected boolean beforeStart() {
        return true;
    }

    /**
     * 执行完成后的处理
     */
    protected void afterDone() {
    }

    /**
     * 处理.tar.gz文件、tar.gz中的.war文件，中的.jar文件
     *
     * @param jarInputInTarWar
     * @param warNameOfTarEntry
     * @param jarNameOfTarWarEntry
     */
    protected abstract boolean handleJarFileInTarWar(ZipInputStream jarInputInTarWar, String warNameOfTarEntry, String jarNameOfTarWarEntry);

    /**
     * 处理.tar.gz中的.war文件
     *
     * @param warInputInTar
     * @param tarEntry
     */
    protected abstract boolean handleWarFileInTar(ZipInputStream warInputInTar, TarEntry tarEntry);

    /**
     * 处理.tar.gz中的其他类型文件
     *
     * @param inputStream
     * @param tarEntry
     */
    protected abstract boolean handleOtherFileInTar(TarInputStream inputStream, TarEntry tarEntry);

    /**
     * 判断指定的文件类型是否为.class
     *
     * @param fileName
     * @return
     */
    protected boolean checkClassFileType(String fileName) {
        return StringUtils.endsWithIgnoreCase(fileName, JavaCG2Constants.EXT_CLASS);
    }

    /**
     * 判断指定的文件类型是否为.jar
     *
     * @param fileName
     * @return
     */
    protected boolean checkJarFileType(String fileName) {
        return StringUtils.endsWithIgnoreCase(fileName, JavaCG2Constants.EXT_JAR);
    }

    /**
     * 判断指定的文件类型是否为.war
     *
     * @param fileName
     * @return
     */
    protected boolean checkWarFileType(String fileName) {
        return StringUtils.endsWithIgnoreCase(fileName, JavaCG2Constants.EXT_WAR);
    }

    /**
     * 获取.class文件的包名前缀
     * 格式：（a/b/c/d/）
     *
     * @param fileName
     * @return
     */
    protected String getClassFilePackagePrefix(String fileName) {
        if (fileName.startsWith(JavaCG2Constants.WEB_INF_CLASSES)) {
            fileName = fileName.substring(JavaCG2Constants.WEB_INF_CLASSES.length());
        } else if (fileName.startsWith(JavaCG2Constants.BOOT_INF_CLASSES)) {
            fileName = fileName.substring(JavaCG2Constants.BOOT_INF_CLASSES.length());
        }
        String[] packageArray = StringUtils.split(fileName, "/");
        if (packageArray.length == 1) {
            return null;
        }
        List<String> packageList = new ArrayList<>();
        for (int i = 1; i < packageArray.length; i++) {
            if (i + 1 > packageArray.length) {
                // 假如包名使用/分隔后长度不够则结束循环
                break;
            }
            packageList.add(packageArray[i - 1]);
        }
        return StringUtils.join(packageList, "/") + "/";
    }

    // 将包名前缀记录到Set中
    protected void recordClassFilePackagePrefix(Set<String> packagePrefixSet, String entryName) {
        String packagePrefix = getClassFilePackagePrefix(entryName);
        if (packagePrefix == null) {
            return;
        }
        packagePrefixSet.add(packagePrefix);
    }
}

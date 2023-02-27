package com.adrninistrator.jacg.unzip;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;

import java.io.File;
import java.util.HashSet;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * @author adrninistrator
 * @date 2023/1/11
 * @description:
 */
public abstract class AbstractZipEntryHandler {
    protected final Set<String> handledFilePathSet = new HashSet<>();

    public abstract void handleZipEntry(ZipEntry ze, String fileName, ZipInputStream zis, String rootDirName);

    protected void writeFile(ZipEntry ze, ZipInputStream zis, String rootDirName, String destDirName, String fileName) {
        try {
            String destFilePath = rootDirName + "/" + destDirName + "/" + fileName;
            if (!handledFilePathSet.add(destFilePath)) {
                // 已处理过则返回
                return;
            }

            File destFile = new File(destFilePath);
            if (destFile.exists()) {
                System.out.println("文件已存在，不覆盖: " + destFilePath);
                return;
            }

            System.out.println("文件不存在，写入: " + destFilePath);
            byte[] fileContent = new byte[(int) ze.getSize()];
            IOUtils.read(zis, fileContent);
            FileUtils.writeByteArrayToFile(destFile, fileContent);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

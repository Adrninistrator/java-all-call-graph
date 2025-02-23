package com.adrninistrator.jacg.unzip;

import com.adrninistrator.jacg.common.enums.InputDirEnum;
import com.adrninistrator.jacg.util.JACGFileUtil;
import com.adrninistrator.jacg.util.JACGFileUtilNoLogger;
import com.adrninistrator.javacg2.common.enums.JavaCG2DirEnum;
import com.adrninistrator.javacg2.entry.JavaCG2Entry;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * @author adrninistrator
 * @date 2021/6/24
 * @description:
 */

public class UnzipFile {

    public static void main(String[] args) {
        UnzipFile unzipFile = new UnzipFile();
        unzipFile.unzipJACG();
        unzipFile.unzipJavaCG2();
    }

    private void unzipJACG() {
        String jarFilePath = JACGFileUtil.getJarFilePathOfClass(UnzipFile.class);
        System.out.println("java-all-call-graph jar包路径: " + jarFilePath);
        if (!new File(jarFilePath).exists()) {
            System.out.println("文件路径不正确: " + jarFilePath);
        }

        String rootDirName = chooseRootDirName();

        for (InputDirEnum inputDirEnum : InputDirEnum.values()) {
            if (!JACGFileUtilNoLogger.isDirectoryExists(rootDirName + "/" + UnzipFileConstants.DIR_RESOURCES + "/" + inputDirEnum.getDirName(), true)) {
                return;
            }
        }
        if (!JACGFileUtilNoLogger.isDirectoryExists(rootDirName + "/" + UnzipFileConstants.DIR_JAVA + "/" + UnzipFileConstants.DIR_TEST_JAVA_FILE, true)) {
            return;
        }

        handleZipFile(jarFilePath, rootDirName, new AbstractZipEntryHandler() {
            @Override
            public void handleZipEntry(ZipEntry ze, String fileName, ZipInputStream zis, String rootDirName) {
                boolean configDir = false;
                for (InputDirEnum inputDirEnum : InputDirEnum.values()) {
                    if (fileName.startsWith(inputDirEnum.getDirName())) {
                        configDir = true;
                        break;
                    }
                }

                if (configDir) {
                    writeFile(ze, zis, rootDirName, UnzipFileConstants.DIR_RESOURCES, fileName);
                } else if (fileName.startsWith(UnzipFileConstants.DIR_TEST_JAVA_FILE) && fileName.endsWith(UnzipFileConstants.FILE_JAVA)) {
                    writeFile(ze, zis, rootDirName, UnzipFileConstants.DIR_JAVA, fileName);
                }
            }
        });
    }

    private void unzipJavaCG2() {
        String jarFilePath = JACGFileUtil.getJarFilePathOfClass(JavaCG2Entry.class);
        System.out.println("java-callgraph2 jar包路径: " + jarFilePath);
        if (!new File(jarFilePath).exists()) {
            System.out.println("文件路径不正确: " + jarFilePath);
        }

        String rootDirName = chooseRootDirName();
        for (JavaCG2DirEnum javaCG2DirEnum : JavaCG2DirEnum.values()) {
            if (!javaCG2DirEnum.isInput()) {
                continue;
            }
            if (!JACGFileUtilNoLogger.isDirectoryExists(rootDirName + "/" + UnzipFileConstants.DIR_RESOURCES + "/" + javaCG2DirEnum.getDirName(), true)) {
                return;
            }
        }

        handleZipFile(jarFilePath, rootDirName, new AbstractZipEntryHandler() {
            @Override
            public void handleZipEntry(ZipEntry ze, String fileName, ZipInputStream zis, String rootDirName) {
                for (JavaCG2DirEnum javaCG2DirEnum : JavaCG2DirEnum.values()) {
                    if (!javaCG2DirEnum.isInput()) {
                        continue;
                    }
                    if (fileName.startsWith(javaCG2DirEnum.getDirName() + "/")) {
                        writeFile(ze, zis, rootDirName, UnzipFileConstants.DIR_RESOURCES, fileName);
                    }
                }
            }
        });
    }

    private void handleZipFile(String zipFilePath, String rootDirName, AbstractZipEntryHandler zipEntryHandler) {
        try (ZipInputStream zis = new ZipInputStream(new FileInputStream(zipFilePath))) {
            ZipEntry ze = zis.getNextEntry();
            while (ze != null) {
                if (!ze.isDirectory()) {
                    zipEntryHandler.handleZipEntry(ze, ze.getName(), zis, rootDirName);
                }
                // close this ZipEntry
                zis.closeEntry();
                ze = zis.getNextEntry();
            }
            // close last ZipEntry
            zis.closeEntry();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private String chooseRootDirName() {
        if (JACGFileUtilNoLogger.isDirectoryExists(UnzipFileConstants.DIR_TEST, false)) {
            return UnzipFileConstants.DIR_TEST;
        }
        if (JACGFileUtilNoLogger.isDirectoryExists(UnzipFileConstants.DIR_UNIT_TEST, false)) {
            return UnzipFileConstants.DIR_UNIT_TEST;
        }
        String rootDirName = UnzipFileConstants.DIR_DEFAULT_HEAD + System.currentTimeMillis();
        if (JACGFileUtilNoLogger.isDirectoryExists(rootDirName, true)) {
            return rootDirName;
        }
        return null;
    }
}

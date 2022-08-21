package com.adrninistrator.jacg.unzip;

import com.adrninistrator.jacg.common.enums.InputDirEnum;
import com.adrninistrator.jacg.util.FileUtilNoLogger;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * @author adrninistrator
 * @date 2021/6/24
 * @description:
 */

public class UnzipFile {

    public static final String DIR_TEST_JAVA_FILE = "test/jacg";
    public static final String DIR_TEST = "src/test";
    public static final String DIR_UNIT_TEST = "src/unit.test";
    public static final String DIR_DEFAULT_HEAD = "~jacg-";
    public static final String DIR_JAVA = "java";
    public static final String DIR_RESOURCES = "resources";
    public static final String FILE_JAVA = ".java";

    public static Set<String> handledFilePathSet = new HashSet<>();

    public static void main(String[] args) {
        String jarFilePath = UnzipFile.class.getProtectionDomain().getCodeSource().getLocation().getFile();
        System.out.println("当前jar包路径: " + jarFilePath);
        if (!new File(jarFilePath).exists()) {
            System.out.println("文件路径不正确: " + jarFilePath);
        }

        String rootDirName = chooseRootDirName();

        if (!FileUtilNoLogger.isDirectoryExists(rootDirName + "/" + DIR_RESOURCES + "/" + InputDirEnum.IDE_CONFIG.getDirName(), true) ||
                !FileUtilNoLogger.isDirectoryExists(rootDirName + "/" + DIR_RESOURCES + "/" + InputDirEnum.IDE_SQL.getDirName(), true) ||
                !FileUtilNoLogger.isDirectoryExists(rootDirName + "/" + DIR_RESOURCES + "/" + InputDirEnum.IDE_KEYWORD_CONF.getDirName(), true) ||
                !FileUtilNoLogger.isDirectoryExists(rootDirName + "/" + DIR_RESOURCES + "/" + InputDirEnum.IDE_EXTENSIONS.getDirName(), true) ||
                !FileUtilNoLogger.isDirectoryExists(rootDirName + "/" + DIR_JAVA + "/" + DIR_TEST_JAVA_FILE, true)) {
            return;
        }

        try (ZipInputStream zis = new ZipInputStream(new FileInputStream(jarFilePath))) {
            ZipEntry ze = zis.getNextEntry();
            while (ze != null) {
                if (!ze.isDirectory()) {
                    String fileName = ze.getName();
                    if (fileName.startsWith(InputDirEnum.IDE_CONFIG.getDirName()) ||
                            fileName.startsWith(InputDirEnum.IDE_SQL.getDirName()) ||
                            fileName.startsWith(InputDirEnum.IDE_KEYWORD_CONF.getDirName()) ||
                            fileName.startsWith(InputDirEnum.IDE_EXTENSIONS.getDirName())) {
                        writeFile(ze, zis, rootDirName, DIR_RESOURCES, fileName);
                    } else if (fileName.startsWith(DIR_TEST_JAVA_FILE) && fileName.endsWith(FILE_JAVA)) {
                        writeFile(ze, zis, rootDirName, DIR_JAVA, fileName);
                    }
                }
                //close this ZipEntry
                zis.closeEntry();
                ze = zis.getNextEntry();
            }
            //close last ZipEntry
            zis.closeEntry();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static String chooseRootDirName() {
        if (FileUtilNoLogger.isDirectoryExists(DIR_TEST, false)) {
            return DIR_TEST;
        }
        if (FileUtilNoLogger.isDirectoryExists(DIR_UNIT_TEST, false)) {
            return DIR_UNIT_TEST;
        }
        String rootDirName = DIR_DEFAULT_HEAD + System.currentTimeMillis();
        if (FileUtilNoLogger.isDirectoryExists(rootDirName, true)) {
            return rootDirName;
        }
        return null;
    }

    private static void writeFile(ZipEntry ze, ZipInputStream zis, String rootDirName, String destDirName, String fileName) throws IOException {
        String destFilePath = rootDirName + "/" + destDirName + "/" + fileName;

        if (handledFilePathSet.contains(destFilePath)) {
            return;
        }
        handledFilePathSet.add(destFilePath);

        File destFile = new File(destFilePath);
        if (destFile.exists()) {
            System.out.println("文件已存在，不覆盖: " + destFilePath);
            return;
        }

        System.out.println("文件不存在，写入: " + destFilePath);
        byte[] fileContent = new byte[(int) ze.getSize()];
        IOUtils.read(zis, fileContent);
        FileUtils.writeByteArrayToFile(destFile, fileContent);
    }

}

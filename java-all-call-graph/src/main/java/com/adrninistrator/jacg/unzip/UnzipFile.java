package com.adrninistrator.jacg.unzip;

import com.adrninistrator.jacg.common.Constants;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;

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

    public static final String DIR_TEST_JAVA_FILE = "test/jacg";
    public static final String DIR_TEST = "src/test";
    public static final String DIR_UNIT_TEST = "src/unit.test";
    public static final String DIR_DEFAULT_HEAD = "~jacg-";
    public static final String DIR_JAVA = "java";
    public static final String DIR_RESOURCES = "resources";
    public static final String FLAG_FSP = "/";

    public static void main(String[] args) {
        String jarFilePath = UnzipFile.class.getProtectionDomain().getCodeSource().getLocation().getFile();
        System.out.println("当前jar包路径: " + jarFilePath);
        if (!new File(jarFilePath).exists()) {
            System.out.println("文件路径不正确: " + jarFilePath);
        }

        String rootDirName = chooseRootDirName();

        if (!isDirectoryExists(rootDirName + FLAG_FSP + DIR_RESOURCES + FLAG_FSP + Constants.DIR_CONFIG, true) ||
                !isDirectoryExists(rootDirName + FLAG_FSP + DIR_JAVA + FLAG_FSP + DIR_TEST_JAVA_FILE, true)) {
            return;
        }

        try (ZipInputStream zis = new ZipInputStream(new FileInputStream(jarFilePath))) {
            ZipEntry ze = zis.getNextEntry();
            while (ze != null) {
                if (!ze.isDirectory()) {
                    String fileName = ze.getName();
                    if (fileName.startsWith(Constants.DIR_CONFIG)) {
                        writeFile(ze, zis, rootDirName, DIR_RESOURCES, fileName);
                    } else if (fileName.startsWith(DIR_TEST_JAVA_FILE)) {
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
        if (isDirectoryExists(DIR_TEST, false)) {
            return DIR_TEST;
        }
        if (isDirectoryExists(DIR_UNIT_TEST, false)) {
            return DIR_UNIT_TEST;
        }
        String rootDirName = DIR_DEFAULT_HEAD + System.currentTimeMillis();
        if (isDirectoryExists(rootDirName, true)) {
            return rootDirName;
        }
        return null;
    }

    private static void writeFile(ZipEntry ze, ZipInputStream zis, String rootDirName, String destDirName, String fileName) throws IOException {
        String destFilePath = rootDirName + FLAG_FSP + destDirName + FLAG_FSP + fileName;
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

    /**
     * 判断目录是否存在，不存在时尝试创建
     *
     * @param dirPath 需要判断的目录路径
     * @param tryMake 是否尝试创建目录
     * @return true：指定路径的目录存在（已存在或新创建），false：目录不存在（指定路径为文件，或创建失败）
     */
    public static boolean isDirectoryExists(String dirPath, boolean tryMake) {
        File file = new File(dirPath);
        if (file.exists()) {
            if (file.isDirectory()) {
                return true;
            }

            System.err.println("已存在文件: " + dirPath);
            return false;
        }

        if (!tryMake) {
            return false;
        }

        // 目录不存在，则尝试创建
        if (file.mkdirs()) {
            System.out.println("创建目录: " + dirPath);
            return true;
        }

        System.err.println("创建目录失败: " + dirPath);
        return false;
    }
}

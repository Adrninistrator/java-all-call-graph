package com.adrninistrator.jacg.util;

import com.adrninistrator.jacg.common.JACGConstants;
import net.lingala.zip4j.io.inputstream.ZipInputStream;
import net.lingala.zip4j.model.LocalFileHeader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/7/26
 * @description:
 */
public class JACGFindClassUtil {

    private static final Logger logger = LoggerFactory.getLogger(JACGFindClassUtil.class);

    /**
     * 获取指定类所在目录中的所有类名，不包含抽象类（Abstract开头）、内部类
     *
     * @param clazz
     * @return
     */
    public static List<String> getClassNameListFromDirOrJar(Class<?> clazz) {
        // 从指定类所在目录获取所有的文件路径
        List<String> filePathList = getFilePathFromDirOrJar(clazz);
        if (filePathList == null) {
            return null;
        }

        String packageName = JACGClassMethodUtil.getPackageNameFromFullClassName(clazz.getName());
        List<String> classNameList = new ArrayList<>(filePathList.size());
        for (String filePath : filePathList) {
            String fileName = JACGFileUtil.getFileNameFromPath(filePath);
            if (fileName.startsWith("Abstract") || fileName.contains("$")) {
                continue;
            }

            String filePathWithDot = JACGFileUtil.replaceFileSeparator2Dot(filePath);
            int index = filePathWithDot.indexOf(packageName);
            if (index == -1) {
                logger.error("文件类名路径 {} 中不包含指定包名 {}", filePathWithDot, packageName);
                return null;
            }
            String className = filePathWithDot.substring(index, filePathWithDot.length() - JACGConstants.EXT_CLASS.length());
            classNameList.add(className);
        }

        return classNameList;
    }

    /**
     * 从指定类所在目录获取所有的文件路径
     *
     * @param clazz
     * @return
     */
    public static List<String> getFilePathFromDirOrJar(Class<?> clazz) {
        String classPath = null;
        try {
            // 以下getResource()方法参数需要指定为""，以获取到对应类所在目录
            classPath = clazz.getResource("").getPath();
            if (!classPath.contains(".jar")) {
                /*
                    当前类不在jar包中，本地项目执行的情况
                    示例：
                    /D:/java-callgraph-dir/java-all-call-graph/out/production/classes/com/adrninistrator/jacg/util/
                 */
                List<String> filePathList = new ArrayList<>();
                JACGFileUtil.searchDir(classPath, null, filePathList, JACGConstants.EXT_CLASS);
                return filePathList;
            }

            /*
                当前类在jar包中，在其他项目中通过jar包引用的情况
                示例：
                file:/D:/java-callgraph2-2.0.4.jar!/com/adrninistrator/javacg/stat/
             */
            int index = classPath.indexOf(JACGConstants.FLAG_EXCLAMATION);
            if (index == -1) {
                logger.error("jar包路径 {} 中不包含 {}", classPath, JACGConstants.FLAG_EXCLAMATION);
                return null;
            }

            String jarFilePath = classPath.substring(0, index);
            if (jarFilePath.startsWith(JACGConstants.FLAG_FILE_PROTOCOL)) {
                jarFilePath = jarFilePath.substring(JACGConstants.FLAG_FILE_PROTOCOL.length());
            }

            String dirPath = classPath.substring(index + 1);
            if (dirPath.startsWith("/")) {
                dirPath = dirPath.substring(1, dirPath.length() - 1);
            }

            // 查找jar包中指定目录中指定文件后缀的文件路径
            return findFilePathInJarDir(jarFilePath, dirPath, JACGConstants.EXT_CLASS);
        } catch (Exception e) {
            logger.error("error {} ", classPath, e);
            return null;
        }
    }

    /**
     * 查找jar包中指定目录中指定文件后缀的文件路径
     *
     * @param jarFilePath
     * @param dirPath
     * @param fileExt
     * @return
     */
    public static List<String> findFilePathInJarDir(String jarFilePath, String dirPath, String fileExt) {
        try (ZipInputStream zis = new ZipInputStream(new FileInputStream(jarFilePath))) {
            List<String> filePathList = new ArrayList<>();
            LocalFileHeader fileHeader;
            while ((fileHeader = zis.getNextEntry()) != null) {
                if (!fileHeader.isDirectory()) {
                    String fileName = fileHeader.getFileName();
                    if (fileName.startsWith(dirPath) && fileName.endsWith(fileExt)) {
                        filePathList.add(fileName);
                    }
                }
            }
            return filePathList;
        } catch (IOException e) {
            logger.error("error ", e);
            return null;
        }
    }

    private JACGFindClassUtil() {
        throw new IllegalStateException("illegal");
    }
}

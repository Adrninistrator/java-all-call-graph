package com.adrninistrator.jacg.util;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.util.JavaCGClassMethodUtil;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
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
    public static List<String> getOrdinaryClassNameListFromDirOrJar(Class<?> clazz) {
        List<String> classNameList = getClassNameListFromDirOrJar(clazz);
        if (classNameList == null) {
            return null;
        }
        List<String> newClassNameList = new ArrayList<>();
        for (String className : classNameList) {
            String simpleClassName = JavaCGClassMethodUtil.getSimpleClassNameFromFull(className);
            if (simpleClassName.startsWith("Abstract") || simpleClassName.contains("$")) {
                continue;
            }
            newClassNameList.add(className);
        }
        return newClassNameList;
    }

    /**
     * 获取指定类所在目录中的所有类名
     *
     * @param clazz
     * @return
     */
    public static List<String> getClassNameListFromDirOrJar(Class<?> clazz) {
        String classDirPathRaw = null;
        String classDirPathTail = JavaCGClassMethodUtil.getPackageName(clazz.getName()).replace('.', '/');
        List<String> classNameList = new ArrayList<>();
        try {
            // 以下getResource()方法参数需要指定为""，以获取到对应类所在目录
            classDirPathRaw = clazz.getResource("").getPath();
            if (!classDirPathRaw.contains(".jar")) {
                /*
                    当前类不在jar包中，本地项目执行的情况
                    示例：
                    /D:/java-callgraph-dir/java-all-call-graph/out/production/classes/com/adrninistrator/jacg/util/
                 */
                File classDir = new File(classDirPathRaw);
                String classDirPath = classDir.getAbsolutePath().replace('\\', '/');
                String rootPath = StringUtils.substringBeforeLast(classDirPath, classDirPathTail);
                List<String> filePathList = new ArrayList<>();
                // 在对应目录查找class文件
                JACGFileUtil.searchDir(classDirPathRaw, null, filePathList, JACGConstants.EXT_CLASS);
                for (String filePath : filePathList) {
                    String filePathTail = StringUtils.substringAfter(filePath.replace('\\', '/'), rootPath);
                    classNameList.add(StringUtils.substringBeforeLast(filePathTail, JACGConstants.EXT_CLASS).replace('/', '.'));
                }
                return classNameList;
            }

            /*
                当前类在jar包中，在其他项目中通过jar包引用的情况
                示例：
                file:/D:/java-callgraph2-2.0.4.jar!/com/adrninistrator/javacg/stat/
             */
            int index = classDirPathRaw.indexOf(JACGConstants.FLAG_EXCLAMATION);
            if (index == -1) {
                logger.error("jar包路径 {} 中不包含 {}", classDirPathRaw, JACGConstants.FLAG_EXCLAMATION);
                return null;
            }

            String jarFilePath = classDirPathRaw.substring(0, index);
            if (jarFilePath.startsWith(JACGConstants.FLAG_FILE_PROTOCOL)) {
                jarFilePath = jarFilePath.substring(JACGConstants.FLAG_FILE_PROTOCOL.length());
            }

            String dirPath = classDirPathRaw.substring(index + 1);
            if (dirPath.startsWith("/")) {
                dirPath = dirPath.substring(1, dirPath.length() - 1);
            }

            // 查找jar包中指定目录中指定文件后缀的文件路径
            List<String> jarFilePathList = JACGFileUtil.findFilePathInJarDir(jarFilePath, dirPath, JACGConstants.EXT_CLASS);
            if (jarFilePathList == null) {
                return null;
            }
            for (String currentJarFilePath : jarFilePathList) {
                String currentClassName = StringUtils.substringBeforeLast(currentJarFilePath, JavaCGConstants.EXT_CLASS).replace('/', '.');
                classNameList.add(currentClassName);
            }
            return classNameList;
        } catch (Exception e) {
            logger.error("error {} ", classDirPathRaw, e);
            return null;
        }
    }

    private JACGFindClassUtil() {
        throw new IllegalStateException("illegal");
    }
}

package com.adrninistrator.jacg.util;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import com.adrninistrator.javacg.util.JavaCGFileUtil;
import com.adrninistrator.javacg.util.JavaCGUtil;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2021/6/18
 * @description:
 */

public class JACGFileUtil {

    private static final Logger logger = LoggerFactory.getLogger(JACGFileUtil.class);

    public static File findFile(String filePath) {
        // 尝试通过文件路径获取文件
        File file = new File(filePath);
        if (file.exists()) {
            logger.info("通过文件路径获取文件 {}", filePath);
            return file;
        }

        // 尝试从classpath中获取文件，路径以/开头
        URL url = JACGFileUtil.class.getResource("/" + filePath);
        if (url != null && "file".equals(url.getProtocol())) {
            /*
                当URL中的protocol为"file"时，说明对应的资源为独立文件的形式
                若为"jar"则说明对应的资源是jar包中的文件，不能通过以下方式处理
             */
            logger.info("从classpath中获取文件 {}", url);
            try {
                return new File(url.toURI());
            } catch (Exception e) {
                logger.error("error {} ", url, e);
                return null;
            }
        }

        return null;
    }

    public static InputStream getFileInputStream(String filePath) throws FileNotFoundException {
        File file = findFile(filePath);
        if (file != null) {
            return new FileInputStream(file);
        }

        /*
            尝试从jar包中读取，路径需要以/开头，从根目录读取，路径中的分隔符需要为/
            不能使用以下方式获取File对象
                new File(xxx.class.getResource("path").toURI())
            否则会出现异常
                java.lang.IllegalArgumentException: URI is not hierarchical
         */
        InputStream inputStream = JACGFileUtil.class.getResourceAsStream("/" + filePath);
        if (inputStream == null) {
            logger.error("未找到文件 {}", filePath);
            throw new JavaCGRuntimeException("未找到文件 " + filePath);
        }

        logger.info("从jar包中获取文件 {}", JACGFileUtil.class.getResource("/" + filePath));
        return inputStream;
    }

    public static String readFile2String(String filePath) {
        try (InputStream inputStream = getFileInputStream(filePath)) {
            return readInputStream2String(inputStream);
        } catch (Exception e) {
            logger.error("error {} ", filePath, e);
            return null;
        }
    }

    public static String readInputStream2String(InputStream inputStream) throws IOException {
        return IOUtils.toString(inputStream, StandardCharsets.UTF_8);
    }

    public static String readFile2String(File file) throws IOException {
        return FileUtils.readFileToString(file, StandardCharsets.UTF_8);
    }

    /**
     * 读取文件内容到Set中，忽略以#开关的行
     *
     * @param filePath 文件路径
     * @return
     */
    public static Set<String> readFile2Set(String filePath) {
        return readFile2Set(filePath, JavaCGConstants.FLAG_HASHTAG);
    }

    /**
     * 读取文件内容到Set中
     *
     * @param filePath     文件路径
     * @param ignorePrefix 每行需要忽略的前缀，可为null
     * @return
     */
    public static Set<String> readFile2Set(String filePath, String ignorePrefix) {
        try (InputStream inputStream = getFileInputStream(filePath)) {
            List<String> list = IOUtils.readLines(inputStream, StandardCharsets.UTF_8);
            Set<String> set = new HashSet<>(list.size());

            // 是否需要根据前缀忽略
            boolean checkIgnore = StringUtils.isNotBlank(ignorePrefix);
            for (String line : list) {
                if (StringUtils.isBlank(line) ||
                        (checkIgnore && line.startsWith(ignorePrefix))) {
                    continue;
                }

                set.add(line);
            }

            return set;
        } catch (Exception e) {
            logger.error("error {} ", filePath, e);
            return null;
        }
    }

    /**
     * 读取文件内容到List中，忽略以#开关的行
     *
     * @param filePath 文件路径
     * @return
     */
    public static List<String> readFile2List(String filePath) {
        return readFile2List(filePath, JavaCGConstants.FLAG_HASHTAG);
    }

    /**
     * 读取文件内容到List中
     *
     * @param filePath     文件路径
     * @param ignorePrefix 每行需要忽略的前缀，可为null
     * @return
     */
    public static List<String> readFile2List(String filePath, String ignorePrefix) {
        try (BufferedReader br = JavaCGFileUtil.genBufferedReader(getFileInputStream(filePath))) {
            List<String> list = new ArrayList<>();
            String line;
            boolean checkIgnore = StringUtils.isNotBlank(ignorePrefix);
            while ((line = br.readLine()) != null) {
                if (StringUtils.isNotBlank(line)) {
                    if (checkIgnore && line.startsWith(ignorePrefix)) {
                        continue;
                    }

                    list.add(line);
                }
            }
            return list;
        } catch (Exception e) {
            logger.error("处理文件出现异常 {}", filePath);
            return null;
        }
    }


    /**
     * 判断目录是否存在，不存在时尝试创建
     *
     * @param dirPath 需要判断的目录路径
     * @return true: 指定路径的目录存在（已存在或新创建），false: 目录不存在（指定路径为文件，或创建失败）
     */
    public static boolean isDirectoryExists(String dirPath) {
        return isDirectoryExists(new File(dirPath));
    }

    /**
     * 判断目录是否存在，不存在时尝试创建
     *
     * @param dirFile 需要判断的目录对象
     * @return true: 指定路径的目录存在（已存在或新创建），false: 目录不存在（指定路径为文件，或创建失败）
     */
    public static boolean isDirectoryExists(File dirFile) {
        if (dirFile.exists()) {
            if (dirFile.isDirectory()) {
                logger.debug("目录已存在: {}", dirFile.getAbsolutePath());
                return true;
            }

            logger.error("已存在同名文件: {}", dirFile.getAbsolutePath());
            return false;
        }

        try {
            Files.createDirectories(dirFile.toPath());
            logger.info("创建目录: {}", dirFile.getAbsolutePath());
            return true;
        } catch (FileAlreadyExistsException e) {
            logger.warn("尝试创建目录但已存在: {}", dirFile.getAbsolutePath());
            return true;
        } catch (IOException e) {
            logger.error("error {} ", dirFile.getAbsolutePath(), e);
            return false;
        }
    }

    /**
     * 查找指定目录中指定类型的文件，不遍历子目录
     *
     * @param dirPath
     * @param fileExt
     * @return
     */
    public static List<File> findFileInCurrentDir(String dirPath, String... fileExt) {
        File dir = new File(dirPath);
        if (!dir.exists() || !dir.isDirectory()) {
            logger.error("目录不存在，或不是目录 {}", dirPath);
            return Collections.emptyList();
        }

        File[] files = dir.listFiles();
        if (ArrayUtils.isEmpty(files)) {
            return new ArrayList<>(0);
        }

        List<File> fileList = new ArrayList<>(files.length);
        for (File file : files) {
            if (StringUtils.endsWithAny(file.getName(), fileExt)) {
                fileList.add(file);
            }
        }

        return fileList;
    }

    /**
     * 将指定的一个或多个文本类型的源文件合并为目录文件
     *
     * @param destFilePath 目标文件路径
     * @param srcFileList  源文件列表
     * @return
     */
    public static boolean combineTextFile(String destFilePath, List<File> srcFileList) {
        if (JavaCGUtil.isCollectionEmpty(srcFileList)) {
            logger.error("指定的源文件列表为空");
            return false;
        }

        try (BufferedWriter writer = JavaCGFileUtil.genBufferedWriter(destFilePath)) {
            for (File file : srcFileList) {
                // 拷贝指定文件的内容
                if (!copyFileContent(writer, file)) {
                    return false;
                }
            }
            return true;
        } catch (Exception e) {
            logger.error("error {} ", destFilePath, e);
            return false;
        }
    }

    // 拷贝指定文件的内容
    public static boolean copyFileContent(BufferedWriter writer, File file) {
        try (BufferedReader br = JavaCGFileUtil.genBufferedReader(file)) {
            String line;
            while ((line = br.readLine()) != null) {
                writer.write(line + JACGConstants.NEW_LINE);
            }
            writer.write(JACGConstants.NEW_LINE);
            return true;
        } catch (Exception e) {
            logger.error("error {} ", file.getAbsolutePath(), e);
            return false;
        }
    }

    public static boolean isFileExists(String filePath) {
        File file = new File(filePath);
        return file.exists() && file.isFile();
    }

    public static String getCanonicalPath(String filePath) {
        try {
            return new File(filePath).getCanonicalPath();
        } catch (IOException e) {
            logger.error("error {} ", filePath, e);
            return null;
        }
    }

    public static String getCanonicalPath(File file) {
        try {
            return file.getCanonicalPath();
        } catch (IOException e) {
            logger.error("error {} ", file.getAbsolutePath(), e);
            return null;
        }
    }

    public static String getFileMd5(String filePath) {
        try (InputStream input = new FileInputStream(filePath)) {
            byte[] md5 = DigestUtils.md5(input);
            return Base64.getEncoder().encodeToString(md5);
        } catch (Exception e) {
            logger.error("error {} ", filePath, e);
            return null;
        }
    }

    public static long getFileLastModified(String filePath) {
        try {
            return new File(filePath).lastModified();
        } catch (Exception e) {
            logger.error("error ", e);
            return 0L;
        }
    }

    /**
     * 生成文件
     *
     * @param filePath 文件路径
     * @return true: 文件已存在，或创建成功，false: 文件不存在且创建失败
     */
    public static boolean createNewFile(String filePath) {
        File file = new File(filePath);
        if (file.exists() && file.isFile()) {
            logger.info("文件已存在1 {}", filePath);
            return true;
        }

        try {
            // 判断文件所在目录是否存在，若不存在则创建
            File dir = file.getParentFile();
            if (!isDirectoryExists(dir)) {
                return false;
            }

            Files.createFile(file.toPath());
            logger.info("生成文件 {}", filePath);
            return true;
        } catch (FileAlreadyExistsException e) {
            logger.warn("文件已存在2 {}", filePath);
            return true;
        } catch (IOException e) {
            logger.error("error ", e);
            return false;
        }
    }

    /**
     * 从目录中查找需要处理的文件
     *
     * @param dirPath         需要查找的目录
     * @param subDirPathSet   保存查找到的目录，可为空
     * @param subFilePathList 保存查找到的文件列表
     * @param fileExts        需要查找的文件后缀，可为空
     */
    public static void searchDir(String dirPath, Set<String> subDirPathSet, List<String> subFilePathList, String... fileExts) {
        File dir = new File(dirPath);
        File[] files = dir.listFiles();
        if (files == null) {
            return;
        }

        for (File file : files) {
            if (file.isDirectory()) {
                // 目录，递归
                searchDir(file.getAbsolutePath(), subDirPathSet, subFilePathList, fileExts);
            } else {
                // 文件
                String filePath = file.getAbsolutePath();
                if (fileExts == null || checkFileExt(filePath, fileExts)) {
                    // 若未指定文件后缀，则允许任意文件后缀；若有指定文件后缀，则需要判断是否符合预期
                    if (subDirPathSet != null) {
                        subDirPathSet.add(dirPath);
                    }
                    subFilePathList.add(filePath);
                }
            }
        }
    }

    /**
     * 判断文件后缀是否符合预期
     *
     * @param filePath
     * @param fileExts
     * @return
     */
    public static boolean checkFileExt(String filePath, String... fileExts) {
        if (fileExts == null) {
            // 未指定文件后缀时认为不符合
            return false;
        }

        for (String fileExt : fileExts) {
            if (StringUtils.endsWithIgnoreCase(filePath, fileExt)) {
                return true;
            }
        }
        return false;
    }

    /**
     * 根据文件路径获取文件名
     *
     * @param filePath
     * @return
     */
    public static String getFileNameFromPath(String filePath) {
        if (filePath == null) {
            return null;
        }

        String tmpFilePath = replaceFilePathSeparator(filePath);
        int lastSeparatorIndex = tmpFilePath.lastIndexOf("/");

        if (lastSeparatorIndex == -1) {
            return filePath;
        }

        return tmpFilePath.substring(lastSeparatorIndex + 1);
    }

    /**
     * 获取不包含后缀的文件名
     *
     * @param fileName 文件名
     * @return
     */
    public static String getFileNameWithOutExt(String fileName) {
        return getFileNameWithOutExt(fileName, JavaCGConstants.FLAG_DOT);
    }

    /**
     * 获取不包含后缀的文件名
     *
     * @param fileName    文件名
     * @param fileExtFlag 文件后缀标记，默认使用.
     * @return
     */
    public static String getFileNameWithOutExt(String fileName, String fileExtFlag) {
        if (fileName == null || fileExtFlag == null) {
            return null;
        }

        int lastDotIndex = fileName.lastIndexOf(fileExtFlag);
        if (lastDotIndex == -1) {
            return fileName;
        }

        return fileName.substring(0, lastDotIndex);
    }

    /**
     * 将文件路径中的\替换为/
     *
     * @param filePath
     * @return
     */
    public static String replaceFilePathSeparator(String filePath) {
        return StringUtils.replace(filePath, "\\", "/");
    }

    /**
     * 判断文件路径中是否包含目录分隔符\、/
     *
     * @param filePath
     * @return
     */
    public static boolean checkFilePathContainsSeparator(String filePath) {
        return StringUtils.containsAny(filePath, "\\", "/");
    }

    /**
     * 获取文件所在目录名
     *
     * @param filePath
     * @return
     */
    public static String getFileParentDirName(String filePath) {
        if (filePath == null) {
            return null;
        }

        String tmpFilePath = replaceFilePathSeparator(filePath);
        String[] array = StringUtils.splitPreserveAllTokens(tmpFilePath, "/");
        if (array.length < 2) {
            return null;
        }

        // 使用分隔符进行分隔后，返回倒数第二个数据
        return array[array.length - 2];
    }

    /**
     * 重命名文件
     *
     * @param oldFilePath
     * @param newFilePath
     * @return
     */
    public static boolean renameFile(String oldFilePath, String newFilePath) {
        File oldFile = new File(oldFilePath);
        if (!oldFile.exists() || !oldFile.isFile()) {
            logger.error("旧文件不存在或不是文件 {}", oldFilePath);
            return false;
        }

        File newFile = new File(newFilePath);
        if (newFile.exists()) {
            logger.error("新文件已存在 {}", newFilePath);
            return false;
        }

        // 判断新文件所在目录是否存在，若不存在则创建
        File newDir = newFile.getParentFile();
        if (!isDirectoryExists(newDir)) {
            return false;
        }

        if (!oldFile.renameTo(newFile)) {
            logger.error("重命名文件失败 {} {}", oldFilePath, newFilePath);
            return false;
        }

        logger.info("重命名文件 {} {}", oldFilePath, newFilePath);
        return true;
    }

    /**
     * 拷贝文件
     *
     * @param srcFile
     * @param destFile
     * @return
     */
    public static boolean copyFile(File srcFile, File destFile) {
        try {
            Files.copy(srcFile.toPath(), destFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    private JACGFileUtil() {
        throw new IllegalStateException("illegal");
    }
}

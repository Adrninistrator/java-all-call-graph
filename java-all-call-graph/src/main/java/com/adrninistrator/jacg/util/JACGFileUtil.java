package com.adrninistrator.jacg.util;

import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import net.lingala.zip4j.io.inputstream.ZipInputStream;
import net.lingala.zip4j.model.LocalFileHeader;
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
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author adrninistrator
 * @date 2021/6/18
 * @description:
 */

public class JACGFileUtil {

    private static final Logger logger = LoggerFactory.getLogger(JACGFileUtil.class);

    private static final Pattern JAR_VERSION_PATTERN = Pattern.compile("-[0-9]");

    public static String readFile2String(String filePath) {
        try (InputStream inputStream = JavaCG2FileUtil.getFileInputStream(filePath)) {
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
     * 查找指定目录下指定类型的文件，不遍历子目录
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
            return Collections.emptyList();
        }

        List<File> fileList = new ArrayList<>(files.length);
        for (File file : files) {
            if (file.isFile() && StringUtils.endsWithAny(file.getName(), fileExt)) {
                fileList.add(file);
            }
        }
        return fileList;
    }

    /**
     * 查找指定目录下指定类型的文件，不遍历子目录
     *
     * @param dirPath
     * @param fileExt
     * @return
     */
    public static List<String> findFilePathInCurrentDir(String dirPath, String... fileExt) {
        List<File> fileList = findFileInCurrentDir(dirPath, fileExt);
        List<String> filePathList = new ArrayList<>(fileList.size());
        for (File file : fileList) {
            filePathList.add(file.getAbsolutePath());
        }
        return filePathList;
    }

    /**
     * 查找指定目录下的目录，不遍历子目录
     *
     * @param dirPath
     * @return
     */
    public static List<File> findDirInCurrentDir(String dirPath) {
        File dir = new File(dirPath);
        if (!dir.exists() || !dir.isDirectory()) {
            logger.warn("目录不存在，或不是目录 {}", dirPath);
            return Collections.emptyList();
        }

        File[] files = dir.listFiles();
        if (ArrayUtils.isEmpty(files)) {
            return Collections.emptyList();
        }

        List<File> fileList = new ArrayList<>(files.length);
        for (File file : files) {
            if (file.isDirectory()) {
                fileList.add(file);
            }
        }
        return fileList;
    }

    /**
     * 查找指定目录下的目录，不遍历子目录
     *
     * @param dirPath
     * @return
     */
    public static List<String> findDirPathInCurrentDir(String dirPath) {
        List<File> fileList = findDirInCurrentDir(dirPath);
        List<String> filePathList = new ArrayList<>(fileList.size());
        for (File file : fileList) {
            filePathList.add(file.getAbsolutePath());
        }
        return filePathList;
    }

    /**
     * 将指定的一个或多个文本类型的源文件合并为目录文件
     *
     * @param destFilePath 目标文件路径
     * @param srcFileList  源文件列表
     * @param append       是否追加文件内容
     * @return
     */
    public static boolean combineTextFile(String destFilePath, List<File> srcFileList, boolean append) {
        if (JavaCG2Util.isCollectionEmpty(srcFileList)) {
            logger.error("指定的源文件列表为空");
            return false;
        }

        try (BufferedWriter writer = JavaCG2FileUtil.genBufferedWriter(destFilePath, append)) {
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
        try (BufferedReader br = JavaCG2FileUtil.genBufferedReader(file)) {
            String line;
            while ((line = br.readLine()) != null) {
                writer.write(line + JavaCG2Constants.NEW_LINE);
            }
            writer.write(JavaCG2Constants.NEW_LINE);
            return true;
        } catch (Exception e) {
            logger.error("error {} ", file.getAbsolutePath(), e);
            return false;
        }
    }

    /**
     * 获取文件MD5
     *
     * @param filePath
     * @return
     */
    public static String getFileMd5(String filePath) {
        try (InputStream input = new FileInputStream(filePath)) {
            return DigestUtils.md5Hex(input);
        } catch (Exception e) {
            logger.error("error {} ", filePath, e);
            return null;
        }
    }

    /**
     * 获得文件修改时间的字符串形式，精度到秒
     *
     * @param filePath
     * @return
     */
    public static String getFileLastModifiedTime(String filePath) {
        try {
            long lastModified = new File(filePath).lastModified();
            Date date = new Date(lastModified);
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            return sdf.format(date);
        } catch (Exception e) {
            logger.error("error ", e);
            return "";
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
            if (!JavaCG2FileUtil.isDirectoryExists(dir)) {
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
     * 获取不包含后缀的文件名
     *
     * @param fileName 文件名
     * @return
     */
    public static String getFileNameWithOutExt(String fileName) {
        return getFileNameWithOutExt(fileName, JavaCG2Constants.FLAG_DOT);
    }

    /**
     * 获取不包含后缀的文件名
     *
     * @param fileName    文件名
     * @param fileExtFlag 文件后缀标志，默认使用.
     * @return
     */
    public static String getFileNameWithOutExt(String fileName, String fileExtFlag) {
        if (fileName == null || fileExtFlag == null) {
            return null;
        }

        // 不使用StringUtils.substringBeforeLast，因为当源字符串没有标记字符串时结果为空
        int lastDotIndex = fileName.lastIndexOf(fileExtFlag);
        if (lastDotIndex == -1) {
            return fileName;
        }
        return fileName.substring(0, lastDotIndex);
    }

    /**
     * 获取jar文件名不包含后缀及版本号的内容
     *
     * @param jarFileName
     * @return
     */
    public static String getJarFileHead(String jarFileName) {
        if (jarFileName == null) {
            return null;
        }
        String fileNameWithOutExt = getFileNameWithOutExt(jarFileName);

        Matcher matcher = JAR_VERSION_PATTERN.matcher(fileNameWithOutExt);
        if (!matcher.find()) {
            // 未找到版本号
            return fileNameWithOutExt;
        }
        return fileNameWithOutExt.substring(0, matcher.start());
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

        String tmpFilePath = JavaCG2FileUtil.replaceFilePath2Slash(filePath);
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
        if (!JavaCG2FileUtil.isDirectoryExists(newDir)) {
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

    /**
     * 根据jar包中的文件路径获取文件名
     *
     * @param filePath
     * @return
     */
    public static String getFileNameFromPathInJar(String filePath) {
        return StringUtils.substringAfterLast(filePath, "/");
    }

    /**
     * 将文件路径中的分隔符替换为.
     *
     * @param filePath
     * @return
     */
    public static String replaceFileSeparator2Dot(String filePath) {
        String tmp = StringUtils.replace(filePath, "\\", ".");
        return StringUtils.replace(tmp, "/", ".");
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

    private JACGFileUtil() {
        throw new IllegalStateException("illegal");
    }
}

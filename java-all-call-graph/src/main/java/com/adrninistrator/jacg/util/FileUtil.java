package com.adrninistrator.jacg.util;

import com.adrninistrator.jacg.common.JACGConstants;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2021/6/18
 * @description:
 */

public class FileUtil {

    private static final Logger logger = LoggerFactory.getLogger(FileUtil.class);

    private static ClassLoader classLoader = FileUtil.class.getClassLoader();

    private static String classpath = FileUtil.class.getResource("/").getPath();

    public static File findFile(String filePath) throws URISyntaxException {
        File file = new File(filePath);
        if (file.exists()) {
            return file;
        }

        URL url = classLoader.getResource(filePath);
        if (url != null) {
            return new File(url.toURI());
        }
        return new File(classpath + filePath);
    }

    public static String readFile2String(String filePath) {
        try {
            File file = findFile(filePath);
            return readFile2String(file);
        } catch (Exception e) {
            logger.error("error ", e);
            return null;
        }
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
        return readFile2Set(filePath, JACGConstants.FLAG_HASHTAG);
    }

    /**
     * 读取文件内容到Set中
     *
     * @param filePath     文件路径
     * @param ignorePrefix 每行需要忽略的前缀，可为null
     * @return
     */
    public static Set<String> readFile2Set(String filePath, String ignorePrefix) {
        try {
            File file = findFile(filePath);
            List<String> list = FileUtils.readLines(file, StandardCharsets.UTF_8);

            Set<String> set = new HashSet<>(list.size());

            boolean checkIgnore = StringUtils.isNotBlank(ignorePrefix);

            for (String line : list) {
                String lineTrim = line.trim();
                if (StringUtils.isNotBlank(lineTrim)) {
                    if (checkIgnore && lineTrim.startsWith(ignorePrefix)) {
                        continue;
                    }

                    set.add(lineTrim);
                }
            }

            return set;
        } catch (Exception e) {
            logger.error("error ", e);
            return null;
        }
    }

    /**
     * 读取文件内容到List中
     *
     * @param filePath 文件路径
     * @return
     */
    public static List<String> readFile2List(String filePath) {
        try {
            File file = findFile(filePath);
            return FileUtils.readLines(file, StandardCharsets.UTF_8);
        } catch (Exception e) {
            logger.error("error ", e);
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
        File file = new File(dirPath);
        if (file.exists()) {
            if (file.isDirectory()) {
                logger.info("directory exists: {}", dirPath);
                return true;
            }

            logger.error("file exists: {}", dirPath);
            return false;
        }

        // 目录不存在，则尝试创建
        if (file.mkdirs()) {
            logger.info("mkdirs: {}", dirPath);
            return true;
        }

        logger.error("mkdirs fail: {}", dirPath);
        return false;
    }

    /**
     * 查找指定目录中指定类型的文件，不遍历子目录
     *
     * @param dirPath
     * @param fileExt
     * @return
     */
    public static List<File> findFileInDir(String dirPath, String fileExt) {
        File dir = new File(dirPath);
        if (!dir.exists() || !dir.isDirectory()) {
            logger.error("目录不存在，或不是目录 {}", dirPath);
            return null;
        }

        File[] files = dir.listFiles();
        if (files == null || files.length == 0) {
            return new ArrayList<>(0);
        }

        List<File> fileList = new ArrayList<>(files.length);
        for (File file : files) {
            if (file.getName().endsWith(fileExt)) {
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
        if (CommonUtil.isCollectionEmpty(srcFileList)) {
            logger.error("指定的源文件列表为空");
            return false;
        }

        try (BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(destFilePath), StandardCharsets.UTF_8))) {
            for (File file : srcFileList) {
                String fileContent = readFile2String(file);

                out.write(fileContent);
                out.write(JACGConstants.NEW_LINE);
            }
            return true;
        } catch (Exception e) {
            logger.error("error ", e);
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
            logger.error("error ", e);
            return null;
        }
    }

    public static String getFileMd5(String filePath) {
        try (InputStream input = new FileInputStream(filePath)) {
            byte[] md5 = DigestUtils.md5(input);
            return Base64.encodeBase64String(md5);
        } catch (Exception e) {
            logger.error("error ", e);
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

    private FileUtil() {
        throw new IllegalStateException("illegal");
    }
}

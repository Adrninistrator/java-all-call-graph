package com.adrninistrator.jacg.util;

import com.adrninistrator.jacg.common.JACGConstants;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
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

    public static File findFile(String filePath) {
        // 尝试通过文件路径获取文件
        File file = new File(filePath);
        if (file.exists()) {
            logger.info("通过文件路径获取文件 {}", filePath);
            return file;
        }

        // 尝试从classpath中获取文件，路径以/开头
        URL url = FileUtil.class.getResource("/" + filePath);
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
                new File(xxx.class.getResource("path“).toURI())
            否则会出现异常
                java.lang.IllegalArgumentException: URI is not hierarchical
         */
        InputStream inputStream = FileUtil.class.getResourceAsStream("/" + filePath);
        if (inputStream == null) {
            logger.error("未找到文件 {}", filePath);
            throw new RuntimeException("未找到文件 " + filePath);
        }

        logger.info("从jar包中获取文件 {}", FileUtil.class.getResource("/" + filePath));
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
        try (InputStream inputStream = getFileInputStream(filePath)) {
            List<String> list = IOUtils.readLines(inputStream, StandardCharsets.UTF_8);

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
            logger.error("error {} ", filePath, e);
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
        try (InputStream inputStream = getFileInputStream(filePath)) {
            return IOUtils.readLines(inputStream, StandardCharsets.UTF_8);
        } catch (Exception e) {
            logger.error("error {} ", filePath, e);
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

        try {
            Files.createDirectories(file.toPath());
            logger.info("create directory: {}", dirPath);
            return true;
        } catch (FileAlreadyExistsException e) {
            logger.warn("try to create directory but exists: {}", dirPath);
            return true;
        } catch (IOException e) {
            logger.error("error {} ", dirPath, e);
            return true;
        }
    }

    /**
     * 查找指定目录中指定类型的文件，不遍历子目录
     *
     * @param dirPath
     * @param fileExt
     * @return
     */
    public static List<File> findFileInCurrentDir(String dirPath, String fileExt) {
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
     * 查找指定目录中指定类型的文件，遍历子目录
     *
     * @param dirPath
     * @param fileExt
     * @param resultFileList
     */
    public static void findFileInSubDir(String dirPath, String fileExt, List<File> resultFileList) {
        File dir = new File(dirPath);
        File[] files = dir.listFiles();
        if (files == null) {
            return;
        }

        for (File file : files) {
            if (file.isDirectory()) {
                findFileInSubDir(file.getPath(), fileExt, resultFileList);
            } else if (file.getName().endsWith(fileExt)) {
                resultFileList.add(file);
            }
        }
    }

    /**
     * 将指定的一个或多个文本类型的源文件合并为目录文件
     *
     * @param destFilePath 目标文件路径
     * @param srcFileList  源文件列表
     * @return
     */
    public static boolean combineTextFile(String destFilePath, List<File> srcFileList) {
        if (JACGUtil.isCollectionEmpty(srcFileList)) {
            logger.error("指定的源文件列表为空");
            return false;
        }

        try (BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(destFilePath), StandardCharsets.UTF_8))) {
            for (File file : srcFileList) {
                String fileContent = readFile2String(file);
                if (StringUtils.isNotBlank(fileContent)) {
                    // 仅当文件内容非空时才进行合并
                    out.write(fileContent + JACGConstants.NEW_LINE);
                }
            }
            return true;
        } catch (Exception e) {
            logger.error("error {} ", destFilePath, e);
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
            return Base64.encodeBase64String(md5);
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

    private FileUtil() {
        throw new IllegalStateException("illegal");
    }
}

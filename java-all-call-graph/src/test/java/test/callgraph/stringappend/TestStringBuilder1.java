package test.callgraph.stringappend;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

/**
 * @author adrninistrator
 * @date 2024/4/23
 * @description:
 */
public class TestStringBuilder1 {
    private static final Logger logger = LoggerFactory.getLogger(TestStringBuilder1.class);

    public void test1a() throws IOException {
        String filePath = new StringBuilder("file-").append(System.currentTimeMillis()).append(".txt").toString();
        new File(filePath).createNewFile();
    }

    public void test1b() throws IOException {
        String filePath = new StringBuilder("file-").append(System.currentTimeMillis()).append(System.currentTimeMillis()).append(".txt").toString();
        new File(filePath).createNewFile();
    }

    public void test2a() throws IOException {
        String filePath = new StringBuilder(String.valueOf(System.currentTimeMillis())).append(".txt").toString();
        try (FileOutputStream fos = new FileOutputStream(filePath)) {
            fos.write("abc".getBytes(StandardCharsets.UTF_8));
        }
    }

    public void test2b() throws IOException {
        String filePath = new StringBuilder().append(System.currentTimeMillis()).append(".txt").toString();
        try (FileOutputStream fos = new FileOutputStream(filePath)) {
            fos.write("abc".getBytes(StandardCharsets.UTF_8));
        }
    }

    public void test3a() throws IOException {
        try (FileOutputStream fos = new FileOutputStream(new StringBuilder(String.valueOf(System.currentTimeMillis())).append(".txt").toString())) {
            fos.write("abc".getBytes(StandardCharsets.UTF_8));
        }
    }

    public void test3b() throws IOException {
        try (FileOutputStream fos = new FileOutputStream(new StringBuilder(System.getProperty("")).append(".txt").toString())) {
            fos.write("abc".getBytes(StandardCharsets.UTF_8));
        }
    }

    public String test4() {
        return new StringBuilder("file-").append(System.currentTimeMillis()).append(".txt").toString();
    }

    public void test5() throws IOException {
        String filePath = new StringBuilder("file-").append(System.currentTimeMillis()).append(".txt").toString();
        System.out.println(filePath);
        logger.info("filePath {}", filePath);
    }

    public void test6a() {
        test6b(System.currentTimeMillis());
        test6c(new StringBuilder(String.valueOf(System.currentTimeMillis())));
        test6c(new StringBuilder().append(System.currentTimeMillis()));
    }

    public void test6b(long data) {
        logger.info("{}", data);
    }

    public void test6c(StringBuilder data) {
        logger.info("{}", data);
    }

    public void test7a() {
        StringBuilder stringBuilder = new StringBuilder();
        logger.info("{}", stringBuilder);
        stringBuilder.append("1");
        logger.info("{}", stringBuilder);
        stringBuilder.append(System.currentTimeMillis());
        logger.info("{}", stringBuilder);
        stringBuilder.append("2");
        logger.info("{}", stringBuilder);
    }

    public void test7b() {
        StringBuilder stringBuilder = new StringBuilder(String.valueOf(System.currentTimeMillis()));
        logger.info("{}", stringBuilder);
        stringBuilder.append("1");
        logger.info("{}", stringBuilder);
        stringBuilder.append(System.currentTimeMillis());
        logger.info("{}", stringBuilder);
        stringBuilder.append("2");
        logger.info("{}", stringBuilder);
    }

    public void test8() {
        StringBuilder stringBuilder = new StringBuilder();
        int a = (int) System.currentTimeMillis() % 7;
        if (a == 1) {
            stringBuilder.append(System.currentTimeMillis());
            logger.info("{}", stringBuilder);
        } else if (a == 2) {
            stringBuilder.append(System.getProperty(""));
            logger.info("{}", stringBuilder);
        }
    }
}

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
public class TestStringBuffer1 {
    private static final Logger logger = LoggerFactory.getLogger(TestStringBuffer1.class);

    public void test1a() throws IOException {
        String filePath = new StringBuffer("file-").append(System.currentTimeMillis()).append(".txt").toString();
        new File(filePath).createNewFile();
    }

    public void test1b() throws IOException {
        String filePath = new StringBuffer("file-").append(System.currentTimeMillis()).append(System.currentTimeMillis()).append(".txt").toString();
        new File(filePath).createNewFile();
    }

    public void test2a() throws IOException {
        String filePath = new StringBuffer(String.valueOf(System.currentTimeMillis())).append(".txt").toString();
        try (FileOutputStream fos = new FileOutputStream(filePath)) {
            fos.write("abc".getBytes(StandardCharsets.UTF_8));
        }
    }

    public void test2b() throws IOException {
        String filePath = new StringBuffer().append(System.currentTimeMillis()).append(".txt").toString();
        try (FileOutputStream fos = new FileOutputStream(filePath)) {
            fos.write("abc".getBytes(StandardCharsets.UTF_8));
        }
    }

    public void test3a() throws IOException {
        try (FileOutputStream fos = new FileOutputStream(new StringBuffer(String.valueOf(System.currentTimeMillis())).append(".txt").toString())) {
            fos.write("abc".getBytes(StandardCharsets.UTF_8));
        }
    }

    public void test3b() throws IOException {
        try (FileOutputStream fos = new FileOutputStream(new StringBuffer(System.getProperty("")).append(".txt").toString())) {
            fos.write("abc".getBytes(StandardCharsets.UTF_8));
        }
    }

    public String test4() {
        return new StringBuffer("file-").append(System.currentTimeMillis()).append(".txt").toString();
    }

    public void test5() throws IOException {
        String filePath = new StringBuffer("file-").append(System.currentTimeMillis()).append(".txt").toString();
        System.out.println(filePath);
        logger.info("filePath {}", filePath);
    }

    public void test6a() {
        test6b(System.currentTimeMillis());
        test6c(new StringBuffer(String.valueOf(System.currentTimeMillis())));
        test6c(new StringBuffer().append(System.currentTimeMillis()));
    }

    public void test6b(long data) {
        logger.info("{}", data);
    }

    public void test6c(StringBuffer data) {
        logger.info("{}", data);
    }

    public void test7a() {
        StringBuffer stringBuffer = new StringBuffer();
        logger.info("{}", stringBuffer);
        stringBuffer.append("1");
        logger.info("{}", stringBuffer);
        stringBuffer.append(System.currentTimeMillis());
        logger.info("{}", stringBuffer);
        stringBuffer.append("2");
        logger.info("{}", stringBuffer);
    }

    public void test7b() {
        StringBuffer stringBuffer = new StringBuffer(String.valueOf(System.currentTimeMillis()));
        logger.info("{}", stringBuffer);
        stringBuffer.append("1");
        logger.info("{}", stringBuffer);
        stringBuffer.append(System.currentTimeMillis());
        logger.info("{}", stringBuffer);
        stringBuffer.append("2");
        logger.info("{}", stringBuffer);
    }

    public void test8() {
        StringBuffer stringBuffer = new StringBuffer();
        int a = (int) System.currentTimeMillis() % 7;
        if (a == 1) {
            stringBuffer.append(System.currentTimeMillis());
            logger.info("{}", stringBuffer);
        } else if (a == 2) {
            stringBuffer.append(System.getProperty(""));
            logger.info("{}", stringBuffer);
        }
    }
}

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
public class TestStringBuilder2 {
    private static final Logger logger = LoggerFactory.getLogger(TestStringBuilder2.class);

    public void test1() throws IOException {
        String filePath = "file-" + System.currentTimeMillis() + ".txt";
        new File(filePath).createNewFile();
    }

    public void test2() throws IOException {
        String filePath = System.currentTimeMillis() + ".txt";
        try (FileOutputStream fos = new FileOutputStream(filePath)) {
            fos.write("abc".getBytes(StandardCharsets.UTF_8));
        }
    }

    public void test3() throws IOException {
        try (FileOutputStream fos = new FileOutputStream(System.currentTimeMillis() + ".txt")) {
            fos.write("abc".getBytes(StandardCharsets.UTF_8));
        }
    }

    public String test4() {
        return "file-" + System.currentTimeMillis() + ".txt";
    }

    public void test5() throws IOException {
        String filePath = "file-" + System.currentTimeMillis() + ".txt";
        System.out.println(filePath);
        logger.info("filePath {}", filePath);
    }

    public void test6() throws IOException {
        String filePath = "file-";
        filePath = filePath + System.currentTimeMillis() + ".txt";
        new File(filePath).createNewFile();
    }

    public void test7() throws IOException {
        String filePath = "file-";
        filePath += System.currentTimeMillis() + ".txt";
        new File(filePath).createNewFile();
    }
}

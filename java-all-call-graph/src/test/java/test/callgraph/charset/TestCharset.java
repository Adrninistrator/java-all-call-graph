package test.callgraph.charset;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

/**
 * @author adrninistrator
 * @date 2023/6/28
 * @description:
 */
public class TestCharset {
    private static final Logger logger = LoggerFactory.getLogger(TestCharset.class);

    private final String stringField1 = "abc";
    private String stringField2 = "abc2";
    private static final String STATIC_STRING_FIELD1 = "abc";
    private String a = "1" + '/';
    private StringBuilder stringBuilder = new StringBuilder("a").append("b");
    private int length = stringBuilder.length();

    public void test() throws UnsupportedEncodingException {
        byte[] bytesA1 = "abc1".getBytes("utf-8");
        byte[] bytesA2 = "abc2".getBytes(Charset.forName("utf-8"));
        byte[] bytesA3 = "abc3".getBytes(StandardCharsets.UTF_8);

        byte[] bytesF1 = StringUtils.substring("abc1", 0).getBytes("utf-8");
        byte[] bytesF2 = StringUtils.substring("abc2", 1).getBytes(Charset.forName("utf-8"));
        byte[] bytesF3 = StringUtils.substring("abc3", 2).getBytes(StandardCharsets.UTF_8);

        try (BufferedReader reader = new BufferedReader(new java.io.InputStreamReader(new FileInputStream(""), "ISO-8859-1"), 1024)) {
            reader.readLine();
        } catch (Exception e) {
            logger.error("error ", e);
        }
        try (BufferedReader reader = new BufferedReader(new java.io.InputStreamReader(new FileInputStream(""), Charset.forName("ISO-8859-1")), 1024)) {
            reader.readLine();
        } catch (Exception e) {
            logger.error("error ", e);
        }
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(""), StandardCharsets.ISO_8859_1))) {
            reader.readLine();
        } catch (Exception e) {
            logger.error("error ", e);
        }

        String strB1 = new String("abc1".getBytes(), "gbk");
        String strB2 = new String("abc2".getBytes(), Charset.forName("gb2312"));
    }

    public void test1a(String str) {
        int i = 0;
        i = StringUtils.indexOf(stringField1, "a");
        i = StringUtils.indexOf(stringField2, "a2");
        i = StringUtils.indexOf(STATIC_STRING_FIELD1, "b");
        i = StringUtils.indexOf(returnString1(), "c");
        i = StringUtils.indexOf(TestCharset.returnString2(), "d");
        i = StringUtils.indexOf(str, str);

        String str1 = StringUtils.substring("abc", 1);
    }

    public void test1b(String str) {
        int i = 0;
        i = StringUtils.lastIndexOf(stringField1, "a");
        i = StringUtils.lastIndexOf(stringField2, "a2");
        i = StringUtils.lastIndexOf(STATIC_STRING_FIELD1, "b");
        i = StringUtils.lastIndexOf(returnString1(), "c");
        i = StringUtils.lastIndexOf(TestCharset.returnString2(), "d");
        i = StringUtils.lastIndexOf(str, str);
    }

    public void test2a(String str) {
        int i = 0;
        i = stringField1.indexOf("a");
        i = stringField2.indexOf("b");
        i = STATIC_STRING_FIELD1.indexOf(returnString1());
        i = returnString1().indexOf(stringField2);
        i = TestCharset.returnString2().indexOf(STATIC_STRING_FIELD1);
        i = "abc123".indexOf(str);
        i = str.indexOf(str);

        String str1 = "aaa".substring(1);
    }

    public void test2b(String str) {
        int i = 0;
        i = stringField1.lastIndexOf("a");
        i = stringField2.lastIndexOf("b");
        i = STATIC_STRING_FIELD1.lastIndexOf(returnString1());
        i = returnString1().lastIndexOf(stringField2);
        i = TestCharset.returnString2().lastIndexOf(STATIC_STRING_FIELD1);
        i = "abc123".lastIndexOf(str);
        i = str.lastIndexOf(str);
    }

    private String returnString1() {
        return "";
    }

    private static String returnString2() {
        return "";
    }
}

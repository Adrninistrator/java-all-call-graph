package test.call_graph.method_call;

import test.call_graph.common.TestConstants;
import test.call_graph.extend.I2_1_1;
import test.call_graph.extend_complex.AbstractSuperClassA;
import test.call_graph.extend_complex.ChildClassA1;

import java.math.BigDecimal;

/**
 * @author adrninistrator
 * @date 2021/8/9
 * @description:
 */
public class TestMCCaller {

    public static final String TEST_STRING = "r_test_string";

    private static final AbstractSuperClassA[] SUPER_CLASSA_ARRAY = new ChildClassA1[]{};

    private TestMCCallee testMCCallee = new TestMCCallee();

    public void test1a() {
        str();
        TestMCCallee.test1("\uD83D\uDCE9");
    }

    public void test1b() {
        str();
        String a = "tmp";
        TestMCCallee.test1("aa2");
    }

    public void test1c() {
        TestMCCallee.test1(str());
    }

    public void test1d() {
        String a = "tmp";
        TestMCCallee.test1(str());
    }

    public void test1e() {
        TestMCCallee.test1(TestMCCallee.TEST_STRING);
    }

    public void test1f() {
        String str = "aaaa";
        TestMCCallee.test1(str);
    }

    public void test1g() {
        String str = null;
        if (System.currentTimeMillis() % 1000 == 0) {
            str = "aaaa";
        } else {
            str = "bbbb";
        }

        String a = str();
        String b = str();

        TestMCCallee.test1(str);
    }

    public void test1h(String aaa) {
        TestMCCallee.test1(aaa);
    }

    public void test2a() {
        testMCCallee.test2("aa1", "aa2");
    }

    public void test2b() {
        testMCCallee.test2(TEST_STRING,
                "aa2");
    }

    public void test2c() {
        String str1 = null;
        String str2 = null;
        if (System.currentTimeMillis() % 1000 == 0) {
            str1 = "aaaa";
            str2 = "aaaa";
        } else {
            str1 = "bbbb";
        }

        if (System.currentTimeMillis() % 1000 == 0) {
            str2 = "aaaa2";
        } else {
            str2 = "bbbb2";
        }

        String a = str();
        String b = str();

        testMCCallee.test2(str1,
                str2);
    }

    public void test3a() {
        TestMCCallee.test3("aa1",
                str(),
                TestConstants.TEST_STRING);
    }

    public void test3b() {
        TestMCCallee.test3("aa1",
                null,
                TestConstants.TEST_STRING);
    }

    public void test4() {
        run(getInt(), str(), getBigDecimal());
    }

    public void test4a() {
        run(0, str(), getBigDecimal());
    }

    public void test4b() {
        run(getInt(), null, getBigDecimal());
    }

    public void test4c() {
        run(getInt(), str(), null);
    }

    public void test4d() {
        TestMCCallee.run(getInt(), str(), getBigDecimal());
    }

    public void test4e() {
        TestMCCallee.run(new Integer(1).intValue(), new String(""), new BigDecimal(1234));
    }

    public void test4f() {
        TestMCCallee.run(new Integer(1).intValue(), new String(""),
                new BigDecimal("123".toCharArray(), 123, 234));
    }

    public void test4g() {
        String aa = TestMCCallee.run(getInt(), "abc",
                new BigDecimal("123".toCharArray(), 123, 234));
    }

    public void test4h() {
        handleStringArrayArgs(new String[]{"1", "2"});
        String[] ss = new String[]{"3", "4"};
        int a = 1;
        handleStringArrayArgs(ss);

        handleIntArrayArgs(new int[]{11, 22});
    }

    public void test4i() {
        run(new Integer(1).intValue(), TestMCCallee.run(getInt(), "abc",
                new BigDecimal("123".toCharArray(), 123, 234)), BigDecimal.TEN);
    }

    public void test5() {
        run2(new I2_1_1());
    }

    private String str() {
        return "aaa";
    }

    private int getInt() {
        return 0;
    }

    private BigDecimal getBigDecimal() {
        return BigDecimal.ZERO;
    }

    private void run(int a, String b, BigDecimal c) {
        AbstractSuperClassA[] superClassAArray = returnArray1();
    }

    private void handleStringArrayArgs(String[] aa) {
        System.out.println(aa == null);
    }

    private void handleIntArrayArgs(int[] aa) {
    }

    private void run2(I2_1_1 i) {
    }

    public AbstractSuperClassA[] returnArray1() {
        return new ChildClassA1[]{};
    }

    public AbstractSuperClassA[] returnArray2() {
        return SUPER_CLASSA_ARRAY;
    }
}

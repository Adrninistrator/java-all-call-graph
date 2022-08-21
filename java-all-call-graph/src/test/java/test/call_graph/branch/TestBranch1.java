package test.call_graph.branch;

/**
 * @author adrninistrator
 * @date 2022/6/5
 * @description:
 */
public class TestBranch1 {

    private void test1() {
        int i = (int) System.currentTimeMillis() % 10;
        if (i == 1) {
            System.out.println(i);
        }
    }

    private void test2() throws InterruptedException {
        int i = (int) System.currentTimeMillis() % 10;
        if (i == 1) {
            System.out.println(i);
        } else {
            Thread.sleep(i);
        }
    }

    private void test3() throws InterruptedException {
        int i1 = (int) System.currentTimeMillis() % 10;
        int i2 = (int) System.currentTimeMillis() % 10;
        if (i1 == 1) {
            System.out.println(i1);
        }
        if (i2 == 2) {
            System.out.println(i2);
        } else {
            Thread.sleep(i2);
        }
    }

    private void test4() throws InterruptedException {
        int i1 = (int) System.currentTimeMillis() % 10;
        int i2 = (int) System.currentTimeMillis() % 10;
        if (i1 == 1) {
            System.out.println(i1);
        } else {
            Thread.sleep(i2);
        }
        if (i2 == 2) {
            System.out.println(i2);
        }
    }

    private void test5a() {
        int i = (int) System.currentTimeMillis() % 10;
        String s1 = (i == 3 ? "a" : "b");
        System.out.println(s1);
    }

    private void test5b() {
        int i1 = (int) System.currentTimeMillis() % 10;
        int i2 = (int) System.currentTimeMillis() % 10;
        String s1 = (i1 == 3 ? i2 == 4 ? "a1" : "b1" : i2 == 5 ? "a2" : "b2");
        System.out.println(s1);
    }

    private void test5c() {
        int i = (int) System.currentTimeMillis() % 10;
        String s1 = (i == 3 ? testString("a") : testString("b"));
        System.out.println(s1);
    }

    private void test5d() {
        int i = (int) System.currentTimeMillis() % 10;
        String s1 = (i == 3 ? testString(testString("a") + "1") : testString("b"));
        System.out.println(s1);
    }

    private void test6() {
        int i = (int) System.currentTimeMillis() % 10;
        String s1 = (i == 3 ? "a1" : "b1");
        String s2 = (i == 5 ? "a2" : "b2");
        String s3 = (i == 7 ? "a3" : "b3");
        System.out.println(s1 + " " + s2 + " " + s3);
    }

    private void test7() {
        int i1 = (int) System.currentTimeMillis() % 10;
        int i2 = (int) System.currentTimeMillis() % 10;
        if (i1 > 5) {
            System.out.println("a1");
        } else {
            System.out.println("a2");
        }
        System.out.println("1");
        if (i2 < 3) {
            System.out.println("b1");
        } else {
            System.out.println("b2");
        }
        System.out.println("2");
    }

    private void test8() {
        int i1 = (int) System.currentTimeMillis() % 10;
        int i2 = (int) System.currentTimeMillis() % 10;
        if (i1 > 5) {
            System.out.println("a1");
            if (i2 < 3) {
                System.out.println("b1");
            } else {
                System.out.println("b2");
            }
        } else {
            System.out.println("a2");
        }
        System.out.println("1");
    }

    private String testString(String data) {
        return data + System.currentTimeMillis();
    }
}

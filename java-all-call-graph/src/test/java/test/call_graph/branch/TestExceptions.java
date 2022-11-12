package test.call_graph.branch;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;

/**
 * @author adrninistrator
 * @date 2022/9/10
 * @description:
 */
public class TestExceptions {

    public void test1a() {
        String s1 = "111";
        try {
            s1 = "222";
            int a = 1;
            int b = 1 / (a - 1);
        } catch (Exception e) {
            System.out.println(s1);
            e.printStackTrace();
        }
        System.out.println("done1");
    }

    public void test1b() {
        try {
            int a = 1;
            int b = 1 / (a - 1);
        } catch (ArithmeticException e) {
            System.currentTimeMillis();
            e.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
        }
        System.out.println("done1");
    }

    public void test1c() {
        try {
            int a = 1;
            int b = 1 / (a - 1);
        } catch (ArithmeticException e) {
            System.currentTimeMillis();
            e.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            System.getProperty("ss1");
            System.setProperty("ss2", "ss2-v");
        }
        System.out.println("done1");
    }

    public void test1d() {
        try {
            int a = 1;
            int b = 1 / (a - 1);
        } finally {
            System.getProperty("ss1");
        }
        System.out.println("done1");
    }

    public void test1e() {
        try {
            int a = 1;
            int b = 1 / (a - 1);

            FileInputStream input = new FileInputStream("a.txt");
        } catch (ArithmeticException | FileNotFoundException e) {
            e.printStackTrace();
        }
        System.out.println("done1");
    }

    public void test1f() {
        try {
            try {
                int a = 1;
                int b = 1 / (a - 1);

            } catch (ArithmeticException e) {
                e.printStackTrace();
            }

            FileInputStream input = new FileInputStream("a.txt");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public void test2() {
        try (InputStream input = new FileInputStream("a.txt")) {
            int i = input.read();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void test2a() {
        try {
            InputStream input = new FileInputStream("a.txt");
            Throwable var2 = null;

            try {
                int var3 = input.read();
            } catch (Throwable var12) {
                var2 = var12;
                throw var12;
            } finally {
                if (input != null) {
                    if (var2 != null) {
                        try {
                            input.close();
                        } catch (Throwable var11) {
                            var2.addSuppressed(var11);
                        }
                    } else {
                        input.close();
                    }
                }

            }
        } catch (Exception var14) {
            var14.printStackTrace();
        }
    }

    public void test3() {
        try {
            int a = 1;
            int b = 1 / (a - 1);
            try (InputStream input = new FileInputStream("a.txt")) {
                int i = input.read();
            } catch (Exception e) {
                e.printStackTrace();
            }
        } catch (Exception e) {
            e.printStackTrace();

            try (InputStream input = new FileInputStream("b.txt")) {
                int i = input.read();
            } catch (Exception e1) {
                e1.printStackTrace();
            }
        }
    }

    public void test3a() {
        try {
            int a = 1;
            int b = 1 / (a - 1);
        } catch (Exception e) {
            e.printStackTrace();

            try {
                System.getProperty("abc");
            } catch (Exception e1) {
                e1.printStackTrace();
            }
        }
    }

    protected String test4(final String type1, final String type2) {
        ClassLoader classLoader = this.getClass().getClassLoader();
        Class<?> class1;
        try {
            class1 = Class.forName(type1, false, classLoader);
        } catch (ClassNotFoundException e) {
            throw new TypeNotPresentException(type1, e);
        }
        Class<?> class2;
        try {
            class2 = Class.forName(type2, false, classLoader);
        } catch (ClassNotFoundException e) {
            throw new TypeNotPresentException(type2, e);
        }
        if (class1.isAssignableFrom(class2)) {
            return type1;
        }
        if (class2.isAssignableFrom(class1)) {
            return type2;
        }

        return "ok";
    }

    public void test5a() {
        try {
        } catch (Exception e) {
        } finally {
        }
    }

    public void test5b() {
        try {
            System.getProperty("111");
        } catch (Exception e) {
        } finally {
        }
    }

    public void test5c() {
        try {
        } finally {
        }
    }

    public void test6() {
        try {
            throw new RuntimeException("111");
        } finally {
            System.getProperty("222");
        }
    }
}

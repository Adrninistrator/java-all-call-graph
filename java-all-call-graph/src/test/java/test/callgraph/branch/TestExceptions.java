package test.callgraph.branch;

import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.WeakHashMap;

/**
 * @author adrninistrator
 * @date 2022/9/10
 * @description:
 */
public class TestExceptions {

    private static final Logger logger = LoggerFactory.getLogger(TestExceptions.class);

    public static void test1aa() {
        String s1 = "111";
        try {
            s1 = "222";
            long a = System.currentTimeMillis() % 7;
            if (a == 1) {
                return;
            }
            if (a == 2) {
                long b = 1 / (a - 1);
                System.out.println(b);
            }
            if (a == 3) {
                long b = 1 / (a - 2);
                System.out.println(b);
            }
        } catch (Exception e) {
            System.out.println(s1);
            e.printStackTrace();
            if (System.currentTimeMillis() % 7 == 2) {
                if (System.getProperty("abc") != null) {
                    System.setProperty("aa", "bb");
                }
                if (System.getenv("bbb") != null) {
                    System.setErr(null);
                }
            }
            if (System.currentTimeMillis() % 5 == 2) {
                if (System.getProperty("abc222") != null) {
                    System.setProperty("aa222", "bb222");
                }
                if (System.getenv("bbb222") != null) {
                    System.setErr(null);
                }
            }
        }
        System.out.println("done1");
    }

    public boolean test1ab() {
        String s1 = "111";
        try {
            s1 = "222";
            int a = 1;
            try {
                int b = 1 / (a - 1);
                System.out.println(b);
            } catch (Exception e) {
                e.printStackTrace();
            }
            return true;
        } catch (Exception e) {
            if (System.getProperty("abc") != null) {
                System.setProperty("aa", "bb");
            }
            System.out.println(s1);
            System.out.println(
                    e.getMessage()
            );
            System.out.println(e.getMessage()
                    .trim()
            );
            e.printStackTrace();
        }
        System.out.println("done1");
        return false;
    }

    public boolean test1ac() {
        String s1 = "111";
        try {
            s1 = "222";
            int a = 1;
            int b = 1 / (a - 1);
            if (b == 0) {
                return true;
            }
            return false;
        } catch (Exception e) {
            int i1 = 1;
            int i2 = 2;
            int i3 = 3;
            int i4 = 4;
            int i5 = 5;
            int i6 = 6;
            Exception e1 = new Exception();
            System.out.println(i1 + i2 + i3 + i4 + i5 + i6 + e1.toString());
            if (System.getProperty("abc") != null) {
                System.setProperty("aa", "bb");
            }
            System.out.println(s1);
            e.printStackTrace();
            return false;
        }
    }

    public static void test1ad() {
        String s1 = "111";
        try {
            s1 = "222";
            int a = 1;
            int b = 1 / (a - 1);
        } catch (Exception e) {
            Exception e1 = e;
            e1.printStackTrace();
        }
        System.out.println("done1");
    }

    public static void test1a() {
        String s1 = "111";
        try {
            s1 = "222";
            int a = 1;
            int b = 1 / (a - 1);
        } catch (Exception e) {
            logger.error("", e);
            System.out.println(s1);
            e.printStackTrace();
        }
        System.out.println("done1");
    }

    public boolean test1b() {
        try {
            int a = 1;
            int b = 1 / (a - 1);
        } catch (ArithmeticException e) {
            logger.error("{}", e.getMessage());
            System.currentTimeMillis();
            e.printStackTrace();
            return false;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
        System.out.println("done1");
        return true;
    }

    public boolean test1c() {
        try {
            int a = 1;
            int b = 1 / (a - 1);
            if (b == 0) {
                return false;
            }
        } catch (ArithmeticException e) {
            System.currentTimeMillis();
            System.err.println(e.getMessage());
            return false;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        } finally {
            System.getProperty("ss1");
            System.setProperty("ss2", "ss2-v");
        }
        System.out.println("done1");
        return true;
    }

    public static void test1da(int a) {
        try {
            System.out.println("ss1");
            int b = 1 / (a - 1);
            System.out.println(b);
        } catch (ArithmeticException e) {
            System.out.println("ArithmeticException");
            throw e;
        } catch (Exception e) {
            System.out.println("Exception");
            throw new RuntimeException("RuntimeException");
        } finally {
            print("ss2");
        }
        System.out.println("done1");
    }

    public static void test1daa(int a) {
        try {
            System.out.println("ss1");
            int b = 1 / (a - 1);
            System.out.println(b);
        } catch (ArithmeticException e) {
            System.out.println("ArithmeticException");
        } catch (Exception e) {
            System.out.println("Exception");
        } finally {
            print("ss2");
        }
        System.out.println("done1");
    }

    public static void test1dab(int a) {
        try {
            System.out.println("ss1");
            int b = 1 / (a - 1);
            System.out.println(b);
            FileInputStream input = new FileInputStream("a.txt");
        } catch (ArithmeticException | IOException e) {
            e.printStackTrace();
        }
        System.out.println("done1");
    }

    public static void test1dac(int a) {
        try {
            System.out.println("ss1");
            int b = 1 / (a - 1);
            System.out.println(b);
            FileInputStream input = new FileInputStream("a.txt");
        } catch (ArithmeticException | IOException e) {
            e.printStackTrace();
        } finally {
            print("ss2");
        }
        System.out.println("done1");
    }

    public static void test1dad(int a) {
        try {
            System.out.println("ss1");
            int b = 1 / (a - 1);
            System.out.println(b);
        } catch (ArithmeticException e) {
            System.out.println("ArithmeticException");
            throw e;
        } catch (Exception e) {
            System.out.println("Exception");
            System.out.println(e.getMessage().trim());
            throw new RuntimeException(
                    e.getMessage()
            );
        } finally {
            print("ss2");
        }
        System.out.println("done1");
    }

    public static void test1db(int a) throws Exception {
        try {
            System.out.println("ss1");
            int b = 1 / (a - 1);
            System.out.println(b);
        } catch (Exception e) {
            throw (e instanceof ArithmeticException) ? e : new RuntimeException(e);
        } finally {
            print("ss2");
        }
        System.out.println("done1");
    }

    private static void print(String data) {
        System.out.println(data);
    }

    public static void test1d() {
        try {
            int a = 1;
            int b = 1 / (a - 1);
        } finally {
            System.getProperty("ss1");
        }
        System.out.println("done1");
    }

    public int test1e() {
        try {
            int a = 1;
            int b = 1 / (a - 1);

            FileInputStream input = new FileInputStream("a.txt");
            return input.available() > 100 ? 1 : 0;
        } catch (ArithmeticException | IOException e) {
            e.printStackTrace();
        }
        System.out.println("done1");
        return 0;
    }

    public static void test1f() {
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

    public static void test1g() {
        try {
            int a = 1;
            int b = 1 / (a - 1);
        } catch (ArithmeticException e) {
            System.err.println("ArithmeticException e1");
            e.printStackTrace();
        } catch (Exception e) {
            System.err.println("Exception e1");
            e.printStackTrace();
        }
        System.out.println("done1");

        try {
            int a = 1;
            int b = 1 / (a - 1);
        } catch (Exception e) {
            System.err.println("e2");
            e.printStackTrace();
        }
        System.out.println("done2");
    }

    public static void test2() {
        try (InputStream input = new FileInputStream("a.txt")) {
            int i = input.read();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void test2a() {
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

    public static void test2b() throws IOException {
        try (InputStream input = new FileInputStream("a.txt")) {
            int i = input.read();
        }
    }

    public static void test2c() {
        InputStream input = null;
        try {
            input = new FileInputStream("a.txt");
            int var3 = input.read();
            System.out.println(var3);
        } catch (Exception var14) {
            System.err.println(var14.getMessage());
        } finally {
            if (input != null) {
                try {
                    input.close();
                } catch (Throwable var11) {
                    var11.printStackTrace();
                }
            }
        }
    }

    public static void test2d() {
        try {
            int a = 1;
            int b = 1 / (a - 1);
            System.out.println(b);
        } catch (Exception e) {
            long a = System.currentTimeMillis() % 7;
            if (a == 3) {
                throw e;
            }
            throw new RuntimeException(e);
        }
        System.out.println("done");
    }

    public static void test2e() {
        try {
            int a = 1;
            int b = 1 / (a - 1);
            System.out.println(b);
        } catch (ArithmeticException e) {
            e.printStackTrace();
        } finally {
            try {
                int i = 10;
                while (i > 0) {
                    System.out.println(i);
                    i--;
                }
            } catch (Exception e1) {
                System.err.println(e1.getMessage());
            }
        }
        System.out.println("done");
    }

    public static void test3() {
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

    public static void test3a(int a) {
        Exception exception = null;
        try {
            int b = 1 / (a - 1);
        } catch (Exception e) {
            exception = e;
            e.printStackTrace();

            try {
                System.getProperty("abc");
            } catch (Exception e1) {
                e1.printStackTrace();
            }
        }
        if (exception != null) {
            exception.printStackTrace();
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

    public static void test5a() {
        try {
        } catch (Exception e) {
        } finally {
        }
    }

    public static void test5b() {
        try {
            System.getProperty("111");
        } catch (Exception e) {
        } finally {
        }
    }

    public static void test5c() {
        try {
        } finally {
        }
    }

    public static void test6() {
        try {
            throw new JavaCGRuntimeException("111");
        } finally {
            System.getProperty("222");
        }
    }

    public static int test7() {
        while (0 < time()) {
            try {
                Thread.sleep(500);
            } catch (Exception e) {
                logger.error("error1 ", e);
                return -1;
            }
        }
        return 0;
    }

    public static int test7a() {
        for (int i = 0; i < 10; i++) {
            try {
                Thread.sleep(500);
                if (time() == 3) {
                    return 1;
                }
                System.out.println("done");
            } catch (Exception e) {
                logger.error("error1 ", e);
                return -1;
            }
        }
        return 0;
    }

    public static int test7b() {
        System.out.println("begin");
        try {
            while (0 < time()) {
                try {
                    Thread.sleep(500);
                } catch (Exception e) {
                    logger.error("error1 ", e);
                    return -1;
                }
            }
        } catch (Exception e) {
            logger.error("error2 ", e);
            return -1;
        }
        return 0;
    }

    public static void test7c() {
        while (true) {
            try {
                Thread.sleep(1000L);
                System.out.println("after sleep");
            } catch (InterruptedException e) {
                logger.warn("interrupted: {}", e.getMessage());
                break;
            } catch (Exception e) {
                logger.error("error: ", e);
            }
        }
    }

    public static void test7d() {
        while (true) {
            try {
                Thread.sleep(1000L);
                System.out.println("after sleep");
            } catch (InterruptedException e) {
                logger.warn("interrupted: {}", e.getMessage());
                break;
            }
        }
        System.setProperty("a", "b");
    }

    public static void test7e() {
        while (true) {
            try {
                Thread.sleep(1000L);
                System.out.println("after sleep");
            } catch (InterruptedException e) {
                logger.warn("interrupted: {}", e.getMessage());
            }
        }
    }

    public static void test7f() {
        try {
            int port = 8080;
            System.out.println("Listening on port " + port);
            ServerSocket serverSocket = new ServerSocket(port);
            while (true) {
                System.out.println("Waiting to accept a new client.");
                Socket socket = serverSocket.accept();
                System.out.println("Connected to client at " + socket.getInetAddress());
                System.out.println("Starting new socket node.");
                new Thread(() -> System.out.println("test " + socket.getInetAddress())).start();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void test8() {
        try {
            int a = 1;
            int b = 1 / (a - 1);
            throw new RuntimeException("1");
        } catch (ArithmeticException e) {
            logger.error("error1 ", e);
            throw new RuntimeException("2");
        } catch (Exception e) {
            logger.error("error2 ", e);
            // 以下写法不会识别出有使用catch的异常对象
            throw new RuntimeException(Objects.toString(e));
        }
    }

    public static int test9() throws IOException {
        try (InputStream input = new FileInputStream("a.txt")) {
            return input.read();
        }
    }

    public static List<String> getPomeLineList() throws IOException {
        List<String> pomLineList = new ArrayList<>();
        try (FileReader fileReader = new FileReader("pom.xml");
             BufferedReader bufferedReader = new BufferedReader(fileReader)) {
            String line;
            while ((line = bufferedReader.readLine()) != null) {
                pomLineList.add(line);
            }
            return pomLineList;
        }
    }

    private final Set<InputStream> inputStreams = Collections.newSetFromMap(new WeakHashMap<>());

    private IOException releaseInputStreams(IOException exceptionChain) {
        synchronized (this.inputStreams) {
            for (InputStream inputStream : new ArrayList<>(this.inputStreams)) {
                try {
                    inputStream.close();
                } catch (IOException ex) {
                    exceptionChain = addToExceptionChain(exceptionChain, ex);
                }
            }
            this.inputStreams.clear();
        }
        return exceptionChain;
    }

    private IOException addToExceptionChain(IOException exceptionChain, IOException ex) {
        if (exceptionChain != null) {
            exceptionChain.addSuppressed(ex);
            return exceptionChain;
        }
        return ex;
    }

    private static int time() {
        return (int) System.currentTimeMillis() % 7;
    }
}

package test.callgraph.branch;


import com.adrninistrator.javacg.exceptions.JavaCGRuntimeException;
import test.callgraph.enums.DbStatementEnum;

/**
 * @author adrninistrator
 * @date 2022/6/5
 * @description:
 */
public class TestBranch2 {

    private void test1() {
        int a = 0;
        int i = (int) System.currentTimeMillis() % 10;
        switch (i) {
            case 1:
                a = 2;
                break;
            case 4:
                a = 3;
                break;
            case 7:
                a = 4;
                break;
        }
        System.out.println(a);
    }

    private void test2() {
        int i = (int) System.currentTimeMillis() % 10;
        try {
            int a = 1 / i - 1;
            System.out.println("aa " + a);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void test3() {
        int a = 0;
        int i = (int) System.currentTimeMillis() % 10;
        for (int ii = 0; ii <= i; ii++) {
            if (ii == 0) {
                System.out.println(ii);
            }
            if (ii == 1) {
                System.out.println(ii);
                break;
            }
        }
        System.out.println(a);
    }

    private void test4() {
        DbStatementEnum dbStatementEnum = DbStatementEnum.getFromStatement(String.valueOf(System.currentTimeMillis()));
        switch (dbStatementEnum) {
            case DSE_SELECT:
                System.out.println("1");
                break;
            case DSE_INSERT:
                System.out.println("2");
                break;
            case DSE_REPLACE:
                System.out.println("3");
                break;
            case DSE_UPDATE:
                System.out.println("4");
                break;
            case DSE_DELETE:
                System.out.println("5");
                break;
            default:
                System.out.println("a");
                break;
        }
    }

    private void test5() {
        int i = (int) System.currentTimeMillis() % 10;
        if (i == 0) {
            return;
        } else if (i == 1) {
            throw new JavaCGRuntimeException("illegal");
        } else if (i == 2) {
            System.getProperty("aa");
        }

        System.out.println("done1");
    }

    private void test6() {
        int i = (int) System.currentTimeMillis() % 10;
        switch (i) {
            default:
                System.out.println(i);
                break;
        }
        System.setProperty("a", "b");
    }

    private void test7() {
        String str = "init";
        int i = (int) System.currentTimeMillis() % 10;
        if (i == 0) {
            str = "0";
            System.out.println(str);
            return;
        } else if (i == 1) {
            str = "1";
            System.out.println(str);
            throw new JavaCGRuntimeException("illegal");
        } else if (i == 2) {
            str = "2";
            System.out.println(str);
            return;
        }

        System.out.println(str);
    }
}

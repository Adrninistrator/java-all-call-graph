package test.call_graph.branch;

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
            default:
                a = 5;
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
}

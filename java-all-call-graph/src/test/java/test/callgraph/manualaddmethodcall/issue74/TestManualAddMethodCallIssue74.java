package test.callgraph.manualaddmethodcall.issue74;

import org.junit.Test;

/**
 * @author adrninistrator
 * @date 2025/4/23
 * @description:
 */
public class TestManualAddMethodCallIssue74 {

    @Test
    public void test1() {
        NotFoundCallBack notFoundCallBack1 = new NotFoundCallBack() {
            @Override
            public void execute() {
                doSomeThing();
            }
        };
        notFoundCallBack1.execute();
    }

    @Test
    public void test2() {
        NotFoundCallBack notFoundCallBack2 = () -> doSomeThing();
        notFoundCallBack2.execute();
    }

    @Test
    public void test3() {
        NotFoundCallBack notFoundCallBack3 = TestManualAddMethodCallIssue74::doSomeThing;
        notFoundCallBack3.execute();
    }

    static void doSomeThing() {
        System.out.println("ok");
        System.getProperty("test");
    }
}

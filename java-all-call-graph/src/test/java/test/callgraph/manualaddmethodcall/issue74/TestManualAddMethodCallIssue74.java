package test.callgraph.manualaddmethodcall.issue74;

/**
 * @author adrninistrator
 * @date 2025/4/23
 * @description:
 */
public class TestManualAddMethodCallIssue74 {

    public static void main1(String[] args) {
        NotFoundCallBack notFoundCallBack1 = new NotFoundCallBack() {
            @Override
            public void execute() {
                doSomeThing();
            }
        };
    }

    public static void main2(String[] args) {
        NotFoundCallBack notFoundCallBack2 = () -> doSomeThing();
    }

    public static void main3(String[] args) {
        NotFoundCallBack notFoundCallBack3 = TestManualAddMethodCallIssue74::doSomeThing;
    }

    static void doSomeThing() {
        System.getProperty("");
    }
}

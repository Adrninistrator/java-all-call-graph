package test.call_graph.manual_add_method_call.fixed;

/**
 * @author adrninistrator
 * @date 2022/4/16
 * @description:
 */
public class TestFixedManualAddMethodCall {
    public String test1() {
        return new FixedService1a().invoke();
    }

    public String test2(String s) {
        return new AbstractFixedService1() {
            @Override
            protected String execute() {
                return String.valueOf(System.currentTimeMillis());
            }
        }.invoke();
    }

    public String test3() {
        return new AbstractFixedService1() {
            @Override
            protected String execute() {
                System.setProperty("g", "h");
                return "";
            }
        }.invoke();
    }

    public String test4() {
        return new AbstractFixedServiceChild1() {
            @Override
            protected String execute() {
                System.setErr(null);
                return "";
            }
        }.invoke();
    }

    public String test5() {
        return new AbstractFixedServiceChild1() {
            @Override
            protected String execute() {
                System.setSecurityManager(null);
                return "";
            }
        }.invoke();
    }

    public String test6() {
        return new FixedOtherJarService1a().invoke();
    }

    public String test7() {
        return new FixedOtherJarServiceChild1a().invoke();
    }
}

package test.call_graph.manual_add_callgraph.fixed;

/**
 * @author adrninistrator
 * @date 2022/4/16
 * @description:
 */
public class TestFixedManualAddCallGraph {
    public String test1() {
        return new FixedService1b().invoke();
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
                System.setProperty("", "");
                return "";
            }
        }.invoke();
    }

    public String test4() {
        return new AbstractFixedService1a() {
            @Override
            protected String execute() {
                System.setErr(null);
                return "";
            }
        }.invoke();
    }

    public String test5() {
        return new AbstractFixedService1a() {
            @Override
            protected String execute() {
                System.setSecurityManager(null);
                return "";
            }
        }.invoke();
    }
}

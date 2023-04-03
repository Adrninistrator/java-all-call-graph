package test.call_graph.manual_add_method_call.fixed;

/**
 * @author adrninistrator
 * @date 2023/3/24
 * @description:
 */
public class FixedOtherJarServiceChild1a extends AbstractFixedOtherJarServiceChild1 {
    @Override
    protected String execute() {
        return System.getProperty("aaa");
    }
}

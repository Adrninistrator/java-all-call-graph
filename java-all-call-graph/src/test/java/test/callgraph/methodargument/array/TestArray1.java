package test.callgraph.methodargument.array;

import java.text.MessageFormat;

/**
 * @author adrninistrator
 * @date 2023/3/13
 * @description:
 */
public class TestArray1 {
    public void test() {
        MessageFormat mf = new MessageFormat("...");
        Object[] objects = {1, "2"};
        mf.format(objects);
    }
}

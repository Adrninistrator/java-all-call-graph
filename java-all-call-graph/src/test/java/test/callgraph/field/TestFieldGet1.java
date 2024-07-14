package test.callgraph.field;

import test.callgraph.field.dto.TestFieldDto1;

/**
 * @author adrninistrator
 * @date 2024/7/5
 * @description:
 */
public class TestFieldGet1 {

    public void test1() {
        TestFieldDto1 testFieldDto1 = new TestFieldDto1();
        for (String str : testFieldDto1.getStrList1()) {
            System.out.println(str);
        }
    }
}

package test.callgraph.superjdk.date;

import java.io.IOException;

/**
 * @author adrninistrator
 * @date 2024/5/20
 * @description:
 */
public class TestDate1 {

    public void test() throws IOException {
        DateChild1 dateChild1 = new DateChild1();
        dateChild1.getTime();

        DateChild1_2 dateChild1_2 = new DateChild1_2();
        dateChild1_2.getTime();
        dateChild1_2.testDate1();
    }
}

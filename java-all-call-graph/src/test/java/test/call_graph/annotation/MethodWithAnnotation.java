package test.call_graph.annotation;

/**
 * @author adrninistrator
 * @date 2022/8/20
 * @description:
 */
public class MethodWithAnnotation {

    @TestAnnotation(value = "")
    public void test1() {
        System.out.println("");
    }

    @TestAnnotation(value = "abc")
    public void test2() {
        System.out.println("");
    }
}

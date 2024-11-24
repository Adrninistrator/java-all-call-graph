package test.callgraph.methodargument;

import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/3/7
 * @description:
 */
public class TestArgumentGenerics2 {

    List<?> list1;
    List<? extends Number> list2;
    List<? super Integer> list3;

    public List<?> test1(List<?> list) {
        return null;
    }

    public List<? extends Number> test2(List<? extends Number> list) {
        return null;
    }

    public List<? super Integer> test3(List<? super Integer> list) {
        return null;
    }

    public void testAll() {
        TestArgumentGenerics1 testArgumentGenerics1 = new TestArgumentGenerics1();
        testArgumentGenerics1.test1(null);
        testArgumentGenerics1.test2(null);
        testArgumentGenerics1.test3(0, null);
        testArgumentGenerics1.test4(null);
        testArgumentGenerics1.test5(null);
    }

    public void testUse() {
        List<Number> list1 = new ArrayList<>();
        List<Integer> list2 = new ArrayList<>();
        test1(list1);
        test2(list1);
        test3(list1);

        test1(list2);
        test2(list2);
        test3(list2);
    }

    public static void main(String[] args) {
        TestArgumentGenerics2 testArgumentGenerics2 = new TestArgumentGenerics2();
        testArgumentGenerics2.testUse();
    }

    public List<?> getList1() {
        return list1;
    }

    public void setList1(List<?> list1) {
        this.list1 = list1;
    }

    public List<? extends Number> getList2() {
        return list2;
    }

    public void setList2(List<? extends Number> list2) {
        this.list2 = list2;
    }

    public List<? super Integer> getList3() {
        return list3;
    }

    public void setList3(List<? super Integer> list3) {
        this.list3 = list3;
    }
}

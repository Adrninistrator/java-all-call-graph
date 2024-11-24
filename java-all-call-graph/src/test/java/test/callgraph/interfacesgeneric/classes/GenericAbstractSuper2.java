package test.callgraph.interfacesgeneric.classes;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/8/8
 * @description:
 */
public abstract class GenericAbstractSuper2<T1, T2> {
    public abstract void test(T1 t1, T2 t2, String str, int i);

    private T1 t1;
    private T1[] t1s;
    private List<T2> t2List;
    private List<T2[]> t2sList;

    public T1 test2(T1 t1) {
        return null;
    }

    public T1[] test3(T1[] t1s) {
        return null;
    }

    public List<T2> test4(List<T2> t2) {
        return null;
    }

    public List<T2[]> test5(List<T2[]> t2) {
        return null;
    }

    public T1 getT1() {
        return t1;
    }

    public void setT1(T1 t1) {
        this.t1 = t1;
    }

    public T1[] getT1s() {
        return t1s;
    }

    public void setT1s(T1[] t1s) {
        this.t1s = t1s;
    }

    public List<T2> getT2List() {
        return t2List;
    }

    public void setT2List(List<T2> t2List) {
        this.t2List = t2List;
    }

    public List<T2[]> getT2sList() {
        return t2sList;
    }

    public void setT2sList(List<T2[]> t2sList) {
        this.t2sList = t2sList;
    }
}

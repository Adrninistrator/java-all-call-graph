package test.call_graph.extend_complex.only_class;

/**
 * @author adrninistrator
 * @date 2022/9/11
 * @description:
 */
public class TestExtendComplex {

    public void test1() {
        int a = (int) System.currentTimeMillis() % 10;
        if (a == 1) {
            AbstractSuperClassA superClassA = new ChildClassA1();
            superClassA.entryA();
        } else {
            AbstractSuperClassA superClassA = new ChildClassA2();
            superClassA.entryA();
        }
    }

    public void test2() {
        int a = (int) System.currentTimeMillis() % 10;
        AbstractSuperClassA superClassA;
        if (a == 1) {
            superClassA = new ChildClassA1();
            superClassA.entryA();
        } else if (a == 2) {
            superClassA = new ChildClassA2();
            superClassA.entryA();
        } else {
            superClassA = new ChildClassB1();
        }

        superClassA.entryA();
    }

    private AbstractSuperClassA gen() {
        return null;
    }

    private void test3() {
        AbstractSuperClassA superClassA = (ChildClassA1) gen();
        superClassA.entryA();

        ((ChildClassB1) gen()).entryA();
        ((ChildClassB1) gen()).runA();
    }
}

package test.callgraph.extendcomplex;

import test.callgraph.annotation.MethodWithAnnotation;

/**
 * @author adrninistrator
 * @date 2022/9/11
 * @description:
 */
public class TestExtendComplex {
    private AbstractSuperClassA superClassA1;
    private AbstractSuperClassA superClassA2;
    private AbstractSuperClassB superClassB1;
    private AbstractSuperClassB superClassB2;
    private AbstractSuperClassB[] superClassB22Array;

    public TestExtendComplex() {
        int a = (int) System.currentTimeMillis() % 10;
        if (a == 1) {
            superClassA1 = new ChildClassA1();
        } else {
            superClassA1 = new ChildClassA2();
        }
        superClassA2 = new ChildClassA2();
        superClassB1 = new ChildClassB1();
        superClassB2 = new ChildClassB2();
        superClassB22Array = new ChildClassB2[]{};
    }

    public TestExtendComplex(String test) {
        superClassA2 = new ChildClassA1();
    }

    public void test1() {
        int a = (int) System.currentTimeMillis() % 10;
        if (a == 1) {
            AbstractSuperClassA superClassA = new ChildClassA1();
            superClassA.entryA();
        } else {
            AbstractSuperClassA superClassA = new ChildClassA2();
            superClassA.entryA();
        }

        superClassA1.entryA();
        superClassA2.entryA();
        superClassB1.entryA();
        superClassB2.entryA();
        for (AbstractSuperClassB superClassB22 : superClassB22Array) {
            superClassB22.entryA();
        }

        new MethodWithAnnotation().test1();
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

    private void test4() {
        superClassB22Array = new AbstractSuperClassB[]{superClassB1, superClassB2};
        test5(superClassB22Array);
    }

    private void test5(AbstractSuperClassB[] superClassBArray) {
    }
}

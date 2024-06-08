package test.callgraph.fieldrelationships.frb;

/**
 * @author adrninistrator
 * @date 2023/7/23
 * @description:
 */
public class FRBClass1 {
    public void test1() {
        FRBDtoCycle1 frbDtoCycle1 = new FRBDtoCycle1();
        FRBDtoCycle2 frbDtoCycle2 = new FRBDtoCycle2();
        frbDtoCycle2.setI1(frbDtoCycle1.getI1());
        frbDtoCycle2.setL1(frbDtoCycle1.getL1());
        frbDtoCycle2.setStr1(frbDtoCycle1.getStr1());
        frbDtoCycle2.setStr2(frbDtoCycle1.getStr2());
    }

    public void test2() {
        FRBDtoCycle2 frbDtoCycle2 = new FRBDtoCycle2();
        FRBDtoCycle1 frbDtoCycle1 = new FRBDtoCycle1();
        frbDtoCycle1.setI1(frbDtoCycle2.getI1());
        frbDtoCycle1.setL1(frbDtoCycle2.getL1());
        frbDtoCycle1.setStr1(frbDtoCycle2.getStr1());
        frbDtoCycle1.setStr2(frbDtoCycle2.getStr2());
    }
}

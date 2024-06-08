package test.callgraph.fieldrelationships.frc;

import java.lang.reflect.InvocationTargetException;

/**
 * @author adrninistrator
 * @date 2023/7/25
 * @description:
 */
public class FRCClass1 {

    public static final FRCDtoA fRCDtoA1 = System.getProperty("test") != null ? new FRCDtoA() : new FRCDtoB();

    private ChildInterfaceFRC1 childInterfaceFRC1;

    public void test1() throws InvocationTargetException, IllegalAccessException {
        FRCDtoA frcDtoA = new FRCDtoA();
        FRCDtoB frcDtoB = new FRCDtoB();
        org.springframework.beans.BeanUtils.copyProperties(frcDtoA, frcDtoB);
        org.apache.commons.beanutils.BeanUtils.copyProperties(frcDtoB, frcDtoA);
    }

    public void test2() throws InvocationTargetException, IllegalAccessException {
        FRCDtoA frcDtoA = new FRCDtoA();
        FRCDtoC frcDtoC = new FRCDtoC();
        org.springframework.beans.BeanUtils.copyProperties(frcDtoA, frcDtoC);
        org.apache.commons.beanutils.BeanUtils.copyProperties(frcDtoC, frcDtoA);
    }

    public void test3() throws InvocationTargetException, IllegalAccessException {
        FRCDtoB frcDtoB = new FRCDtoB();
        FRCDtoC frcDtoC = new FRCDtoC();
        org.springframework.beans.BeanUtils.copyProperties(frcDtoB, frcDtoC);
        org.apache.commons.beanutils.BeanUtils.copyProperties(frcDtoC, frcDtoB);
    }

    public void test4() {
        FRCDtoA frcDtoA = new FRCDtoA();
        FRCDtoB frcDtoB = new FRCDtoB();
        frcDtoA.setiField1(frcDtoB.getiField1());
        childInterfaceFRC1.save(frcDtoA);
    }

    public void test5() {
        FRCDtoA frcDtoA = childInterfaceFRC1.query("");
        FRCDtoB frcDtoB = new FRCDtoB();
        frcDtoB.setiField1(frcDtoA.getiField1());
        System.out.println(frcDtoB);
    }

    private FRCDtoA genFRCDtoA() {
        return new FRCDtoB();
    }

    private void test6() {
        FRCDtoB frcDtoB = (FRCDtoB) genFRCDtoA();
        FRCDtoC frcDtoC = new FRCDtoC();
        frcDtoC.setiField1(frcDtoB.getiField1());
        frcDtoC.setStrField1(frcDtoB.getStr1());
        System.out.println(frcDtoC);
    }

    private void test7() {
        FRCDtoA frcDtoA = genFRCDtoA();
        FRCDtoC frcDtoC = new FRCDtoC();
        FRCDtoB frcDtoB = (FRCDtoB) frcDtoA;
        frcDtoC.setiField1(frcDtoB.getiField1());
        frcDtoC.setStrField1(frcDtoB.getStr1());
        System.out.println(frcDtoC);
    }
}

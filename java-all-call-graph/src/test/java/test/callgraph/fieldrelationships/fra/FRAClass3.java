package test.callgraph.fieldrelationships.fra;

import java.lang.reflect.InvocationTargetException;

/**
 * @author adrninistrator
 * @date 2023/7/22
 * @description:
 */
public class FRAClass3 {

    public void test1() {
        FRADtoB fraDtoB = new FRADtoB();
        FRADtoC fraDtoC = new FRADtoC();
        fraDtoC.setI1(fraDtoB.getIntB1());
        fraDtoC.setL1(fraDtoB.getLongB1());
        fraDtoC.setStr1(fraDtoB.getStrFieldB1());
        fraDtoC.setStr2OtherName(fraDtoB.getStr2());
    }

    public void test2() throws InvocationTargetException, IllegalAccessException {
        FRADtoA fraDtoA = new FRADtoA();
        FRADtoB fraDtoB = new FRADtoB();
        org.springframework.beans.BeanUtils.copyProperties(fraDtoA, fraDtoB);
        org.apache.commons.beanutils.BeanUtils.copyProperties(fraDtoB, fraDtoA);
    }

    public void test3() throws InvocationTargetException, IllegalAccessException {
        FRADtoA fraDtoA = new FRADtoA();
        FRADtoC fraDtoC = new FRADtoC();
        org.springframework.beans.BeanUtils.copyProperties(fraDtoA, fraDtoC);
        org.apache.commons.beanutils.BeanUtils.copyProperties(fraDtoC, fraDtoA);
    }

    public void test4() throws InvocationTargetException, IllegalAccessException {
        FRADtoB fraDtoB = new FRADtoB();
        FRADtoC fraDtoC = new FRADtoC();
        org.springframework.beans.BeanUtils.copyProperties(fraDtoB, fraDtoC);
        org.apache.commons.beanutils.BeanUtils.copyProperties(fraDtoC, fraDtoB);
    }
}

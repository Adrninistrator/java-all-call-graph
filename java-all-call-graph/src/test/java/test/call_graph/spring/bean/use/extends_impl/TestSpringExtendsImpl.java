package test.call_graph.spring.bean.use.extends_impl;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import test.call_graph.spring.bean.define.extends_impl.SpEI_CCC_SuperAClass1;
import test.call_graph.spring.bean.define.extends_impl.SpEI_C_Class1;
import test.call_graph.spring.bean.define.extends_impl.SpEI_ICC_Interface1;
import test.call_graph.spring.bean.define.extends_impl.SpEI_IC_Interface1;
import test.call_graph.spring.bean.define.extends_impl.SpEI_IIICC_SuperBAInterface1;
import test.call_graph.spring.bean.define.extends_impl.SpEI_IIICC_SuperBInterface1;

import javax.inject.Inject;

/**
 * @author adrninistrator
 * @date 2023/5/19
 * @description:
 */
@Service
public class TestSpringExtendsImpl {

    private static final Logger logger = LoggerFactory.getLogger(TestSpringExtendsImpl.class);

    @Autowired
    private SpEI_C_Class1 spEI_c_class1;

    @Inject
    private SpEI_IC_Interface1 spEI_IC_ImplClass1;

    @Autowired
    private SpEI_CCC_SuperAClass1 spEI_CCC_ChildClass1;

    @Inject
    private SpEI_ICC_Interface1 spEI_ICC_ChildClass1;

    @Autowired
    private SpEI_IIICC_SuperBInterface1 spEI_IIICC_ChildBAClass1;

    public void test() {
        spEI_c_class1.test();
        spEI_IC_ImplClass1.test();
        spEI_CCC_ChildClass1.test();
        spEI_ICC_ChildClass1.test();
        spEI_IIICC_ChildBAClass1.testB();

        logger.info("{}", spEI_c_class1.getClass().getName());
        logger.info("{}", spEI_IC_ImplClass1.getClass().getName());
        logger.info("{}", spEI_CCC_ChildClass1.getClass().getName());
        logger.info("{}", spEI_ICC_ChildClass1.getClass().getName());
        logger.info("{}", spEI_IIICC_ChildBAClass1.getClass().getName());
    }
}

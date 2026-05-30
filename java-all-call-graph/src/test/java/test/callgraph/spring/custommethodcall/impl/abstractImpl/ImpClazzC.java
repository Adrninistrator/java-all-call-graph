package test.callgraph.spring.custommethodcall.impl.abstractImpl;

import org.springframework.stereotype.Service;
import test.callgraph.spring.custommethodcall.service.AbstractClazz;

@Service
public class ImpClazzC extends AbstractClazz {
    @Override
    protected int getA() {
        return 2;
    }

    @Override
    protected int step1(Object input) {
        return 0;
    }

    @Override
    protected int step2(Object input) {
        return 0;
    }

    @Override
    protected int step3(Object input) {
        return 0;
    }
}

package test.callgraph.spring.custommethodcall.service;

import java.util.Collections;
import java.util.Map;

public abstract class AbstractClazz implements Itrade{
    protected abstract int getA();

    @Override
    public Map<String, Object> execute(Object input) {
        log("begin");
        int a = step1(input);
        int b = step2(input);
        int c = step3(input);
        log("end");
        return Collections.singletonMap("result", getA()+a+b+c);
    }

    private void log(String msg) {
        System.out.println(msg);
    }

    protected abstract int step1(Object input);
    protected abstract int step2(Object input);
    protected abstract int step3(Object input);
}

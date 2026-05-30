package test.callgraph.spring.custommethodcall.router;

import test.callgraph.spring.custommethodcall.service.MyInvoker;
import test.callgraph.spring.custommethodcall.util.SpringUtil;

import java.util.Map;

public class RouteClazz {
    private String tradeName;

    private Object input;

    private boolean asyInvoke;

    public RouteClazz(String tradeName, Object input) {
        this.tradeName = tradeName;
        this.input = input;
    }

    public void setAsyInvoke(boolean asyInvoke) {
        this.asyInvoke = asyInvoke;
    }

    public Map<String, Object> invoke() {
        MyInvoker invoker = getMyInvoker();
        return invoker.invoker(input, tradeName);
    }

    private MyInvoker getMyInvoker() {
        MyInvoker invoker = null;
        if (asyInvoke) {
            invoker = SpringUtil.getBean("invoker1");
        } else {
            invoker = SpringUtil.getBean("invoker2");
        }
        return invoker;
    }
}

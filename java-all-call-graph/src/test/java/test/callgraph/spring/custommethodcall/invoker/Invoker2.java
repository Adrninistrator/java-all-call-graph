package test.callgraph.spring.custommethodcall.invoker;

import org.springframework.stereotype.Service;
import test.callgraph.spring.custommethodcall.service.Itrade;
import test.callgraph.spring.custommethodcall.service.MyInvoker;
import test.callgraph.spring.custommethodcall.util.SpringUtil;

import java.util.Map;

@Service
public class Invoker2 implements MyInvoker {
    @Override
    public Map<String, Object> invoker(Object input, String tadeName) {
        Itrade clazz =  SpringUtil.getBean(tadeName);
        return clazz.execute(input);
    }

    private String tradeName;

    public String getTradeName() {
        return tradeName;
    }

    public void setTradeName(String tradeName) {
        this.tradeName = tradeName;
    }
}

package test.callgraph.spring.custommethodcall.impl.interfaceImpl;

import org.springframework.stereotype.Service;
import test.callgraph.spring.custommethodcall.service.Itrade;

import java.util.Collections;
import java.util.Map;

@Service("ImpClazzE")
public class ImpClazzE implements Itrade {
    @Override
    public Map<String, Object> execute(Object input) {
        Map<String,Object> map = (Map<String,Object>)input;
        double result = getSquareRoot(Integer.parseInt(map.get("n1").toString())) + getSquareRoot(Integer.parseInt(map.get("n2").toString()));
        return Collections.singletonMap("result", result);
    }

    public double getSquareRoot(int a) {
        if (a < 0) {
            throw new IllegalArgumentException("不能对负数求平方根: " + a);
        }
        return Math.sqrt(a);
    }
    
}

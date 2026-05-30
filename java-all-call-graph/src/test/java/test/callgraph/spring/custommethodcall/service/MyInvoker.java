package test.callgraph.spring.custommethodcall.service;

import java.util.Map;

public interface MyInvoker {
    Map<String, Object> invoker(Object input, String tadeName);
}

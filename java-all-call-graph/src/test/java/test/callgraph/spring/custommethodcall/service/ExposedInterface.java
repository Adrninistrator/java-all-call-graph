package test.callgraph.spring.custommethodcall.service;

import java.util.Map;

public interface ExposedInterface {
    Map<String, Object> execute(Map<String, Object> map);
}

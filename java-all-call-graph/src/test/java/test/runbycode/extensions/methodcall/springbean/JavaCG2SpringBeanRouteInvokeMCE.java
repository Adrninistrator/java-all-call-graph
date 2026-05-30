package test.runbycode.extensions.methodcall.springbean;

import com.adrninistrator.jacg.extensions.methodcall.JavaCG2ObjInstanceMethodNameMCE;
import test.callgraph.spring.custommethodcall.router.RouteClazz;

/**
 * @author adrninistrator
 * @date 2025/5/25
 * @description: java-callgraph2 组件方法调用处理扩展类
 * 识别 RouteClazz.invoke() 方法调用，创建占位方法调用记录
 * 当找到 RouteClazz.invoke() 方法调用时，获得对应被调用对象的构造函数中的参数1的值（Spring Bean名称），
 * 通过该值获得对应的Spring Bean对应的类名进行替换，得到调用对应类的.execute()方法的调用关系
 */
public class JavaCG2SpringBeanRouteInvokeMCE extends JavaCG2ObjInstanceMethodNameMCE {

    public static final String CALL_TYPE = "spring_bean_route_invoke";

    @Override
    protected boolean checkHandleCalleeMethod(String calleeClassName, String calleeMethodName, String calleeMethodArgTypes) {
        return RouteClazz.class.getName().equals(calleeClassName) &&
                "invoke".equals(calleeMethodName) &&
                "()".equals(calleeMethodArgTypes);
    }

    @Override
    protected String chooseMethodCallType() {
        return CALL_TYPE;
    }
}

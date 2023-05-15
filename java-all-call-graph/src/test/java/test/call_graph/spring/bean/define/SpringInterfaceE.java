package test.call_graph.spring.bean.define;

/**
 * @author adrninistrator
 * @date 2023/4/26
 * @description:
 */
public interface SpringInterfaceE {
    /*
        当前接口存在两个实现类时，可以通过@Autowired、@Resource注解注入对应的Bean，不需要指定name或type，字段名需要为类名首字母小写形式
     */
    void test1();
}

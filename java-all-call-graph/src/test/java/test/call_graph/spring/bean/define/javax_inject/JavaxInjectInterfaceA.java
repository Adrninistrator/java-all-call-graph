package test.call_graph.spring.bean.define.javax_inject;

/**
 * @author adrninistrator
 * @date 2023/4/26
 * @description:
 */
public interface JavaxInjectInterfaceA {
    /*
        @Named、@Inject注解配套使用
        @Named注解用于定义Bean
        @Inject注解用于注入Bean
        用法：
        - @Named注解若指定名称，则@Inject注解对应字段名应等于@Named注解的value
        - @Named注解若不指定名称，则@Inject注解对应字段名应等于@Named注解所在类名首字母小写
     */
    void test1();
}

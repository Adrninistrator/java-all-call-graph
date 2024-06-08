package test.callgraph.lambda;

import org.springframework.transaction.support.TransactionTemplate;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.function.Function;

/**
 * @author adrninistrator
 * @date 2022/4/7
 * @description:
 */
public class TestLambda {
    public void test() {
        Map<String, String> map = new HashMap<>();
        map.computeIfAbsent("1", k -> System.getProperty("1"));
    }

    public void testList() {
        List<String> list = new ArrayList<>();
        list.forEach(e -> testString(e));
    }

    private void testString(String data) {
        System.out.println(data);
    }

    public void test2() {
        Function f = o -> o;
    }

    public void runCallable() {
        useCallable(new Callable<String>() {
            @Override
            public String call() throws Exception {
                return System.getProperty("abc");
            }
        });

        useCallable(() -> "time" + System.currentTimeMillis());
    }

    public void useCallable(Callable<String> callable) {
        try {
            callable.call();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private String genString1() {
        return "";
    }

    public void testTransactionTemplateLambda(String flagLambda2) {
        String flagLambda1 = "aaa";
        String flagLambda3 = genString1();
        TransactionTemplate transactionTemplate = new TransactionTemplate();
        String valueOuter = transactionTemplate.execute(status -> {
            String value = System.getProperty(flagLambda1);
            String value2 = System.getProperty(flagLambda2);
            String value3 = System.getProperty(flagLambda3);
            System.out.println(value);
            System.out.println(value2);
            System.out.println(value3);
            return value;
        });
        System.out.println(valueOuter);
    }
}

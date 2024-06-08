package test.callgraph.innerclass;

import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;
import org.springframework.transaction.support.TransactionCallbackWithoutResult;
import org.springframework.transaction.support.TransactionTemplate;

/**
 * @author adrninistrator
 * @date 2023/8/8
 * @description:
 */
public class TestUseInnerClass {

    public TestInClass.TestInInnerData test1(TestInClass.TestInInnerData testInInnerData) {
        return testInInnerData;
    }

    public TestInClass.TestInInnerData.TestInInnerData2 test2(TestInClass.TestInInnerData.TestInInnerData2 testInInnerData2) {
        return testInInnerData2;
    }

    public void testTransactionTemplateInnerClass() {
        String flagLambda = "bbb";
        TransactionTemplate transactionTemplate = new TransactionTemplate();
        String valueOuter = transactionTemplate.execute(new TransactionCallback<String>() {
            @Override
            public String doInTransaction(TransactionStatus status) {
                String value = System.getProperty(flagLambda);
                System.out.println(value);
                return value;
            }
        });
        System.out.println(valueOuter);
    }

    public void testTransactionCallbackWithoutResultInnerClass() {
        String flagLambda = "bbb";
        TransactionTemplate transactionTemplate = new TransactionTemplate();
        transactionTemplate.execute(new TransactionCallbackWithoutResult() {
            @Override
            protected void doInTransactionWithoutResult(TransactionStatus status) {
                String value = System.getProperty(flagLambda);
                System.out.println(value);
            }
        });
    }
}

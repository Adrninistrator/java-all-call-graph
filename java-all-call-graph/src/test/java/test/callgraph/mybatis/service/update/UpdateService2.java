package test.callgraph.mybatis.service.update;

import org.springframework.stereotype.Service;
import test.callgraph.mybatis.dao.TestTableGjcusdMapper;
import test.callgraph.mybatis.dto.mq.send.SendReq1_1;
import test.callgraph.mybatis.dto.mq.send.SendReq2;
import test.callgraph.mybatis.entity.TestTableGjcusd;

/**
 * @author adrninistrator
 * @date 2023/11/4
 * @description:
 */
@Service
public class UpdateService2 {

    private TestTableGjcusdMapper testTableGjcusdMapper;

    public void updateInCurrentMethod1() {
        SendReq1_1 sendReq1_1 = new SendReq1_1();
        testTableGjcusdMapper.update1(sendReq1_1.getData(), sendReq1_1.getData2());
    }

    public void updateInCurrentMethod2() {
        SendReq1_1 sendReq1_1 = new SendReq1_1();
        TestTableGjcusd testTableGjcusd = new TestTableGjcusd();
        testTableGjcusd.setIdC(sendReq1_1.getData());
        testTableGjcusd.setFlag1C(sendReq1_1.getData2());
        testTableGjcusdMapper.update2(testTableGjcusd, sendReq1_1.getData3());
    }

    public void updateInCurrentMethod3() {
        SendReq2 sendReq2 = new SendReq2();
        SendReq1_1 sendReq1_1 = new SendReq1_1();
        sendReq2.setData(sendReq1_1.getData3());
        testTableGjcusdMapper.update3(sendReq1_1.getData(), sendReq2);
    }

    public void updateInCurrentMethod4() {
        SendReq2 sendReq2 = new SendReq2();
        SendReq1_1 sendReq1_1 = new SendReq1_1();
        sendReq2.setData(sendReq1_1.getData());
        sendReq2.setData1(sendReq1_1.getData2());
        testTableGjcusdMapper.update4(sendReq2);
    }

    public void updateInCurrentMethod5() {
        boolean flag = System.currentTimeMillis() % 7 == 1;
        SendReq2 sendReq2 = new SendReq2();
        SendReq1_1 sendReq1_1 = new SendReq1_1();
        sendReq2.setData(flag ? sendReq1_1.getData() : sendReq1_1.getData3());
        sendReq2.setData1(flag ? sendReq1_1.getData2() : sendReq1_1.getData3());
        testTableGjcusdMapper.update4(sendReq2);
    }

    public void updateInCurrentMethod6() {
        SendReq2 sendReq2 = new SendReq2();
        SendReq1_1 sendReq1_1 = new SendReq1_1();
        String data = sendReq1_1.getData();
        sendReq2.setData(data);
        sendReq2.setData1(data);
        testTableGjcusdMapper.update4(sendReq2);
    }

    public void updateInOtherMethod1Caller() {
        SendReq1_1 sendReq1_1 = new SendReq1_1();
        updateInOtherMethod1Callee1(sendReq1_1.getData(), sendReq1_1.getData2(), sendReq1_1.getData3());
    }

    public void updateInOtherMethod1Callee1(String data, String data2, String data3) {
        TestTableGjcusd testTableGjcusd = new TestTableGjcusd();
        testTableGjcusd.setIdC(data);
        testTableGjcusd.setFlag1C(data2);
        testTableGjcusdMapper.update2(testTableGjcusd, data3);
    }

    public void updateInOtherMethod1Callee2(String data, String data2, String data3) {
        TestTableGjcusd testTableGjcusd = new TestTableGjcusd();
        testTableGjcusd.setIdC(data);
        testTableGjcusd.setFlag1C(data2);
        testTableGjcusdMapper.update2(testTableGjcusd, data3);
    }

    public void updateInOtherMethod2Caller() {
        SendReq2 sendReq2 = new SendReq2();
        SendReq1_1 sendReq1_1 = new SendReq1_1();
        boolean flag = System.currentTimeMillis() % 7 == 1;
        String value1 = updateInOtherMethod2Callee(sendReq1_1, sendReq1_1.getData());
        sendReq2.setData(flag ? value1 : sendReq1_1.getData3());
        testTableGjcusdMapper.update3(sendReq1_1.getData(), sendReq2);
    }

    public String updateInOtherMethod2Callee(SendReq1_1 sendReq1_1, String data) {
        boolean flag = System.currentTimeMillis() % 7 == 1;
        return flag ? sendReq1_1.getData2() : data;
    }

    public void updateInOtherMethodCycleCaller() {
        SendReq2 sendReq2 = new SendReq2();
        SendReq1_1 sendReq1_1 = new SendReq1_1();
        int cycleTimes = 0;
        String value1 = updateInOtherMethodCycleCallee(sendReq1_1, cycleTimes);
        sendReq2.setData(value1);
        testTableGjcusdMapper.update3(sendReq1_1.getData(), sendReq2);
    }

    public String updateInOtherMethodCycleCallee(SendReq1_1 sendReq1_1, int cycleTimes) {
        if (cycleTimes > 3) {
            return sendReq1_1.getData2();
        }
        cycleTimes++;
        return updateInOtherMethodCycleCallee(sendReq1_1, cycleTimes);
    }
}

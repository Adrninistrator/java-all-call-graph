package test.callgraph.mybatis.service.insert;

import org.springframework.stereotype.Service;
import test.callgraph.mybatis.dao.TestTableGjcusdMapper;
import test.callgraph.mybatis.dto.mq.send.SendReq2;
import test.callgraph.mybatis.entity.TestTableGjcusd;

/**
 * @author adrninistrator
 * @date 2023/10/30
 * @description:
 */
@Service
public class InsertService1 {

    private TestTableGjcusdMapper testTableMapper;

    public void insertInCurrentMethodByNew() {
        SendReq2 sendReq2 = new SendReq2();
        TestTableGjcusd testTable = new TestTableGjcusd();
        testTable.setIdC(sendReq2.getData1());
        testTable.setFlag1C(sendReq2.getData2());

        testTableMapper.insert(testTable);
    }

    public void insertInCurrentMethodByNewObject() {
        SendReq2 sendReq2 = new SendReq2();
        TestTableGjcusd testTable = new TestTableGjcusd();
        testTable.setIdC(sendReq2.getData1());
        testTable.setFlag1C(sendReq2.getData2());

        testTableMapper.insertObject(testTable);
        testTable.setFlag1C(sendReq2.getData1());
    }

    private SendReq2 genSendReq2() {
        return new SendReq2();
    }

    private TestTableGjcusd genTestTableGjcusd() {
        return new TestTableGjcusd();
    }

    public void insertInCurrentMethodByMethod() {
        SendReq2 sendReq2 = genSendReq2();
        TestTableGjcusd testTable = genTestTableGjcusd();
        testTable.setIdC(sendReq2.getData1());
        testTable.setFlag1C(sendReq2.getData2());

        testTableMapper.insert(testTable);
    }

    public void insertInCurrentMethodByArgs(SendReq2 sendReq2, TestTableGjcusd testTable) {
        testTable.setIdC(sendReq2.getData1());
        testTable.setFlag1C(sendReq2.getData2());

        testTableMapper.insert(testTable);
    }

    public void insertInCurrentMethodByArgsMix(SendReq2 sendReq2, TestTableGjcusd testTable1, TestTableGjcusd testTable2) {
        testTable1.setIdC(sendReq2.getData1());
        testTable1.setFlag1C(sendReq2.getData2());

        testTable2.setIdC(sendReq2.getData1());
        testTable2.setFlag1C(sendReq2.getData2());

        TestTableGjcusd testTable3 = genTestTableGjcusd();
        testTable3.setIdC(sendReq2.getData1());
        testTable3.setFlag1C(sendReq2.getData2());

        TestTableGjcusd testTable4 = genTestTableGjcusd();
        testTable4.setIdC(sendReq2.getData1());
        testTable4.setFlag1C(sendReq2.getData2());

        int flag = (int) System.currentTimeMillis() % 5;
        TestTableGjcusd usedTestTableGjcusd = null;
        switch (flag) {
            case 0:
                usedTestTableGjcusd = testTable1;
                break;
            case 1:
                usedTestTableGjcusd = testTable2;
                break;
            case 2:
                usedTestTableGjcusd = testTable3;
                break;
            case 3:
                usedTestTableGjcusd = testTable4;
                break;
            default:
                break;
        }

        testTableMapper.insert(usedTestTableGjcusd);
    }
}

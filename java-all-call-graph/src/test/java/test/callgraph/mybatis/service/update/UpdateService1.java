package test.callgraph.mybatis.service.update;

import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import test.callgraph.mybatis.dao.TestTableGjcusdMapper;
import test.callgraph.mybatis.dto.mq.send.SendReq1_1;
import test.callgraph.mybatis.entity.TestTableGjcusd;

/**
 * @author adrninistrator
 * @date 2023/11/3
 * @description: update操作
 */
@Service
public class UpdateService1 {

    private TestTableGjcusdMapper testTableGjcusdMapper;

    public void updateInCurrentMethod1() {
        SendReq1_1 sendReq1_1 = new SendReq1_1();
        TestTableGjcusd testTableGjcusd = new TestTableGjcusd();
        testTableGjcusd.setIdC(StringUtils.trim(sendReq1_1.getData()));
        String data2 = sendReq1_1.getData2();
        testTableGjcusd.setFlag1C(data2);
        testTableGjcusdMapper.updateByPrimaryKeySelective(testTableGjcusd);
    }

    public void updateObjectInCurrentMethod1() {
        SendReq1_1 sendReq1_1 = new SendReq1_1();
        TestTableGjcusd testTableGjcusd = new TestTableGjcusd();
        testTableGjcusd.setIdC(sendReq1_1.getData());
        testTableGjcusd.setFlag1C(sendReq1_1.getData2());
        testTableGjcusdMapper.updateObject(testTableGjcusd);
    }
}

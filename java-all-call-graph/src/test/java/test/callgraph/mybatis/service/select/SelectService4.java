package test.callgraph.mybatis.service.select;

import com.adrninistrator.jacg.util.JACGJsonUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import test.callgraph.mybatis.dao.TestTableGjcusdMapper;
import test.callgraph.mybatis.dto.mq.send.SendReq1_1;
import test.callgraph.mybatis.dto.mq.send.SendReq1_2;
import test.callgraph.mybatis.dto.mq.send.SendReq1_2_1_1;
import test.callgraph.mybatis.dto.mq.send.SendReq2;
import test.callgraph.mybatis.dto.mq.send.SendRsp1;

/**
 * @author adrninistrator
 * @date 2023/10/23
 * @description: select语句对应的返回字段
 */
@Service
public class SelectService4 {
    private static final Logger logger = LoggerFactory.getLogger(SelectService4.class);

    private TestTableGjcusdMapper testTableGjcusdMapper;

    public void useInCurrentMethod1() {
        SendReq1_2 sendReq1_2 = new SendReq1_2();
        SendReq2 sendReq2 = new SendReq2();
        SendRsp1 sendRsp1 = testTableGjcusdMapper.selectDto1(sendReq1_2.getData(), sendReq2.getData());
        logger.info("{}", sendRsp1);
    }

    public void useInCurrentMethod2() {
        SendReq1_1 sendReq1_1 = new SendReq1_1();
        SendReq2 sendReq2 = new SendReq2();
        sendReq1_1.setData(sendReq2.getData());
        sendReq1_1.setData2(sendReq2.getData2());
        SendRsp1 sendRsp1 = testTableGjcusdMapper.selectByDto1(sendReq1_1);

        SendReq1_2_1_1 sendReq1_2_1_1 = new SendReq1_2_1_1();
        sendReq1_2_1_1.setData(sendRsp1.getId());
        sendReq1_2_1_1.setData2(sendRsp1.getValue());
        logger.info("{}", JACGJsonUtil.getJsonStr(sendReq1_2_1_1));
    }
}

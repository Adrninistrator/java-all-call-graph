package test.callgraph.mybatis.dao;

import org.apache.ibatis.annotations.Param;
import test.callgraph.mybatis.dto.mq.send.SendReq1_1;
import test.callgraph.mybatis.dto.mq.send.SendReq2;
import test.callgraph.mybatis.dto.mq.send.SendRsp1;
import test.callgraph.mybatis.entity.TestTableGjcusd;

import java.util.List;

public interface TestTableGjcusdMapper extends BaseMapper<TestTableGjcusd> {
    int deleteByPrimaryKey(String idC);

    int insert(TestTableGjcusd record);

    int insertSelective(TestTableGjcusd record);

    TestTableGjcusd selectByPrimaryKey(String idC);

    int updateByPrimaryKeySelective(TestTableGjcusd record);

    int updateByPrimaryKey(TestTableGjcusd record);

    //
    TestTableGjcusd selectByPrimaryKey2(String idC);

    int deleteByOtherTable(String idC);

    String selectString1(String idC);

    String selectString2(@Param("idC") String idC);

    SendRsp1 selectDto1(@Param("idC") String idC, @Param("flag1C") String flag1C);

    SendRsp1 selectDto2(String idC, String flag1C);

    SendRsp1 selectByDto1(SendReq1_1 record);

    TestTableGjcusd selectByOtherTable(String idC);

    TestTableGjcusd selectUseParam(@Param("record") TestTableGjcusd record);

    TestTableGjcusd selectUseDftName(TestTableGjcusd record);

    List<String> selectIdList(String flag1C);

    List<SendRsp1> selectDtoList(String flag1C);

    int update1(@Param("idC") String idC, @Param("flag1C") String flag1C);

    int update2(@Param("record") TestTableGjcusd record, @Param("newFlag1C") String newFlag1C);

    int update3(@Param("idC") String idC, @Param("other") SendReq2 sendReq2);

    int update4(SendReq2 record);

    int update5(TestTableGjcusd record, String newFlag1C);

    int insert1(String idC);

    int replaceInsert1(TestTableGjcusd record);

    int insertList1(List<TestTableGjcusd> list);

    int insertList2(@Param("recordList") List<TestTableGjcusd> list);

    int insertList3(@Param("recordList") List<TestTableGjcusd> list);

    int insertList4(List<TestTableGjcusd> list);
}
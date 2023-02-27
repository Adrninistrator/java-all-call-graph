package test.other;

import com.adrninistrator.jacg.util.JACGJsonUtil;
import com.fasterxml.jackson.core.type.TypeReference;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2022/8/28
 * @description:
 */
public class TestJSON {
    private static final Logger logger = LoggerFactory.getLogger(TestJSON.class);

    @Test
    public void test1() {
        String json = "[\"1\",\"2\",\"3\",\"4\"]";
        List<String> list = JACGJsonUtil.getObjFromJsonStr(json, new TypeReference<List<String>>() {
        });
        logger.info("{}", JACGJsonUtil.getJsonStr(list));
    }

    @Test
    public void test2() {
        String json = "{\"valueB\":\"vb1\",\"valueA\":\"va1\"}";
        Map<String, Object> map = JACGJsonUtil.getObjFromJsonStr(json, new TypeReference<Map<String, Object>>() {
        });
        logger.info("{}", JACGJsonUtil.getJsonStr(map));
    }

    @Test
    public void test3() {
        String json = "[{\"valueB\":\"vb1\\r\\n\",\"valueA\":\"va1\"},{\"valueB\":\"va2\",\"valueA\":\"va2\"}]";
        List<Map<String, Object>> list = JACGJsonUtil.getObjFromJsonStr(json, new TypeReference<List<Map<String, Object>>>() {
        });
        logger.info("{}", JACGJsonUtil.getJsonStr(list));
    }

    @Test
    public void test4() {
        String json = "[{\"value\":\"aaa\",\"annotations\":[{\"valueB\":\"va1\",\"valueA\":\"va1\"},{\"valueB\":\"va2\\r\\n\",\"valueA\":\"va2\"}]},{\"value\":\"bbb\"," +
                "\"annotations\":[{\"valueB\":\"vb1\",\"valueA\":\"vb1\"},{\"valueB\":\"vb2\",\"valueA\":\"vb2\"}]}]";
        List<Map<String, Object>> list = JACGJsonUtil.getObjFromJsonStr(json, new TypeReference<List<Map<String, Object>>>() {
        });
        logger.info("{}", JACGJsonUtil.getJsonStr(list));
    }
}

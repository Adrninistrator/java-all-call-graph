package test.callgraph.error;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.callgraph.error.fake.Jedis;
import test.callgraph.error.fake.Pipeline;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2024/11/3
 * @description:
 */
public class TestLoop1 {
    private static final Logger logger = LoggerFactory.getLogger(TestLoop1.class);

    private static HashMap<String, String> token_tax_map = null;
    private static HashMap<String, String> tax_token_map = null;
    private static int ct_repeat = 0;
    private static int ct_error = 0;

    private void cache(String redis_info) {
        long begin = System.currentTimeMillis();
        Jedis jedis = null;

        try {
            String host = redis_info.split(",")[0].split(":")[0];
            int port = Integer.parseInt(redis_info.split(",")[0].split(":")[1]);
            String password = redis_info.split(",")[1];
            jedis = new Jedis(host, port);
            jedis.auth(password);
            Set<String> keys = jedis.keys("*");
            int i = 0;
            Pipeline p = jedis.pipelined();
            List<Object> result = new ArrayList(100000);
            Iterator var12 = keys.iterator();

            String key;
            while (var12.hasNext()) {
                key = (String) var12.next();

                try {
                    p.hget(key, "context");
                    if (i == 10000) {
                        result.addAll(p.syncAndReturnAll());
                        i = 0;
                    }

                    ++i;
                } catch (Exception var22) {
                }
            }

            result.addAll(p.syncAndReturnAll());
            logger.info("key size : " + keys.size());
            logger.info("result size : " + result.size());
            i = 0;
            var12 = keys.iterator();

            while (var12.hasNext()) {
                key = (String) var12.next();
                String value = result.get(i++) + "";

                try {
                    value = value.substring(value.indexOf("t_p_id\":\""));
                    value = value.substring(15, value.indexOf("\",\""));
                    if (tax_token_map.containsKey(value)) {
                        ++ct_repeat;
                    }

                    tax_token_map.put(value, key);
                    token_tax_map.put(key, value);
                } catch (Exception var21) {
                    ++ct_repeat;
                }
            }
        } catch (Exception var23) {
            Exception e = var23;
            logger.error("cache异常", e);
        } finally {
            if (jedis != null) {
                jedis.close();
            }

        }

        long end = System.currentTimeMillis();
        logger.info("共缓存了" + tax_token_map.size() + "条数据，cache 耗时:" + (end - begin) + "ms");
    }
}

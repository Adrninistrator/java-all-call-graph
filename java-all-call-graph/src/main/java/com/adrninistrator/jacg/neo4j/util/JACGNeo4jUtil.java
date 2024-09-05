package com.adrninistrator.jacg.neo4j.util;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.util.JACGUtil;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.neo4j.repository.Neo4jRepository;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2024/7/25
 * @description:
 */
public class JACGNeo4jUtil {

    private static final Logger logger = LoggerFactory.getLogger(JACGNeo4jUtil.class);

    // 批量写入数据耗时大于以下时间时，在日志中记录
    public static final long SAVE_ALL_LOG_TIME = 1000L;

    /**
     * 公共的批量写入数据方法
     *
     * @param repository
     * @param list
     * @return
     */
    @SuppressWarnings("unchecked")
    public static Iterable<?> saveAll(Neo4jRepository repository, List<?> list) {
        long startTime = System.currentTimeMillis();
        Iterable<?> result = repository.saveAll(list);
        long spendTime = System.currentTimeMillis() - startTime;
        if (spendTime > SAVE_ALL_LOG_TIME) {
            logger.info("{} 写入数据数量 {} 耗时 {} 秒", list.get(0).getClass().getSimpleName(), list.size(), JACGUtil.getSecondsFromMilli(spendTime));
        }
        return result;
    }

    /**
     * 格式化索引中的属性名称
     *
     * @param properties
     * @return
     */
    public static String formatPropertiesInIndex(String[] properties) {
        return StringUtils.join(properties, JACGConstants.FLAG_COMMA_WITH_SPACE);
    }

    /**
     * 生成索引名称
     *
     * @param nodeName
     * @param indexSeq
     * @return
     */
    public static String genIndexName(String nodeName, int indexSeq) {
        return "idx_" + nodeName + JACGConstants.FLAG_UNDER_LINE + indexSeq;
    }

    /**
     * 获取索引名称中的序号
     *
     * @param indexName
     * @return
     */
    public static Integer getIndexSeq(String indexName) {
        // 获得索引名称中最后一个下划线后的内容
        String indexNameTail = StringUtils.substringAfterLast(indexName, JACGConstants.FLAG_UNDER_LINE);
        if (!JavaCG2Util.isNumStr(indexNameTail)) {
            return null;
        }
        return Integer.valueOf(indexNameTail);
    }

    private JACGNeo4jUtil() {
        throw new IllegalStateException("illegal");
    }
}

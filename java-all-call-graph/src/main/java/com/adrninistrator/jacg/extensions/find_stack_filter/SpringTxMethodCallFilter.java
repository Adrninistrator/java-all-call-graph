package com.adrninistrator.jacg.extensions.find_stack_filter;

import com.adrninistrator.jacg.common.JACGCommonNameConstants;
import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.util.JACGCallGraphFileUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import org.apache.commons.lang3.StringUtils;

/**
 * @author adrninistrator
 * @date 2023/2/20
 * @description: 搜索Spring事务调用过滤器扩展类
 */
public class SpringTxMethodCallFilter implements FindStackKeywordFilterInterface {
    private static final String TRANSACTION_TEMPLATE_EXECUTE_METHOD =
            JACGCommonNameConstants.SPRING_TRANSACTION_TEMPLATE_CLASS + JavaCGConstants.FLAG_COLON + "execute" + JavaCGConstants.FLAG_LEFT_BRACKET;
    private static final String TRANSACTIONAL_ANNOTATION = JACGConstants.FLAG_AT + JACGCommonNameConstants.SPRING_TX_ANNOTATION;


    @Override
    public boolean filterByLine() {
        return true;
    }

    @Override
    public boolean filter(String line) {
        // 使用Spring事务模板
        if (StringUtils.contains(line, TRANSACTION_TEMPLATE_EXECUTE_METHOD)) {
            return true;
        }

        if (!StringUtils.contains(line, TRANSACTIONAL_ANNOTATION)) {
            return false;
        }
        // 使用Spring事务注解，若当前行的方法调用层级不是0，则需要处理
        return !JACGCallGraphFileUtil.isCallGraphLevel0(line);
    }
}

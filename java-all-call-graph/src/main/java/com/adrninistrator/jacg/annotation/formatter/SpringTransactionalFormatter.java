package com.adrninistrator.jacg.annotation.formatter;

import com.adrninistrator.jacg.common.JACGCommonNameConstants;
import com.adrninistrator.jacg.dto.annotation.BaseAnnotationAttribute;
import com.adrninistrator.jacg.dto.annotation.StringAnnotationAttribute;
import com.adrninistrator.javacg.common.JavaCGConstants;

import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/2/16
 * @description:
 */
public class SpringTransactionalFormatter extends AbstractAnnotationFormatter {
    @Override
    public boolean checkHandleAnnotation(String annotationName) {
        return JACGCommonNameConstants.SPRING_TX_ANNOTATION.equals(annotationName);
    }

    @Override
    public String handleAnnotation(String fullMethod, String className, String annotationName, Map<String, BaseAnnotationAttribute> attributesMap) {
        StringAnnotationAttribute stringAnnotationAttribute = annotationHandler.queryAttribute4MethodAnnotation(fullMethod,
                JACGCommonNameConstants.SPRING_TX_ANNOTATION,
                JACGCommonNameConstants.SPRING_TX_ATTRIBUTE_PROPAGATION,
                StringAnnotationAttribute.class);
        if (stringAnnotationAttribute == null) {
            // @Transactional注解未指定propagation时，直接返回
            return annotationName;
        }

        // 返回注解类名(propagation=xxx)
        return annotationName + JavaCGConstants.FLAG_LEFT_BRACKET + JACGCommonNameConstants.SPRING_TX_ATTRIBUTE_PROPAGATION + JavaCGConstants.FLAG_EQUAL
                + stringAnnotationAttribute.getAttributeString() + JavaCGConstants.FLAG_RIGHT_BRACKET;
    }
}

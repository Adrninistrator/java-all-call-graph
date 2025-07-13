package com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.util.JACGXmlUtil;
import com.adrninistrator.javacg2.common.JavaCG2Constants;
import com.adrninistrator.javacg2.common.enums.JavaCG2YesNoEnum;
import com.adrninistrator.javacg2.extensions.codeparser.AbstractSaveData2FileParser;
import com.adrninistrator.javacg2.util.JavaCG2Util;
import org.apache.commons.lang3.StringUtils;
import org.jdom2.Element;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ConfigurableApplicationContext;

import java.io.IOException;
import java.io.InputStream;

/**
 * @author adrninistrator
 * @date 2023/1/2
 * @description: 获取XML中定义的Spring信息，包括定时任务、AOP信息，但不获取Bean信息
 */
public class SpringXmlCodeParser extends AbstractSaveData2FileParser {
    private static final Logger logger = LoggerFactory.getLogger(SpringXmlCodeParser.class);

    public static final String FILE_NAME_SPRING_TASK_XML = "spring_task_xml";
    public static final String FILE_NAME_SPRING_AOP_POINTCUT_XML = "spring_aop_pointcut_xml";
    public static final String FILE_NAME_SPRING_AOP_ASPECT_XML = "spring_aop_aspect_xml";
    public static final String FILE_NAME_SPRING_AOP_ADVICE_XML = "spring_aop_advice_xml";
    public static final String FILE_NAME_SPRING_SCAN_PACKAGE_XML = "spring_scan_package_xml";

    public static final String SPRING_AOP_ADVICE_BEFORE = "aop:before";
    public static final String SPRING_AOP_ADVICE_AFTER = "aop:after";
    public static final String SPRING_AOP_ADVICE_AROUND = "aop:around";
    public static final String SPRING_AOP_ADVICE_AFTER_RETURNING = "aop:after-returning";
    public static final String SPRING_AOP_ADVICE_AFTER_THROWING = "aop:after-throwing";

    public static final String[] SPRING_AOP_ADVICES = new String[]{
            SPRING_AOP_ADVICE_BEFORE,
            SPRING_AOP_ADVICE_AFTER,
            SPRING_AOP_ADVICE_AROUND,
            SPRING_AOP_ADVICE_AFTER_RETURNING,
            SPRING_AOP_ADVICE_AFTER_THROWING
    };

    @Override
    public String[] chooseFileNames() {
        return new String[]{FILE_NAME_SPRING_TASK_XML,
                FILE_NAME_SPRING_AOP_POINTCUT_XML,
                FILE_NAME_SPRING_AOP_ASPECT_XML,
                FILE_NAME_SPRING_AOP_ADVICE_XML,
                FILE_NAME_SPRING_SCAN_PACKAGE_XML};
    }

    // 指定需要处理xml文件
    @Override
    public String[] chooseJarEntryOtherFileExt() {
        return new String[]{JACGConstants.EXT_XML};
    }

    // 处理.xml文件
    @Override
    public boolean parseJarEntryOtherFile(InputStream inputStream, String jarEntryPath, String jarEntryName) {
        Element root;
        try {
            root = JACGXmlUtil.parseXmlRootElement(inputStream);
        } catch (Exception e) {
            // 解析XML文件出错时不退出执行
            logger.warn("解析XML文件出错 {} {}", jarEntryPath, e.getMessage());
            return true;
        }

        try {
            if (!"beans".equals(root.getName())) {
                logger.debug("跳过非Spring XML 1: {}", jarEntryPath);
                return true;
            }

            logger.info("处理Spring XML文件 {}", jarEntryPath);
            for (Element element : root.getChildren()) {
                String elementQualifiedName = element.getQualifiedName();
                if ("task:scheduled-tasks".equals(elementQualifiedName)) {
                    handleSpringTask(element, jarEntryPath);
                } else if ("aop:config".equals(elementQualifiedName)) {
                    handleSpringAop(element, jarEntryPath);
                } else if ("context:component-scan".equals(elementQualifiedName)) {
                    handleSpringComponentScan(element, jarEntryPath);
                }
            }
            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    private void handleSpringTask(Element element, String jarEntryPath) throws IOException {
        for (Element element2 : element.getChildren()) {
            if ("task:scheduled".equals(element2.getQualifiedName())) {
                String beanName = element2.getAttributeValue("ref");
                String methodName = element2.getAttributeValue("method");
                writeData2Files(FILE_NAME_SPRING_TASK_XML, beanName, methodName, jarEntryPath);
            }
        }
    }

    private void handleSpringAop(Element element, String jarEntryPath) throws IOException {
        for (Element element2 : element.getChildren()) {
            String elementQualifiedName2 = element2.getQualifiedName();
            if ("aop:pointcut".equals(elementQualifiedName2)) {
                String pointcutId = element2.getAttributeValue("id");
                String expression = element2.getAttributeValue("expression");
                writePointcut(pointcutId, expression, jarEntryPath);
            } else if ("aop:aspect".equals(elementQualifiedName2)) {
                String aspectId = element2.getAttributeValue("id");
                String ref = element2.getAttributeValue("ref");
                String order = element2.getAttributeValue("order", String.valueOf(Integer.MAX_VALUE));
                writeData2Files(FILE_NAME_SPRING_AOP_ASPECT_XML, aspectId, ref, order, jarEntryPath);

                for (Element element3 : element2.getChildren()) {
                    String elementQualifiedName3 = element3.getQualifiedName();
                    if ("aop:pointcut".equals(elementQualifiedName3)) {
                        String pointcutId = element3.getAttributeValue("id");
                        String expression = element3.getAttributeValue("expression");
                        writePointcut(pointcutId, expression, jarEntryPath);
                    } else if (StringUtils.equalsAny(elementQualifiedName3, SPRING_AOP_ADVICES)) {
                        String method = element3.getAttributeValue("method");
                        String pointcut = element3.getAttributeValue("pointcut", "");
                        String pointcutRef = element3.getAttributeValue("pointcut-ref", "");
                        JavaCG2YesNoEnum base64 = JavaCG2YesNoEnum.NO;
                        if (StringUtils.isNotBlank(pointcut) && JavaCG2Util.checkNeedBase64(pointcut)) {
                            base64 = JavaCG2YesNoEnum.YES;
                            pointcut = JavaCG2Util.base64Encode(pointcut);
                        }
                        writeData2Files(FILE_NAME_SPRING_AOP_ADVICE_XML, aspectId, method, element3.getQualifiedName(), pointcutRef, base64.getStrValue(), pointcut,
                                order, jarEntryPath);
                    }
                }
            }
        }
    }

    private void writePointcut(String pointcutId, String expression, String jarEntryPath) throws IOException {
        JavaCG2YesNoEnum base64 = JavaCG2YesNoEnum.NO;
        if (JavaCG2Util.checkNeedBase64(expression)) {
            base64 = JavaCG2YesNoEnum.YES;
            expression = JavaCG2Util.base64Encode(expression);
        }
        writeData2Files(FILE_NAME_SPRING_AOP_POINTCUT_XML, pointcutId, base64.getStrValue(), expression, jarEntryPath);
    }

    private void handleSpringComponentScan(Element element, String jarEntryPath) throws IOException {
        String basePackageStr = element.getAttributeValue("base-package");
        /*
            org.springframework.context.annotation.ComponentScanBeanDefinitionParser.parse
            以上方法中会调用 StringUtils.tokenizeToStringArray 方法
         */
        String[] basePackages = org.springframework.util.StringUtils.tokenizeToStringArray(basePackageStr, ConfigurableApplicationContext.CONFIG_LOCATION_DELIMITERS);
        for (int i = 0; i < basePackages.length; i++) {
            writeData2Files(FILE_NAME_SPRING_SCAN_PACKAGE_XML, JavaCG2Constants.FILE_KEY_SPRING_DEFINE_IN_XML, String.valueOf(i), basePackages[i], jarEntryPath);
        }
    }
}

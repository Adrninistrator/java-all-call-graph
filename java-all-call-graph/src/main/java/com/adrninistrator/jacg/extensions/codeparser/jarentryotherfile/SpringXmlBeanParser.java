package com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.util.JACGXmlUtil;
import com.adrninistrator.javacg2.dto.spring.SpringBeanInXml;
import com.adrninistrator.javacg2.extensions.codeparser.SpringXmlBeanParserInterface;
import org.apache.commons.lang3.StringUtils;
import org.jdom2.Element;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author adrninistrator
 * @date 2023/3/7
 * @description: 对Spring XML文件中的bean解析的类
 */
public class SpringXmlBeanParser implements SpringXmlBeanParserInterface {
    private static final Logger logger = LoggerFactory.getLogger(SpringXmlBeanParser.class);

    /*
        Spring Bean对应的Map
        key Bean名称
        value Bean的类名
     */
    private Map<String, String> beanMap;

    // XML文件中定义的Spring Bean信息
    private List<SpringBeanInXml> springBeanInXmlList;

    @Override
    public void initCodeParser() {
        beanMap = new HashMap<>();
        springBeanInXmlList = new ArrayList<>();
    }

    @Override
    public String getBeanClass(String beanId) {
        return beanMap.get(beanId);
    }

    @Override
    public List<SpringBeanInXml> getBeanInXml() {
        return springBeanInXmlList;
    }

    @Override
    public String[] chooseJarEntryOtherFileExt() {
        return new String[]{JACGConstants.EXT_XML};
    }

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
                return true;
            }

            logger.info("处理Spring XML文件 {}", jarEntryPath);
            for (Element element : root.getChildren()) {
                if (!"bean".equals(element.getQualifiedName())) {
                    continue;
                }

                String beanId = element.getAttributeValue("id");
                String beanClass = element.getAttributeValue("class");
                if (StringUtils.isNoneBlank(beanId, beanClass)) {
                    // Spring XML文件中定义的bean可不指定id属性，这种情况下不处理
                    beanMap.put(beanId, beanClass);
                    SpringBeanInXml springBeanInXml = new SpringBeanInXml();
                    springBeanInXml.setSpringBeanName(beanId);
                    springBeanInXml.setClassName(beanClass);
                    springBeanInXml.setXmlFilePath(jarEntryPath);
                    springBeanInXmlList.add(springBeanInXml);
                }
            }
            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }
}

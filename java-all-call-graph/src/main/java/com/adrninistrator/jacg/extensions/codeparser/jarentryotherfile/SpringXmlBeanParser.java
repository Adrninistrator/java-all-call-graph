package com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.util.JACGXmlUtil;
import com.adrninistrator.javacg2.extensions.codeparser.SpringXmlBeanParserInterface;
import org.apache.commons.lang3.StringUtils;
import org.jdom2.Element;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.InputStream;
import java.util.HashMap;
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

    @Override
    public void initCodeParser() {
        beanMap = new HashMap<>();
    }

    @Override
    public String getBeanClass(String beanId) {
        return beanMap.get(beanId);
    }

    @Override
    public Map<String, String> getBeanMap() {
        return beanMap;
    }

    @Override
    public String[] chooseJarEntryOtherFileExt() {
        return new String[]{JACGConstants.EXT_XML};
    }

    @Override
    public void parseJarEntryOtherFile(InputStream inputStream, String jarEntryPath, String jarEntryName) {
        try {
            Element root = JACGXmlUtil.parseXmlRootElement(inputStream);
            if (!"beans".equals(root.getName())) {
                return;
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
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}

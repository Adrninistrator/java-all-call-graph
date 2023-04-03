package com.adrninistrator.jacg.extensions.code_parser.jar_entry_other_file;

import com.adrninistrator.jacg.util.JACGXmlUtil;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.extensions.code_parser.SpringXmlBeanParserInterface;
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
    public String[] chooseJarEntryOtherFileExt() {
        return JavaCGConstants.FILE_EXT_ARRAY_XML;
    }

    @Override
    public void parseJarEntryOtherFile(InputStream inputStream, String jarEntryName) {
        try {
            Element root = JACGXmlUtil.parseXmlRootElement(inputStream);
            if (!"beans".equals(root.getName())) {
                return;
            }

            logger.info("处理Spring XML文件 {}", jarEntryName);
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

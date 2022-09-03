package com.adrninistrator.jacg.other.spring;

import com.adrninistrator.jacg.xml.NoOpEntityResolver;
import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 * @author adrninistrator
 * @date 2022/8/30
 * @description: 获取Spring XML中定义的定时任务
 */
public class SpringXmlTaskHandler {
    private static final Logger logger = LoggerFactory.getLogger(SpringXmlTaskHandler.class);

    /**
     * 获取Spring XML中定义的定时任务
     *
     * @param xmlFilePath XML文件路径
     * @param taskMap     保存
     * @return
     */
    public static boolean getSpringTaskInXml(String xmlFilePath, Map<String, Set<String>> taskMap) {
        File file = new File(xmlFilePath);
        if (!file.exists() || !file.isFile()) {
            logger.error("文件不存在或不是文件 {}", xmlFilePath);
            return false;
        }

        if (taskMap == null) {
            logger.error("传入参数不允许为空");
            return false;
        }

        try (InputStream inputStream = new FileInputStream(xmlFilePath)) {
            // 处理Spring XML中定义的定时任务
            handleSpringTaskInXml(xmlFilePath, inputStream, taskMap);
            return true;
        } catch (Exception e) {
            logger.error("error ", e);
            return false;
        }
    }

    // 处理Spring XML中定义的定时任务
    private static void handleSpringTaskInXml(String xmlFilePath, InputStream inputStream, Map<String, Set<String>> taskMap) throws DocumentException {
        SAXReader reader = new SAXReader();
        // 不读取DTD
        reader.setEntityResolver(new NoOpEntityResolver());

        Document document = reader.read(inputStream);

        Element root = document.getRootElement();
        if (!"beans".equals(root.getName())) {
            logger.info("跳过非Spring XML 1: {}", xmlFilePath);
            return;
        }

        // 以上用于跳过非Spring的XML文件
        logger.info("开始处理Spring XML: {}", xmlFilePath);

        for (Iterator it = root.elementIterator(); it.hasNext(); ) {
            Element element = (Element) it.next();
            if (!"task:scheduled-tasks".equals(element.getQName().getQualifiedName())) {
                continue;
            }

            for (Iterator it2 = element.elementIterator(); it2.hasNext(); ) {
                Element element2 = (Element) it2.next();
                if (!"task:scheduled".equals(element2.getQName().getQualifiedName())) {
                    continue;
                }

                String beanName = element2.attributeValue("ref");
                String method = element2.attributeValue("method");

                Set<String> methodSet = taskMap.computeIfAbsent(beanName, k -> new HashSet<>());
                methodSet.add(method);
            }
        }
    }

    private SpringXmlTaskHandler() {
        throw new IllegalStateException("illegal");
    }
}

package com.adrninistrator.jacg.extensions.codeparser.jarentryotherfile;

import com.adrninistrator.jacg.common.JACGConstants;
import com.adrninistrator.jacg.util.JACGXmlUtil;
import com.adrninistrator.javacg2.extensions.codeparser.AbstractSaveData2FileParser;
import com.adrninistrator.javacg2.util.JavaCG2FileUtil;
import org.jdom2.Element;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.InputStream;

/**
 * @author adrninistrator
 * @date 2023/1/2
 * @description: 获取XML中定义的Spring定时任务信息
 */
public class SpringTaskXmlCodeParser extends AbstractSaveData2FileParser {
    private static final Logger logger = LoggerFactory.getLogger(SpringTaskXmlCodeParser.class);

    public static final String FILE_NAME = "spring_task_xml";

    @Override
    public String chooseFileName() {
        return FILE_NAME;
    }

    // 指定需要处理xml文件
    @Override
    public String[] chooseJarEntryOtherFileExt() {
        return new String[]{JACGConstants.EXT_XML};
    }

    // 处理.xml文件
    @Override
    public void parseJarEntryOtherFile(InputStream inputStream, String jarEntryPath) {
        try {
            Element root = JACGXmlUtil.parseXmlRootElement(inputStream);
            if (!"beans".equals(root.getName())) {
                logger.debug("跳过非Spring XML 1: {}", jarEntryPath);
                return;
            }

            logger.info("处理Spring XML文件 {}", jarEntryPath);
            for (Element element : root.getChildren()) {
                if (!"task:scheduled-tasks".equals(element.getQualifiedName())) {
                    continue;
                }

                for (Element element2 : element.getChildren()) {
                    if (!"task:scheduled".equals(element2.getQualifiedName())) {
                        continue;
                    }

                    String beanName = element2.getAttributeValue("ref");
                    String methodName = element2.getAttributeValue("method");
                    JavaCG2FileUtil.write2FileWithTab(writer, beanName, methodName);
                }
            }
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }
}

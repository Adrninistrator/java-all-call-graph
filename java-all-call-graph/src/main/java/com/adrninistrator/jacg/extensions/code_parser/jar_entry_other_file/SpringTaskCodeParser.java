package com.adrninistrator.jacg.extensions.code_parser.jar_entry_other_file;

import com.adrninistrator.jacg.dto.write_db.WriteDbData4SpringTask;
import com.adrninistrator.javacg.common.JavaCGConstants;
import com.adrninistrator.javacg.extensions.code_parser.JarEntryOtherFileParser;
import com.adrninistrator.javacg.xml.JavaCGXmlParser;
import org.jdom2.Element;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

/**
 * @author adrninistrator
 * @date 2023/1/2
 * @description: 获取Spring定时任务信息
 */
public class SpringTaskCodeParser implements JarEntryOtherFileParser {
    private static final Logger logger = LoggerFactory.getLogger(SpringTaskCodeParser.class);

    private static SpringTaskCodeParser LAST_INSTANCE;

    private List<WriteDbData4SpringTask> springTaskList;

    public static SpringTaskCodeParser getLastInstance() {
        return LAST_INSTANCE;
    }

    public SpringTaskCodeParser() {
        LAST_INSTANCE = this;
    }

    public List<WriteDbData4SpringTask> getSpringTaskList() {
        return springTaskList;
    }

    @Override
    public void initCodeParser() {
        springTaskList = new ArrayList<>();
    }

    // 指定需要处理xml文件
    @Override
    public String[] chooseJarEntryOtherFileExt() {
        return JavaCGConstants.FILE_EXT_ARRAY_XML;
    }

    // 处理.xml文件
    @Override
    public void parseJarEntryOtherFile(InputStream inputStream, String jarEntryName) {
        try {
            Element root = JavaCGXmlParser.parseXmlRootElement(inputStream);
            if (!"beans".equals(root.getName())) {
                logger.debug("跳过非Spring XML 1: {}", jarEntryName);
                return;
            }

            logger.info("开始处理Spring XML: {}", jarEntryName);
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

                    WriteDbData4SpringTask writeDbData4SpringTask = new WriteDbData4SpringTask();
                    writeDbData4SpringTask.setSpringBeanName(beanName);
                    writeDbData4SpringTask.setMethodName(methodName);

                    springTaskList.add(writeDbData4SpringTask);
                }
            }
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }
}

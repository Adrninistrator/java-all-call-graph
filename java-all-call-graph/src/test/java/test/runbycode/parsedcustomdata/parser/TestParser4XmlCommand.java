package test.runbycode.parsedcustomdata.parser;

import com.adrninistrator.jacg.util.JACGClassMethodUtil;
import com.adrninistrator.jacg.util.JACGXmlUtil;
import com.adrninistrator.javacg2.extensions.codeparser.AbstractSaveData2FileParser;
import org.jdom2.Element;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.InputStream;

/**
 * @author adrninistrator
 * @date 2025/6/6
 * @description:
 */
public class TestParser4XmlCommand {

    private static final Logger logger = LoggerFactory.getLogger(TestParser4XmlCommand.class);

    public static final String DATA_TYPE = "xml_command";

    private final AbstractSaveData2FileParser saveData2FileParser;

    public TestParser4XmlCommand(AbstractSaveData2FileParser saveData2FileParser) {
        this.saveData2FileParser = saveData2FileParser;
    }

    public void handle(InputStream inputStream, String jarEntryPath, String jarEntryName) {
        if (!"entry_command.xml".equals(jarEntryName)) {
            return;
        }

        logger.info("开始处理XML文件 {}", jarEntryPath);
        try {
            Element root = JACGXmlUtil.parseXmlRootElement(inputStream);
            for (Element commandElement : root.getChildren("command")) {
                String key = commandElement.getChildText("key");
                String className = commandElement.getChildText("class");
                String methodName = commandElement.getChildText("method");
                logger.info("找到信息 {} {} {}", key, className, methodName);
                // 需要用于查询的字段写到key
                String classAndMethod = JACGClassMethodUtil.genClassAndMethodName(className, methodName);
                saveData2FileParser.writeData2File(DATA_TYPE, classAndMethod, key);
            }
        } catch (Exception e) {
            logger.error("error ", e);
        }
    }
}

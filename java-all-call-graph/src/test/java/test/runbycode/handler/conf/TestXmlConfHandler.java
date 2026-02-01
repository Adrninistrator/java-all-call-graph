package test.runbycode.handler.conf;

import com.adrninistrator.jacg.dto.writedb.WriteDbData4PropertiesConf;
import com.adrninistrator.jacg.dto.writedb.WriteDbData4XmlConf;
import com.adrninistrator.jacg.handler.conf.XmlConfHandler;
import com.adrninistrator.jacg.handler.dto.XmlConfWithProperties;
import com.adrninistrator.javacg2.conf.enums.JavaCG2OtherConfigFileUseListEnum;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import test.runbycode.base.TestRunByCodeBase;

import java.util.List;

/**
 * @author adrninistrator
 * @date 2026/1/23
 * @description: 测试XmlConfHandler查询功能
 */
public class TestXmlConfHandler extends TestRunByCodeBase {
    private static final Logger logger = LoggerFactory.getLogger(TestXmlConfHandler.class);

    /**
     * 查询XML属性值公共方法
     * 入参为XML文件序号、元素序号、元素属性名称
     * 调用"根据元素的属性名称获取属性值-支持从.properties文件获取"方法获取元素属性值，若有获取到则打印
     *
     * @param xmlConfHandler XmlConfHandler实例
     * @param xmlFileSeq     XML文件序号
     * @param elementSeq     元素序号
     * @param attributeName  元素属性名称
     */
    private void queryXmlPropValue(XmlConfHandler xmlConfHandler, int xmlFileSeq, int elementSeq, String attributeName) {
        // 调用XmlConfHandler.queryAttributeValueWithProperties方法
        // 入参包括：XML文件序号、属性所在元素序号、元素的属性名称
        // 返回XmlConfWithProperties类型，包含原始值、默认值和properties配置列表
        XmlConfWithProperties xmlConfWithProperties = xmlConfHandler.queryAttributeValueWithProperties(xmlFileSeq, elementSeq, attributeName);

        String originalValue = xmlConfWithProperties.getOriginalValue();
        if (originalValue == null) {
            logger.warn("未找到元素（序号{}）属性 {} 的值", elementSeq, attributeName);
            return;
        }

        logger.info("元素（序号{}）属性 {} 的值: {}", elementSeq, attributeName, originalValue);

        // 打印默认值（如果有）
        String defaultValue = xmlConfWithProperties.getDefaultValue();
        if (defaultValue != null) {
            logger.info("    默认值: {}", defaultValue);
        }

        // 打印从properties文件查询到的记录
        List<WriteDbData4PropertiesConf> propConfList = xmlConfWithProperties.getPropertiesConfList();
        if (propConfList != null && !propConfList.isEmpty()) {
            for (WriteDbData4PropertiesConf propConf : propConfList) {
                logger.info("    在properties参数表中找到匹配记录 - 文件: {}, 键: {}, 值: {}",
                        propConf.getPropertiesFileName(),
                        propConf.getPropertiesKey(),
                        propConf.getPropertiesValue());
            }
        }
    }

    @Test
    public void $test0WriteDb() {
        javaCG2ConfigureWrapper.setOtherConfigList(JavaCG2OtherConfigFileUseListEnum.OCFULE_JAR_DIR,
                "out/test/resources");
        commonWriteDbForce();
    }

    @Test
    public void test() {
        try (XmlConfHandler xmlConfHandler = new XmlConfHandler(configureWrapper)) {
            // 调用XmlConfHandler.queryXmlConfByElementAttribute方法
            // 入参包括：XML嵌套的元素名称、元素名称、属性名称、属性值
            // 返回List类型的匹配的XML元素属性记录
            List<WriteDbData4XmlConf> beanList = xmlConfHandler.queryXmlConfByElementAttribute(
                    "beans.bean",    // XML嵌套的元素名称
                    "bean",          // 元素名称
                    "class",         // 属性名称
                    "com.alibaba.druid.pool.DruidDataSource"  // 属性值
            );

            //            printObjectContent(beanList, "beans.bean元素属性（bean，class=DruidDataSource）");

            if (beanList.isEmpty()) {
                logger.warn("未找到beans.bean元素属性");
                return;
            }

            WriteDbData4XmlConf beanRecord = beanList.get(0);
            Integer xmlFileSeq = beanRecord.getXmlFileSeq();
            Integer beanElementSeq = beanRecord.getInElementSeq();
            logger.info("找到beans.bean元素属性，XML文件序号: {}, 所在元素序号: {}", xmlFileSeq, beanElementSeq);

            // 调用XmlConfHandler.queryChildrenByElementSeq方法
            // 入参包括：XML文件序号、元素序号、用于查询的子元素名称
            // 返回List类型的匹配的子元素记录
            List<WriteDbData4XmlConf> propertyList = xmlConfHandler.queryChildrenByElementSeq(
                    xmlFileSeq,        // XML文件序号
                    beanElementSeq,    // 元素序号
                    "property"         // 子元素名称
            );
            //            printObjectContent(propertyList, "property子元素");

            if (propertyList == null || propertyList.isEmpty()) {
                logger.warn("未找到property子元素");
                return;
            }

            // 处理每个property子元素
            for (WriteDbData4XmlConf property : propertyList) {
                Integer propertyElementSeq = property.getElementSeq();

                // 调用XmlConfHandler.checkAttributeExists方法
                // 入参包括：XML文件序号、属性所在元素序号、元素的属性名称、元素的属性值
                // 返回boolean类型的判断结果
                // 查询是否存在属性名称="name"，且属性值="timeBetweenEvictionRunsMillis"的元素属性
                // 假如元素属性存在，则使用以上公共方法"查询XML属性值"查询当前子元素中属性名称="value"的属性值
                if (xmlConfHandler.checkAttributeExists(
                        xmlFileSeq,                      // XML文件序号
                        propertyElementSeq,              // 属性所在元素序号
                        "name",                          // 元素的属性名称
                        "timeBetweenEvictionRunsMillis"  // 元素的属性值
                )) {
                    queryXmlPropValue(xmlConfHandler, xmlFileSeq, propertyElementSeq, "value");
                }

                // 调用XmlConfHandler.checkAttributeExists方法
                // 入参包括：XML文件序号、属性所在元素序号、元素的属性名称、元素的属性值
                // 返回boolean类型的判断结果
                // 查询是否存在属性名称="name"，且属性值="timeBetweenLogStatsMillis"的元素属性
                // 假如元素属性存在，则使用以上公共方法"查询XML属性值"查询当前子元素中属性名称="value"的属性值
                if (xmlConfHandler.checkAttributeExists(
                        xmlFileSeq,                    // XML文件序号
                        propertyElementSeq,            // 属性所在元素序号
                        "name",                        // 元素的属性名称
                        "timeBetweenLogStatsMillis"    // 元素的属性值
                )) {
                    queryXmlPropValue(xmlConfHandler, xmlFileSeq, propertyElementSeq, "value");
                }

                // 调用XmlConfHandler.checkAttributeExists方法
                // 入参包括：XML文件序号、属性所在元素序号、元素的属性名称、元素的属性值
                // 返回boolean类型的判断结果
                // 查询是否存在属性名称="name"，且属性值="minEvictableIdleTimeMillis"的元素属性
                // 假如元素属性存在，则使用以上公共方法"查询XML属性值"查询当前子元素中属性名称="value"的属性值
                if (xmlConfHandler.checkAttributeExists(
                        xmlFileSeq,                   // XML文件序号
                        propertyElementSeq,           // 属性所在元素序号
                        "name",                       // 元素的属性名称
                        "minEvictableIdleTimeMillis"  // 元素的属性值
                )) {
                    queryXmlPropValue(xmlConfHandler, xmlFileSeq, propertyElementSeq, "value");
                }
            }
        }
    }
}


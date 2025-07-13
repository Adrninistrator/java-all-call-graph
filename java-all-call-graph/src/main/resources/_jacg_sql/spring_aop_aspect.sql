CREATE TABLE if not exists jacg_spring_aop_aspect_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  type varchar(1) NOT NULL COMMENT '类型，j: 在Java代码中定义，x: 在XML文件中定义',
  xml_aspect_id varchar(255) NOT NULL COMMENT 'XML中定义的aspect的ID',
  xml_aspect_ref varchar(255) NOT NULL COMMENT 'XML中定义的aspect对应的Bean名称',
  aspect_order int NOT NULL COMMENT 'aspect排序数值',
  class_name varchar(255) NOT NULL COMMENT '类名',
  define_xml_path varchar(255) NOT NULL COMMENT '在XML中定义时对应的文件路径',
  PRIMARY KEY (record_id),
  INDEX idx_saas_axi_{appName}(xml_aspect_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='Spring AOP aspect信息';
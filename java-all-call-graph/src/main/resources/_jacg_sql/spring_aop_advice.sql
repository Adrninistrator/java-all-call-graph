CREATE TABLE if not exists jacg_spring_aop_advice_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  type varchar(1) NOT NULL COMMENT '类型，j: 在Java代码中定义，x: 在XML文件中定义',
  xml_aspect_id varchar(255) NOT NULL COMMENT 'XML中定义的aspect的ID',
  xml_aspect_method_name varchar(255) NOT NULL COMMENT 'XML中定义的aspect的方法名称',
  advice_type varchar(20) NOT NULL COMMENT 'advice类型',
  xml_pointcut_ref text NOT NULL COMMENT 'XML中的pointcut-ref名称',
  expression text NOT NULL COMMENT 'pointcut表达式',
  aspect_order int NOT NULL COMMENT 'aspect排序数值',
  advice_full_method text NOT NULL COMMENT 'advice的完整方法',
  advice_method_return_type varchar(255) NOT NULL COMMENT 'advice方法的返回类型',
  advice_method_hash varchar(32) NOT NULL COMMENT 'advice方法hash+字节数',
  aspect_class_name varchar(255) NOT NULL COMMENT '对应aspect的类名',
  define_xml_path varchar(255) NOT NULL COMMENT '在XML中定义时对应的文件路径',
  PRIMARY KEY (record_id),
  INDEX idx_saad_ad_{appName}(advice_type)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='Spring AOP advice信息';
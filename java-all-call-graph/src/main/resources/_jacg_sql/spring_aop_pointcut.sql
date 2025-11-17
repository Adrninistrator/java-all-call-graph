CREATE TABLE IF NOT EXISTS jacg_spring_aop_pointcut_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  type varchar(1) NOT NULL COMMENT '类型，j: 在Java代码中定义，x: 在XML文件中定义',
  xml_pointcut_id varchar(100) NOT NULL COMMENT 'XML中定义的pointcut的ID',
  expression text NOT NULL COMMENT 'pointcut表达式',
  full_method text NOT NULL COMMENT '在Java代码中定义时所在的完整方法',
  define_xml_path text NOT NULL COMMENT '在XML中定义时对应的文件路径',
  PRIMARY KEY (record_id),
  INDEX idx_sap_pi_{appName}(xml_pointcut_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='Spring AOP pointcut信息';
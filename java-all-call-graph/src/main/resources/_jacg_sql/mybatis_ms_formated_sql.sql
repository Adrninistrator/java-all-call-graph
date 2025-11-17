CREATE TABLE IF NOT EXISTS jacg_mybatis_ms_formated_sql_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  xml_file_name varchar(255) NOT NULL COMMENT 'MyBatis XML文件名',
  sql_id varchar(200) NOT NULL COMMENT 'MyBatis XML中的sql id，即Mapper的方法名',
  sql_seq tinyint NOT NULL COMMENT 'sql文本序号，从0开始',
  xml_element_name varchar(15) NOT NULL COMMENT 'XML元素名称，如select、insert、update等',
  formated_sql text NOT NULL COMMENT '格式化后的sql文本',
  sql_hash varchar(32) NOT NULL COMMENT '格式化后的sql文本hash+字节数',
  mapper_simple_class_name varchar(300) NOT NULL COMMENT 'MyBatis Mapper唯一类名',
  mapper_class_name varchar(300) NOT NULL COMMENT 'MyBatis Mapper完整类名',
  xml_file_path varchar(500) NOT NULL COMMENT 'MyBatis XML文件路径',
  result_map_id varchar(100) NULL COMMENT 'XML的resultMap ID',
  result_map_hash varchar(32) NULL COMMENT 'XML对应的resultMap内容hash+字节数',
  PRIMARY KEY (record_id),
  INDEX idx_mmfs_xfnsi_{appName}(xml_file_name, sql_id),
  INDEX idx_mmfs_scnsi_{appName}(mapper_simple_class_name(255), sql_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='MyBatis XML的sql、Mapper相关信息（使用MySQL）';
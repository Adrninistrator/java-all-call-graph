CREATE TABLE if not exists jacg_properties_conf_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  properties_key varchar(255) NOT NULL COMMENT 'properties配置名称',
  properties_file_path varchar(500) NOT NULL COMMENT 'properties配置文件路径',
  properties_file_name varchar(255) NOT NULL COMMENT 'properties配置文件名',
  properties_value text COLLATE utf8mb4_bin NOT NULL COMMENT 'properties配置内容',
  PRIMARY KEY (record_id),
  INDEX idx_pc_pk_{appName}(properties_key),
  INDEX idx_pc_pfn_{appName}(properties_file_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='properties文件配置信息表';
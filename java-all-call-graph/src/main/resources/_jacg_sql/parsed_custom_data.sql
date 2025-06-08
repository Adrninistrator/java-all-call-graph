CREATE TABLE if not exists jacg_parsed_custom_data_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  data_type varchar(30) NOT NULL COMMENT '数据类型，代表当前数据的类型，格式没有限制',
  data_key varchar(300) NOT NULL COMMENT '数据的key，格式没有限制',
  data_value text COLLATE utf8mb4_bin NOT NULL COMMENT '数据内容，格式没有限制',
  PRIMARY KEY (record_id),
  INDEX idx_pcd_dtdk_{appName}(data_type, data_key)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='解析jar文件时获取的自定义数据表';
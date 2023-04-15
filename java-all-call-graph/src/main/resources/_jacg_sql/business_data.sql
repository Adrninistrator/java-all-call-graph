CREATE TABLE if not exists jacg_business_data_{appName} (
  call_id int NOT NULL COMMENT '方法调用序号',
  data_type varchar(30) NOT NULL COMMENT '数据类型，默认类型参考BusinessDataTypeEnum枚举类，也支持自定义类型',
  data_value text COLLATE utf8mb4_bin NOT NULL COMMENT '数据内容，JSON字符串格式',
  PRIMARY KEY (call_id, data_type),
  INDEX idx_bd_dt_{appName} (data_type)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='方法调用业务功能数据表';
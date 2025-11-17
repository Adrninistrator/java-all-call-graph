CREATE TABLE IF NOT EXISTS jacg_method_return_call_id_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  method_hash varchar(32) NOT NULL COMMENT '方法hash+字节数',
  return_call_id int NOT NULL COMMENT '方法返回值对应的方法调用序号，从1开始',
  full_method text NOT NULL COMMENT '完整方法（类名+方法名+参数）',
  return_type varchar(255) NOT NULL COMMENT '方法返回类型，包含数组标志',
  equivalent_conversion tinyint NOT NULL COMMENT '是否返回等值转换前的方法调用，1:是，0:否',
  PRIMARY KEY (record_id),
  INDEX idx_mrci_mhrci_{appName}(method_hash, return_call_id),
  INDEX idx_mrci_rci_{appName}(return_call_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='方法返回值对应的方法调用序号信息表';
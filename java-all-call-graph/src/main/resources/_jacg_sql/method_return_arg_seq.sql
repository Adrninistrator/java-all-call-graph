CREATE TABLE if not exists jacg_method_return_arg_seq_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  method_hash varchar(32) NOT NULL COMMENT '方法hash+字节数',
  return_arg_seq int NOT NULL COMMENT '方法返回值对应的方法参数序号，从0开始',
  full_method text NOT NULL COMMENT '完整方法（类名+方法名+参数）',
  return_type varchar(255) NOT NULL COMMENT '方法返回类型，包含数组标志',
  equivalent_conversion tinyint NOT NULL COMMENT '是否返回等值转换前的方法参数，1:是，0:否',
  PRIMARY KEY (record_id),
  INDEX idx_mras_mhras_{appName}(method_hash, return_arg_seq)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='方法返回值对应的方法参数序号信息表';
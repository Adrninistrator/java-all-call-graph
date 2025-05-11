CREATE TABLE if not exists jacg_method_return_const_value_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  method_hash varchar(30) NOT NULL COMMENT '方法hash+字节数',
  seq int NOT NULL COMMENT '某个方法返回的常量值序号，从0开始',
  const_type varchar(30) NOT NULL COMMENT '常量类型，含义参考 JavaCG2ConstantTypeEnum 类',
  const_value text COLLATE utf8mb4_bin NOT NULL COMMENT '常量的值',
  full_method text NOT NULL COMMENT '完整方法（类名+方法名+参数）',
  return_type varchar(255) NOT NULL COMMENT '方法返回类型，包含数组标志',
  PRIMARY KEY (record_id),
  INDEX idx_mrcv_mhs_{appName}(method_hash, seq)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='方法返回的常量值（含null）';
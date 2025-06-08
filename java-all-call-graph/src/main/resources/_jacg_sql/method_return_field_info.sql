CREATE TABLE if not exists jacg_method_return_field_info_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  method_hash varchar(32) NOT NULL COMMENT '方法hash+字节数',
  seq int NOT NULL COMMENT '某个方法返回的字段信息序号，从0开始',
  static_field tinyint NOT NULL COMMENT '方法返回的字段是否为静态，1:是，0:否',
  field_of_this tinyint NOT NULL COMMENT '方法返回的字段是否属于this对象，1:是，0:否',
  field_in_simple_class_name varchar(255) NOT NULL COMMENT '方法返回的字段所在的类唯一类名',
  simple_field_type_nad varchar(255) NOT NULL COMMENT '方法返回的字段类型唯一类名（不包含数组标志）',
  field_array_dimensions tinyint NOT NULL COMMENT '方法返回的字段数组类型的维度，为0代表不是数组类型',
  field_name varchar(255) NOT NULL COMMENT '方法返回的字段名称',
  field_in_class_name varchar(255) NOT NULL COMMENT '方法返回的字段所在的类完整类名',
  field_type_nad varchar(255) NOT NULL COMMENT '方法返回的字段类型完整类名（不包含数组标志）',
  full_method text NOT NULL COMMENT '完整方法（类名+方法名+参数）',
  return_type varchar(255) NOT NULL COMMENT '方法返回类型，包含数组标志',
  PRIMARY KEY (record_id),
  INDEX idx_mrfi_mhs_{appName}(method_hash, seq),
  INDEX idx_mrfi_fiscn_{appName}(field_in_simple_class_name),
  INDEX idx_mrfi_fst_{appName}(simple_field_type_nad)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='方法返回的字段（含枚举）';
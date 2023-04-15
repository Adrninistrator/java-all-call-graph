CREATE TABLE if not exists jacg_method_return_generics_type_{appName} (
  record_id int NOT NULL COMMENT '记录id',
  method_hash varchar(30) NOT NULL COMMENT '方法hash+字节数',
  simple_class_name varchar(500) NOT NULL COMMENT '唯一类名',
  type varchar(5)  NOT NULL COMMENT '类型，t:参数类型，gt:参数泛型类型',
  type_seq tinyint NOT NULL COMMENT '类型序号，参数类型固定为0，参数泛型类型从0开始',
  simple_generics_type varchar(500) NOT NULL COMMENT '泛型类型或参数类型唯一类名',
  generics_type varchar(500) NOT NULL COMMENT '泛型类型或参数类型类名',
  full_method text NOT NULL COMMENT '完整方法（类名+方法名+参数）',
  PRIMARY KEY (record_id),
  UNIQUE INDEX uni_mrgt_mh_{appName}(method_hash, type, type_seq),
  INDEX idx_mrgt_scn_{appName}(simple_class_name(255)),
  INDEX idx_mrgt_sgt_{appName}(simple_generics_type(255))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='方法返回泛型类型';
CREATE TABLE if not exists jacg_method_argument_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  method_hash varchar(30) NOT NULL COMMENT '方法hash+字节数',
  arg_seq int NOT NULL COMMENT '参数序号，从0开始',
  simple_class_name varchar(255) NOT NULL COMMENT '唯一类名',
  simple_arg_type_nad varchar(255) NOT NULL COMMENT '参数类型唯一类名（不包含数组标志）',
  arg_name varchar(255) NOT NULL COMMENT '参数名称',
  array_dimensions tinyint NOT NULL COMMENT '参数数组类型的维度，为0代表不是数组类型',
  arg_category varchar(5) NOT NULL COMMENT '参数类型分类，J:JDK中的类型，C:自定义类型',
  exists_generics_type tinyint NOT NULL COMMENT '是否存在泛型类型，1:是，0:否',
  arg_type_nad varchar(255) NOT NULL COMMENT '参数类型类名（不包含数组标志）',
  full_method text NOT NULL COMMENT '完整方法（类名+方法名+参数）',
  return_type varchar(255) NOT NULL COMMENT '方法返回类型，包含数组标志',
  PRIMARY KEY (record_id),
  INDEX idx_marg_mhas_{appName}(method_hash, arg_seq),
  INDEX idx_marg_scn_{appName}(simple_class_name),
  INDEX idx_marg_sat_{appName}(simple_arg_type_nad)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='方法参数类型';
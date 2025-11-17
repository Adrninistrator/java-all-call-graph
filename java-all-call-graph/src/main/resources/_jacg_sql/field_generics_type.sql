CREATE TABLE IF NOT EXISTS jacg_field_generics_type_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  simple_class_name varchar(300) NOT NULL COMMENT '唯一类名',
  field_name varchar(200) NOT NULL COMMENT '字段名',
  type varchar(5)  NOT NULL COMMENT '类型，t:字段类型，gt:字段中的泛型类型',
  type_seq tinyint NOT NULL COMMENT '类型序号，字段类型固定为0，字段的泛型类型从0开始',
  simple_generics_type_nad varchar(255) NOT NULL COMMENT '非静态字段类型或其中的泛型类型唯一类名（不包含数组标志）',
  generics_array_dimensions tinyint DEFAULT NULL COMMENT '非静态字段中的泛型数组类型的维度，为0代表不是数组类型',
  type_variables_name varchar(255) NOT NULL COMMENT '非静态字段中的泛型类型变量名称',
  wildcard varchar(8) NOT NULL COMMENT '非静态字段中的泛型通配符',
  reference_type varchar(255) NOT NULL COMMENT '非静态字段中的泛型通配符引用的类型',
  generics_category varchar(5) NOT NULL COMMENT '非静态字段中的泛型类型分类，J:JDK中的类型，C:自定义类型',
  generics_type_nad varchar(255) NOT NULL COMMENT '非静态字段类型或其中的泛型类型类名（不包含数组标志）',
  class_name varchar(300) NOT NULL COMMENT '完整类名',
  PRIMARY KEY (record_id),
  INDEX idx_fgt_cfs_{appName}(simple_class_name(255), field_name, type, type_seq),
  INDEX idx_fgt_sgt_{appName}(simple_generics_type_nad)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='非静态字段中涉及的泛型类型';
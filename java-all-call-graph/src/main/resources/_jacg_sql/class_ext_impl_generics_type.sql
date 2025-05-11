CREATE TABLE if not exists jacg_class_ext_impl_generics_type_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  simple_class_name varchar(255) NOT NULL COMMENT '唯一类名',
  ext_type char(1) NOT NULL COMMENT '继承或实现类型，e:继承，i:实现',
  seq int NOT NULL COMMENT '继承或实现的序号，从0开始',
  super_itf_simple_class_name varchar(255) NOT NULL COMMENT '父类或接口的唯一类名',
  generics_seq tinyint NOT NULL COMMENT '类的继承或实现中的泛型类型序号，从0开始',
  simple_generics_type_nad varchar(255) NOT NULL COMMENT '类的继承或实现中的泛型类型唯一类名（不包含数组标志）',
  generics_array_dimensions tinyint DEFAULT NULL COMMENT '类的继承或实现中的泛型数组类型的维度，为0代表不是数组类型',
  type_variables_name varchar(255) NOT NULL COMMENT '类的继承或实现中的泛型类型变量名称',
  generics_category varchar(5) NOT NULL COMMENT '类的继承或实现中的泛型类型分类，J:JDK中的类型，C:自定义类型',
  generics_type_nad varchar(255) NOT NULL COMMENT '类的继承或实现中的泛型类型类名（不包含数组标志）',
  class_name varchar(255) NOT NULL COMMENT '完整类名',
  super_itf_class_name varchar(255) NOT NULL COMMENT '父类或接口的类名',
  PRIMARY KEY (record_id),
  INDEX idx_ceigt_scn_{appName}(simple_class_name),
  INDEX idx_ceigt_siscn_{appName}(super_itf_simple_class_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='类的继承或实现的泛型信息';
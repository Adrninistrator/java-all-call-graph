CREATE TABLE if not exists jacg_class_sig_ext_impl_generics_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  simple_class_name varchar(255) NOT NULL COMMENT '唯一类名',
  generics_name varchar(255) NOT NULL COMMENT '签名中的泛型名称',
  seq int NOT NULL COMMENT '签名中泛型的序号，从0开始',
  ext_type char(1) NOT NULL COMMENT '继承或实现类型，e:继承，i:实现',
  super_itf_simple_class_name varchar(255) NOT NULL COMMENT '父类或接口的唯一类名',
  super_itf_generics_extends_class_name varchar(255) NOT NULL COMMENT '签名中的父类或接口的泛型继承的父类类名',
  super_itf_seq int NOT NULL COMMENT '父类或接口的签名中泛型的序号，从0开始',
  class_name varchar(255) NOT NULL COMMENT '完整类名',
  super_itf_class_name varchar(255) NOT NULL COMMENT '父类或接口的类名',
  PRIMARY KEY (record_id),
  INDEX idx_cseig_scn_{appName}(simple_class_name),
  INDEX idx_cseig_siscn_{appName}(super_itf_simple_class_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='类的签名中继承或实现的泛型关系';
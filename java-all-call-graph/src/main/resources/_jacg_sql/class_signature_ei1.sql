CREATE TABLE if not exists jacg_class_signature_ei1_{appName} (
  record_id int NOT NULL COMMENT '记录id，从1开始',
  simple_class_name varchar(255) NOT NULL COMMENT '唯一类名',
  type char(1) NOT NULL COMMENT '类型，e:继承，i:实现',
  super_itf_class_name varchar(255) NOT NULL COMMENT '父类或接口的类名',
  seq int NOT NULL COMMENT '涉及继承或实现的签名信息的序号，从0开始',
  sign_class_name varchar(255) NOT NULL COMMENT '签名中的完整类名',
  sign_generics_name varchar(255) NOT NULL COMMENT '签名中的泛型名称',
  class_name varchar(255) NOT NULL COMMENT '完整类名',
  PRIMARY KEY (record_id),
  INDEX idx_csei1_scn_{appName}(simple_class_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='类的签名中涉及继承与实现的信息表1';
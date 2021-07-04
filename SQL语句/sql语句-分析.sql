-- ����������
select full_name, simple_name from class_name_wfts_fcts_adm where simple_name in
(
select simple_name from class_name_wfts_fcts_adm group by simple_name having count(simple_name) > 1
)

-- ���������෽������
select * from method_call_wfts_fcts_adm where caller_class_name like '%.%'
select * from method_call_wfts_fcts_adm where callee_class_name like '%.%'

-- ������ķ���������
select max(length(caller_method)),max(length(callee_method)) from method_call_wfts_fcts_adm

-- �����ض�����
select * from method_call_wfts_fcts_adm where caller_full_class_name like '%.dto.%'
select * from method_call_wfts_fcts_adm where callee_full_class_name like '%.entity.%'

-- �����ض��ؼ��ַ���
select * from method_call_wfts_fcts_adm where caller_method like '%<init%'
select * from method_call_wfts_fcts_adm where callee_method like '%<init%'

-- ����ȥ����������
select distinct(caller_full_class_name) from method_call_wfts_fcts_adm order by caller_full_class_name
select distinct(callee_full_class_name) from method_call_wfts_fcts_adm order by callee_full_class_name

-- ���ұ����ô������ķ���
select r.method, count(r.method) as cm from
(
select concat(callee_full_class_name,':',callee_method_name) as method from method_call_wfts_fcts_adm
) as r group by method order by cm desc

-- ���ұ����ô���������
select callee_full_class_name, count(callee_full_class_name) as cc from method_call_wfts_fcts_adm group by callee_full_class_name order by cc desc

-- ���Ұ����ؼ��ֵ�ȥ����������
select distinct(caller_full_class_name) from method_call_wfts_fcts_adm where caller_full_class_name like '%.proto.%'
select distinct(callee_full_class_name) from method_call_wfts_fcts_adm where callee_full_class_name like '%.proto.%'

-- ����ȥ�ط�����
select caller_method_name,count(caller_method_name) as cc from method_call_wfts_fcts_adm group by caller_method_name order by cc desc, caller_method_name
select callee_method_name,count(callee_method_name) as cc from method_call_wfts_fcts_adm group by callee_method_name order by cc desc, callee_method_name

-- �����ض�������
select * from method_call_wfts_fcts_adm where caller_method_name = ''
select * from method_call_wfts_fcts_adm where callee_method_name = ''

-- �����ض������÷���������Ӧ����
select distinct(callee_full_class_name), callee_method_name from method_call_wfts_fcts_adm where callee_method_name in ('toDenseJson','')
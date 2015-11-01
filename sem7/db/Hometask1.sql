/*
  File:   Hometask1.sql
  Course: DataBase practice (471 group, term 7)
  Author: Aleksei Semin (alllex.semin@gmail.com)
  Date:   09.2015 (c)
  Misc:   Script works in default workspace of apex.oracle.com 
*/

-- Preliminaries
/*
alter table emp drop constraint emp_sal_cons;
drop table dept1;
drop view responsible_managers;
drop view winter_comers;
drop sequence dept1_seq;
*/
-- Task 1

-- Look at user tables
select * from user_tables;

-- Days since hiring
select ename, round(sysdate - hiredate) as "Days working" from emp;

-- Task 2

-- Look at user constraints
select * from user_constraints;

-- Count of constraints
select count(*) as "Constraints count" from user_constraints;

-- Add new constraint on salary
alter table emp add constraint emp_sal_cons check (sal between 500 and 5000);

-- Check whether constraint was added
select * from user_constraints where constraint_name='EMP_SAL_CONS';

-- Task 3

-- Look at user indexes
select * from user_indexes;

-- Create index-organized table same as DEPT
create table dept1
(
    deptno number constraint deptno_nn not null, 
    dname varchar2(14),
    loc   varchar2(13),
    constraint deptno_pk primary key (deptno)
)
organization index;

-- Task 4

-- Add view of employees who was hired in winter
create view winter_comers 
as select empno, ename 
from emp 
where extract(month from hiredate) in ('12', '1', '2')
with check option;

-- Add view of managers who looks after 3 or more employees
create view responsible_managers 
as select ename 
from emp mngs 
where 3 <= (select count(*) from emp wkrs where wkrs.mgr=mngs.empno) 
with check option;

-- Look at "winter-comers"-view
select * from winter_comers;

-- Look at "responsible managers" view
select * from responsible_managers;

-- Task 5

-- Create suitable sequence for DEPT1 table's DEPTNO column
create sequence dept1_seq 
start with 10 
increment by 10 
cache 5;

-- Generate several numbers
select dept1_seq.nextval from dual; 
select dept1_seq.nextval from dual; 
select dept1_seq.nextval from dual; 

-- Check whether new sequence has been added
select * from user_sequences where sequence_name='DEPT1_SEQ';

-- Task 6

-- Create factorial function
create or replace function factorial (n in number) 
return number as 
begin 
    if (n <= 1) then 
        return 1; 
    end if; 
    return n * factorial(n-1); 
end; 

/
-- Test factorial function
select factorial(0), factorial(1), factorial(3), factorial(5) from dual; 

-- Task 7

-- Create countring employee function
create or replace function emp_count 
return number is 
    var_count number;
begin 
    select count(*) into var_count from emp; 
    return var_count; 
end;
/
-- Test of EMP_COUNT
select emp_count from dual;

-- Task 8

-- Create procedure for statistics
create or replace procedure stats
(   emp_count    out number,
    dep_count    out number,
    pos_count    out number,
    sal_sum      out number
) is
begin
    select count(*) into emp_count from emp;
    select count(*) into dep_count from dept;
    select count(*) into pos_count 
    from (select distinct job from emp);
    select sum(sal) into sal_sum from emp;
end;

/
-- Use STATS to print the statistics
declare 
    emp_count number;
    dep_count number;
    pos_count number;
    sum_salary number;
begin
    stats(emp_count, dep_count, pos_count, sum_salary);
    dbms_output.put_line('Employee count = '   || emp_count); 
    dbms_output.put_line('Department count = ' || dep_count); 
    dbms_output.put_line('Position count = '   || pos_count); 
    dbms_output.put_line('Sum of salaries = '  || sum_salary); 
end;
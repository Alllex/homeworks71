/*
  File:   Hometask3.sql
  Course: DataBase practice (471 group, term 7)
  Author: Aleksei Semin (alllex.semin@gmail.com)
  Date:   09.2015 (c)
  Misc:   Script works in default workspace of apex.oracle.com 
*/

-- Preliminaries for logger

drop trigger journal_on_create;
drop procedure logerr;
drop procedure logmsg;
drop sequence journal_id_seq;
drop table journal;

-- Logger description

create table journal
(
    id number constraint journal_id_nn not null,
    time date,
    src varchar(256),
    msg varchar(256),
    constraint journal_id_pk primary key (id)
);

create sequence journal_id_seq start with 1 increment by 1 cache 5;

create or replace procedure logmsg
    ( src in varchar
    , msg in varchar
    ) 
as
    pragma AUTONOMOUS_TRANSACTION;
begin
    insert into journal values (journal_id_seq.nextval, sysdate, src, msg);
    commit;
exception
    when others then
        return;
end logmsg;
/
create or replace procedure logerr (src in varchar) as
begin
    logmsg(src, substr(sqlerrm, 1, 100));
end logerr;
/
create or replace trigger journal_on_create
after create on schema
begin
    logmsg('journal_on_create', 'Created: ' || dictionary_obj_name());
end journal_on_create;
/

-- Preliminaries 

drop trigger lecture_check;
drop trigger room_cap_check_on_ins;
drop sequence schedule_id_seq;
drop table schedule;
drop sequence lecture_id_seq;
drop table lecture;
drop table classroom;
drop table study_group;
drop sequence lecturer_id_seq;
drop table lecturer;


-- Tables and sequences declaration

create table lecturer
(
    id number constraint lecturer_id_nn not null,
    name varchar(256) constraint lecturer_name_nn not null,
    constraint lecturer_id_pk primary key (id)
);

create sequence lecturer_id_seq start with 1 increment by 1 cache 5;

create table study_group
(
    id number constraint study_group_id_nn not null,
    amount number constraint study_group_amount_nn not null,
    constraint study_group_id_pk primary key (id)
);

create table classroom
(
    id number constraint classroom_id_nn not null,
    capacity number constraint classroom_capacity_nn not null,
    has_projector number(1, 0),
    has_computers number(1, 0),
    constraint classroom_id_pk primary key (id)
);

create table lecture
(
    id number constraint lecture_id_nn not null,
    ord_num number,
    day_num number,
    subject varchar(256),
    lecturer_id number,
    classroom_id number,
    
    constraint lecture_id_pk primary key (id),
    
    constraint lecture_lecturer_id_fk 
        foreign key (lecturer_id)  
        references lecturer(id),
        
    constraint lecture_classroom_id_fk 
        foreign key (classroom_id)
        references classroom(id)
);

create sequence lecture_id_seq start with 1 increment by 1 cache 5;

create table schedule
(
    id number constraint schedule_id_nn not null,
    lecture_id number,
    study_group_id number,
    
    constraint schedule_id_pk primary key (id),
    
    constraint schedule_lecture_id_fk
        foreign key (lecture_id)
        references lecture(id),
    
    constraint schedule_study_group_id_fk
        foreign key (study_group_id)
        references study_group(id)
);

create sequence schedule_id_seq start with 1 increment by 1 cache 5;

-- Procedures for adding data 

create or replace procedure add_lecturer (name in varchar) as
begin
    insert into lecturer values (lecturer_id_seq.nextval, name);
end add_lecturer;
/
create or replace procedure add_study_group
    ( num in number
    , amount in number ) as 
begin
    insert into study_group values (num, amount);
end add_study_group;
/
create or replace procedure add_classroom
    ( num in number
    , cap in number
    , proj in number
    , comp in number ) 
as
    has_proj number;
    has_comp number;
begin
    has_proj := 0;
    if proj > 0 then has_proj := 1; end if;
    has_comp := 0;
    if comp > 0 then has_comp := 1; end if;
    insert into classroom values (num, cap, has_proj, has_comp);
end add_classroom;
/
create or replace procedure add_standard_classroom (num in number) as
begin
    add_classroom(num, 30, 0, 0);
end add_standard_classroom;
/
create or replace procedure add_presentation_room (num in number) as
begin
    add_classroom(num, 50, 1, 0);
end add_presentation_room;
/
create or replace procedure add_computer_room (num in number) as
begin
    add_classroom(num, 20, 1, 1);
end add_computer_room;
/
create or replace procedure add_lecture
    ( day in number
    , ord in number
    , subj in varchar
    , lect in number
    , room in number ) as
begin
    insert into lecture values
        (lecture_id_seq.nextval, ord, day, subj, lect, room);
end add_lecture;
/
create or replace procedure add_group_on_lecture
    ( group_id in number
    , lecture_id in number ) as
begin
    insert into schedule values
        (schedule_id_seq.nextval, lecture_id, group_id);
end add_group_on_lecture;
/

-- Triggers for consistency

create or replace trigger lecture_check
before insert or update on lecture
for each row
declare
    room_cnt number;
    lect_cnt number;
    ord_num_out_of_range exception;
    day_num_out_of_range exception;
    classroom_is_busy exception;
    lecturer_is_busy exception;
begin
    if (:new.ord_num < 1 or :new.ord_num > 6) then
        raise ord_num_out_of_range;
    end if;
    
    if (:new.day_num < 1 or :new.day_num > 7) then
        raise day_num_out_of_range;
    end if;
    
    select count(*) into room_cnt from lecture 
        where ord_num=:new.ord_num and classroom_id=:new.classroom_id;
        
    if (room_cnt > 0) then
        raise classroom_is_busy;
    end if;
    
    select count(*) into lect_cnt from lecture
        where ord_num=:new.ord_num and lecturer_id=:new.lecturer_id;
    
    if (lect_cnt > 0) then
        raise lecturer_is_busy;
    end if;
    
exception
    when ord_num_out_of_range then
    begin
        logmsg('lecture_check', 'ERROR: lecture ordinal number is out of range');
        raise_application_error(-20701, 'lecture ordinal number is out of range');
    end;
    when day_num_out_of_range then
    begin
        logmsg('lecture_check', 'ERROR: lecture day of week is out of range');
        raise_application_error(-20702, 'lecture day of week is out of range');
    end;
    when classroom_is_busy then 
    begin
        logmsg('lecture_check', 'ERROR: classroom is not available at this time');
        raise_application_error(-20703, 'classroom is not available at this time');
    end;
    when lecturer_is_busy then 
    begin
        logmsg('lecture_check', 'ERROR: lecturer is not available at this time');
        raise_application_error(-20704, 'lecturer is not available at this time');
    end;
    when others then logerr('lecture_check');

end lecture_check;
/
create or replace trigger room_cap_check_on_ins
before insert on schedule
for each row
declare
    capacity number;
    occupied number;
    available number;
    queried number;
    room_overflow exception;
begin

    select (select capacity
            from classroom cr 
            where cr.id=lt.classroom_id) 
        into capacity from lecture lt
        where lt.id=:new.lecture_id;
        
    select sum(sg.amount) 
    into occupied
    from schedule sd 
    join study_group sg 
    on sd.study_group_id=sg.id 
    where sd.lecture_id=:new.lecture_id;
         
    select sum(amount) into queried from study_group sg
        where sg.id=:new.study_group_id;
        
    available := capacity - occupied;
    
    if (queried > available) then 
        raise room_overflow;
    end if;

exception 
    when no_data_found then
        null;
    when room_overflow then
    begin
        logmsg('classroom_capacity_check', 'ERROR: classroom overflow');
        raise_application_error(-20704, 'classroom overflow');
    end;
    when others then logerr('classroom_capacity_check');
    
end classroom_capacity_check;
/

-- Filling witrh data
begin
    add_lecturer('Ivanov');
    add_lecturer('Petrov');
    add_lecturer('Sidorov');
    add_lecturer('Einstein');
    add_lecturer('Dickens');
    
    add_study_group(411, 10);
    add_study_group(412, 15);
    add_study_group(471, 8);
    add_study_group(441, 20);
    add_study_group(442, 23);
    add_study_group(443, 18);
    add_study_group(444, 25);
    add_study_group(461, 12);
    add_study_group(421, 16);
    add_study_group(422, 21);
                    
    add_standard_classroom(2510);
    add_standard_classroom(2511);
    add_standard_classroom(2512);
    add_standard_classroom(2513);
    add_standard_classroom(2514);
                           
    add_presentation_room(405);
    add_presentation_room(2448);
                    
    add_computer_room(2408);
    add_computer_room(2410);
    add_computer_room(2412);
    add_computer_room(2414);
    add_computer_room(2442);
    add_computer_room(2444);
    add_computer_room(2446);
end;
/

select * from lecturer;
select * from study_group;
select * from classroom;


-- Add normal lectures
declare
    lect_id number;
begin
    select id into lect_id from lecturer where upper(name)='PETROV';
    add_lecture(1, 1, 'Matan', lect_id, 2511);
    add_lecture(1, 2, 'Matan', lect_id, 2511);
    
    select id into lect_id from lecturer where upper(name)='DICKENS';
    add_lecture(1, 1, 'Literature', lect_id, 2448);
    add_lecture(1, 4, 'Literature', lect_id, 2514);
    
    select id into lect_id from lecturer where upper(name)='EINSTEIN';
    add_lecture(1, 3, 'Physics', lect_id, 405);
    
    select id into lect_id from lecturer where upper(name)='SIDOROV';
    add_lecture(1, 5, 'Algebra', lect_id, 2514);
    
    
    add_group_on_lecture(412, 1);
    add_group_on_lecture(411, 2);
    
    add_group_on_lecture(411, 3);
    add_group_on_lecture(412, 4);
    
    add_group_on_lecture(411, 5);
    add_group_on_lecture(412, 5);
    
    add_group_on_lecture(411, 6);
    
end;
/

select * from lecture;
select * from schedule;

-- Add inconsistent lectures
begin -- Fail the lecture check on day of week
   add_lecture(10, 1, 'xxx', 1, 100);
end;
/
begin -- Fail the lecture check on ordinal number
   add_lecture(2, 100, 'xxx', 1, 100);
end;
/
begin -- Fail the lecture check on available classroom
   add_lecture(1, 1, 'xxx', 1, 2511);
end;
/
declare
    lect_id number;
begin -- Fail the lecture check on available lecturer
    select id into lect_id from lecturer where upper(name)='EINSTEIN';
   add_lecture(1, 3, 'xxx', lect_id, 2513);
end;
/
begin -- Classroom overflow 
    add_group_on_lecture(444, 5);
    add_group_on_lecture(471, 5);
end;
/










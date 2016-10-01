drop database if exists holdem;
create database holdem;
use holdem;
-- create table user(id int primary key, name varchar(70), password varchar(70));
-- create table puzzle_type(id int primary key, name varchar(30));
-- create table score(user_id int, puzzle_type_id int, attempts int, solved int, primary key(user_id, puzzle_id), foreign key(user_id) references user(id), foreign key(puzzle_type_id) references puzzle_type(id)
-- insert into user(id, name, password)
--     values (1, "rene", "1234");
-- insert into puzzle_type (id, name)
--       values (1, "potodds")
--     , values (2, "outs")
--     , values (3, "besthand")
--     , values (4, "expectationvalue");
-- insert into score(user_id, puzzle_type_id, attempts, solved)
--       values (1, 2, 5, 4)
--     , values (1, 1, 3, 3);

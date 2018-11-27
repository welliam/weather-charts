drop table if exists WEATHER;

create table WEATHER (
    id integer primary key,
    created integer not null,
    temperature integer not null,
    humidity integer not null,
    location text not null
);

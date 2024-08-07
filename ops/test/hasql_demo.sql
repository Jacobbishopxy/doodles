/*
User details table.
*/
create table "user"
  (
    "id" serial not null primary key,
    "email" text not null unique,
    "password" bytea not null,
    "name" text not null,
    "phone" text null,
    "admin" bool not null
  );

/*
Notifications associated with users.
*/
create table "notification"
  (
    "id" serial not null primary key,
    "user" int4 not null references "user",
    "message" text not null,
    "read" bool not null
  );

/*
Index for listing notifications by user.
*/
create index "notification_user"
  on "notification"
  ("user");

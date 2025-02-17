begin;

create table users (
	user_id serial primary key
  , user_name varchar(255) not null unique
  , email varchar(255) not null unique
  , password text not null
  , created_at timestamptz default now()
  , updated_at timestamptz default now()
);

create table user_profile_image (
	user_id int references users on delete cascade primary key
 ,  user_profile_image text not null
 ,  created_at timestamptz default now()
 ,  updated_at timestamptz default now()
);

create table admin (
	admin_id serial primary key
  , admin_name varchar(255) not null unique
  , email varchar(255) not null unique
  , password text not null
  , created_at timestamptz default now()
  , updated_at timestamptz default now()
);

insert into admin (admin_name,email,password) values ('batman','bruce@abc.com','$2b$10$csql5X9xPxuvH.DYMFNQS.arK2KEMTKOmuZcLKFjicgIDwOZ0tV5a');

create table community (
	community_id serial primary key
  , community_name varchar(255) not null unique
  , community_description text not null
  , created_at timestamptz default now()
  , updated_at timestamptz default now()
);

alter table community add column label_list jsonb;

create table thread (
	thread_id serial primary key,
	thread_title varchar(255) not null,
	thread_description text,
	user_id int references users on delete cascade on update cascade,
	community_id int references community on delete cascade on update cascade,
	created_at timestamptz default now(),
	updated_at timestamptz default now()
);

create table vote_thread (
	user_id int references users on delete cascade on update cascade,
	thread_id int references thread on delete cascade on update cascade,
	vote bool not null,
	created_at timestamptz default now(),
	updated_at timestamptz default now(),
	primary key (user_id,thread_id)
);

create table comment (
	comment_id serial primary key,
	user_id int not null references users on delete cascade on update cascade,
	thread_id int not null references thread on delete cascade on update cascade,
	comment_content varchar(255) not null,
	parent_comment_id int references comment(comment_id) on delete cascade on update cascade,
	created_at timestamptz default now(),
	updated_at timestamptz default now()
);

alter table thread alter column user_id set not null;
alter table thread alter column thread_id set not null;

create table vote_comment (
	comment_id int references comment on delete cascade on update cascade,
	user_id int references users on delete cascade on update cascade,
	vote boolean not null,
	created_at timestamptz default now(),
	updated_at timestamptz default now()
);
CREATE TABLE user_email_verify_otp (
	user_id int REFERENCES users ON DELETE CASCADE ON UPDATE CASCADE PRIMARY key,
	otp SMALLINT NOT NULL,
	created_at timestamptz DEFAULT now()
);

alter table users add column is_verified bool default false;
ALTER TABLE users ALTER COLUMN password DROP NOT NULL;

CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
  NEW."updated_at" = NOW();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

create or replace TRIGGER set_updated_at_users
BEFORE UPDATE ON users
FOR EACH ROW
EXECUTE FUNCTION update_updated_at_column();

create or replace TRIGGER set_updated_at_user_profile_image
BEFORE UPDATE ON user_profile_image
FOR EACH ROW
EXECUTE FUNCTION update_updated_at_column();


create or replace TRIGGER set_updated_at_admin
BEFORE UPDATE ON admin
FOR EACH ROW
EXECUTE FUNCTION update_updated_at_column();


create or replace TRIGGER set_updated_at_community
BEFORE UPDATE ON community
FOR EACH ROW
EXECUTE FUNCTION update_updated_at_column();


create or replace TRIGGER set_updated_at_thread
BEFORE UPDATE ON thread
FOR EACH ROW
EXECUTE FUNCTION update_updated_at_column();


create or replace TRIGGER set_updated_at_vote_thread
BEFORE UPDATE ON vote_thread
FOR EACH ROW
EXECUTE FUNCTION update_updated_at_column();


create or replace TRIGGER set_updated_at_comment
BEFORE UPDATE ON comment
FOR EACH ROW
EXECUTE FUNCTION update_updated_at_column();

create or replace TRIGGER set_updated_at_vote_comment
BEFORE UPDATE ON vote_comment
FOR EACH ROW
EXECUTE FUNCTION update_updated_at_column();

INSERT INTO community
(community_id, community_name, community_description, created_at, updated_at, label_list)
VALUES(1, 'Haskell', 'Community for haskell programming language', '2025-02-17 19:09:47.081', '2025-02-17 19:09:47.081', '["programming"]'::jsonb);
INSERT INTO community
(community_id, community_name, community_description, created_at, updated_at, label_list)
VALUES(2, 'Functional programming', 'All things functional', '2025-02-17 19:10:14.433', '2025-02-17 19:10:14.433', '["programming"]'::jsonb);

commit;

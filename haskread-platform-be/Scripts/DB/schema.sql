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

commit;
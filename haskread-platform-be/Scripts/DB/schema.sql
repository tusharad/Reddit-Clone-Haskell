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
 ,  user_profile_image bytea not null
 ,  created_at timestamptz default now()
 ,  updated_at timestamptz default now()
);

create table admin (
	admin_id serial primary key
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
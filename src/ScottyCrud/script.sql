create table posts (post_id serial primary key,post_title varchar(80),post_description text,user_id int references users on delete cascade,created_at timestamp default now(),update_at timestamp default now());
create table comments (comment_id serial primary key,comment_content text,createdAt timestamptz default now(),updatedAt timestamptz default now(),user_id int references users(user_id) on delete set null,post_id int references posts (post_id) on delete set null);

users {
   user_id
  ,user_email
  , password
}

posts {
   post_id
  ,post_title
  ,post_description
  ,user_id      f_key
  ,createdAt
  ,updatedAt
}

comments {
    comment_id
  , comment_content
  , createdAt
  , updatedAt
  , user_id    f_key
  , post_id    f_key
}

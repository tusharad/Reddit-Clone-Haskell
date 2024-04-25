begin;

create schema reddit_haskell;


CREATE TABLE reddit_haskell.category (
	category_id serial4 NOT NULL,
	category_name varchar(50) NULL,
	CONSTRAINT category_pkey PRIMARY KEY (category_id)
);


CREATE TABLE reddit_haskell.users (
	user_id serial4 NOT NULL,
	user_email varchar(30) NULL,
	"password" text NULL,
	user_name varchar(50) NULL,
	is_verified bool DEFAULT false NULL,
	"token" text NULL,
	CONSTRAINT users_pkey PRIMARY KEY (user_id),
	CONSTRAINT users_user_name_key UNIQUE (user_name)
);


CREATE TABLE reddit_haskell.posts (
	post_id serial4 NOT NULL,
	post_title varchar(80) NULL,
	post_description text NULL,
	user_id int4 NULL,
	created_at timestamptz DEFAULT now() NULL,
	update_at timestamptz DEFAULT now() NULL,
	category_id int4 NULL,
	file_path text NULL,
	CONSTRAINT posts_pkey PRIMARY KEY (post_id),
	CONSTRAINT posts_category_id_fkey FOREIGN KEY (category_id) REFERENCES reddit_haskell.category(category_id) ON DELETE CASCADE,
	CONSTRAINT posts_user_id_fkey FOREIGN KEY (user_id) REFERENCES reddit_haskell.users(user_id) ON DELETE CASCADE
);

CREATE TABLE reddit_haskell."comments" (
	comment_id serial4 NOT NULL,
	comment_content text NULL,
	createdat timestamptz DEFAULT now() NULL,
	updatedat timestamptz DEFAULT now() NULL,
	user_id int4 NULL,
	post_id int4 NULL,
	parent_comment_id int4 NULL,
	CONSTRAINT comments_pkey PRIMARY KEY (comment_id),
	CONSTRAINT comments_parent_comment_id_fkey FOREIGN KEY (parent_comment_id) REFERENCES reddit_haskell."comments"(comment_id) ON DELETE SET NULL,
	CONSTRAINT comments_post_id_fkey FOREIGN KEY (post_id) REFERENCES reddit_haskell.posts(post_id) ON DELETE SET NULL,
	CONSTRAINT comments_user_id_fkey FOREIGN KEY (user_id) REFERENCES reddit_haskell.users(user_id) ON DELETE SET NULL
);

INSERT INTO reddit_haskell.category (category_name) VALUES
	 ('haskell'),
	 ('android');

commit;

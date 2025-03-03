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
INSERT INTO community
(community_id, community_name, community_description, created_at, updated_at, label_list)
VALUES(5, 'Foodies', 'Share recipes and food experiences', '2025-01-02 17:30:00.000', '2025-01-02 17:30:00.000', '["food", "cooking"]'::jsonb);
INSERT INTO community
(community_id, community_name, community_description, created_at, updated_at, label_list)
VALUES(6, 'TravelBugs', 'Travel stories and tips', '2025-01-03 19:30:00.000', '2025-01-03 19:30:00.000', '["travel", "adventure"]'::jsonb);
INSERT INTO community
(community_id, community_name, community_description, created_at, updated_at, label_list)
VALUES(7, 'BookClub', 'Book reviews and discussions', '2025-01-04 21:30:00.000', '2025-01-04 21:30:00.000', '["books", "reading"]'::jsonb);
INSERT INTO community
(community_id, community_name, community_description, created_at, updated_at, label_list)
VALUES(8, 'FitnessFans', 'Fitness tips and motivation', '2025-01-05 23:30:00.000', '2025-01-05 23:30:00.000', '["fitness", "health"]'::jsonb);
INSERT INTO community
(community_id, community_name, community_description, created_at, updated_at, label_list)
VALUES(3, 'TechTalk', 'A community for discussing technology and innovation.', '2025-02-20 23:02:44.563', '2025-02-21 00:29:42.159', '["programming"]'::jsonb);

INSERT INTO users
(user_id, user_name, email, "password", created_at, updated_at, is_verified)
VALUES(14, 'john_doe2', 'john@example.com', '$2b$10$4.zg9A1kjdLwC2l8j/Lliex.NXjn1PJyEyzzSBuhfmjFA4a3FO.WW', '2025-01-01 15:30:00.000', '2025-02-21 00:35:52.396', true);
INSERT INTO users
(user_id, user_name, email, "password", created_at, updated_at, is_verified)
VALUES(15, 'jane_smith', 'jane@example.com', '$2b$10$4.zg9A1kjdLwC2l8j/Lliex.NXjn1PJyEyzzSBuhfmjFA4a3FO.WW', '2025-01-02 17:30:00.000', '2025-02-21 19:35:07.792', true);
INSERT INTO users
(user_id, user_name, email, "password", created_at, updated_at, is_verified)
VALUES(16, 'bob_jones', 'bob@example.com', '$2b$10$4.zg9A1kjdLwC2l8j/Lliex.NXjn1PJyEyzzSBuhfmjFA4a3FO.WW', '2025-01-03 19:30:00.000', '2025-02-21 00:35:52.403', true);
INSERT INTO users
(user_id, user_name, email, "password", created_at, updated_at, is_verified)
VALUES(17, 'alice_wong', 'alice@example.com', '$2b$10$4.zg9A1kjdLwC2l8j/Lliex.NXjn1PJyEyzzSBuhfmjFA4a3FO.WW', '2025-01-04 21:30:00.000', '2025-02-21 19:35:07.821', true);
INSERT INTO users
(user_id, user_name, email, "password", created_at, updated_at, is_verified)
VALUES(18, 'mike_brown', 'mike@example.com', '$2b$10$4.zg9A1kjdLwC2l8j/Lliex.NXjn1PJyEyzzSBuhfmjFA4a3FO.WW', '2025-01-05 23:30:00.000', '2025-02-21 00:35:52.409', true);
INSERT INTO users
(user_id, user_name, email, "password", created_at, updated_at, is_verified)
VALUES(19, 'sarah_davis', 'sarah@example.com', '$2b$10$4.zg9A1kjdLwC2l8j/Lliex.NXjn1PJyEyzzSBuhfmjFA4a3FO.WW', '2025-01-07 01:30:00.000', '2025-02-21 19:35:07.824', true);
INSERT INTO users
(user_id, user_name, email, "password", created_at, updated_at, is_verified)
VALUES(20, 'tom_wilson', 'tom@example.com', '$2b$10$4.zg9A1kjdLwC2l8j/Lliex.NXjn1PJyEyzzSBuhfmjFA4a3FO.WW', '2025-01-08 03:30:00.000', '2025-02-21 00:35:52.415', true);
INSERT INTO users
(user_id, user_name, email, "password", created_at, updated_at, is_verified)
VALUES(21, 'emma_clark', 'emma@example.com', '$2b$10$4.zg9A1kjdLwC2l8j/Lliex.NXjn1PJyEyzzSBuhfmjFA4a3FO.WW', '2025-01-08 14:30:00.000', '2025-02-21 19:35:07.828', true);
INSERT INTO users
(user_id, user_name, email, "password", created_at, updated_at, is_verified)
VALUES(22, 'brownpeacock316', 'ratushar46@gmail.com', NULL, '2025-02-21 00:29:57.963', '2025-02-21 00:29:57.963', true);

INSERT INTO thread
(thread_id, thread_title, thread_description, user_id, community_id, created_at, updated_at)
VALUES(16, 'Best way to learn Haskell', 'I''m new to Haskell and trying to figure out where to start. Does anyone have any recommendations for learning resources or communities?', 18, 1, '2025-02-23 13:11:55.572', '2025-02-23 13:11:55.572');
INSERT INTO thread
(thread_id, thread_title, thread_description, user_id, community_id, created_at, updated_at)
VALUES(20, 'Fitness Goals - Motivation and Advice', 'I''m trying to reach my fitness goals, but I need some motivation and advice from others. Can anyone share their experiences or tips?', 22, 1, '2025-02-23 13:14:23.124', '2025-02-23 13:14:23.124');
INSERT INTO thread
(thread_id, thread_title, thread_description, user_id, community_id, created_at, updated_at)
VALUES(17, 'What''s the point of functional programming?', 'I''ve been trying to understand the concept of functional programming, but I''m still confused. Can someone explain it in simple terms?', 14, 3, '2025-02-23 13:12:21.335', '2025-02-23 13:15:27.501');
INSERT INTO thread
(thread_id, thread_title, thread_description, user_id, community_id, created_at, updated_at)
VALUES(18, 'Foodie Frenzy - Best Restaurants in the City', 'I''m looking for some recommendations on the best restaurants in the city. Any foodies out there who can share their favorite spots?', 16, 5, '2025-02-23 13:12:51.646', '2025-02-23 13:15:27.513');
INSERT INTO thread
(thread_id, thread_title, thread_description, user_id, community_id, created_at, updated_at)
VALUES(19, 'Traveling on a Budget - Tips and Tricks', 'I''m planning a trip soon and want to save some money. Does anyone have any tips or tricks for traveling on a budget?', 17, 6, '2025-02-23 13:13:15.221', '2025-02-23 13:15:27.517');

INSERT INTO "comment"
(comment_id, user_id, thread_id, comment_content, parent_comment_id, created_at, updated_at)
VALUES(11, 14, 16, 'I started with the official Haskell book and then moved on to some online tutorials. I found the Haskell book to be really comprehensive and easy to understand.', NULL, '2025-02-23 15:39:12.195', '2025-02-23 15:39:12.195');
INSERT INTO "comment"
(comment_id, user_id, thread_id, comment_content, parent_comment_id, created_at, updated_at)
VALUES(12, 15, 16, 'I agree with user 3! I also found the official book to be really helpful. Have you tried any online resources, like Codecademy or FreeCodeCamp?', 11, '2025-02-23 15:39:24.779', '2025-02-23 15:45:08.242');
INSERT INTO "comment"
(comment_id, user_id, thread_id, comment_content, parent_comment_id, created_at, updated_at)
VALUES(13, 16, 16, 'I''ve been trying to learn Haskell for months now, but I just can''t seem to get the hang of it. Has anyone else struggled with this?', NULL, '2025-02-23 15:39:44.419', '2025-02-23 15:45:08.247');
INSERT INTO "comment"
(comment_id, user_id, thread_id, comment_content, parent_comment_id, created_at, updated_at)
VALUES(14, 14, 17, 'Functional programming is all about avoiding side effects and making your code more predictable and composable. It''s like a puzzle – you have to find the right 
pieces to fit together.', NULL, '2025-02-23 15:40:20.969', '2025-02-23 15:41:26.525');
INSERT INTO "comment"
(comment_id, user_id, thread_id, comment_content, parent_comment_id, created_at, updated_at)
VALUES(15, 17, 17, 'I like how functional programming encourages you to think about the code as a whole, rather than just focusing on individual lines. It''s really helped me improve 
my problem-solving skills.', 14, '2025-02-23 15:40:34.153', '2025-02-23 15:45:08.250');
INSERT INTO "comment"
(comment_id, user_id, thread_id, comment_content, parent_comment_id, created_at, updated_at)
VALUES(16, 20, 18, 'Hands down, my favorite spot is that little bistro on Main St. They have amazing pasta dishes and a great selection of craft beers.', NULL, '2025-02-23 15:41:52.586', '2025-02-23 15:45:08.253');
INSERT INTO "comment"
(comment_id, user_id, thread_id, comment_content, parent_comment_id, created_at, updated_at)
VALUES(17, 21, 18, 'I''ve been meaning to try that place out. Does anyone have any recommendations for brunch spots?', 16, '2025-02-23 15:42:09.501', '2025-02-23 15:45:08.257');
INSERT INTO "comment"
(comment_id, user_id, thread_id, comment_content, parent_comment_id, created_at, updated_at)
VALUES(18, 22, 19, 'One thing I always do when traveling is research my destination thoroughly. Look for free museums, parks, and walking tours to get the most out of your trip.', NULL, '2025-02-23 15:42:25.431', '2025-02-23 15:45:08.260');
INSERT INTO "comment"
(comment_id, user_id, thread_id, comment_content, parent_comment_id, created_at, updated_at)
VALUES(19, 18, 19, 'That''s a great idea! I also like to use travel apps like Google Maps and Rome2rio to find the best deals on flights, hotels, and activities.', 18, '2025-02-23 15:42:35.654', '2025-02-23 15:45:08.262');
INSERT INTO "comment"
(comment_id, user_id, thread_id, comment_content, parent_comment_id, created_at, updated_at)
VALUES(20, 18, 20, 'I''ve been trying to get into a regular workout routine, but it''s hard to stay motivated. Does anyone have any advice or tips for staying on track?', NULL, '2025-02-23 15:43:02.049', '2025-02-23 15:45:08.267');
INSERT INTO "comment"
(comment_id, user_id, thread_id, comment_content, parent_comment_id, created_at, updated_at)
VALUES(21, 19, 20, 'I''ve found that having a workout buddy really helps. Not only does it make exercise more fun, but you''re also accountable to each other.', 20, '2025-02-23 15:43:10.224', '2025-02-23 15:45:08.270');

INSERT INTO vote_thread
(user_id, thread_id, vote, created_at, updated_at)
VALUES(18, 16, true, '2025-02-23 13:13:31.030', '2025-02-23 13:13:31.030');
INSERT INTO vote_thread
(user_id, thread_id, vote, created_at, updated_at)
VALUES(18, 17, true, '2025-02-23 13:13:32.485', '2025-02-23 13:13:32.485');
INSERT INTO vote_thread
(user_id, thread_id, vote, created_at, updated_at)
VALUES(18, 18, true, '2025-02-23 13:13:33.482', '2025-02-23 13:13:33.482');
INSERT INTO vote_thread
(user_id, thread_id, vote, created_at, updated_at)
VALUES(18, 19, true, '2025-02-23 13:13:34.947', '2025-02-23 13:13:34.947');
INSERT INTO vote_thread
(user_id, thread_id, vote, created_at, updated_at)
VALUES(22, 20, true, '2025-02-23 13:14:27.019', '2025-02-23 13:14:27.019');
INSERT INTO vote_thread
(user_id, thread_id, vote, created_at, updated_at)
VALUES(22, 16, true, '2025-02-23 13:14:29.239', '2025-02-23 13:14:29.239');
INSERT INTO vote_thread
(user_id, thread_id, vote, created_at, updated_at)
VALUES(22, 17, true, '2025-02-23 13:14:30.126', '2025-02-23 13:14:30.126');
INSERT INTO vote_thread
(user_id, thread_id, vote, created_at, updated_at)
VALUES(22, 18, false, '2025-02-23 13:14:34.810', '2025-02-23 13:14:34.810');
INSERT INTO vote_thread
(user_id, thread_id, vote, created_at, updated_at)
VALUES(22, 19, true, '2025-02-23 13:14:35.883', '2025-02-23 13:14:35.883');
INSERT INTO vote_thread
(user_id, thread_id, vote, created_at, updated_at)
VALUES(14, 20, true, '2025-02-23 15:43:11.677', '2025-02-23 15:43:11.677');
INSERT INTO vote_thread
(user_id, thread_id, vote, created_at, updated_at)
VALUES(14, 16, true, '2025-02-23 15:43:19.684', '2025-02-23 15:43:19.684');
INSERT INTO vote_thread
(user_id, thread_id, vote, created_at, updated_at)
VALUES(14, 17, true, '2025-02-23 15:43:21.132', '2025-02-23 15:43:21.132');
INSERT INTO vote_thread
(user_id, thread_id, vote, created_at, updated_at)
VALUES(14, 18, false, '2025-02-23 15:43:23.529', '2025-02-23 15:43:23.529');
INSERT INTO vote_thread
(user_id, thread_id, vote, created_at, updated_at)
VALUES(14, 19, true, '2025-02-23 15:43:24.738', '2025-02-23 15:43:24.738');

INSERT INTO vote_comment
(comment_id, user_id, vote, created_at, updated_at)
VALUES(20, 14, true, '2025-02-23 15:43:13.740', '2025-02-23 15:43:13.740');
INSERT INTO vote_comment
(comment_id, user_id, vote, created_at, updated_at)
VALUES(21, 14, false, '2025-02-23 15:43:14.530', '2025-02-23 15:43:14.530');
INSERT INTO vote_comment
(comment_id, user_id, vote, created_at, updated_at)
VALUES(18, 14, true, '2025-02-23 15:43:27.025', '2025-02-23 15:43:27.025');
INSERT INTO vote_comment
(comment_id, user_id, vote, created_at, updated_at)
VALUES(19, 14, true, '2025-02-23 15:43:27.912', '2025-02-23 15:43:27.912');
INSERT INTO vote_comment
(comment_id, user_id, vote, created_at, updated_at)
VALUES(16, 14, false, '2025-02-23 15:43:32.663', '2025-02-23 15:43:32.663');
INSERT INTO vote_comment
(comment_id, user_id, vote, created_at, updated_at)
VALUES(17, 14, true, '2025-02-23 15:43:33.447', '2025-02-23 15:43:33.447');
INSERT INTO vote_comment
(comment_id, user_id, vote, created_at, updated_at)
VALUES(14, 14, false, '2025-02-23 15:43:39.552', '2025-02-23 15:43:39.552');
INSERT INTO vote_comment
(comment_id, user_id, vote, created_at, updated_at)
VALUES(15, 14, true, '2025-02-23 15:43:40.610', '2025-02-23 15:43:40.610');
INSERT INTO vote_comment
(comment_id, user_id, vote, created_at, updated_at)
VALUES(11, 14, true, '2025-02-23 15:43:47.618', '2025-02-23 15:43:47.618');
INSERT INTO vote_comment
(comment_id, user_id, vote, created_at, updated_at)
VALUES(12, 14, true, '2025-02-23 15:43:48.521', '2025-02-23 15:43:48.521');
INSERT INTO vote_comment
(comment_id, user_id, vote, created_at, updated_at)
VALUES(13, 14, true, '2025-02-23 15:43:49.775', '2025-02-23 15:43:49.775');

ALTER TABLE thread ADD COLUMN attachment text;
ALTER TABLE thread ADD COLUMN attachment_size integer;
ALTER TABLE thread ADD COLUMN attachment_name varchar(500);
alter table user_profile_image add column image_name text not null default 'example.png';
commit;

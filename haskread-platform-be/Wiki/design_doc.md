## HaskRead - Reddit clone written in Haskell.

### Design Document

- Author : Tushar Adhatrao
- Date : 2021-06-06
- Reviewer : Tushar Adhatrao

### Overview

This design document will provide you a brief of the main features of the application, the architecture of the application, and the technologies used in the application. Also, it will provide you with the details of the database schema and the API endpoints. The document
will also provide you with the details of the deployment process and the future scope of the application, as well as the challenges faced during the development of the application and the motivation behind certain design choices.

### Context

The goal of the project is to mimic the functionality of the Reddit platform. The application will allow users to create posts, comment on posts, upvote and downvote posts, and comments. The application will also allow users to create communities and join communities. The application will also have a search functionality to search for posts and communities. The application will also have a user profile page where users can see their posts, comments, and communities they have joined. The project will try to mimic the functionality of the Reddit platform as closely as possible.
The project will try to use the best practices of software development and will try to follow the principles of clean code, SOLID principles, and design patterns. The project will also try to use the best practices of functional programming and will try to use the best practices of Haskell programming language.

### Motivation

The motivation behind the project is to learn production grade haskell and it's best practices. Another motivation was to learn certain
libraries and frameworks. The project will also try to follow certain design patterns.

### Milestones

- Right now, the development has not started yet. Finding time to work on the project is itself a milestone. As we progress, we will update the milestones.

### Architecture

The application will be a monolithic application. The application will have a frontend and a backend. The backend will be written in `Haskell` using the `servant` library, the fronend will be written in `purescript`. The application will use `postgresql` as the database. The application will use `docker` for containerization. The application will use `github actions` for CI/CD.

### Technologies and libraries

- Servant : Servant is a haskell library for writing web servers. We will use servant for writing the backend of the application.
- Aeson : Aeson is a haskell library for parsing and encoding JSON. We will use aeson for parsing and encoding JSON in the application.
- Postgresql : Postgresql is a relational database. We will use postgresql as the database for the application.
- Docker : Docker is a containerization tool. We will use docker for containerizing the application.
- Github Actions : Github actions is a CI/CD tool. We will use github actions for CI/CD.
- Dhall : Dhall is a configuration language. We will use dhall for configuration management in the application.
- Haxl : Haxl is a library for data fetching. We will use haxl for data fetching in the application.
- Servant-auth-server : Servant-auth-server is a library for authentication. We will use servant-auth-server for authentication in the application.
- Orville-postgres : Orville-postgres is a library for interacting with postgresql. We will use orville-postgres for interacting with postgresql in the application.
- Type-level-programming : We will use type-level-programming for type-level programming in the application.
- Lens : Lens is a library for working with data structures. We will use lens for working with data structures in the application.
- Tasty : Tasty is a library for testing. We will use tasty for testing in the application.

### Some Notes:

- All password must be hashed before storing in the database.
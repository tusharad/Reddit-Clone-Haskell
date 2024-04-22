/localhost:3000/email_confirmation/?user_id&hash/encrypted
# HaskRead

HaskRead is a web-based blogging application designed to facilitate hands-on learning with the Scotty Web-Framework. This application allows users to experience the simplicity and power of Scotty alongside other technologies like Blaze-html for templating and PostgreSQL for database management.

## Technologies Used

- **Scotty**: A lightweight web framework for Haskell, inspired by Ruby's Sinatra.
- **Blaze-html**: A library for Haskell that provides an HTML templating engine.
- **PostgreSQL**: An open-source relational database system.

## Features

1. User Authentication using JWT.
2. CRUD on POST.
3. Nested Comments.


## Getting Started

To get HaskRead up and running on your local machine, follow these simple steps:

### Prerequisites

Ensure you have the following installed:
- [Git](https://git-scm.com/)
- [Cabal](https://www.haskell.org/cabal/download.html)
- [PostgreSQL](https://www.postgresql.org/download/)

### Installation

1. **Clone the repository**

   Open your terminal and clone the Scotty-Crud repository using Git:
   ```
   git clone https://github.com/yourusername/HaskRead.git
   ```
   Navigate into the project directory:
   ```
   cd HaskRead
   ```

2. **Set up the database**

   Make sure PostgreSQL is running. Create a new database for the project and note down the credentials; you'll need to configure these in the code.

3. **Configure your environment**

   Update the database configuration in the application to match your PostgreSQL settings.

4. **Run the application**

   Use Cabal to run the application:
   ```
   cabal run
   ```

The application should now be running on your local server. Open your web browser and navigate to the address provided in your terminal to start using HaskRead.



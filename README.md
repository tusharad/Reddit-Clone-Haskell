Certainly! Here's a README template for the Scotty-Crud GitHub repository based on the provided context:

---

# Scotty-Crud

Scotty-Crud is a web-based blogging application designed to facilitate hands-on learning with the Scotty Web-Framework. This application allows users to experience the simplicity and power of Scotty alongside other technologies like Blaze-html for templating and PostgreSQL for database management. Whether you're new to Haskell web frameworks or looking to sharpen your skills, Scotty-Crud offers a practical approach to understanding web application development with Scotty.

## Technologies Used

- **Scotty**: A lightweight web framework for Haskell, inspired by Ruby's Sinatra.
- **Blaze-html**: A library for Haskell that provides an HTML templating engine.
- **PostgreSQL**: An open-source relational database system.

## Features

1. **Login/Signup**: Secure authentication using JWT tokens ensures that users can safely login or sign up to access the application.
2. **Add Blogs**: Users can create and publish blogs, sharing their thoughts, knowledge, or any content they wish to put out there.
3. **Add Comments**: Engage with the community by adding comments to blogs, fostering an interactive environment.

## Getting Started

To get Scotty-Crud up and running on your local machine, follow these simple steps:

### Prerequisites

Ensure you have the following installed:
- [Git](https://git-scm.com/)
- [Cabal](https://www.haskell.org/cabal/download.html)
- [PostgreSQL](https://www.postgresql.org/download/)

### Installation

1. **Clone the repository**

   Open your terminal and clone the Scotty-Crud repository using Git:
   ```
   git clone https://github.com/yourusername/scotty-crud.git
   ```
   Navigate into the project directory:
   ```
   cd scotty-crud
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

The application should now be running on your local server. Open your web browser and navigate to the address provided in your terminal to start using Scotty-Crud.

## Contributing

Contributions to Scotty-Crud are welcome! Whether it's bug fixes, feature additions, or improvements to the documentation, your help is appreciated. Please feel free to fork the repository and submit a pull request with your changes.

## License

Scotty-Crud is released under the [MIT License](LICENSE). Feel free to use, modify, and distribute the code as you see fit.

---

/login - GET - login.html
/signup - GET - signup.html

/loginPerson - POST
/signupPerson - POST

name
email
password
store it in DB



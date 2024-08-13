### HaskRead Platform UI

This application is the base variant of the actual UI code. It would be written in HTML,CSS and JS using Bulma CSS framework.
This application would be helpful to create the skeleton structure, colorscheme of the application.

#### Directory structure:

- assests
    - JS
    - CSS
    - Images
- index.html

#### Tools and resources

- CSS framework: [Bulma CSS](https://bulma.io/)
- Icons: [Box Icons](https://boxicons.com/)

Hi, I am building a frontend application in HTML,CSS and JS with Bulma CSS framework. Please help me with it from time to time.
It should be responsive (mobile first approach).
The Frontend applcation is going to be a reddit clone, containing following things:
 - Home page:
    - Navbar:
    	- Logo on the right side with link to home. 
        - Search bar on the middle with search icon logo.
        - If user not logged in, a Login/Signup button.
        - If user logged in, a button with username with their profile picture.
    - List of posts:
        - A list of cards containing:
          1. community name at the top right corner in small.
          2. Bold centered post title.
          3. Small, grayed text description.
          4. Optional (Image,vide).
          5. Small botton row, containig upvote, downvote comment and share button.
          6. On right corner, same hight as posts, there should be name of the author.
             also, time and date (how old is the post)
          7. Pagination at the bottom
    - List of communities:
        - There should be a side bar on the left, containing list of communities.
    - List of recently, visited/ or trending posts.
        - On the right side, there should be a list of posts which are recently visited.
    - A footer bar, containing copyright and go to top button


Tips:
- Image Optimization: Use compressed images and modern formats like WebP.
- Lazy Loading: Implement lazy loading for images and other resources.
- Minification: Minify CSS, JS, and HTML files to reduce load times.
- Caching: Use browser caching and CDN for faster load times.
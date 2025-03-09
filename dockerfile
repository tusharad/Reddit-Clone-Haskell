FROM ubuntu:22.04

# Set the working directory to /home/app
WORKDIR /home/app

# Copy the backend and UI executables from the current directory to /home/app
COPY haskread-platform-be-exe .
COPY haskread-platform-ui-exe .

# Copy the static folder to /home/app/static
COPY static ./static

# Ensure both binaries have executable permissions
RUN chmod +x haskread-platform-be-exe haskread-platform-ui-exe

# Install Supervisord to manage multiple processes
RUN apt-get update && apt-get install -y supervisor libpq-dev

COPY Caddyfile .

RUN apt install -y debian-keyring debian-archive-keyring apt-transport-https curl && \
    curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/gpg.key' | gpg --dearmor -o /usr/share/keyrings/caddy-stable-archive-keyring.gpg && \
    curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/debian.deb.txt' | tee /etc/apt/sources.list.d/caddy-stable.list && \
    apt update && apt install caddy -y

# Set the environment variable for Google Cloud credentials
ENV GOOGLE_APPLICATION_CREDENTIALS=/home/app_config/some-service-account.json

EXPOSE 5000

# Start Supervisord to run both processes
CMD ["supervisord", "-c", "/home/app_config/supervisord.conf"]

FROM haskell:9.4.8-slim as builder

WORKDIR /code
COPY    . /code
EXPOSE  3001
RUN     cabal update
RUN     apt-get update && \
        apt-get install build-essential libpq-dev -y
RUN     cabal build
RUN     BUILD_PATH=$(cabal exec which Scotty-Crud) && \
        mkdir /app && \
        cp $BUILD_PATH /app && \
        cp /code/appsetting.json /app

FROM ubuntu:22.04
COPY --from=builder /app /app
COPY --from=builder /code/static /app/static
WORKDIR /app
RUN     apt-get update && \
        apt-get install libpq-dev -y && \
        mkdir uploads  
# Making uploads folder to store uploaded files 
CMD     [ "./Scotty-Crud","appsetting.json" ]
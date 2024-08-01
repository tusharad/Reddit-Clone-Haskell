sayHello:
	echo "Hello from makeFile"

startTestDB:
	docker run \
		--name haskread_test_pg_db \
		-e POSTGRES_PASSWORD=1234 \
		-e POSTGRES_USER=tushar \
		-e POSTGRES_DB=haskread_test_db \
		-p 5434:5432 \
		--rm -d \
		postgres:16.3-alpine3.20

stopTestDB:
	docker stop haskread_test_pg_db

startTest:
	echo "Running tests"
	make startTestDB
	sleep 4
	stack test
	make stopTestDB

startDB:
	docker run \
	--name haskread_dev_pg_db \
    -e POSTGRES_PASSWORD=1234 \
    -e POSTGRES_USER=tushar \
    -e POSTGRES_DB=haskread_dev_db \
    -v /home/user/haskell/Reddit-Clone-Haskell/haskread-platform-be/.dockermnt/pgDB:/var/lib/postgresql/data \
	-p 5433:5432 \
	--rm -d \
    postgres:16.3-alpine3.20

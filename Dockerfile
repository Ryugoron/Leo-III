# Start with installing scala and leo-III
FROM hseeberger/scala-sbt:8u222_1.3.5_2.13.1
MAINTAINER "max.wisniewski@fu-berlin.de"

RUN apt update -y
RUN apt-get install gcc make libc-dev -y

COPY . /app
WORKDIR /app
RUN make all

# Wrap Leo-III in a webserver
RUN apt-get install golang-go -y

RUN go build ./contrib/http/main.go

RUN ls

ENTRYPOINT ./main
EXPOSE 8080

#!/usr/bin/env bash
set -e

docker compose stop

docker compose rm -f

export $(cat .env | xargs)

docker compose up --build

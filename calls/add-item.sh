#! /usr/bin/env bash

# This demonstrates a call to /a/new/items to add an item to a todo list.

set -e


# Get logged in.

curl --cookie-jar a -d username="alice" -d password="secret" http://127.0.0.1:7249/login

echo Before:
curl --cookie a http://127.0.0.1:7249/a/lists
echo

echo Adding an item...
curl --cookie a \
  -d aiNamespace="alice" \
  -d aiTodoListName="start-servant" \
  -d aiDescription="Log calls to /a/new/items" \
  http://127.0.0.1:7249/a/new/items

echo After:
curl --cookie a http://127.0.0.1:7249/a/lists

rm a

# number-game
project for fun to make a game in Elm

## How to run it:
If you want to run this project, you will have to use a tool such as elm-live to run the app, and json-server to host the levels so that the app can read them
It's also important that you host the server on localhost:5019.

elm-live src/Main.elm --pushstate
json-server --watch server/level.json -a localhost -p 5019 --delay 1000

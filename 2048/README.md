
This directory is structured as follows.

* `src` has the source files of the game.
* `tests` has tests for the game logic.
* `doc` has support for generating the project's documentation.
* `useri` has a copy of the `js_of_ocaml` backend of `useri`.
* `support` some support files for developping the project.

To build the project use the `build` script. It supports the following
targets:

```
./build          # builds the app, the tests and the docs
./build app [-b] # builds the web app and reloads it in the browser (-b)
./build test     # builds and runs the tests
./build doc [-b] # builds project's documentation and reloads browser (-b)
./build clean    # cleans the project
```
